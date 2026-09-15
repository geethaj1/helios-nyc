# Prototype NYC school intervention runs: epidemic flu, portable air cleaners in
# schools only, at several coverage levels with random and risk-targeted
# placement, compared against a no-intervention baseline.
#
# Prerequisites:
#   1. data/schools_nyc.rda built by data-raw/DATASET.R
#   2. helios-nyc installed, e.g. devtools::install("~/helios-nyc")
#
# PROVISIONAL INPUTS (replace before reporting results):
#   - Transmission rates are the helios flu archetype values, which have not yet
#     been recalibrated to R0 ~1.5 for the NYC population.
#   - The air cleaner effect is a placeholder distribution (see air_cleaner below).

library(helios)

source(system.file("nyc", "nyc_parameters.R", package = "helios", mustWork = TRUE))

# Set to TRUE for a quick end-to-end check with a small population.
smoke_test <- FALSE

population <- if (smoke_test) 10000 else 100000
initial_exposed <- if (smoke_test) 20 else 100
simulation_days <- if (smoke_test) 30 else 250
n_reps <- if (smoke_test) 1 else 3
coverages <- c(0.4, 0.6, 0.8, 1.0)
coverage_types <- c("random", "targeted_riskiness")
school_ach_scenario <- "batterman"
n_cores <- min(parallel::detectCores() - 1, 8)
output_dir <- file.path("results", "nyc_school_test")

# Placeholder portable air cleaner effect: mean +3.5 eACH per covered school,
# between NYC's deployed purifiers (~2.5 eACH: two Intellipure units, CADR
# 129-145 cfm each, in a ~184 m3 classroom) and purifiers sized to the CDC
# 5 eACH target. A lognormal multiplier with sdlog = 0.4 (mean 1) varies the
# effect between schools, giving roughly 1.6-7.6 eACH across the central 95%.
make_air_cleaner <- function(coverage) {
  make_intervention(
    name = "portable_air_cleaner",
    delta_function = function(delta) delta,
    delta_params = list(delta = 3.5),
    variation = TRUE,
    variation_params = list(sdlog = 0.4),
    coverage = coverage
  )
}

base_parameters <- function(seed) {
  parameters_list <- get_parameters(
    overrides = list(
      human_population = population,
      number_initial_S = population - initial_exposed,
      number_initial_E = initial_exposed,
      simulation_time = simulation_days,
      endemic_or_epidemic = "epidemic",
      seed = seed
    ),
    archetype = "flu"
  )
  apply_nyc_parameters(parameters_list, school_ach_scenario = school_ach_scenario)
}

# One row per simulation. The seed depends only on the replicate, so every arm
# within a replicate shares the same population and random stream up to the
# point where the intervention allocation begins.
arms <- rbind(
  data.frame(arm = "baseline", coverage = 0, coverage_type = "none"),
  expand.grid(
    arm = "air_cleaner",
    coverage = coverages,
    coverage_type = coverage_types,
    stringsAsFactors = FALSE
  )
)
runs <- merge(arms, data.frame(rep = seq_len(n_reps)))
runs$seed <- 1000 + runs$rep

run_one <- function(i) {
  run <- runs[i, ]
  parameters_list <- base_parameters(run$seed)
  if (run$arm == "air_cleaner") {
    parameters_list <- set_intervention_ach(
      parameters_list = parameters_list,
      setting = "school",
      coverage_target = "square_footage",
      coverage_type = run$coverage_type,
      timestep = 1,
      intervention = make_air_cleaner(run$coverage)
    )
  }
  started <- Sys.time()
  result <- run_simulation(parameters_list)$result
  result$arm <- run$arm
  result$coverage <- run$coverage
  result$coverage_type <- run$coverage_type
  result$rep <- run$rep
  result$seed <- run$seed
  result$runtime_secs <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  result
}

message(sprintf(
  "Running %d simulations (%d arms x %d replicates) on %d cores: population %s, %d days",
  nrow(runs), nrow(arms), n_reps, n_cores, format(population, big.mark = ","), simulation_days
))
started <- Sys.time()
outputs <- parallel::mclapply(seq_len(nrow(runs)), run_one, mc.cores = n_cores, mc.preschedule = FALSE)
failed <- vapply(outputs, inherits, logical(1), what = "try-error")
if (any(failed)) {
  stop("Simulations failed for runs: ", paste(which(failed), collapse = ", "), "\n", outputs[[which(failed)[1]]])
}
results <- do.call(rbind, outputs)
message(sprintf("Finished in %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))

# Per-run totals. E_new counts new infections per timestep; seeded exposures are
# added so the total covers everyone infected during the epidemic.
totals <- do.call(rbind, lapply(split(results, list(results$arm, results$coverage, results$coverage_type, results$rep), drop = TRUE), function(d) {
  data.frame(
    arm = d$arm[1],
    coverage = d$coverage[1],
    coverage_type = d$coverage_type[1],
    rep = d$rep[1],
    infections = sum(d$E_new, na.rm = TRUE) + initial_exposed,
    hospitalisations = sum(d$H_new, na.rm = TRUE),
    deaths = max(d$D_count, na.rm = TRUE),
    peak_infectious = max(d$I_mild_count + d$I_hosp_count, na.rm = TRUE),
    still_infectious_at_end = tail(d$I_mild_count + d$I_hosp_count, 1)
  )
}))

baseline <- totals[totals$arm == "baseline", c("rep", "infections")]
names(baseline)[2] <- "baseline_infections"
totals <- merge(totals, baseline, by = "rep")
totals$attack_rate <- totals$infections / population
totals$infections_averted_pct <- 100 * (1 - totals$infections / totals$baseline_infections)

summary_table <- aggregate(
  cbind(attack_rate, infections_averted_pct, hospitalisations, deaths, still_infectious_at_end) ~ arm + coverage + coverage_type,
  data = totals,
  FUN = mean
)
summary_table <- summary_table[order(summary_table$coverage_type, summary_table$coverage), ]
print(summary_table, row.names = FALSE, digits = 3)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(runs = runs, results = results, totals = totals, summary = summary_table), file.path(output_dir, "nyc_school_test.rds"))
message("Saved to ", file.path(output_dir, "nyc_school_test.rds"))
