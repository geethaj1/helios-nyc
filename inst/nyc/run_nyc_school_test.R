# NYC school air cleaning runs: epidemic flu, interventions in schools only, at
# several coverage levels with random and risk-targeted placement, compared against
# a no-intervention baseline.
#
# Interventions are defined in inst/nyc/nyc_parameters.R:
#   - ashrae_241: covered schools brought up to the ASHRAE Standard 241 classroom
#     target (about 9.2 eACH in total for a typical NYC classroom)
#   - nyc_current_purifiers: NYC's current classroom purifiers (about +2.5 eACH)
#
# Run from the helios-nyc repository root. Prerequisite: helios-nyc installed,
# e.g. devtools::install("~/helios-nyc").
#
# PROVISIONAL INPUT: transmission rates are the helios flu archetype values, which
# give R0 about 1.5 in the NYC prototype but have not been formally recalibrated.

library(helios)

nyc_parameters_file <- file.path("inst", "nyc", "nyc_parameters.R")
if (!file.exists(nyc_parameters_file)) {
  nyc_parameters_file <- system.file("nyc", "nyc_parameters.R", package = "helios", mustWork = TRUE)
}
source(nyc_parameters_file)

# Set to TRUE for a quick end-to-end check with a small population.
smoke_test <- FALSE

intervention_names <- c("ashrae_241")
population <- if (smoke_test) 10000 else 100000
initial_exposed <- if (smoke_test) 20 else 100
simulation_days <- if (smoke_test) 30 else 200
n_reps <- if (smoke_test) 1 else 3
coverages <- c(0.4, 0.6, 0.8, 1.0)
coverage_types <- c("random", "targeted_riskiness")
school_ach_scenario <- "batterman"
output_dir <- file.path("results", if (smoke_test) "nyc_school_epidemic_smoke" else "nyc_school_epidemic")

# Use every physical core but one, leaving the remaining core for the operating
# system. Logical (hyperthreaded) cores are not counted: simulations are compute
# bound, so running one worker per logical core oversubscribes the machine and
# slows the batch down. Set HELIOS_NYC_CORES to override.
detect_cores <- function() {
  override <- Sys.getenv("HELIOS_NYC_CORES", "")
  if (nzchar(override)) {
    override <- suppressWarnings(as.integer(override))
    if (!is.na(override) && override >= 1L) {
      return(override)
    }
    warning("HELIOS_NYC_CORES is not a positive integer; ignoring it.")
  }
  n <- parallel::detectCores(logical = FALSE)
  if (is.na(n) || n < 1L) {
    n <- parallel::detectCores(logical = TRUE)
  }
  if (is.na(n) || n < 1L) {
    n <- 1L
  }
  max(1L, n - 1L)
}
n_cores <- detect_cores()

# Parallel workers are separate R processes whose working directory is not
# guaranteed to match this session's, so paths they use are made absolute here.
nyc_parameters_file <- normalizePath(nyc_parameters_file, winslash = "/", mustWork = TRUE)

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
  data.frame(intervention = "none", coverage = 0, coverage_type = "none"),
  expand.grid(
    intervention = intervention_names,
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
  if (run$intervention != "none") {
    parameters_list <- set_intervention_ach(
      parameters_list = parameters_list,
      setting = "school",
      coverage_target = "square_footage",
      coverage_type = run$coverage_type,
      timestep = 1,
      intervention = nyc_school_intervention_scenarios[[run$intervention]](run$coverage)
    )
  }
  started <- Sys.time()
  result <- run_simulation(parameters_list)$result
  result$intervention <- run$intervention
  result$coverage <- run$coverage
  result$coverage_type <- run$coverage_type
  result$rep <- run$rep
  result$seed <- run$seed
  result$runtime_secs <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  result
}

message(sprintf(
  "Running %d simulations (%d arms x %d replicates) on %d cores: population %s, %d days, interventions: %s",
  nrow(runs), nrow(arms), n_reps, n_cores, format(population, big.mark = ","), simulation_days,
  paste(intervention_names, collapse = ", ")
))
message(sprintf(
  "ASHRAE 241 classroom target: %.1f eACH; NYC current purifiers: +%.1f eACH",
  ashrae_241_target_ach, nyc_current_purifier_ach
))
# Objects the workers need. Each worker is a fresh R process, so everything
# run_one() references must either be exported here or recreated by the
# library() and source() calls below.
worker_objects <- c(
  "nyc_parameters_file", "population", "initial_exposed", "simulation_days",
  "school_ach_scenario", "runs", "base_parameters", "run_one"
)

# A PSOCK cluster is used rather than parallel::mclapply(), which relies on
# forking and so is not available on Windows. Tasks are wrapped in try() so that
# one failure does not abort the batch, matching the check below.
run_in_parallel <- function(x, fun_name) {
  workers <- min(n_cores, length(x))
  if (workers <= 1L) {
    return(lapply(x, function(i) try(do.call(fun_name, list(i)), silent = TRUE)))
  }
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, worker_objects, envir = environment())
  parallel::clusterEvalQ(cl, {
    library(helios)
    source(nyc_parameters_file)
    NULL
  })
  parallel::parLapplyLB(cl, x, function(i) try(do.call(fun_name, list(i)), silent = TRUE))
}

started <- Sys.time()
outputs <- run_in_parallel(seq_len(nrow(runs)), "run_one")
failed <- vapply(outputs, inherits, logical(1), what = "try-error")
if (any(failed)) {
  stop("Simulations failed for runs: ", paste(which(failed), collapse = ", "), "\n", outputs[[which(failed)[1]]])
}
results <- do.call(rbind, outputs)
message(sprintf("Finished in %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))

# Per-run totals. E_new counts new infections per timestep; seeded exposures are
# added so the total covers everyone infected during the epidemic.
totals <- do.call(rbind, lapply(split(results, list(results$intervention, results$coverage, results$coverage_type, results$rep), drop = TRUE), function(d) {
  data.frame(
    intervention = d$intervention[1],
    coverage = d$coverage[1],
    coverage_type = d$coverage_type[1],
    rep = d$rep[1],
    infections = sum(d$E_new, na.rm = TRUE) + initial_exposed,
    hospitalisations = sum(d$H_new, na.rm = TRUE),
    deaths = max(d$D_count, na.rm = TRUE),
    still_infectious_at_end = tail(d$I_mild_count + d$I_hosp_count, 1)
  )
}))

baseline <- totals[totals$intervention == "none", c("rep", "infections")]
names(baseline)[2] <- "baseline_infections"
totals <- merge(totals, baseline, by = "rep")
totals$attack_rate <- totals$infections / population
totals$infections_averted_pct <- 100 * (1 - totals$infections / totals$baseline_infections)

summary_table <- aggregate(
  cbind(attack_rate, infections_averted_pct, hospitalisations, deaths, still_infectious_at_end) ~ intervention + coverage + coverage_type,
  data = totals,
  FUN = mean
)
summary_table <- summary_table[order(summary_table$intervention, summary_table$coverage_type, summary_table$coverage), ]
print(summary_table, row.names = FALSE, digits = 3)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    settings = list(
      population = population,
      simulation_days = simulation_days,
      n_reps = n_reps,
      school_ach_scenario = school_ach_scenario,
      ashrae_241_target_ach = ashrae_241_target_ach,
      nyc_current_purifier_ach = nyc_current_purifier_ach
    ),
    runs = runs,
    results = results,
    totals = totals,
    summary = summary_table
  ),
  file.path(output_dir, "nyc_school_epidemic.rds")
)
message("Saved to ", file.path(output_dir, "nyc_school_epidemic.rds"))
