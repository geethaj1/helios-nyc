# NYC school air cleaning runs in endemic mode: flu with waning immunity and a
# small daily importation of infections, interventions in schools only.
#
# Design: for each replicate, a no-intervention burn-in is run once and its
# simulation state saved. Every arm (baseline and each intervention scenario) is
# then resumed from that saved state for the analysis period, so all arms within a
# replicate share the same population and epidemic history up to the point the
# intervention is installed.
#
# Notes on resuming (individual::simulation_loop):
#   - simulation_time for a resumed run is the absolute end day, not extra days.
#   - Timesteps keep counting from the saved state, so the intervention timestep is
#     the burn-in length in model timesteps.
#   - The population is rebuilt from the seed on resume, so the seed and all
#     population and ventilation settings must match the burn-in.
#
# Interventions are defined in inst/nyc/nyc_parameters.R (see
# nyc_school_intervention_scenarios).
#
# Run from the helios-nyc repository root. Prerequisite: helios-nyc installed.
#
# PROVISIONAL INPUTS (replace before reporting results):
#   - Transmission rates are the helios flu archetype values (R0 about 1.5 in the
#     NYC epidemic prototype), not yet formally recalibrated.
#   - Waning immunity and importation are assumed values (see below).

library(helios)

nyc_parameters_file <- file.path("inst", "nyc", "nyc_parameters.R")
if (!file.exists(nyc_parameters_file)) {
  nyc_parameters_file <- system.file("nyc", "nyc_parameters.R", package = "helios", mustWork = TRUE)
}
source(nyc_parameters_file)

# Set to TRUE for a quick end-to-end check with a small population and short runs.
smoke_test <- FALSE

intervention_name <- "ashrae_241"
population <- if (smoke_test) 10000 else 100000
initial_exposed <- if (smoke_test) 20 else 100
burn_in_days <- if (smoke_test) 60 else 3 * 365
analysis_days <- if (smoke_test) 60 else 5 * 365
n_reps <- if (smoke_test) 1 else 3
coverages <- c(0.4, 0.6, 0.8, 1.0)
coverage_types <- c("random", "targeted_riskiness")
school_ach_scenario <- "batterman"
output_dir <- file.path("results", if (smoke_test) "nyc_school_endemic_smoke" else "nyc_school_endemic")

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

# Immunity lasts one year on average (recovered individuals return to susceptible
# at rate 1 / 365 per day).
duration_immune_days <- 365

# prob_inf_external is a per-person daily rate of infection from outside the
# population. Setting it to 0.5 / population gives about 0.5 imported infections
# per day when everyone is susceptible, and proportionally fewer otherwise.
imported_infections_per_day <- 0.5

end_day <- burn_in_days + analysis_days

base_parameters <- function(seed, simulation_time) {
  parameters_list <- get_parameters(
    overrides = list(
      human_population = population,
      number_initial_S = population - initial_exposed,
      number_initial_E = initial_exposed,
      simulation_time = simulation_time,
      endemic_or_epidemic = "endemic",
      duration_immune = duration_immune_days,
      prob_inf_external = imported_infections_per_day / population,
      seed = seed
    ),
    archetype = "flu"
  )
  apply_nyc_parameters(parameters_list, school_ach_scenario = school_ach_scenario)
}

dt <- base_parameters(seed = 1, simulation_time = burn_in_days)$dt
burn_in_timesteps <- round(burn_in_days / dt)

arms <- rbind(
  data.frame(intervention = "none", coverage = 0, coverage_type = "none"),
  expand.grid(
    intervention = intervention_name,
    coverage = coverages,
    coverage_type = coverage_types,
    stringsAsFactors = FALSE
  )
)
arms$arm_id <- ifelse(
  arms$intervention == "none",
  "baseline",
  sprintf("%s_%s_%03d", arms$intervention, arms$coverage_type, round(100 * arms$coverage))
)
runs <- merge(arms, data.frame(rep = seq_len(n_reps)))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Parallel workers are separate R processes whose working directory is not
# guaranteed to match this session's, so paths they use are made absolute here.
nyc_parameters_file <- normalizePath(nyc_parameters_file, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

burn_in_path <- function(rep) file.path(output_dir, sprintf("burn_in_rep%02d.rds", rep))
arm_path <- function(arm_id, rep) file.path(output_dir, sprintf("arm_%s_rep%02d.rds", arm_id, rep))

# Completed runs are saved individually and skipped on rerun, so an interrupted
# batch can be restarted without repeating finished work.
run_burn_in <- function(rep) {
  path <- burn_in_path(rep)
  if (file.exists(path)) {
    return(path)
  }
  seed <- 1000 + rep
  started <- Sys.time()
  out <- run_simulation(base_parameters(seed, burn_in_days))
  saveRDS(
    list(
      rep = rep,
      seed = seed,
      result = out$result,
      state = out$state,
      runtime_secs = as.numeric(difftime(Sys.time(), started, units = "secs"))
    ),
    path
  )
  path
}

run_arm <- function(i) {
  run <- runs[i, ]
  path <- arm_path(run$arm_id, run$rep)
  if (file.exists(path)) {
    return(path)
  }
  burn_in <- readRDS(burn_in_path(run$rep))
  parameters_list <- base_parameters(burn_in$seed, end_day)
  if (run$intervention != "none") {
    parameters_list <- set_intervention_ach(
      parameters_list = parameters_list,
      setting = "school",
      coverage_target = "square_footage",
      coverage_type = run$coverage_type,
      timestep = burn_in_timesteps,
      intervention = nyc_school_intervention_scenarios[[run$intervention]](run$coverage)
    )
  }
  started <- Sys.time()
  out <- run_simulation(parameters_list, state = burn_in$state)
  result <- out$result[out$result$timestep > burn_in_timesteps, ]
  result$intervention <- run$intervention
  result$arm_id <- run$arm_id
  result$coverage <- run$coverage
  result$coverage_type <- run$coverage_type
  result$rep <- run$rep
  result$runtime_secs <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  saveRDS(result, path)
  path
}

check_failures <- function(outputs, label) {
  failed <- vapply(outputs, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop(label, " failed for runs: ", paste(which(failed), collapse = ", "), "\n", outputs[[which(failed)[1]]])
  }
}

# Objects the workers need. Each worker is a fresh R process, so everything the
# task functions reference must either be exported here or recreated by the
# library() and source() calls below.
worker_objects <- c(
  "nyc_parameters_file", "population", "initial_exposed", "burn_in_days",
  "analysis_days", "end_day", "school_ach_scenario", "intervention_name",
  "duration_immune_days", "imported_infections_per_day", "output_dir",
  "burn_in_timesteps", "runs", "base_parameters", "burn_in_path", "arm_path",
  "run_burn_in", "run_arm"
)

# A PSOCK cluster is used rather than parallel::mclapply(), which relies on
# forking and so is not available on Windows. Tasks are wrapped in try() so that
# one failure does not abort the batch, matching check_failures() below.
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

message(sprintf(
  "Endemic batch: population %s, burn-in %d days, analysis %d days, %d arms x %d replicates, %d cores, intervention: %s",
  format(population, big.mark = ","), burn_in_days, analysis_days, nrow(arms), n_reps, n_cores, intervention_name
))

started <- Sys.time()
burn_in_outputs <- run_in_parallel(seq_len(n_reps), "run_burn_in")
check_failures(burn_in_outputs, "Burn-in")
message(sprintf("Burn-in finished after %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))

arm_outputs <- run_in_parallel(seq_len(nrow(runs)), "run_arm")
check_failures(arm_outputs, "Arm runs")
message(sprintf("All runs finished after %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))

# Summaries over the analysis period. E_new counts infections from transmission
# within the population; n_external_infections counts imported infections.
results <- do.call(rbind, lapply(unlist(arm_outputs), readRDS))
results$day <- results$timestep * dt
results$analysis_year <- pmin(ceiling((results$day - burn_in_days) / 365), ceiling(analysis_days / 365))

deaths_at_burn_in_end <- vapply(seq_len(n_reps), function(rep) {
  tail(readRDS(burn_in_path(rep))$result$D_count, 1)
}, numeric(1))

totals <- do.call(rbind, lapply(split(results, list(results$arm_id, results$rep), drop = TRUE), function(d) {
  data.frame(
    arm_id = d$arm_id[1],
    intervention = d$intervention[1],
    coverage = d$coverage[1],
    coverage_type = d$coverage_type[1],
    rep = d$rep[1],
    transmitted_infections = sum(d$E_new, na.rm = TRUE),
    imported_infections = sum(d$n_external_infections, na.rm = TRUE),
    hospitalisations = sum(d$H_new, na.rm = TRUE),
    deaths = max(d$D_count, na.rm = TRUE) - deaths_at_burn_in_end[d$rep[1]]
  )
}))
totals$infections <- totals$transmitted_infections + totals$imported_infections
totals$annual_infections_per_100k <- totals$infections / (analysis_days / 365) / population * 1e5

baseline <- totals[totals$intervention == "none", c("rep", "infections")]
names(baseline)[2] <- "baseline_infections"
totals <- merge(totals, baseline, by = "rep")
totals$infections_averted_pct <- 100 * (1 - totals$infections / totals$baseline_infections)

summary_table <- aggregate(
  cbind(annual_infections_per_100k, infections_averted_pct, imported_infections, hospitalisations, deaths) ~ arm_id + intervention + coverage + coverage_type,
  data = totals,
  FUN = mean
)
summary_table <- summary_table[order(summary_table$coverage_type, summary_table$coverage), ]

# Baseline infections by analysis year, to check that the burn-in reached a
# stable endemic level.
baseline_by_year <- aggregate(
  cbind(E_new, n_external_infections) ~ rep + analysis_year,
  data = results[results$intervention == "none", ],
  FUN = sum
)
baseline_by_year$infections <- baseline_by_year$E_new + baseline_by_year$n_external_infections

print(summary_table, row.names = FALSE, digits = 3)
cat("\nBaseline infections by analysis year (per replicate):\n")
print(baseline_by_year[order(baseline_by_year$rep, baseline_by_year$analysis_year), c("rep", "analysis_year", "infections")], row.names = FALSE)

saveRDS(
  list(
    settings = list(
      population = population,
      burn_in_days = burn_in_days,
      analysis_days = analysis_days,
      n_reps = n_reps,
      duration_immune_days = duration_immune_days,
      imported_infections_per_day = imported_infections_per_day,
      school_ach_scenario = school_ach_scenario,
      intervention = intervention_name,
      ashrae_241_target_ach = ashrae_241_target_ach
    ),
    runs = runs,
    totals = totals,
    summary = summary_table,
    baseline_by_year = baseline_by_year
  ),
  file.path(output_dir, "endemic_summary.rds")
)
message("Saved summary to ", file.path(output_dir, "endemic_summary.rds"))
