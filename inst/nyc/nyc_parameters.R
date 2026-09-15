# NYC analysis settings applied on top of get_parameters().
#
# Source this file from calibration and simulation scripts so that every run
# uses the same NYC-specific choices, for example:
#   source("inst/nyc/nyc_parameters.R")
#   params <- apply_nyc_parameters(get_parameters(archetype = "sars_cov_2"))
#
# Household size and age composition need no setting here: with the default
# household_distribution_country = "USA", helios-nyc samples the NYC five-borough
# RTI synthetic population (see data-raw/DATASET.R).

# School ventilation (air changes per hour) scenarios.
#
# Main analysis ("batterman"): Batterman et al. (2017), Indoor Air,
# doi:10.1111/ina.12384, measured school-day classroom air change rates of
# 2.0 +/- 1.3 per hour in 37 recently constructed or renovated US schools.
# helios draws each school's ACH from a normal distribution truncated at 0, so
# the inputs were solved to give a realised mean of 2.0 and SD of 1.3.
#
# Sensitivity analysis ("batterman_untruncated"): the published values entered
# directly, which give a realised mean of about 2.17 and SD of about 1.15.
nyc_school_ach_scenarios <- list(
  batterman = list(mean = 1.35, sd = 1.73),
  batterman_untruncated = list(mean = 2.0, sd = 1.3)
)

# Students per school staff member. Computed from NYC public schools in 2018-19
# (schools_nyc) as total enrollment divided by total full-time-equivalent teachers,
# across schools reporting teachers (about 13.5, versus the helios default of 20).
# Only teachers are counted, so non-teaching staff are not represented.
nyc_school_student_staff_ratio <- with(
  schools_nyc[!is.na(schools_nyc$teachers_fte) & schools_nyc$teachers_fte > 0, ],
  sum(enrollment) / sum(teachers_fte)
)

apply_nyc_parameters <- function(parameters_list, school_ach_scenario = "batterman") {
  if (!(school_ach_scenario %in% names(nyc_school_ach_scenarios))) {
    stop(
      "school_ach_scenario must be one of: ",
      paste(names(nyc_school_ach_scenarios), collapse = ", ")
    )
  }
  school_ach <- nyc_school_ach_scenarios[[school_ach_scenario]]
  parameters_list <- set_setting_specific_ach(
    parameters_list,
    setting = "school",
    mean = school_ach$mean,
    sd = school_ach$sd
  )
  parameters_list$school_student_staff_ratio <- nyc_school_student_staff_ratio
  parameters_list
}
