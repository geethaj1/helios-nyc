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

# Air cleaning intervention scenarios for schools, with effects in equivalent air
# changes per hour (eACH).
#
# Typical NYC classroom: about 650 sq ft with 10 ft ceilings (about 184 m3) and
# about 25 occupants (NYC average class size of 23.5-25.8 in 2023-26, plus a
# teacher), giving about 7.4 m3 of air per occupant.
cfm_to_m3_per_hour <- 1.699011
nyc_classroom_volume_m3 <- 650 * 10 * 0.0283168
nyc_classroom_occupants <- 25

# ASHRAE Standard 241 (2023) requires 40 cfm of equivalent clean airflow per person
# in classrooms, counting outdoor air ventilation, filtration and air cleaning
# together. For a typical NYC classroom this is about 9.2 eACH in total.
ashrae_241_classroom_cfm_per_person <- 40
ashrae_241_target_ach <- ashrae_241_classroom_cfm_per_person * cfm_to_m3_per_hour *
  nyc_classroom_occupants / nyc_classroom_volume_m3

# NYC's current classroom purifiers: two Intellipure Compact units per classroom
# with a clean air delivery rate of about 129-145 cfm each (independent and
# manufacturer testing), about 2.5 eACH at full fan speed in a typical classroom.
nyc_current_purifier_cfm_per_classroom <- 2 * 137
nyc_current_purifier_ach <- nyc_current_purifier_cfm_per_classroom * cfm_to_m3_per_hour /
  nyc_classroom_volume_m3

nyc_school_intervention_scenarios <- list(
  # Covered schools are brought up to the ASHRAE 241 target: each receives the
  # additional clean air its baseline ventilation lacks, and schools already at or
  # above the target receive none.
  ashrae_241 = function(coverage) {
    make_intervention(
      name = "ashrae_241",
      delta_depends_on_baseline_ach = TRUE,
      delta_function = function(ach, target) max(target - ach, 0),
      delta_params = list(target = ashrae_241_target_ach),
      coverage = coverage
    )
  },
  nyc_current_purifiers = function(coverage) {
    make_intervention(
      name = "nyc_current_purifiers",
      delta_function = function(delta) delta,
      delta_params = list(delta = nyc_current_purifier_ach),
      coverage = coverage
    )
  }
)
