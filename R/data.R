#' Data on English schools from the 2022/2023 School Census
#'
#' A data frame with 24,442 rows and 268 columns.
#'
#' @source <https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics/2022-23>
#' @family data
"schools_uk"

#' Data on US schools from 2019/2020 from the National Center for Education Statistics
#'
#' A data frame with 91 rows and 6 columns.
#'
#' @source <https://nces.ed.gov/programs/digest/d21/tables/dt21_216.40.asp>
#' @family data
"schools_usa"

#' Data from ONS 2011 on baseline household demographics from Hinch et al.
#'
#' A data frame with 10,000 rows and 9 columns.
#'
#' @source <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009146>
#' @family data
"baseline_household_demographics_uk"

#' Household age composition for New York City from the RTI synthetic population
#'
#' One row per household in the five boroughs of New York City, giving the number
#' of children (aged 18 and under), adults (19 to 69) and elderly people (70 and
#' over). Generated with the RTI synthetic population pipeline from the 2015-2019
#' American Community Survey, using stochastic rounding of household counts (see
#' `data-raw/DATASET.R`).
#'
#' A data frame with 3,166,264 rows and 3 columns.
#'
#' @source <https://github.com/RTIInternational/rti_synth_pop>
#' @family data
"baseline_household_demographics_usa"

#' New York City public schools, 2018-19 school year
#'
#' Open and new public schools in the five boroughs of New York City with reported
#' enrollment, from the NCES Common Core of Data school directory. Used to sample
#' school sizes when `school_distribution_country = "USA"`.
#'
#' A data frame with 1,817 rows and 11 columns:
#' * `ncessch`: NCES school identifier
#' * `school_name`: school name
#' * `county_code`: county FIPS code (borough)
#' * `street_location`, `zip_location`: school address
#' * `school_type`: NCES school type (1 regular, 2 special education, 3 career and technical, 4 alternative)
#' * `charter`: 1 if a charter school
#' * `lowest_grade_offered`, `highest_grade_offered`: grade range (-1 pre-kindergarten, 0 kindergarten)
#' * `enrollment`: number of students enrolled
#' * `teachers_fte`: full-time-equivalent teachers
#'
#' @source <https://educationdata.urban.org/documentation/schools.html>
#' @family data
"schools_nyc"
