# Make any changes to the saved data for the package here
library(readr)
library(readxl)
library(dplyr)
library(janitor)

baseline_household_demographics_uk <- read_csv(
  "data-raw/Hinch_et_al_baseline_household_demographics.csv"
)
baseline_household_demographics_uk$child <- baseline_household_demographics_uk$a_0_9 +
  baseline_household_demographics_uk$a_10_19
baseline_household_demographics_uk$adult <- baseline_household_demographics_uk$a_20_29 +
  baseline_household_demographics_uk$a_30_39 +
  baseline_household_demographics_uk$a_40_49 +
  baseline_household_demographics_uk$a_50_59 +
  baseline_household_demographics_uk$a_60_69
baseline_household_demographics_uk$elderly <- baseline_household_demographics_uk$a_70_79 +
  baseline_household_demographics_uk$a_80
baseline_household_demographics_uk <- baseline_household_demographics_uk[, c(
  "child",
  "adult",
  "elderly"
)]
usethis::use_data(baseline_household_demographics_uk, overwrite = TRUE)

schools_uk <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_uk, overwrite = TRUE)

# Just the most recent years data (2019 / 2020)
schools_usa <- readxl::read_xls("data-raw/tabn216.40.xls") |>
  tibble::rowid_to_column("rowid") |>
  filter(rowid %in% c(2, 3, 39:53))

# Generate appropriate column names
schools_usa[is.na(schools_usa)] <- ""
names(schools_usa) <- mapply(paste0, schools_usa[1, ], schools_usa[2, ])

schools_usa <- schools_usa |>
  janitor::clean_names() |>
  dplyr::filter(!x23 %in% c(2, 3)) |>
  select(2:9) |>
  dplyr::filter(!x == "Percent") |>
  tidyr::pivot_longer(cols = -x, values_to = "percent", names_to = "type") |>
  dplyr::mutate(
    type = forcats::fct_recode(
      type,
      "total" = "total_1",
      "prekindergarten" = "pre_kinder_garten",
      "elementary" = "elemen_tary",
      "secondary_and_high_all" = "secondary_and_high_all_schools",
      "secondary_and_high_regular" = "regular_schools_1",
      "other" = "other_ungraded_and_not_applicable_not_reported"
    )
  )

schools_usa <- schools_usa |>
  filter(x != "Total") |>
  left_join(
    filter(schools_usa, x == "Total") |>
      select(type, total = percent),
    by = c("type")
  ) |>
  rename(size = x) |>
  mutate(
    size_midpoint = dplyr::case_when(
      size == "Under 100" ~ 50,
      size == "100 to 199" ~ 150,
      size == "200 to 299" ~ 250,
      size == "300 to 399" ~ 350,
      size == "400 to 499" ~ 450,
      size == "500 to 599" ~ 550,
      size == "600 to 699" ~ 650,
      size == "700 to 799" ~ 750,
      size == "800 to 999" ~ 900,
      size == "1,000 to 1,499" ~ 1250,
      size == "1,500 to 1,999" ~ 1750,
      size == "2,000 to 2,999" ~ 2500,
      size == "3,000 or more" ~ 3500
    ),
    percent = as.numeric(percent),
    total = as.numeric(total),
    count = percent / 100 * total,
    year = 2019
  ) |>
  filter(type != "secondary_and_high_regular") |> # category "secondary_and_high_regular" is a subset of "secondary_and_high_all"
  arrange(type) |>
  select(year, type, size, size_midpoint, percent, total, count)
usethis::use_data(schools_usa, overwrite = TRUE)

# New York City (five boroughs) household age composition from the RTI synthetic
# population, generated with https://github.com/RTIInternational/rti_synth_pop
# (2019 ACS 5-year, STATE_INFO = [("NY", "36")]). Task 6 was modified to use
# stochastic rounding of IPF counts; the original nearest-integer rounding dropped
# about a fifth of NYC households and under-represented large households.
# The pipeline outputs are not stored in this repository; set rti_data_dir to the
# local rti_synth_pop data directory.
rti_data_dir <- path.expand("~/rti_synth_pop/data")
nyc_county_fips <- c("36005", "36047", "36061", "36081", "36085")

nyc_households <- arrow::read_parquet(
  file.path(rti_data_dir, "interim", "36_2019_households.parquet"),
  col_select = c("hh_id", "county_fips")
) |>
  dplyr::filter(county_fips %in% nyc_county_fips)

nyc_persons <- arrow::read_parquet(
  file.path(rti_data_dir, "processed", "36_2019_persons.parquet"),
  col_select = c("hh_id", "agep")
) |>
  dplyr::semi_join(nyc_households, by = "hh_id")

# Age classes use the same cut-offs as the previous San Francisco panel.
baseline_household_demographics_usa <- nyc_persons |>
  dplyr::group_by(hh_id) |>
  dplyr::summarise(
    child = sum(agep <= 18),
    adult = sum(agep > 18 & agep <= 69),
    elderly = sum(agep >= 70),
    .groups = "drop"
  ) |>
  dplyr::select(child, adult, elderly)

stopifnot(
  nrow(baseline_household_demographics_usa) == nrow(nyc_households),
  all(rowSums(baseline_household_demographics_usa) >= 1)
)
usethis::use_data(baseline_household_demographics_usa, overwrite = TRUE)

# New York City (five boroughs) public schools for the 2018-19 school year, from
# the NCES Common Core of Data school directory. Downloaded from the Urban
# Institute Education Data Portal API
# (https://educationdata.urban.org/api/v1/schools/ccd/directory/2018/?fips=36)
# and restricted to county codes 36005, 36047, 36061, 36081 and 36085.
# 2018-19 matches the period of the NYC household data (2015-2019 ACS).
# A HIFLD redistribution labelled 2017-18 was rejected because it mixed school
# years and contained duplicate schools.
# Open (status 1) and new (status 3) schools with reported enrollment are kept;
# closed and inactive schools, and schools without an enrollment figure, are
# excluded.
schools_nyc <- readr::read_csv(
  "data-raw/nyc_public_schools_ccd_2018_19.csv",
  col_types = readr::cols(
    ncessch = "c",
    county_code = "c",
    zip_location = "c"
  )
) |>
  dplyr::filter(school_status %in% c(1, 3), enrollment > 0) |>
  dplyr::select(
    ncessch,
    school_name,
    county_code,
    street_location,
    zip_location,
    school_type,
    charter,
    lowest_grade_offered,
    highest_grade_offered,
    enrollment,
    teachers_fte
  )

stopifnot(
  !anyDuplicated(schools_nyc$ncessch),
  all(schools_nyc$county_code %in% nyc_county_fips)
)
usethis::use_data(schools_nyc, overwrite = TRUE)
