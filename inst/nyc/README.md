# NYC analysis: setup on a new machine

Everything here runs on macOS, Linux and Windows. Run all scripts from the
repository root, not from this directory.

## 1. Clone

The repository carries a large amount of legacy simulation output inherited from
upstream helios (roughly 2.8 GB of working tree and 1.2 GB of history). A shallow
clone of the working branch skips the history and is much faster:

```
git clone --depth 1 --branch ach_efficacy_update https://github.com/geethaj1/helios-nyc.git
```

If you need the full history later, `git fetch --unshallow` converts the clone in
place.

On Windows, clone to a short path such as `C:/helios-nyc`. Some Windows tooling
still has trouble with paths longer than 260 characters, and the legacy
`inst/blueprint_output_3_Sep9/` tree contains deeply nested file names.

## 2. Open the project

Open `helios.Rproj` in RStudio. This sets the working directory to the repository
root, which every script below depends on.

## 3. Install dependencies

helios itself contains no compiled code, so a plain source install needs no
compiler. Its one dependency that is not on CRAN, `individual`, does contain C++,
so installing that from source on Windows requires Rtools matching your R version
(Rtools 4.6 for R 4.6.x, from https://cran.r-project.org/bin/windows/Rtools/).

```r
install.packages(c("devtools", "dplyr", "EnvStats", "dqrng", "truncnorm",
                   "ggplot2", "scales", "knitr", "rmarkdown"))
devtools::install_github("mrc-ide/individual@feat/logi_size")
```

## 4. Install helios-nyc

Do this with helios **not** loaded, and with no other R or RStudio session open on
the same library. Windows locks the files of a loaded package, which makes the
install fail partway through and can leave the package's lazy-load database
corrupt.

Restart R (Session > Restart R), then:

```r
devtools::install(".")
```

Look for `* DONE (helios)` at the end. If prompted to update other packages,
declining is fine.

## 5. Verify

Restart R again, then:

```r
library(helios)
packageDescription("helios")$Built
"schools_nyc" %in% data(package = "helios")$results[, "Item"]
nrow(baseline_household_demographics_usa)
source("inst/nyc/nyc_parameters.R")
ashrae_241_target_ach
```

Expect today's date in the build string, `TRUE`, `3166264`, and about `9.23`.

A build date older than your clone means the install did not actually run, and
`schools_nyc` will be missing; repeat step 4. `nyc_parameters.R` reads
`schools_nyc` at source time, so `object 'schools_nyc' not found` is the symptom
of a stale install rather than a problem with that file.

## 6. Run the simulations

```r
source("inst/nyc/run_nyc_school_test.R")      # epidemic
source("inst/nyc/run_nyc_school_endemic.R")   # endemic, with burn-in and resume
```

Both parallelise with a PSOCK cluster, which works identically on all three
platforms. By default they use every physical core but one. To set the worker
count explicitly, before running:

```r
Sys.setenv(HELIOS_NYC_CORES = "16")
```

Both scripts save each completed run to `results/` individually and skip finished
work on a rerun, so an interrupted batch can be restarted without repeating
anything. Set `smoke_test <- TRUE` near the top of either script for a fast
end-to-end check before committing to a full batch.

## 7. Render the report

```r
rmarkdown::render("inst/nyc/nyc_setup_report.Rmd",
                  knit_root_dir = getwd(),
                  output_dir = "results")
```

The results sections are skipped if the corresponding `.rds` files are absent, so
the report renders before the batches finish.

## Note on regenerating package data

`data-raw/DATASET.R` rebuilds the package datasets from the RTI synthetic
population and expects that data at `~/rti_synth_pop/data`. It does not need to
run on the simulation machine: `data/*.rda` are committed and installed with the
package.
