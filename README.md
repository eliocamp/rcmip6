
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcmip6

<!-- badges: start -->
<!-- badges: end -->

The goal of rcmip6 is to search and download data from the CMIP6
project.

## Installation

You can install the development version of rcmip6 like so:

``` r
remotes::install_github("eliocamp/rcmip6")
```

## Example

Search models, variables, etc… using `cmip_search()`

``` r
library(rcmip6)

query <- list(
  type               = "Dataset",
  replica            = "false",
  latest             = "true",
  variable_id        = "tas",
  project            = "CMIP6",
  frequency          = "mon",                          
  table_id           = "Amon",
  experiment_id      = "historical",
  source_id          = "CanESM5"
)

results <- cmip_search(query)
results
#> Found 65 results totalling 3252Mb.
```

Sumary of results:

``` r
as.data.frame(results) |> 
  head(10) |> 
  knitr::kable()
```

| mip_era | institution_id | source_id | experiment_id | sub_experiment_id | experiment_title                          | member_id | realization_index | initialization_index | physics_index | forcing_index | table_id | frequency | datetime_start       | datetime_stop        | variable_id | nominal_resolution | grid_label |     size |
|:--------|:---------------|:----------|:--------------|:------------------|:------------------------------------------|:----------|:------------------|:---------------------|:--------------|:--------------|:---------|:----------|:---------------------|:---------------------|:------------|:-------------------|:-----------|---------:|
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r10i1p2f1 | 10                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03243 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r7i1p2f1  | 7                 | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.02567 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r8i1p2f1  | 8                 | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.02891 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r9i1p2f1  | 9                 | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.02964 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r23i1p2f1 | 23                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03534 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r14i1p2f1 | 14                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03199 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r16i1p2f1 | 16                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03632 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r17i1p2f1 | 17                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.02975 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r22i1p2f1 | 22                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03298 |
| CMIP6   | CCCma          | CanESM5   | historical    | none              | all-forcing simulation of the recent past | r11i1p2f1 | 11                | 1                    | 2             | 1             | Amon     | mon       | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | tas         | 500 km             | gn         | 50.03043 |

The, download the data. Just as a demonstration, download only the first
result

``` r
cmip_root_set("readme_example")   # Set the root folder where to save files 
options(timeout = 360)            # Kind of important for some reason

files <- cmip_download(results[1])
#> Skipping existing file with matching checksum.
```

The files are saved mirroring the source file structure to ensure that
each file is unique.

``` r
cat(system(paste0("tree ", shQuote(cmip_root_get())), intern = TRUE), sep = "\n")
#> readme_example
#> └── CMIP6
#>     └── CMIP
#>         └── CCCma
#>             └── CanESM5
#>                 └── historical
#>                     └── r10i1p2f1
#>                         └── Amon
#>                             └── tas
#>                                 └── gn
#>                                     └── 20190429
#>                                         ├── model.info
#>                                         └── tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
#> 
#> 10 directories, 2 files
```

This structure can be parsed with `cmip_available()`

``` r
cmip_available() |>
  knitr::kable()
```

| mip_era | activity_drs | institution_id | source_id | experiment_id | member_id | table_id | variable_id | grid_label | version  | variable_long_name           | datetime_start       | datetime_stop        | nominal_resolution | files                                                                                                                                       |
|:--------|:-------------|:---------------|:----------|:--------------|:----------|:---------|:------------|:-----------|:---------|:-----------------------------|:---------------------|:---------------------|:-------------------|:--------------------------------------------------------------------------------------------------------------------------------------------|
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r10i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             | readme_example/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc |
