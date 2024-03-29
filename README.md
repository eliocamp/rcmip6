
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcmip6

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/eliocamp/rcmip6/branch/main/graph/badge.svg)](https://app.codecov.io/gh/eliocamp/rcmip6?branch=main)
[![R-CMD-check](https://github.com/eliocamp/rcmip6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eliocamp/rcmip6/actions/workflows/R-CMD-check.yaml)
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
cmip_info(results)
#> Found 65 results (with 0 replicas) totalling 3252Mb.
```

Sumary of results:

``` r
results |> 
  cmip_simplify() |>   # To keep only the most informative columns
  subset(, select = -full_info) |> 
  head(10) |> 
  knitr::kable()
```

| mip_era | activity_drs | institution_id | source_id | experiment_id | member_id | table_id | variable_id | grid_label | version  | variable_long_name           | datetime_start       | datetime_stop        | nominal_resolution |
|:--------|:-------------|:---------------|:----------|:--------------|:----------|:---------|:------------|:-----------|:---------|:-----------------------------|:---------------------|:---------------------|:-------------------|
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r10i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r7i1p2f1  | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r8i1p2f1  | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r9i1p2f1  | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r23i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r14i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r16i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r17i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r22i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |
| CMIP6   | CMIP         | CCCma          | CanESM5   | historical    | r11i1p2f1 | Amon     | tas         | gn         | 20190429 | Near-Surface Air Temperature | 1850-01-16T12:00:00Z | 2014-12-16T12:00:00Z | 500 km             |

The, download the data. Just as a demonstration, download only the first
result

``` r
cmip_root_set("readme_example")   # Set the root folder where to save files 
dir.create(cmip_root_get())
files <- cmip_download(results[1])
#> Requesting metadata...
#> Checking for existing files...
#> Downloading...
#> Downloading 52.5 MB in 1 files...
```

The files are saved mirroring the source file structure to ensure that
each file is unique.

``` r
fs::dir_tree(cmip_root_get())
#> readme_example
#> ├── 2024-02-07_18-17-36_rcmip6.json
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
#>                                         └── tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
```

This structure can be parsed with `cmip_available()`

``` r
cmip_available()
#>                                                                                                                                                     id
#> 1: CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.v20190429.tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc|crd-esgf-drc.ec.gc.ca
#>     version activity_drs activity_id cf_standard_name
#> 1: 20190429         CMIP        CMIP  air_temperature
#>                                                            checksum
#> 1: 7a4b5eb760747b596b846fed00c911f6970f16c0077123dc7f05e6f353d6d9a5
#>    checksum_type
#> 1:        SHA256
#>                                                                                                        citation_url
#> 1: http://cera-www.dkrz.de/WDCC/meta/CMIP6/CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.v20190429.json
#>                data_node data_specs_version
#> 1: crd-esgf-drc.ec.gc.ca           01.00.29
#>                                                                                   dataset_id
#> 1: CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.v20190429|crd-esgf-drc.ec.gc.ca
#>                                                                                                                         dataset_id_template_
#> 1: %(mip_era)s.%(activity_drs)s.%(institution_id)s.%(source_id)s.%(experiment_id)s.%(member_id)s.%(table_id)s.%(variable_id)s.%(grid_label)s
#>                                                                                                                                        directory_format_template_
#> 1: %(root)s/%(mip_era)s/%(activity_drs)s/%(institution_id)s/%(source_id)s/%(experiment_id)s/%(member_id)s/%(table_id)s/%(variable_id)s/%(grid_label)s/%(version)s
#>    experiment_id                          experiment_title frequency
#> 1:    historical all-forcing simulation of the recent past       mon
#>                                                                further_info_url
#> 1: https://furtherinfo.es-doc.org/CMIP6.CCCma.CanESM5.historical.none.r10i1p2f1
#>                                                                                                           grid
#> 1: T63L49 native atmosphere, T63 Linear Gaussian Grid; 128 x 64 longitude/latitude; 49 levels; top level 1 hPa
#>    grid_label         index_node
#> 1:         gn esgf-node.llnl.gov
#>                                                                                                                      instance_id
#> 1: CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.v20190429.tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
#>    institution_id latest
#> 1:          CCCma   TRUE
#>                                                                                                              master_id
#> 1: CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
#>    member_id mip_era model_cohort nominal_resolution
#> 1: r10i1p2f1   CMIP6   Registered             500 km
#>                                                  pid      product project realm
#> 1: hdl:21.14100/5acc469d-b400-3c06-b1de-3f4142d90fc9 model-output   CMIP6 atmos
#>    replica     size source_id source_type sub_experiment_id table_id
#> 1:   FALSE 52462806   CanESM5       AOGCM              none     Amon
#>               timestamp
#> 1: 2019-06-01T04:44:16Z
#>                                                        title
#> 1: tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
#>                                          tracking_id type
#> 1: hdl:21.14100/5331349e-4bf3-4772-90ec-69491dac16c8 File
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                url
#> 1: http://crd-esgf-drc.ec.gc.ca/thredds/fileServer/esgC_dataroot/AR6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/v20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc|application/netcdf|HTTPServer,gsiftp://crd-esgf-drc.ec.gc.ca:2811//esgC_dataroot/AR6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/v20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc|application/gridftp|GridFTP,http://crd-esgf-drc.ec.gc.ca/thredds/dodsC/esgC_dataroot/AR6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/v20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc.html|application/opendap-html|OPENDAP
#>    variable variable_id           variable_long_name variable_units
#> 1:      tas         tas Near-Surface Air Temperature              K
#>    variant_label    _version_ retracted               _timestamp score
#> 1:     r10i1p2f1 1.637114e+18     FALSE 2019-06-23T06:59:01.588Z     1
#>                            access       datetime_start        datetime_stop
#> 1: HTTPServer,GridFTP,OPENDAP,LAS 1850-01-16T12:00:00Z 2014-12-16T12:00:00Z
#>    east_degrees
#> 1:     357.1875
#>                                                                                     geo
#> 1: ENVELOPE(-180.0, -2.8125, 87.8638, -87.8638),ENVELOPE(0.0, 180.0, 87.8638, -87.8638)
#>       geo_units north_degrees number_of_aggregations number_of_files
#> 1: degrees_east       87.8638                      2               1
#>    south_degrees west_degrees
#> 1:      -87.8638            0
#>                                                                                                                                                                                                                 xlink
#> 1: http://cera-www.dkrz.de/WDCC/meta/CMIP6/CMIP6.CMIP.CCCma.CanESM5.historical.r10i1p2f1.Amon.tas.gn.v20190429.json|Citation|citation,http://hdl.handle.net/hdl:21.14100/5acc469d-b400-3c06-b1de-3f4142d90fc9|PID|pid
#>    branch_method short_description is_requested needs_download
#> 1:            NA                NA         TRUE           TRUE
#>                                                                                                                                           file
#> 1: readme_example/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc
```

We can obtain the urls that *would* be downloaded.

Note that these are the *file server* URLs, in at lease some cases we
can substitute “/fileServer/” with “/dodsC/” for the OpenDAP server
URLs, which will work remotely with the NetCDF API.

``` r
urls <- cmip_urls(results)
urls[[1]]
#> [1] "http://crd-esgf-drc.ec.gc.ca/thredds/fileServer/esgC_dataroot/AR6/CMIP6/CMIP/CCCma/CanESM5/historical/r10i1p2f1/Amon/tas/gn/v20190429/tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc"
basename(unlist(urls[1:3]))
#> [1] "tas_Amon_CanESM5_historical_r10i1p2f1_gn_185001-201412.nc"
#> [2] "tas_Amon_CanESM5_historical_r7i1p2f1_gn_185001-201412.nc" 
#> [3] "tas_Amon_CanESM5_historical_r8i1p2f1_gn_185001-201412.nc"
```
