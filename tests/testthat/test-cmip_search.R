query <- list(
  project = "CMIP6",
  type = "Dataset",
  latest = "true",
  frequency = "mon",
  variable_id = "co2mass",
  experiment_id = "historical"
)

query_url <- "https://aims2.llnl.gov/proxy/search?project=CMIP6&offset=0&limit=10&type=Dataset&format=application%2Fsolr%2Bjson&facets=activity_id%2C+data_node%2C+source_id%2C+institution_id%2C+source_type%2C+experiment_id%2C+sub_experiment_id%2C+nominal_resolution%2C+variant_label%2C+grid_label%2C+table_id%2C+frequency%2C+realm%2C+variable_id%2C+cf_standard_name&latest=true&query=*&activity_id=CMIP&experiment_id=historical&frequency=mon&variable_id=co2mass"

root <- tempfile()
dir.create(root)
results <- cmip_search(query)

test_that("Search returns results", {
  expect_s3_class(results, "data.table")
  expect_true(nrow(results) > 0)
})

test_that("URLs obtained from search results", {
  expect_silent(urls <- cmip_urls(results[58:63]))
  expect_length(urls, nrow(results[58:63]))
  expect_type(urls, "list")
  expect_true(all(grepl("^http.*nc$", unlist(urls))))
})

test_that("URL to list works", {
  # I don't test that query == cmip_url_to_list(query_url) because
  # slight discrepancies are not important. What's important is that they
  # are equivalent queries.
  expect_equal(cmip_search(cmip_url_to_list(query_url)), results)

  expect_snapshot_output(cat(list_pretty_format(query)))
})

test_that("search includes replicas by default", {
  expect_true(any(results[, .N, by = instance_id][["N"]] > 1))
})


test_that("cmip_filter_replicas works", {
  filtered <- cmip_filter_replicas(results)

  expect_true(all(filtered[, .N, by = instance_id][["N"]] == 1))
})


test_that("cmip_search interprets character vectors", {
  instances_query <- c(
    "CMIP6.CMIP.CNRM-CERFACS.CNRM-ESM2-1.historical.r6i1p1f2.Omon.tos.gn.v20200117",
    "CMIP6.CMIP.CNRM-CERFACS.CNRM-ESM2-1.historical.r11i1p1f2.Omon.tos.gn.v20200408"
  )
  instances_results <- unique(cmip_search(instances_query)[["instance_id"]])

  expect_equal(sort(instances_query), sort(instances_results))
})


test_that("cmip_simplify works", {
  expect_s3_class(cmip_simplify(results), "data.table")
  expect_equal(results, cmip_unsimplify(cmip_simplify(results)))

  expect_equal(results[1:4], cmip_unsimplify(cmip_simplify(results)[1:4]))

  x <- capture.output(print(cmip_simplify(results)))
  expect_false(any(grepl("full_info", x)))
})

test_that("cmip_info() works", {
  expect_output(cat(cmip_info(results)), "results")
})

test_that("Download works", {
  root <- tempfile()
  dir.create(root)
  expect_error(cmip_root_set(root), NA)
  expect_equal(cmip_root_get(), root)

  suppressMessages(expect_type(files <- cmip_download(results[c(1:2)]), "list"))

  expect_equal(length(files), 2)
  expect_type(unlist(files), "character")

  expect_true(all(file.exists(unlist(files))))
  suppressMessages(expect_message(cmip_download(results[1]), "All files"))
})

test_that("cmip_available() works", {
  expect_s3_class(available <- cmip_available(), "data.table")
  expect_true(nrow(available) > 0)

  expect_type(available$file, "character")
  expect_true(all(file.exists(available$file)))
})

test_that("Failed instances give proper message", {
  failed <- results[["instance_id"]][1:2]
  expect_error(failed_query <- instance_query(failed), NA)
  expect_s3_class(
    failed_results <- eval(parse(text = failed_query)),
    "data.table"
  )
  expect_true(nrow(failed_results) > 0)
})


test_that("year_range argument in cmip_download works", {
  expect_error(cmip_root_set(tempfile()), NA)
  dir.create(cmip_root_get())
  ########
  query <- list(
    type = "Dataset",
    replica = "true", # esg-dn2.nsc.liu.se Is down... again
    latest = "true",
    variable_id = "tos",
    project = "CMIP6",
    grid_label = "gn",
    frequency = "mon",
    # table_id      = "Oday",
    experiment_id = "historical",
    member_id = "r1i1p1f1",
    source_id = "EC-Earth3" # This one has one file per year, ideal to test the range function
  )

  results <- cmip_search(query) %>%
    head(1)
  expect_error(files <- cmip_download(results, year_range = c(1993, 1992)))
  suppressMessages(expect_type(
    files <- cmip_download(results, year_range = c(1993, 1994)),
    "list"
  ))

  expect_length(files, 1)
  expect_length(files[[1]], 2)
  expect_type(unlist(files), "character")

  expect_true(all(file.exists(unlist(files))))
  expect_equal(sum(file.exists(unlist(files))), 2)
})
