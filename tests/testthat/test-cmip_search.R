
query <- list(type = "Dataset",
              replica = "false",
              latest = "true",
              mip_era = "CMIP6",
              frequency = "mon",
              variable_id = "tosga",
              experiment_id = "piControl",
              project = "CMIP6")
query_url <- "http://esgf-node.llnl.gov/esg-search/search/?offset=0&limit=10&type=Dataset&replica=false&latest=true&mip_era=CMIP6&experiment_id=piControl&frequency=mon&variable_id=tosga&project=CMIP6&facets=mip_era%2Cactivity_id%2Cproduct%2Csource_id%2Cinstitution_id%2Csource_type%2Cnominal_resolution%2Cexperiment_id%2Csub_experiment_id%2Cvariant_label%2Cgrid_label%2Ctable_id%2Cfrequency%2Crealm%2Cvariable_id%2Ccf_standard_name%2Cdata_node&format=application%2Fsolr%2Bjson"

root <- tempfile()
dir.create(root)
results <- cmip_search(query)

test_that("Search returns results", {
  expect_s3_class(results, "data.table")
  expect_true(nrow(results) > 0)

})

test_that("URL to list works", {
  # I don't test that query == cmip_url_to_list(query_url) because
  # slight discrepancies are not important. What's important is that they
  # are equivalent queries.
  expect_equal(cmip_search(cmip_url_to_list(query_url)),
               results)
})


test_that("cmip_simplify works", {
  expect_s3_class(cmip_simplify(results), "data.table")
  expect_equal(results,
               cmip_unsimplify(cmip_simplify(results)))

  expect_equal(results[1:4],
               cmip_unsimplify(cmip_simplify(results)[1:4]))
})

test_that("cmip_info() works", {
  expect_output(cmip_info(results), "results")

})

test_that("Download works", {
  expect_error(cmip_root_set(root), NA)
  expect_equal(cmip_root_get(), root)

  suppressMessages(expect_type(files <- cmip_download(results[c(1:2)]), "list"))
  expect_length(files, 2)
  expect_type(files[[1]], "character")

  expect_true(all(file.exists(unlist(files))))
  suppressMessages(expect_message(cmip_download(results[1]), "Skipping"))

})

test_that("cmip_available() works", {
  expect_s3_class(available <- cmip_available(), "data.table")
  expect_true(nrow(available) > 0)

  expect_type(available[["files"]], "list")
  # Removes extra column
  available[, files := NULL]

  available <- available[, colnames(results), with = FALSE]

  expect_equal(results[c(1:2)][order(id)],
               available[order(id)])
})
