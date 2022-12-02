testthat::context("anchor tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading anchors works", {

  ### devtools::load_all();

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "anchor-example-1.rock"));

  testthat::expect_s3_class(testres, "rock_parsedSource");

});

###-----------------------------------------------------------------------------

testthat::test_that("reading anchors works", {

  ### devtools::load_all();

  examplePath <- file.path(system.file(package="rock"), 'extdata', 'streams');

  testres <- rock::parse_sources(
    examplePath
  );

  syncedres <- rock::sync_streams(
    testres
  );

  testthat::expect_s3_class(testres, "rock_parsedSource");

});


###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
