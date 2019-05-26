testthat::context("general ROCK tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a source with no ROCK stuff works properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  res <- parse_source(file.path(examplePath,
                                "lorum-ipsum.rock"));

});

###-----------------------------------------------------------------------------

testthat::test_that("a inductive code tree is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  res <- parse_source(file.path(examplePath,
                                "longer-test.rock"));

  testthat::expect_equal(res$inductiveCodeTrees$codes$inductFather$inducChild3$label,
                         "inducChild3");

});

###-----------------------------------------------------------------------------

testthat::test_that("a single deductive code tree is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  res <- parse_source(file.path(examplePath,
                                "second-test-file.rock"));

  testthat::expect_equal(res$deductiveCodeTrees$parentCode1$someParent$childCode1$label,
                         "childCode1");

});

###-----------------------------------------------------------------------------

testthat::test_that("Multiple sources are read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');


  res <- parse_sources(examplePath,
                       extension="rock");

  testthat::expect_equal(res$deductiveCodeTrees$children$parentCode1$someParent$childCode2$label,
                         "childCode2");

});

###-----------------------------------------------------------------------------

testthat::test_that("multiple sources without deductive code trees are read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  res <- parse_sources(examplePath,
                       regex = "ipsum");

  testthat::expect_null(res$deductiveCodeTrees);

});

###-----------------------------------------------------------------------------

testthat::test_that("A deductive code tree is read correctly from multiple DCT files", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  res <- parse_sources(examplePath,
                       extension="dct");

  testthat::expect_equal(res$deductiveCodeTrees$behavior_xl67k7w8j$intention_71vr5q3q$attitude_71vqm37n$label,
                         "Attitude");

});



# clean_transcript(input=file.path(workingPath, "test", "P.I.int.txt"),
#                  outputFile=file.path(workingPath, "test", "P.I.int.rock"),
#                  extraReplacements=list(c("\\n-\\s", "\n---turn-of-talk---\n")));


