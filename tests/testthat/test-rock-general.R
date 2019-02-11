context("general ROCK tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

test_that("reading a source with no ROCK stuff works properly", {

  res <- parse_source(here::here("tests",
                                 "testthat",
                                 "lorum-ipsum.rock"));

}

###-----------------------------------------------------------------------------

test_that("a inductive code tree is read correctly", {

  res <- parse_source(here::here("tests",
                                 "testthat",
                                 "sylvias-test.rock"));

  testthat::expect_equal(res$inductiveCodeTrees$code$IC$Attack$label,
                         "Attack");

}

###-----------------------------------------------------------------------------

test_that("a single deductive code tree is read correctly", {

  res <- parse_source(here::here("tests",
                                 "testthat",
                                 "second-test-file.rock"));

  testthat::expect_equal(res$deductiveCodeTrees$EM_SEM_Psych$label,
                         "EM_SEM_Psych");

}

###-----------------------------------------------------------------------------

test_that("Multiple sources are read correctly", {

  res <- parse_sources(here::here("tests",
                                  "testthat"),
                       extension="rock");

  testthat::expect_equal(res$deductiveCodeTrees$EM_SEM_Psych$label,
                         "EM_SEM_Psych");

}

###-----------------------------------------------------------------------------

test_that("multiple sources without deductive code trees are read correctly", {

  res <- parse_sources(here::here("tests",
                                  "testthat"),
                       regex = "ipsum");

  testthat::expect_equal(res$deductiveCodeTrees$EM_SEM_Psych$label,
                         "EM_SEM_Psych");

}

###-----------------------------------------------------------------------------

test_that("A deductive code tree is read correctly from multiple DCT files", {

  res <- parse_sources(here::here("tests",
                                  "testthat"),
                       extension="dct");

  testthat::expect_equal(res$deductiveCodeTrees$EM_SEM_Psych$label,
                         "EM_SEM_Psych");

}



# clean_transcript(input=file.path(workingPath, "test", "P.I.int.txt"),
#                  outputFile=file.path(workingPath, "test", "P.I.int.rock"),
#                  extraReplacements=list(c("\\n-\\s", "\n---turn-of-talk---\n")));


