ci_heatmap <- function(x,
                       itemIdentifier = "itemIdentifier",
                       codingScheme = "peterson") {

  if (is.character(codingScheme)) {
    codingScheme <- get0(paste0("codingScheme_", codingScheme));
  }

  if (!inherits(codingScheme, "rock_codingScheme")) {
    stop("As `codingScheme`, pass either a codingScheme as created by ",
         "a call to `rock::create_codingScheme()`, or the name of a ",
         "coding scheme that exists in the `rock` package.");
  }

  if (!inherits(x, c("rock_parsedSource", "rock_parsedSources"))) {
    stop("As `x`, pass one or more parsed sources (as resulting from ",
         "a call to `rock::parse_source()` or `rock::parse_sources()`.");
  }

  mergedSourceDf <- x$mergedSourceDf;

  usedCodes <- intersect(
    codingScheme$codes,
    names(mergedSourceDf)
  );




}
