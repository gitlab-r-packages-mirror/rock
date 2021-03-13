#' Replace code identifiers with their full paths
#'
#' This function replaces the column names in the `mergedSourceDf` data frame
#' in a `rockParsedSource` or `rockParsedSources` object with the full paths
#' to those code identifiers.
#'
#' @param x A `rockParsedSource` or `rockParsedSources` object as returned by
#' a call to [parse_source()] or [parse_sources()].
#' @param stripRootsFromCodePaths Whether to strip the roots first (i.e. the
#' type of code)
#'
#' @return An adapted `rockParsedSource` or `rockParsedSources` object.
#' @export
codeIds_to_codePaths <- function(x,
                                 stripRootsFromCodePaths = rock::opts$get("stripRootsFromCodePaths")) {

  ### Check input class
  if (!(("rockParsedSources" %in% class(x)) |
        ("rockParsedSource"  %in% class(x)))) {
    stop("As `x`, you must pass either an `rockParsedSource` or ",
         "an `rockParsedSources` object (i.e. either the result ",
         "from a call to `rock::parseSource()` or the result from ",
         "a call to `rock::parseSources()`). However, you ",
         "provided an object of class ", vecTxtQ(x), ".");
  }

  ### More convenient than convenience
  codePaths <- x$convenience$codingPaths;

  if (stripRootsFromCodePaths) {
    codePaths <-
      stripCodePathRoot(
        codePaths
      );
  }

  names(x$mergedSourceDf) <-
    ifelse(is.na(codePaths[names(x$mergedSourceDf)]),
           names(x$mergedSourceDf),
           codePaths[names(x$mergedSourceDf)]);

  return(invisible(x));

}
