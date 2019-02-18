#' Collapse the occurrences in utterances into groups
#'
#' This function collapses all occurrences into groups
#' sharing the same identifier, by default the `stanzaId`
#' identifier (`[[sid=..]]`).
#'
#' @param parsedSource The parsed sources as provided by [parse_source()].
#' @param collapseBy The column in the `sourceDf` (in the `parsedSource` object)
#' to collapse by (i.e. the column specifying the groups to collapse).
#' @param columns The columns to collapse; if unspecified (i.e. `NULL`), all
#' codes stored in the `code` object in the `codings` object in the
#' `parsedSource` object are taken (i.e. all used codes in the `parsedSource`
#' object).
#' @param logical Whether to return the counts of the occurrences (`FALSE`) or
#' simply whether any code occurreded in the group at all (`TRUE`).
#'
#' @return A dataframe with one row for each value of of `collapseBy` and columns
#' for `collapseBy` and each of the `columns`, with in the cells the counts (if
#' `logical` is `FALSE`) or `TRUE` or `FALSE` (if `logical` is `TRUE`).
#'
#' @export
collapse_occurrences <- function(parsedSource,
                                 collapseBy = "stanzaId",
                                 columns = NULL,
                                 logical = FALSE) {

  if (!("rockParsedSource" %in% class(parsedSource))) {
    stop("As argument `parsedSource`, you must specify a parsed source, ",
         "as provided by the `rock::parse_source` function!");
  }

  if (is.null(columns)) {
    columns <-
      parsedSource$codings$code;
  }

  if (!(all(columns %in% names(parsedSource$sourceDf)))) {
    stop("Not all columns specified in the `columns` argument exist in ",
         "the `sourceDf` in the `parsedSource` object you provided!");
  }

  if (!(collapseBy %in% names(parsedSource$sourceDf))) {
    stop("The columns specified in the `collapseBy` argument does not exist in ",
         "the `sourceDf` in the `parsedSource` object you provided!");
  }

  sourceDf <-
    parsedSource$sourceDf[, c(collapseBy,
                              columns)];

  if (logical) {
    res <-
      aggregate(sourceDf[, columns],
                sourceDf[, collapseBy, drop=FALSE],
                any);
  } else {
    res <-
      aggregate(sourceDf[, columns],
                sourceDf[, collapseBy, drop=FALSE],
                sum);
  }

  names(res)[1] <-
    collapseBy;

  return(res);

}
