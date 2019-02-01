#' #' @rdname yaml_fragments
#' #' @export
#' delete_yaml_fragments <- function(file,
#'                                   text,
#'                                   delimiterRegEx = "^---$",
#'                                   ignoreOddDelimiters = FALSE,
#'                                   silent=TRUE) {
#'   if (missing(file)) {
#'     if (missing(text)) {
#'       stop("Provide either a `file` or a `text` to scan!");
#'     } else {
#'       allLines <- text;
#'     }
#'   } else {
#'     allLines <- readLines(file);
#'   }
#'
#'   yamlFragments <- grep(delimiterRegEx,
#'                         allLines);
#'
#'   if (length(yamlFragments) == 0) {
#'     return(allLines);
#'   }
#'
#'   if (!ufs::is.even(length(yamlFragments))) {
#'     if (ignoreOddDelimiters) {
#'       yamlFragments <-
#'         yamlFragments[-length(yamlFragments)];
#'     } else {
#'       stop("Extracted an uneven number of lines with specifications ",
#'            "(the regular expression for the specification ",
#'            "delimiter that was specified was '", delimiterRegEx,
#'            "'). To ignore the last delimiter, specify ",
#'            "'ignoreOddDelimiters=TRUE'.");
#'     }
#'   }
#'
#'   yamlFragmentIndices <- seq_along(yamlFragments);
#'
#'   indexSets <- purrr::map2(.x=yamlFragments[ufs::is.odd(yamlFragmentIndices)],
#'                            .y=yamlFragments[ufs::is.even(yamlFragmentIndices)],
#'                            .f=`:`);
#'
#'   return(allLines[-do.call(c,
#'                            indexSets)]);
#'
#' }
