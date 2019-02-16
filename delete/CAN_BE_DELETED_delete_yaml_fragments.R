#' #' Delete all yaml fragments from a file
#' #'
#' #' This function deletes all YAML fragments from a file, returning
#' #' a character vector where each element is a line (but the lines
#' #' comprosing the YAML fragments have been deleted).
#' #'
#' #' This function is called by [parse_source()]; it is normally not
#' #' necessary to call it directly.
#' #'
#' #' @param file The path to a file to scan.
#' #' @param text A character vector to scan, where every element should
#' #' represent one line in the file.
#' #' @param delimiterRegEx The regular expression used to locate YAML
#' #' fragments
#' #' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' #' delete the last delimiter (TRUE) if an odd number of delimiters is
#' #' encountered.
#' #' @param silent Whether to be silent (TRUE) or informative (FALSE).
#' #'
#' #' @return A list of character vectors.
#' #' @examples delete_yaml_fragments(text=c("---", "First YAML fragment", "---",
#' #'                               "Outside of YAML",
#' #'                               "---", "Second fragment", "---",
#' #'                               "Also outside of YAML"));
#' #'
#' #' @export
#' delete_yaml_fragments <- function(file,
#'                                    text,
#'                                    delimiterRegEx = "^---$",
#'                                    ignoreOddDelimiters = FALSE,
#'                                    silent=TRUE) {
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
