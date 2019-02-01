#' #' Extract or delete all yaml fragments from a file
#' #'
#' #' These function simply extract or delete all YAML fragments from a file,
#' #' returning a list of character vectors containing the extracted fragments
#' #' (`extract_yaml_fragments`) or a character vector without the lines that
#' #' specified the YAML fragments (`delete_yaml_fragments`).
#' #'
#' #' This function is called by [parse_source()]; it is normally not
#' #' necessary to call it directly.
#' #'
#' #' @param file The path to a file to scan; takes precedence over `text`.
#' #' @param text A character vector to scan, where every element should
#' #' represent one line in the file; can be specified instead of `file`.
#' #' @param delimiterRegEx The regular expression used to locate YAML
#' #' fragments.
#' #' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' #' delete the last delimiter (TRUE) if an odd number of delimiters is
#' #' encountered.
#' #' @param silent Whether to be silent (TRUE) or informative (FALSE).
#' #'
#' #' @return A list of character vectors.
#' #' @examples extract_yaml_fragments(text=c("---", "First YAML fragment", "---",
#' #'                               "Outside of YAML",
#' #'                               "---", "Second fragment", "---",
#' #'                               "Also outside of YAML"));
#' #'
#' #' @rdname yaml_fragments
#' #' @export
#' extract_yaml_fragments <- function(file,
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
#'     return(NULL);
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
#'   return(lapply(indexSets,
#'                 function(i, x=allLines) {
#'                   return(x[i]);
#'                 }));
#'
#' }
