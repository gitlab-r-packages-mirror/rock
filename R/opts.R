#' Options for the rock package
#'
#' The `rock::opts` object contains three functions to set, get, and reset
#' options used by the rock package. Use `rock::opts$set` to set options,
#' `rock::opts$get` to get options, or `rock::opts$reset` to reset specific or
#' all options to their default values.
#'
#' @usage rock::opts$set(...)
#' rock::opts$get(option, default=FALSE)
#' rock::opts$reset(...)
#'
#' @param ... For `rock::opts$set`, the dots can be used to specify the options
#' to set, in the format `option = value`, for example, `utteranceMarker = "\n"`. For
#' `rock::opts$reset`, a list of options to be reset can be passed.
#' @param option The name of the option to set.
#' @param default The default value to return if the option has not been manually
#' specified.
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("rock.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for the rock!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for the rock!");
  } else {
    return(getOption(paste0("rock.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  dots <- list(...);
  if (length(dots) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    dotNames <- names(dots);
    names(dots) <-
      paste0("rock.", dotNames);
    if (all(dotNames %in% names(opts$defaults))) {
      do.call(opts$set,
              dots);
    } else {
      invalidOptions <-
        !which(all(dotNames %in% names(opts$defaults)));
      stop("Option(s) ", vecTxtQ(option[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for the rock!");
    }
  }
}

opts$defaults <-
  list(codeRegexes = c(codes = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
       idRegexes = c(caseId = "\\[\\[cid[=:]([a-zA-Z0-9._-]+)\\]\\]",
                     stanzaId = "\\[\\[sid[=:]([a-zA-Z0-9._-]+)\\]\\]",
                     coderId = "\\[\\[coderId[=:]([a-zA-Z0-9._-]+)\\]\\]"),
       sectionRegexes = c(paragraphs = "---paragraph-break---",
                          secondary = "---<[a-zA-Z0-9]?>---"),
       uidRegex = "\\[\\[uid[=:]([a-zA-Z0-9._-]+)\\]\\]",
       codeDelimiters = c("[[", "]]"),
       utteranceMarker = "\n",
       fragmentDelimiter = "\n\n-----\n\n",
       inductiveCodingHierarchyMarker = ">",
       codeClass = "code",
       idClass = "identifier",
       sectionClass = "sectionBreak",
       uidClass = "uid",
       utteranceClass = "utterance",
       encoding = "UTF-8");
