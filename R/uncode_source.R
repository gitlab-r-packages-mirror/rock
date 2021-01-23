#' Remove one or more codes from a source
#'
#' These functions remove one or more codes from a source, and make it easy to
#' justify that decision.
#'
#' @param input A file with a source (for `uncode_source`), or a directory
#' with sources (for `uncode_sources`), or an object with one source or
#' multiple sources as produced by one of the `loading_sources` functions.
#' @param codes A character vector with codes to remove.
#' @param output If specified, the coded source will be written here.
#' @param childrenReplaceParents Whether children should be deleted (`FALSE`)
#' or take their parent code's place (`TRUE`).
#' @param recursiveDeletion Whether to also delete a code's parents (`TRUE`),
#' if they have no other children, and keep doing this until the root is
#' reached, or whether to leave parent codes alone (`FALSE`).
#' @param justification The justification for this action.
#' @param justificationFile If specified, the justification is appended to
#' this file. If not, it is saved to the [justifier::workspace()]. This can
#' then be saved or displayed at the end of the R Markdown file or R script
#' using [justifier::save_workspace()].
#' @param preventOverwriting Whether to prevent overwriting existing files
#' when writing the files to `output`.
#' @param encoding The encoding to use.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the recoded source(s) or source(s) object.
#' @rdname uncoding_sources
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' loadedExample <- rock::load_source(exampleFile);
#'
#' @export
uncode_source <- function(input,
                          codes,
                          filter = TRUE,
                          output = NULL,
                          childrenReplaceParents = TRUE,
                          recursiveDeletion = TRUE,
                          decisionLabel = NULL,
                          justification = NULL,
                          justificationFile = rock::opts$get('justificationFile'),
                          preventOverwriting = rock::opts$get('preventOverwriting'),
                          encoding = rock::opts$get('encoding'),
                          silent = rock::opts$get('silent')) {

  return(
    change_source(
      input = input,
      codes = codes,
      func = changeSource_uncode,
      output = output,
      decisionLabel = decisionLabel,
      justification = justification,
      justificationFile = justificationFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent,
      childrenReplaceParents = childrenReplaceParents,
      recursiveDeletion = recursiveDeletion
    )
  );

}

changeSource_uncode <- function(input,
                                codes,
                                filter = TRUE,
                                childrenReplaceParents = TRUE,
                                recursiveDeletion = TRUE,
                                silent = rock::opts$get('silent')) {

  codeDelimiters <- rock::opts$get(codeDelimiters);

  if (length(codes) > 1) {
    ### Sequentially remove codes
    if (!silent) {
      cat0("Multiple codes to remove have been specified: starting ",
           "sequential removal of ", length(codes), " codes.\n");
    }

    for (i in seq_along(codes)) {
      input <-
        changeSource_uncode(
          input = input,
          codes = codes[i],
          filter = filter,
          childrenReplaceParents = childrenReplaceParents,
          recursiveDeletion = recursiveDeletion,
          silent = rock::opts$get('silent')
        );
    }

    res <- input;

  } else {
    ### `codes` has length 1

    regexToDelete <- codes;

    ### Add code delimiters if they're not yet added
    if (!grepl(paste0("^", escapeRegexCharacterClass(codeDelimiters[1])),
               regexToDelete)) {
      regexToDelete <- paste0(escapeRegexCharacterClass(codeDelimiters[1]), regexToDelete);
    }
    if (!grepl(paste0(escapeRegexCharacterClass(codeDelimiters[2]), "$"),
               regexToDelete)) {
      regexToDelete <- paste0(regexToDelete, escapeRegexCharacterClass(codeDelimiters[2]));
    }

    ### Add optional leading space
    regexToDelete <- paste0("\\s?", regexToDelete);

    ### Get clean code text
    cleanCode <-
      sub(
        paste0(".*",
               escapeRegexCharacterClass(codeDelimiters[1]),
               "(.*)",
               escapeRegexCharacterClass(codeDelimiters[2]),
               ".*"),
        "\\1",
        regexToDelete
      );

    if (!silent) {
      cat0("Removing all occurrences of code '",
           cleanCode,
           "'.\n");
    }

    ### Replace regex with nothing, but only for the rows specified
    ### in the filter
    res <- input;
    res[filter] <-
      gsub(
        regexToDelete,
        "",
        res[filter]
      );

  }

  return(res);

}
