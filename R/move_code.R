#' Move a codes to a different parent
#'
#' These functions move a code to a different parent in one or more sources.
#'
#' @param input A file with a source (for `uncode_source`), or a directory
#' with sources (for `uncode_sources`), or an object with one source or
#' multiple sources as produced by one of the `loading_sources` functions.
#' @param codes A character vector with codes to remove.
#' @param output If specified, the coded source will be written here.
#' @param childrenReplaceParents Whether children should be deleted (`FALSE`)
#' or take their parent code's place (`TRUE`). This is ignored if
#' `recursiveDeletion=TRUE`, in which case children are always deleted.
#' @param recursiveDeletion Whether to also delete a code's parents (`TRUE`),
#' if they have no other children, and keep doing this until the root is
#' reached, or whether to leave parent codes alone (`FALSE`). This takes
#' precedence over `childrenReplaceParents`.
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
#' @return Invisibly, the changed source(s) or source(s) object.
#' @rdname moving_codes
#' @examples
#' @export
move_code <- function(input,
                      codes,
                      newAncestry,
                      filter = TRUE,
                      output = NULL,
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
      filter = filter,
      func = changeSource_newAncestry,
      output = output,
      decisionLabel = decisionLabel,
      justification = justification,
      justificationFile = justificationFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent,
      newAncestry = newAncestry
    )
  );

}

changeSource_newAncestry <- function(input,
                                     codes,
                                     newAncestry,
                                     filter,
                                     silent = rock::opts$get('silent')) {

  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  if (length(codes) > 1) {

    ### Sequentially remove codes
    if (!silent) {
      cat0("Multiple codes to move have been specified: starting ",
           "sequential moving of ", length(codes), " codes.\n");
    }

    for (i in seq_along(codes)) {
      input <-
        changeSource_newAncestry(
          input = input,
          codes = codes[i],
          filter = filter,
          newAncestry = newAncestry,
          silent = silent
        );
    }

  } else {
    ### `codes` has length 1

    ### Get clean code, removing any delimiters if they were added
    cleanCode <- codes;

    cleanCode <-
      gsub(
        escapeRegexCharacterClass(codeDelimiters[1]),
        "",
        cleanCode
      );

    cleanCode <-
      gsub(
        escapeRegexCharacterClass(codeDelimiters[2]),
        "",
        cleanCode
      );

    ### Remove leading '>' if it's there
    newAncestry <- sub(paste0("^", inductiveCodingHierarchyMarker),
                       "",
                       newAncestry);

    ### Add trailing '>' if it's not there
    newAncestry <- sub(paste0(inductiveCodingHierarchyMarker, "?$"),
                       inductiveCodingHierarchyMarker,
                       newAncestry);

    if (!silent) {
      cat0("Moving all occurrences of code '",
           cleanCode,
           "' to new ancestry '",
           newAncestry, "'.\n");
    }

    ### Select elements to check

    filteredUtterances <- input[filter];

    utterancesWithMatches <-
      grep(
        cleanCode,
        filteredUtterances
      );

    if (!silent) {
      cat0("Out of the ", length(input), " utterances in the provided source, ",
           sum(filter), " are selected by the filter, ",
           length(utterancesWithMatches), " of which contain the code text.\n");
    }

    ### Create the regular expression to change ancestry
    regexToChangeAncestry <-
      paste0(
        "(\\s?", ### Optional leading space, start capturing expression 1
        escapeRegexCharacterClass(codeDelimiters[1]),
        ")",     ### Stop capturing expression 1 after opening delimiter
        "(",     ### Not really want to capture; just want to force a hierarchy
                 ### marker after any character that occur (if the target code isn't root)
        validCodeCharacters,   ### Start of current ancestry
        "*",      ### End of current ancestry
        inductiveCodingHierarchyMarker,
        ")?",     ### Allow absence of ancestry
        "(",     ### Start capturing code and descendency
        cleanCode,
        inductiveCodingHierarchyMarker,
        "?",
        validCodeCharacters,
        "*",
        escapeRegexCharacterClass(codeDelimiters[2]),
        ")"      ### End capturing of code and descendancy
      );

    if (!silent) {
      cat0("Using regular expression '", regexToChangeAncestry, "'.\n");
    }

    for (i in seq_along(utterancesWithMatches)) {

      if (!silent) {
        cat0("---- PRE: ", filteredUtterances[utterancesWithMatches[i]], "\n");
      }

      ### First simply replace occurrences without ancestry/descendancy
      filteredUtterances[utterancesWithMatches[i]] <-
        gsub(
          regexToChangeAncestry,
          paste0("\\1", newAncestry, "\\3"),
          filteredUtterances[utterancesWithMatches[i]]
        );

      if (!silent) {
        cat0("    POST: ", filteredUtterances[utterancesWithMatches[i]], "\n");
      }

    }

    ### Replace processed rows in the input source
    oldInput <- input;
    input[filter] <- filteredUtterances;
    diffCount <- sum(input != oldInput);

    if (!silent) {
      cat0("Moved ", diffCount, " code instances for code '", codes, "'.\n\n");
    }

  }

  return(input);

}
