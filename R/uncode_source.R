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
                          recursiveDeletion = FALSE,
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
                                filter,
                                childrenReplaceParents = TRUE,
                                recursiveDeletion = FALSE,
                                silent = rock::opts$get('silent')) {

  codeDelimiters <- rock::opts$get("codeDelimiters");
  validCodeCharacters <- rock::opts$get("validCodeCharacters");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

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

    if (!silent) {
      cat0("Removing all occurrences of code '",
           cleanCode,
           "'.\n");
      cat0("If this code has code descendency (i.e. a child code, grandchild ",
           "code, etc), the direct child code will ");
      if (childrenReplaceParents) {
        cat0("replace the removed code.\n")
      } else {
        cat0("also be removed, as will all further descendents.\n");
      }
      cat0("If this code has code ancestry (i.e. a (partial) path to the ",
           "code root is specified with a parent code, potential grandparent ",
           "code, etc), the ancestry specified in that coding instance will ",
           "be ");
      if (recursiveDeletion) {
        cat0("removed as well, as if the code had never been applied.\n")
      } else {
        cat0("retained, so the utterance will remain coded with the ",
             "parent code (and any ancestry of that parent code will ",
             "also be retained in that coding instance).\n");
      }
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

    ### Create a regular expression to delete singular occurrences
    regexToDeleteSingularOccurrence <-
      paste0(
        "\\s?", ### Optional leading space
        escapeRegexCharacterClass(codeDelimiters[1]),
        cleanCode,
        escapeRegexCharacterClass(codeDelimiters[2])
      );

    ### Create a regular expression to delete 'leaf' occurrences
    regexToDeleteLeafOccurrence <-
      paste0(
        "(\\s?", ### Optional leading space, start capturing expression 1
        escapeRegexCharacterClass(codeDelimiters[1]),
        validCodeCharacters,
        "*)",    ### End capturing of ancestry
        inductiveCodingHierarchyMarker,
        cleanCode,
        escapeRegexCharacterClass(codeDelimiters[2])
      );

    ### Create a regular expression to delete full occurrences regardless
    ### of ancestry
    regexToDeleteRecursively <-
      paste0(
        "\\s?", ### Optional leading space
        escapeRegexCharacterClass(codeDelimiters[1]),
        validCodeCharacters,
        "*",
        cleanCode,
        validCodeCharacters,
        "*",
        escapeRegexCharacterClass(codeDelimiters[2])
      );

    regexToDeleteIfDescendentsExist <-
      paste0(
        "(\\s?", ### Optional leading space, start capturing expression 1
        escapeRegexCharacterClass(codeDelimiters[1]),
        validCodeCharacters,
        "*)",    ### End capturing of ancestry
        cleanCode,
        inductiveCodingHierarchyMarker,
        "(",     ### Start capturing of descendancy
        validCodeCharacters,
        "*",
        escapeRegexCharacterClass(codeDelimiters[2]),
        ")"      ### End capturing of descendancy
      );

    for (i in seq_along(utterancesWithMatches)) {

      ### To keep it readable, store temporarily in new variable
      currentUtterance <-
        filteredUtterances[utterancesWithMatches[i]];

      ### First simply replace occurrences without ancestry/descendancy
      currentUtterance <-
        gsub(
          regexToDeleteSingularOccurrence,
          "",
          currentUtterance
        );

      ### If deleting recursively, we can delete all coding instances containing
      ### the target code
      if (recursiveDeletion) {
        currentUtterance <-
          gsub(
            regexToDeleteRecursively,
            "",
            currentUtterance
          );
      } else {
        ### Delete 'leaf' occurrences
        currentUtterance <-
          gsub(
            regexToDeleteLeafOccurrence,
            paste0("\\1", escapeRegexCharacterClass(codeDelimiters[2])),
            currentUtterance
          );

        ### If not deleting recursively, what to do next depends on whether we
        ### can delete all descendents, or whether we should shift the
        ### descendents to the target code's position.

        ### First check whether this utterance has children specified;
        ### otherwise, we don't have to do anything. Either the code doesn't
        ### occur any more in this utterance, or if it does, we shouldn't
        ### do anything because `recursiveDeletion` if FALSE.
        if (grepl(paste0(cleanCode, inductiveCodingHierarchyMarker),
                  currentUtterance)) {
          if (childrenReplaceParents) {
            currentUtterance <-
              gsub(
                regexToDeleteIfDescendentsExist,
                "\\1\\2",
                currentUtterance
              );
          } else {
            currentUtterance <-
              gsub(
                regexToDeleteIfDescendentsExist,
                paste0("\\1", escapeRegexCharacterClass(codeDelimiters[2])),
                currentUtterance
              );
          }
        }
      }

      ### Replace in filteredUtterances
      filteredUtterances[utterancesWithMatches[i]] <-
        currentUtterance;

    }

    ### Replace processed rows in the input source
    input[filter] <- filteredUtterances;

  }

  return(input);

}
