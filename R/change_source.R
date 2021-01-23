#' Do something with a source
#'
#' This function contains the general set of actions that are always used
#' when doing something to change a source (e.g. check the input, document the
#' justification, etc). Users should normally never call this function.
#'
#' @param input A file with a source or an object with a source
#' produced by one of the `loading_sources` functions.
#' @param output If specified, the coded source will be written here.
#' @param func The function to apply.
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param justification The justification for this action.
#' @param justificationFile If specified, the justification is appended to
#' this file. If not, it is saved to the [justifier::workspace()]. This can
#' then be saved or displayed at the end of the R Markdown file or R script
#' using [justifier::save_workspace()].
#' @param preventOverwriting Whether to prevent overwriting existing files
#' when writing the files to `output`.
#' @param encoding The encoding to use.
#' @param silent Whether to be chatty or quiet.
#' @param ... Other arguments to pass to `fnc`.
#'
#' @return Invisibly, the recoded source(s) or source(s) object.
#' @rdname changing_sources
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
change_source <- function(input,
                          codes,
                          func,
                          filter = TRUE,
                          output = NULL,
                          decisionLabel = NULL,
                          justification = NULL,
                          justificationFile = rock::opts$get('justificationFile'),
                          preventOverwriting = rock::opts$get('preventOverwriting'),
                          encoding = rock::opts$get('encoding'),
                          silent = rock::opts$get('silent'),
                          ...) {

  ### Check input
  if (!is.character(input)) {
    stop("With the `input` argument you must pass either the ",
         "path to a file with a source, a character vector ",
         "containing the source, or a ROCK source ",
         "as loaded with load_source or load_sources.\n");
  }

  input <- load_source(input);

  yamlDelimiters <- yaml_delimiter_indices(input);

  if (length(yamlDelimiters) > 1) {
    delimiterMatches <-
      match_consecutive_delimiters(yamlDelimiters);
  }

  ### Store YAML fragments so we can restore them later
  yamlFragments <-
    lapply(
      seq_along(delimiterMatches),
      function(i) {
        return(
          input[
            delimiterMatches[[i]][1]:delimiterMatches[[i]][2]
          ]
        );
      }
    );

  ### Empty lines with YAML fragments
  for (i in seq_along(delimiterMatches)) {
    input[delimiterMatches[[i]][1]:delimiterMatches[[i]][2]] <- "";
  }

  ### Process filter
  if (!("rock_filter" %in% class(filter))) {
    filter <-
      get_source_filter(
        input,
        filter
      );
  }

  ### Get the function to call
  if (is.function(func)) {
    funcName <- deparse(substitute(func));
  } else if (func %in% getNamespaceExports('rock')) {
    funcName <- func;
    func <- getExportedValue("rock", funcName);
  } else if(grepl("^(\\w+)\\:\\:(\\w+)$", "\\1", func)) {
    pkg <- gsub("^(\\w+)\\:\\:(\\w+)$", "\\1", func);
    funcName <- gsub("^(\\w+)\\:\\:(\\w+)$", "\\2", func);
    func <- getExportedValue(pkg, funcName);
  } else if (exists(func)) {
    funcName <- func;
    func <- get(funcName);
  } else {
    stop("Function `", func, "` is not an exported function from the `rock` ",
         "package, nor can I find it anywhere else!");
  }

  ### Call the function
  input <-
    func(input = input,
         codes = codes,
         filter = filter,
         silent = silent,
         ...);

  ### Restore YAML fragments
  for (i in seq_along(delimiterMatches)) {
    input[delimiterMatches[[i]][1]:delimiterMatches[[i]][2]] <-
      yamlFragments[[i]];
  }

  ### Document the justification

  if (!is.null(justification)) {

    if (is.null(decisionLabel)) {
      decisionLabel <-
        paste0(
          "Decided to apply `", funcName, "` to source `",
          as.character(substitute(source)),
          "` for codes [still have to add this bit]."
        );
    }

    decisionObject <-
      justifier::log_decision(
        label = decisionLabel,
        justification = justification
      );

    if (!is.null(justificationFile)) {

      justifier::export_justification(
        decisionObject,
        file = justificationFile,
        append = TRUE
      );

    }

  }

  ### Return, or save and return, the result.

  if (is.null(output)) {
    class(input) <- c("rock_source", "character");
    return(input);
  } else {

    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      if (!silent) {
        message("File '",
                output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the source with added codes to disk.");
      }
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=input,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a source with added codes to file '",
              output,
              "'.");
    }
  }

  class(input) <- c("rock_source", "character");
  return(invisible(input));

}
