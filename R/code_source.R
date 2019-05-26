#' Add one or more codes to one or more sources
#'
#' These functions add codes to one or more
#' sources that were read with one of the
#' `loading_sources` functions.
#'
#' @param input The source, or list of sources, as
#' produced by one of the `loading_sources` functions.
#' @param codes A named character vector, where each element
#' is the code to be added to the matching utterance, and
#' the corresponding name is either an utterance identifier
#' (in which case the utterance with that identifier will be
#' coded with that code), a digit (in which case the utterance
#' at that line number in the source will be coded with that
#' code), or a regular expression, in which case all utterances
#' matching that regular expression will be coded with that
#' source.
#' @param encoding The encoding of the source file.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the coded source object.
#' @rdname coding_sources
#' @export
code_source <- function(input,
                        codes,
                        encoding="UTF-8",
                        silent=FALSE) {

  if (!("rock_source" %in% class(input))) {
    stop("With the `input` argument you must pass a ROCK source ",
         "as loaded with load_source or load_sources.");
  }

  for (i in seq_along(codes)) {
    if (grepl("^\\[\\[.*\\]\\]$", names(codes)[i])) {
      ### The name of this case is an utterance ID;
      ### get indices matching it
      indices <- grep(gsub("^\\[\\[(.*)\\]\\]$",
                           "\\\\[\\\\[\\1\\\\]\\\\]",
                           names(codes)[i]),
                      input);
      ### Append code
      input[indices] <-
        paste(input[indices],
              paste0("[[", codes[i], "]]"),
              sep=" ");
    } else if (grepl("^[0-9]+$", names(codes)[i])) {
      ### It's a number, so a line number; check whether
      ### it's not too high and then add code
      if (names(codes)[i] <= length(input)) {
        input[as.numeric(names(codes)[i])] <-
          paste(input[as.numeric(names(codes)[i])],
                paste0("[[", codes[i], "]]"),
                sep=" ");
      } else {
        stop("You specified a line number (",
             names(codes)[i], ") that's higher than ",
             "the number of lines in the source (",
             length(input),
             ")!");
      }
    } else {
      ### The name of this case is a a regular expression;
      ### get indices matching it
      indices <- grep(names(codes)[i],
                      input);
      ### Append code
      input[indices] <-
        paste(input[indices],
              paste0("[[", codes[i], "]]"),
              sep=" ");
    }
  }

  res <- input;

  class(res) <- c("rock_source", "character");

  return(invisible(res));

}
