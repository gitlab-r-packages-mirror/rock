#' Prepending unique utterance identifiers
#'
#' This function prepending unique utterance identifiers to each
#' utterance (line) in a source. Note that you'll probably want
#' to clean the sources using [clean_sources()] first.
#'
#' @param input The filename or contents of the source.
#' @param output The filename where to write the resulting file.
#' @param encoding The encoding of the file.
#'
#' @return The source with prepended uids, either invisible (if `output`
#' if specified) or visibly (if not).
#' @export
preprend_ids_to_source <- function(input,
                                   output = NULL,
                                   encoding="UTF-8") {
  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    res <- input;
    if ((length(res) == 1) && grepl('\n', res)) {
      res <-
        strsplit(res,
                 "\n")[[1]];
    }
  }

  uids <-
    generate_uids(length(res));

  res <- paste0(uids, " ", res);

  if (is.null(output)) {
    return(res);
  } else {
    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    con <- file(description=output,
                open="w",
                encoding=encoding);
    writeLines(text=res,
               con=con);
    close(con);
    if (!silent) {
      message("I just wrote a file with a source with prepended utterance identifiers (uids) to '",
              output,
              "'. Note that this file will be overwritten if this ",
              "script is ran again. Therefore, make sure to copy it to ",
              "another directory, or rename it, before starting to code this source!");
    }
    invisible(res);
  }

}
