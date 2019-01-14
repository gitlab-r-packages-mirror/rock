#' Cleaning sources
#'
#' @param input for `clean_source`, either a character vector containing the text
#' of the relevant source *or* a path to a file that contains the source text;
#' for `clean_sources`, a path to a directory that contains the sources to clean.
#' @param outputFile If not `NULL`, this is the name (and path) of the file in
#' which to save the cleaned source.
#' @param replacements A list of two-element vectors, where the first element in each
#' vector contains a regular expression to search for in the source(s), and the second
#' element contains the replacement (these are passed as `perl` regular expressions; see
#' \code{\link{regex}} for more information). Instead of regular expressions, simple
#' words or phrases can also be entered of course (since those are valid regular
#' expressions).
#' @param extraReplacements To perform more replacements than the default set, these
#' can be conveniently specified in `extraReplacements`. This prevents you from having to
#' manually copypaste the list of defaults to retain it.
#' @param utteranceSplits This is a vector of regular expressions that specify where to
#' insert breaks between utterances in the source(s). Such breakes are specified using
#' `utteranceMarker`.
#' @param utteranceMarker How to specify breaks between utterances in the source(s). The
#' ROCK convention is to use a newline (`\n`).
#' @param removeNewlines Whether to remove all newline characters from the source before
#' starting to clean them.
#' @param encoding The encoding of the source(s).
#'
#' @return A character vector for `clean_source`, or a list of character vectors , for `clean_sources`.
#' @export
#' @rdname cleaning_sources
#'
#' @examples
clean_source <- function(input,
                         outputFile = NULL,
                         replacements = list(c("[^\\.](\\.\\.)[^\\.]",
                                               "."),
                                             c("[^\\.](\\.\\.\\.\\.+)[^\\.]",
                                               "..."),
                                             c("\\n\\n\\n+",
                                               "\n\n")),
                         extraReplacements = NULL,
                         utteranceSplits = c("([[:alnum:]\\s]\\.(?!\\.\\.)\\s|[\\?\\!]+\\s|…\\s)"),
                         utteranceMarker = "\n",
                         removeNewlines = FALSE,
                         encoding = "UTF-8") {

  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    res <- input;
  }

  if (removeNewlines) {
    res <-
      paste0(res, collapse="");
  } else {
    res <-
      paste0(res, collapse="\n");
  }

  if (!is.null(extraReplacements)) {
    replacements <- c(replacements,
                      extraReplacements);
  }

  if (!is.null(replacements)) {
    for (i in seq_along(replacements)) {
      res <- gsub(replacements[[i]][1],
                  replacements[[i]][2],
                  res,
                  perl=TRUE);
    }
  }

  if (!is.null(utteranceSplits)) {
    for (i in seq_along(utteranceSplits)) {
      res <- gsub(utteranceSplits[i],
                  paste0("\\1", utteranceMarker),
                  res,
                  perl=TRUE);
    }
  }

  if (is.null(outputFile)) {
    return(res);
  } else {
    writeLines(text=res,
               con=con<-file(outputFile,
                             open="w",
                             encoding=encoding));
    close(con);
    invisible(res);
  }

}
