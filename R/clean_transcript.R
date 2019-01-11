clean_transcript <- function(input,
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
