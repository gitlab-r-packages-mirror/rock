#' @rdname cleaning_sources
#' @export
clean_sources <- function(input,
                          output,
                          replacementsPre = list(c("([^\\.])(\\.\\.)([^\\.])",
                                                   "\\1.\\3"),
                                                 c("([^\\.])(\\.\\.\\.\\.+)([^\\.])",
                                                   "\\1...\\3"),
                                                 c("(\\s*\\r?\\n){3,}",
                                                   "\n")),
                          extraReplacementsPre = NULL,
                          utteranceSplits = c("([\\?\\!]+\\s?|\u2026\\s?|[[:alnum:]\\s?]\\.(?!\\.\\.)\\s?)"),
                          utteranceMarker = "\n",
                          replacementsPost = list(c("([^\\,]),([^\\s])",
                                                    "\\1, \\2")),
                          extraReplacementsPost = NULL,
                          removeNewlines = FALSE,
                          encoding = "UTF-8") {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!is.character(output) || !length(output)==1) {
    stop("Only specify a single string as 'output'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (!dir.exists(output)) {
    warning("Directory provided to write to ('",
            output,
            "') does not exist - creating it!");
    dir.create(output,
               recursive = TRUE);
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE);

  for (filename in rawSourceFiles) {
    newFilename <-
      paste0(sub("(.*)\\.[a-zA-Z0-9]+",
                 "\\1",
                 basename(rawSourceFiles)),
             ".rock");
    rock::clean_source(input = filename,
                       outputFile = here::here("data",
                                               "cleaned-sources",
                                               newFilename),
                       replacementsPre=replacementsPre,
                       extraReplacementsPre=extraReplacementsPre,
                       utteranceSplits=utteranceSplits,
                       utteranceMarker=utteranceMarker,
                       replacementsPost=replacementsPost,
                       extraReplacementsPost=extraReplacementsPost,
                       removeNewlines=removeNewlines,
                       encoding=encoding);
  }

}
