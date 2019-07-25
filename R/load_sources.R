#' @rdname loading_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @export
load_sources <- function(input,
                         encoding="UTF-8",
                         filenameRegex=".*",
                         recursive=TRUE,
                         silent=FALSE) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  rawSourceFiles <-
    list.files(input,
               pattern=filenameRegex,
               recursive=recursive,
               full.names=TRUE);

  res <- list();
  for (filename in rawSourceFiles) {
    res[[basename(filename)]] <-
      load_source(filename,
                  encoding=encoding,
                  silent=TRUE);
  }

  if (!silent) {
    message("I just loaded ", length(rawSourceFiles), " sources.");
  }

  class(res) <-
    "rock_loaded_sources_list";

  invisible(res);
}
