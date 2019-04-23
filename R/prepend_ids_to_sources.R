#' @rdname prepending_uids
#' @export
prepend_ids_to_sources <- function(input,
                                   output = NULL,
                                   encoding="UTF-8",
                                   silent=FALSE) {

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

  res <- character();
  for (filename in rawSourceFiles) {
    prepend_ids_to_source(input = filename,
                          output = file.path(output,
                                             basename(filename)),
                          encoding = encoding);
  }
  if (!silent) {
    message("I just added utterenance identifiers to ", length(rawSourceFiles),
            " sources and wrote the new files to path '",
            output,
            "'. Note that these files will all be overwritten if this ",
            "script is ran again. Therefore, make sure to copy them to ",
            "another directory before starting to code those sources!");
  }
  invisible(res);
}
