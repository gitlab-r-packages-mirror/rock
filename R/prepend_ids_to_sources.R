#' @rdname prepending_uids
#' @export
prepend_ids_to_sources <- function(input,
                                   output = NULL,
                                   encoding="UTF-8",
                                   origin=Sys.time(),
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
    tmp <-
      prepend_ids_to_source(input = filename,
                            output = file.path(output,
                                               basename(filename)),
                            origin=origin,
                            encoding = encoding,
                            silent=TRUE);
    last_uid <-
      gsub("^\\[\\[(.*)\\]\\].*$", "\\1", tail(tmp)[1]);
    print(last_uid);
    print(base30toNumeric(last_uid));
    origin <-
      as.POSIXct((1+base30toNumeric('73fb9jnd')) / 100, origin="1970-01-01");
    print(origin);
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
