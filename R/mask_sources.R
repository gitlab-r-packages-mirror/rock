#' @rdname masking_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param outputPrefix,outputSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk.
#' @export
mask_sources <- function(input,
                         output,
                         proportionToMask = 1,
                         outputPrefix = "",
                         outputSuffix = "_masked",
                         maskRegex = "[[:alnum:]]",
                         maskChar = "X",
                         perl = TRUE,
                         recursive=TRUE,
                         filenameRegex=".*",
                         preventOverwriting = rock::opts$get(preventOverwriting),
                         encoding = rock::opts$get(encoding),
                         silent=rock::opts$get(silent)) {

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

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  res <- character();
  for (filename in rawSourceFiles) {
    newFilename <-
      paste0(outputPrefix,
             sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(filename)),
             outputSuffix,
             ".rock");
    if (tolower(output) == "same") {
      newFileDir <-
        dirname(filename);
    } else {
      newFileDir <-
        output;
    }

    mask_source(input = filename,
                output = file.path(newFileDir,
                                   newFilename),
                proportionToMask = proportionToMask,
                maskRegex = maskRegex,
                maskChar = maskChar,
                perl = perl,
                preventOverwriting = preventOverwriting,
                encoding = encoding,
                silent = silent);
    res <-
      c(res,
        newFilename);
  }
  if (!silent) {
    message("I just wrote ", length(rawSourceFiles), " masked sources to path '",
            output,
            "' ",
            ifelse(preventOverwriting,
                   "(unless the files already existed)",
                   "(overwriting any files that may already have existed)"),
            ". Note that these files may all be overwritten if this ",
            "script is ran again (unless `preventOverwriting` is set to `TRUE`).");
  }
  invisible(res);
}
