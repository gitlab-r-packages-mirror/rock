#' Export parsed sources to HTML
#'
#' These function can be used to convert one or more parsed sources to HTML.
#'
#' @param input An object of class `rockParsedSource` (as resulting from a call
#' to `parse_source`) or of class `rockParsedSources` (as resulting from a call
#' to `parse_sources`.
#' @param output Either NULL to not write any files, or, if `input` is a single
#' `rockParsedSource`, the filename to write to, and if `input` is a `rockParsedSources`
#' object, the path to write to. This path will be created with a warning
#' if it does not exist.
#' @param template The template to load; either the name of one
#' of the ROCK templates (currently, only 'default' is available), or
#' the path and filename of a CSS file.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param encoding The encoding to use when writing the exported source(s).
#' @param silent Whether to suppress messages.
#'
#' @return A list of character vectors.
#' @rdname export_to_html
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Parse all example sources in that directory
#' parsedExamples <- rock::parse_sources(examplePath);
#'
#' ### Export results to a temporary directory
#' tmpDir <- tempdir(check = TRUE);
#' prettySources <-
#'   export_to_html(input = parsedExamples,
#'                  output = tmpDir);
#'
#' ### Show first one
#' print(prettySources[[1]]);
#'
#' @export
export_to_html <- function(input,
                           output = NULL,
                           template = "default",
                           preventOverwriting = rock::opts$get(preventOverwriting),
                           encoding = rock::opts$get(encoding),
                           silent=rock::opts$get(silent)) {

  htmlPre <-
    "\n<html><head>\n";

  fullCSS <-
    rock::css(template = "default");

  htmlMid <-
    "\n</head><body>\n";

  htmlPost <-
    "\n</body></html>\n";

  utterancePre <-
    "<div class='utterance'>";

  utterancePost <-
    "</div>\n";

  if ("rockParsedSource" %in% class(input)) {

    res <-
      add_html_tags(x = input$rawSourceDf$utterances_raw);
    res <- paste0(utterancePre, res, utterancePost);
    res <- paste0(htmlPre,
                  fullCSS,
                  htmlMid,
                  paste0(res,
                         collapse="\n"),
                  htmlPost);
    if (is.null(output)) {
      return(res);
    } else if (!dir.exists(dirname(output))) {
      stop("The directory specified to save the output file to ('",
           dirname(output),
           "') does not exist!");
    } else {
      if (file.exists(output) && preventOverwriting) {
        if (!silent) {
          message("File '",
                  output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                  "write the converted source to disk.");
        }
      } else {
        con <- file(description=output,
                  open="w",
                  encoding=encoding);
        writeLines(text=res,
                   con=con);
        close(con);
        if (!silent) {
          message("I just wrote a converted source to file '",
                  output,
                  "'. Note that this file may be overwritten if this ",
                  "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
                  "Therefore, make sure to copy it to ",
                  "another directory, or rename it, before starting to code this source!");
        }
        invisible(res);
      }
    }
  } else if  ("rockParsedSources" %in% class(input)) {
    filenames <- names(input$parsedSources);
    if (!dir.exists(output)) {
      dir.create(output,
                 recursive = TRUE);
    }

    res <-
      lapply(filenames,
             function(x) {
               res <-
                 export_to_html(input=input$parsedSources[[x]],
                                output=file.path(output,
                                                 paste0(basename(x), ".html")),
                                template = template,
                                preventOverwriting = preventOverwriting,
                                encoding = encoding,
                                silent=silent);
             });
  } else {
    stop("As argument 'input', only provide an object with parsed sources, ",
         "such as results from a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. You provided an object of class(es) ",
         rock::vecTxtQ(class(input)), ".");
  }

  if (is.null(output)) {
    return(res);
  } else {
    return(invisible(res));
  }

}
