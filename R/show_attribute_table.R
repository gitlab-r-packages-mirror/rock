#' Show a table with all attributes in the RStudio viewer and/or console
#'
#' @param x A `rockParsedSources` object (the result of a call to
#' `rock::parse_sources`).
#' @param output The output: a character vector with one or more
#' of "`console`" (the raw concatenated input, without conversion
#' to HTML), "`viewer`", which uses the RStudio viewer if available,
#' and one or more filenames in existing directories.
#' @param tableOutputCSS The CSS to use for the HTML table.
#'
#' @return `x`, invisibly, unless being knitted into R Markdown,
#' in which case a [knitr::asis_output()]-wrapped character vector is returned.
#' @export
show_attribute_table <- function(x,
                                 output = rock::opts$get("tableOutput"),
                                 tableOutputCSS = ufs::opts$get("tableOutputCSS")) {
  if (isTRUE(getOption('knitr.in.progress'))) {
    return(knitr::asis_output(paste0(c("\n",
                              knitr::kable(x$attributesDf),
                              "\n"), collapse="\n")));
  } else {
    ufs::exportToHTML(x$attributesDf,
                      output = output,
                      tableOutputCSS = tableOutputCSS);
  }
  return(invisible(x));
}
