show_attribute_table <- function(x,
                                 output = rock::opts$get(tableOutput),
                                 tableOutputCSS = ufs::opts$get(tableOutputCSS)) {
  if (isTRUE(getOption('knitr.in.progress'))) {
    print(knitr::kable(x$attributesDf));
  } else {
    ufs::exportToHTML(x$attributesDf,
                      output = output,
                      tableOutputCSS = tableOutputCSS);
  }
  return(invisible(x));
}
