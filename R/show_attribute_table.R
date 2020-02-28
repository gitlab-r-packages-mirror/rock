show_attribute_table <- function(x) {
  if (knitr::knit_) {
    print(knitr::kable(x$attributesDf));
  } else {
  }
  return(invisible(x));
}
