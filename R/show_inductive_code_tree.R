#' Show the inductive code tree(s)
#'
#' This function shows one or more inductive code trees.
#'
#' @param x A `rockParsedSources` object (the result of a call to
#' `rock::parse_sources`).
#' @param codes A regular expression: only code trees from codes coded
#' with a coding pattern with this name will be shown.
#' @param output Whether to show the code tree in the console (`text`),
#' as a plot (`plot`), or both (`both`).
#' @param headingLevel The level of the heading to insert when showing the
#' code tree as text.
#' @param nodeStyle,edgeStyle,graphStyle Arguments to pass on to,
#' respectively, [data.tree::SetNodeStyle()], [data.tree::SetEdgeStyle()],
#' and [data.tree::SetGraphStyle()].
#'
#' @return `x`, invisibly, unless being knitted into R Markdown,
#' in which case a [knitr::asis_output()]-wrapped character vector is returned.
#' @export
show_inductive_code_tree <- function(x,
                                     codes = ".*",
                                     output = "both",
                                     headingLevel = 3,
                                     nodeStyle = list(shape = "box",
                                                      fontname = "Arial"),
                                     edgeStyle = list(arrowhead = "none"),
                                     graphStyle = list(rankdir = "LR")) {

  if (!(("rockParsedSources" %in% class(x)) |
        ("rockParsedSource"  %in% class(x)))) {
    stop("As `x`, you must pass either an `rockParsedSource` or ",
         "an `rockParsedSources` object (i.e. either the result ",
         "from a call to `rock::parseSource()` or the result from ",
         "a call to `rock::parseSources()`). However, you ",
         "provided an object of class ", vecTxtQ(x), ".");
  }

  trees <- names(x$inductiveCodeTrees);

  res <- c();

  for (i in trees) {
    if (grep("both|text", output)) {
      if (isTRUE(getOption('knitr.in.progress'))) {
        res <- c(res,
                 "\n\n",
                 repStr("#", headingLevel),
                 " Inductive code tree for ",
                 i,
                 "\n\n<pre>",
                 paste0(
                   utils::capture.output(
                     print(
                       x$inductiveCodeTrees[[i]])
                     ),
                   collapse="\n"
                 ),
                 "</pre>");
      } else {
        print(x$inductiveCodeTrees[[i]]);
      }
    }
    if (grep("both|plot", output)) {
      do.call(data.tree::SetNodeStyle,
              c(list(node = x$inductiveCodeTrees[[i]]),
                nodeStyle));
      do.call(data.tree::SetEdgeStyle,
              c(list(node = x$inductiveCodeTrees[[i]]),
                edgeStyle));
      do.call(data.tree::SetGraphStyle,
              c(list(root = x$inductiveCodeTrees[[i]]),
                graphStyle));
      print(plot(x$inductiveCodeTrees[[i]]));
    }
  }

  if (isTRUE(getOption('knitr.in.progress'))) {
    return(knitr::asis_output(c("\n\n",
                                res,
                                "\n\n")));
  }

  return(invisible(x));

}
