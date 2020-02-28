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

  for (i in trees) {
    if (grep("both|text", output)) {
      cat0("\n\n",
           repStr("#", headingLevel),
           " Inductive code tree for ",
           i,
           "\n\n");
      print(x$inductiveCodeTrees[[i]]);
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

  return(invisible(x));

}
