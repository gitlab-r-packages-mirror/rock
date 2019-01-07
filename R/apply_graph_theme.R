#' Apply multiple DiagrammeR global graph attributes
#'
#' @param dctGraph The [DiagrammeR::DiagrammeR] graph to apply the attributes to.
#' @param ... One or more character vectors of length three, where the first element is
#' the attribute, the second the value, and the third, the attribute type (`graph`,
#' `node`, or `edge`).
#'
#' @return The [DiagrammeR::DiagrammeR] graph.
#' @examples extractedSpecs <- extract_dct_specs(text=unlist(strsplit(dct::example_dct_spec, '\n')));
#' dctGraph <- parse_dct_specs(extractedSpecs)$output$graph
#' dctGraph <- apply_graph_theme(dctGraph,
#'                               c("color", "#0000AA", "node"),
#'                               c("fillcolor", "#00FFFF", "node"));
#' @export
apply_graph_theme <- function(graph,
                              ...) {
  for (currentSetting in list(...)) {
    if ((length(currentSetting) != 3) && is.character(currentSetting)) {
      stop("Only provide character vectors of length 3 in the dots (...) argument!");
    } else {
      graph <-
        DiagrammeR::add_global_graph_attrs(graph,
                                           currentSetting[1],
                                           currentSetting[2],
                                           currentSetting[3]);
    }
  }
  return(graph);
}
