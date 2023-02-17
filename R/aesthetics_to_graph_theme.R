aesthetics_to_graph_theme <- function(aestheticConfig) {

  themeVectors <-
    unlist(
      lapply(
        aestheticConfig,
        function(currentAesthetics) {

          ### Process graph attributes, if any
          if ("graph" %in% names(currentAesthetics)) {
            graphAttributeNames <-
              names(currentAesthetics$graph);
            res <-
              mapply(
                c,
                graphAttributeNames,
                unname(unlist(currentAesthetics$graph)),
                rep("graph", length(graphAttributeNames)),
                SIMPLIFY = FALSE
              );
          } else {
            res <- c();
          }

          ### Process node attributes, if any
          if ("nodes" %in% names(currentAesthetics)) {

            areDefaultNodeAttributes <-
              unlist(lapply(currentAesthetics$nodes,
                            `[[`,'type')) == "default";

            if (any(areDefaultNodeAttributes)) {

              defaultNodeAttributes <-
                currentAesthetics$nodes[[which(areDefaultNodeAttributes)]];

              nodeAttributeNames <-
                setdiff(
                  names(defaultNodeAttributes),
                  "type"
                );

              nodeAttributes <-
                mapply(
                  c,
                  nodeAttributeNames,
                  unname(unlist(defaultNodeAttributes[nodeAttributeNames])),
                  rep("node", length(nodeAttributeNames)),
                  SIMPLIFY = FALSE
                );

              res <-
                c(res,
                  nodeAttributes);

            }
          }

          ### Process edge attributes, if any
          if ("edges" %in% names(currentAesthetics)) {

            areDefaultEdgeAttributes <-
              unlist(lapply(currentAesthetics$edges,
                            `[[`,'type')) == "default";

            if (any(areDefaultEdgeAttributes)) {

              defaultEdgeAttributes <-
                currentAesthetics$edges[[which(areDefaultEdgeAttributes)]];

              edgeAttributeNames <-
                setdiff(
                  names(defaultEdgeAttributes),
                  "type"
                );

              edgeAttributes <-
                mapply(
                  c,
                  edgeAttributeNames,
                  unname(unlist(defaultEdgeAttributes[edgeAttributeNames])),
                  rep("edge", length(edgeAttributeNames)),
                  SIMPLIFY = FALSE
                );

              res <-
                c(res,
                  edgeAttributes);

            }
          }


          return(
            unname(
              res
            )
          );
        }
      ),
      recursive = FALSE
    );

  return(
    unname(
      themeVectors
    )
  );

}
