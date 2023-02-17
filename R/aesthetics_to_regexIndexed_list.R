aesthetics_to_regexIndexed_list <- function(aestheticConfig) {

  aestheticConfig <- unname(aestheticConfig);

  customAesthetic <-
    unlist(
      lapply(
        aestheticConfig,
        function(currentAesthetics) {

          ### Process node attributes, if any
          if ("nodes" %in% names(currentAesthetics)) {

            areCustomNodeAttributes <-
              unlist(lapply(currentAesthetics$nodes,
                            `[[`,'type')) != "default";

            if (any(areCustomNodeAttributes)) {

              customNodeAttributes <-
                currentAesthetics$nodes[which(areCustomNodeAttributes)];

              regexes <-
                unlist(lapply(currentAesthetics$nodes,
                              `[[`,'type'));

              nodeAttributesByRegex <-
                lapply(
                  customNodeAttributes,
                  function(x) {
                    return(
                      x[names(x) != "type"]
                    )
                  }
                );
              names(nodeAttributesByRegex) <-
                regexes[areCustomNodeAttributes];

            } else {
              nodeAttributesByRegex <- c();
            }
          } else {
            nodeAttributesByRegex <- c();
          }

          ### Process edge attributes, if any
          if ("edges" %in% names(currentAesthetics)) {

            areCustomEdgeAttributes <-
              unlist(lapply(currentAesthetics$edges,
                            `[[`,'type')) != "default";

            if (any(areCustomEdgeAttributes)) {

              customEdgeAttributes <-
                currentAesthetics$edges[which(areCustomEdgeAttributes)];

              regexes <-
                unlist(lapply(currentAesthetics$edges,
                              `[[`,'type'));

              edgeAttributesByRegex <-
                lapply(
                  customEdgeAttributes,
                  function(x) {
                    return(
                      x[names(x) != "type"]
                    )
                  }
                );
              names(edgeAttributesByRegex) <-
                regexes[areCustomEdgeAttributes];

              browser();

            } else {
              edgeAttributesByRegex <- c();
            }
          } else {
            edgeAttributesByRegex <- c();
          }

          return(
            list(
              nodeAttributes = nodeAttributesByRegex,
              edgeAttributes = edgeAttributesByRegex
            )
          );
        }
      ),
      recursive = FALSE
    );

  return(
    customAesthetic
  );

}
