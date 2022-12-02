#' Synchronize multiple streams
#'
#' This function maps the codes from multiple streams onto a primary stream.
#'
#' @param x The object with the parsed sources.
#' @param primaryStream The identifier of the primary stream.
#' @param anchorsCol The column containing the anchors.
#' @param sourceId The column containing the source identifiers.
#' @param streamId The column containing the stream identifiers.
#'
#' @return
#' @export
#'
#' @examples
sync_streams <- function(x,
                         primaryStream,
                         anchorsCol = rock::opts$get('anchorsCol'),
                         sourceId = rock::opts$get('sourceId'),
                         streamId = rock::opts$get('streamId')) {

  df <- x$mergedSourceDf;

  sourceIds <-
    unique(df[, sourceId]);

  sourceIds <-
    sourceIds[!is.na(sourceIds)];

  streamIds <-
    unique(df[, streamId]);

  streamIds <-
    streamIds[!is.na(streamIds)];

  dfBySource <-
    lapply(
      sourceIds,
      function(currentSourceId) {
        return(
          df[df[, sourceId] == currentSourceId, ]
        );
      }
    );

  names(dfBySource) <- sourceIds;

  dfBySourceAndStream <-
    lapply(
      dfBySource,
      function(currentDfBySource) {
        res <-
          lapply(
            streamIds,
            function(currentStreamId) {
              ### Extract relevant data frame rows
              return(
                currentDfBySource[
                  currentDfBySource[, streamId] == currentStreamId, ]
              );
            }
          );
        names(res) <-
          streamIds;
        return(res);
      }
    );

  anchorVectors <-
    lapply(
      dfBySourceAndStream,
      function(currentDfBySource) {
        return(
          lapply(
            currentDfBySource,
            function(currentDfBySourceAndStream) {
              ### Get and check anchors
              allAnchors <-
                currentDfBySourceAndStream[, anchorsCol];
              allAnchors <-
                sort(
                  allAnchors[!is.na(allAnchors) & nchar(allAnchors) > 0]
                );
              if (any(duplicated(allAnchors))) {
                stop("Found a duplicate anchor!");
              }
              return(allAnchors);
            }
          )
        );
      }
    );

  ### Check for uniqueness
  lapply(
    anchorVectors,
    function(anchorVectorForCurrentSource) {
      anchorDf <-
        as.data.frame(
          syncedres$syncResults$anchorVectors$source1
        );
      res <-
        lapply(
          as.data.frame(
            t(
              anchorDf
            )
          ),
          unique
        );
      if (!all(length(res) == 1)) {
        stop(
          "Not all anchors align!\n\n",
          paste0(
            capture.output(
              print(anchorDf)
            ),
            collapse="\n"
          ),
          "\n\n"
        );
      }
      return(res);
    }
  );

  x$syncResults <-
    list(
      sourceIds = sourceIds,
      streamIds = streamIds,
      dfBySource = dfBySource,
      dfBySourceAndStream = dfBySourceAndStream,
      anchorVectors = anchorVectors
    );

  return(
    invisible(
      x
    )
  );

}
