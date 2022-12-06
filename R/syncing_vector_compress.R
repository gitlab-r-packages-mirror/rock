#' Compress a vector or data frame
#'
#' @param x The vector or data frame
#' @param newLength The new length
#' @param sep The separator used to glue together elements
#'
#' @return The shortened vector
#' @rdname compressing_vectors
#' @export
#'
#' @examples rock::syncing_vector_compress(1:10, 3);
syncing_vector_compress <- function(x,
                                    newLength,
                                    sep = " ") {

  oldLength <- length(x);
  oldIndices <- seq_along(x);
  newIndices <- floor(1 + (oldIndices - .5) / (oldLength / newLength));

  if (oldLength <= newLength) {
    stop("Currently, with length ", oldLength, ", `x` is shorter than ",
         "(or the same length as) `newLength` (", newLength, "). ",
         "Use `rock::sync_vector` to automatically detect whether the vector ",
         "should be shrunk or expanded.");
  }

  newVector <-
    c(
      unlist(
        lapply(
          1:newLength,
          function(newIndex) {
            return(
              paste0(
                x[newIndices == newIndex],
                collapse = sep
              )
            )
          }
        )
      )
    );

  return(newVector);

}
