#' Compress a vector or data frame
#'
#' @param x The vector or data frame
#' @param newLength The new length
#' @param sep When compressing streams, the separator to use. Ignored
#' if `sepFun` is specified.
#' @param compressFun If specified, when compressing streams, instead of pasting
#' elements together using separator `sep`, the vectors are passed to function
#' `compressFun`, which must accept a vector (to compress) and a single integer
#' (with the desired resulting length of the vector).
#'
#' @return The shortened vector
#' @rdname compressing_vectors
#' @export
#'
#' @examples rock::syncing_vector_compress(1:10, 3);
syncing_vector_compress <- function(x,
                                    newLength,
                                    sep = " ",
                                    compressFun = NULL) {

  oldLength <- length(x);
  oldIndices <- seq_along(x);
  newIndices <- floor(1 + (oldIndices - .5) / (oldLength / newLength));

  if (oldLength <= newLength) {
    stop("Currently, with length ", oldLength, ", `x` is shorter than ",
         "(or the same length as) `newLength` (", newLength, "). ",
         "Use `rock::sync_vector` to automatically detect whether the vector ",
         "should be shrunk or expanded.");
  }

  if (is.null(compressFun)) {

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

  } else {

    if (!is.function(compressFun)) {
      stop("As `compressFun`, you must pass a function. You currently passed ",
           "and object of class(es) ", vecTxtQ(class(compressFun)), ".");
    }

    newVector <-
      compressFun(
        x,
        newLength
      );

    if (length(newVector) != newLength) {
      stop("The `compressFun` you specified did not deliver a vector of ",
           "the correct length! `newLength` is ", newLength, ", but the ",
           "vector that was returned had length ", length(newVector), ".");
    }

  }

  return(newVector);

}
