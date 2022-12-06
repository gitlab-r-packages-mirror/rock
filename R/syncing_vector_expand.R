#' Expand a vector
#'
#' @param x The vector
#' @param newLength The new length
#' @param fill Whether to duplicate elements to completely fill the new vector
#'
#' @return The expanded vector
#' @rdname expanding_vectors
#' @export
#'
#' @examples rock::syncing_vector_expand(letters[1:10], 15);
#' rock::syncing_vector_expand(letters[1:10], 15, fill=FALSE);
syncing_vector_expand <- function(x,
                                  newLength,
                                  fill = TRUE) {

  oldLength <- length(x);
  oldIndices <- seq_along(x);
  newIndices <- floor(1 + (oldIndices - .5) / (oldLength / newLength));

  if (oldLength >= newLength) {
    stop("Currently, with length ", oldLength,
         ", `x` is longer than (or the same length as) `newlength` (",
         newLength, "). ",
         "Use `rock::sync_vector` to automatically detect whether the vector ",
         "should be shrunk or expanded.");
  }

  if (length(x) == 1) {
    if (fill) {
      return(rep(x, newLength));
    } else {
      return(c(x, rep("", newLength-1)));
    }
  }

  newVector <- c();

  for (newIndex in 1:newLength) {
    if (newIndex %in% newIndices) {
      newVector[newIndex] <- x[newIndices == newIndex];
      oldValue <- newVector[newIndex];
    } else {
      if (fill) {
        newVector[newIndex] <- oldValue;
      } else {
        newVector[newIndex] <- "";
      }
    }
  }

  return(newVector);

}
