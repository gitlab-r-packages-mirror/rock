#' Sync (expand or compress) a vector
#'
#' @param x The vector
#' @param newLength The new length
#'
#' @return The synced vector
#' @inheritParams syncing_vector_compress,syncing_vector_expand
#' @export
#'
#' @examples rock::sync_vector(letters[1:10], 15);
#' rock::sync_vector(letters[1:10], 5);
sync_vector <- function(x,
                        newLength,
                        sep = " ",
                        fill = TRUE,
                        compressFun = NULL,
                        expandFun = NULL) {

  oldLength <- length(x);

  if (oldLength == newLength) {
    return(x);
  } else if (oldLength < newLength) {
    return(
      syncing_vector_expand(
        x = x,
        newLength = newLength,
        fill = fill,
        expandFun = expandFun
      )
    );
  } else if (oldLength > newLength) {
    return(
      syncing_vector_compress(
        x = x,
        newLength = newLength,
        sep = sep,
        compressFun = compressFun
      )
    );
  }

}
