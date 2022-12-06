#' Sync (expand or compress) a vector
#'
#' @param x The vector
#' @param newLength The new length
#' @param sep The separator used to glue together elements
#' @param fill Whether to duplicate elements to completely fill the new vector
#'
#' @return The synced vector
#' @export
#'
#' @examples rock::sync_vector(letters[1:10], 15);
#' rock::sync_vector(letters[1:10], 5);
sync_vector <- function(x,
                        newLength,
                        sep = " ",
                        fill = TRUE) {

  oldLength <- length(x);

  if (oldLength == newLength) {
    return(x);
  } else if (oldLength < newLength) {
    return(
      syncing_vector_expand(
        x = x,
        newLength = newLength,
        fill = fill
      )
    );
  } else if (oldLength > newLength) {
    return(
      syncing_vector_compress(
        x = x,
        newLength = newLength,
        sep = sep
      )
    );
  }

}
