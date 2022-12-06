#' @rdname expanding_vectors
#' @export
syncing_df_expand <- function(x,
                              newLength,
                              fill = TRUE,
                              expandFun = NULL) {

  res <-
    lapply(
      x,
      syncing_vector_expand,
      newLength = newLength,
      fill = fill
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
