#' @rdname compressing_vectors
#' @export
syncing_df_compress <- function(x,
                                newLength,
                                sep = " ") {

  res <-
    lapply(
      x,
      syncing_vector_compress,
      newLength = newLength,
      sep = sep
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
