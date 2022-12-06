#' @rdname compressing_vectors
#' @export
syncing_df_compress <- function(x,
                                newLength,
                                sep = " ",
                                compressFun = NULL) {

  res <-
    lapply(
      x,
      syncing_vector_compress,
      newLength = newLength,
      sep = sep,
      compressFun = compressFun
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
