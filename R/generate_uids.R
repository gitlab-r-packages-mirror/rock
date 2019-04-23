#' @export
generate_uids <- function(x,
                          origin=Sys.time()) {

  timeNrString <- as.character(round(as.numeric(origin) * 100, 0));
  timeNrs <-
    as.numeric(timeNrString) + (0:(x-1));
  res <-
    unlist(lapply(timeNrs,
                  numericToBase30));
  return(paste0("[[", res, "]]"));
}
