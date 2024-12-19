#' Generate a TSSID
#'
#' A TSSID is a Timestamped Source Identifier. It is a practically
#' unique identifier for a source, conventionally the time and date
#' the source was produced (e.g. when the data were collected, for example
#' the time and date of an interview) in the UTC timezone and in ISO 8601
#' standard format.
#'
#' @param x The date and time to use.
#'
#' @returns The tssid
#' @export
#'
#' @examples generate_tssid();
generate_tssid <- function(x = Sys.time()) {

  return(
    format(
      as.POSIXct(x, tz = "UTC"),
      "%Y%m%dT%H%MZ"
    )
  );

}
