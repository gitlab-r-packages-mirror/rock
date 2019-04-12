#' Conversion between base10 and base30 & base36
#'
#' The conversion functions from base10 to base30 are
#' used by the [generate_id()] functions; the base36
#' functions are just left here for convenience.
#'
#' The symbols to represent the 'base 30' system are the 0-9
#' followed by the alphabet without vowels but including the y. This
#' vector is available as `base30`.
#'
#' @param x The vector to convert (numeric for the `numericTo` functions,
#'          character for the `base30to` and `base36to` funtions).
#'
#' @return The converted vector (numeric for the `base30to` and `base36to` funtions,
#'          character for the `numericTo` functions).
#' @name base30and36conversion
#' @rdname base30and36conversion
#' @examples numericToBase30(654321);
#' base30toNumeric(numericToBase30(654321));
#' @export
numericToBase30 <- function(x) {
  result <- character();
  i <- 1L;
  while (x > 0) {
    result[i] <- base30[x %% 30L + 1L];
    i <- i + 1L;
    x <- x %/% 30L;
  }
  res <- paste(rev(result), sep="", collapse="")
  return(res);
}

base30 <- c(0:9,
            'b', 'c', 'd', 'f', 'g',
            'h' ,'j', 'k', 'l', 'm',
            'n', 'p', 'q', 'r', 's',
            't', 'w', 'x', 'y', 'z');
