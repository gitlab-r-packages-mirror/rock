#' @rdname base30conversion
#' @export
base30toNumeric <- function(x) {
  warning("The `base30toNumeric()` functon in the {rock} package is deprecated. ",
          "Change your code to use the same function in the {squids} package.");
  return(squids::base30toNumeric(x));
}
