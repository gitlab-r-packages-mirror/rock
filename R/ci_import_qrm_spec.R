#' Import a Qualitative Response Model specification
#'
#' @param x A path to a file or an URL to a Google Sheet, passed
#' to [rock::read_spreadsheet()].
#' @param read_ss_args A named list with arguments to pass to
#' [rock::read_spreadsheet()].
#' @param silent Whether to be silent or chatty.
#'
#' @return A `rock_ci_qrm` object.
#' @rdname rock_ci_qrm
#' @export
#'
#' @examples
ci_import_qrm_spec <- function(x,
                               read_ss_args=list(exportGoogleSheet = TRUE),
                               silent = rock::opts$get("silent")) {

  if (is.null(read_ss_args)) {
    read_ss_args <- list(x = x);
  } else {
    read_ss_args <- c(list(x = x),
                      read_ss_args);
  }

  qrm_spec <-
    do.call(
      read_spreadsheet,
      read_ss_args
    );

  class(qrm_spec) <- "rock_qrm_spec";

  res <- list(qrm_spec = qrm_spec);

  languages <- unique(qrm_spec$stimuli$language);

  res$interviewSchemes <- list();

  for (currentLanguage in languages) {
    res$interviewSchemes[[currentLanguage]] <-
      ci_create_interviewScheme(
        qrm_spec,
        language = currentLanguage
      );
  }

  class(res) <- "rock_ci_qrm";

  return(res);

}

#' @rdname rock_ci_qrm
#' @export
#' @method print rock_ci_qrm
print.rock_ci_qrm <- function(x, ...) {

  cat("Qualitative Response Model Specification for Cognitive Interviews\n\n");

  cat0("This specification contains ", nrow(x$qrm_spec$instrument), " items.\n");

  return(invisible(x));

}
