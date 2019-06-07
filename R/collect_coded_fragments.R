#' Create an overview of coded fragments
#'
#' Collect all coded utterances and optionally add some context
#' (utterances before and utterances after) to create ann overview
#' of all coded fragments per code.
#'
#' @param x The parsed source(s) as provided by `rock::parse_source`
#' or `rock::parse_sources`.
#' @param codes The regular expression that matches the codes to include
#' @param context How many utterances before and after the target
#' utterances to include in the fragments.
#' @param rawResult Whether to returnt he raw result, a list of the
#' fragments, or one character value in markdown format.
#' @param cleanUtterances Whether to use the clean or the raw utterances
#' when constructing he fragments (the raw versions contain all codes).
#'
#' @return Either a list of character vectors, or a single character value.
#' @export
#'
#' @examples ### Add later
collect_coded_fragments <- function(x,
                                    codes = ".*",
                                    context = 0,
                                    rawResult = FALSE,
                                    cleanUtterances = TRUE,
                                    silent=TRUE) {

  if (!("rockParsedSource" %in% class(x)) &&
      !("rockParsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{ufs::vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rockParsedSource' ",
                    "or 'rockParsedSources'."));
  }

  codes <- grep(codes,
                x$convenience$codings,
                value=TRUE);
  metadata <- x$convenience$metadataVars;
  dat <- x$mergedSourceDf;

  if (!silent) {
    ufs::cat0("\nThe regular expression passed in argument `codes` ('",
              codes, "') matches the following codings:");
  }

  ### Get line numbers of the fragments to extract,
  ### get fragments, store them
  res <- lapply(codes,
                function(i) {
                  return(lapply(which(dat[, i] == 1),
                           function(center) {
                             res <- seq(center - context,
                                        center + context);
                             ### Shift forwards or backwards to make sure early or late
                             ### fragments don't exceed valid utterance (line) numbers
                             res <- res - min(0, (min(res) - 1));
                             res <- res - max(0, (max(res) - nrow(dat)));
                             if (cleanUtterances) {
                               return(paste0(dat[res, 'utterances_clean'],
                                             collapse="\n"));
                             } else {
                               return(paste0(dat[res, 'utterances_raw'],
                                             collapse="\n"));
                             }
                           }));
                });

  if (rawResult) {
    names(res) <-
      codes;
    return(res);
  } else {
    ### Combine all fragments within each code
    res <- lapply(res,
                  paste0,
                  collapse="\n\n-----\n\n");
    ### Unlist into vector
    res <- unlist(res);
    ### Add titles
    res <- paste0("## ", codes, "\n\n",
                  res, "\n\n-----\n");
    ### Collapse into one character value
    res <- paste0(res, collapse="\n");
    return(res);
  }
}
