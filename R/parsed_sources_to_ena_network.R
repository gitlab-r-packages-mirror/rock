#' Create an ENA network out of one or more parsed sources
#'
#' @param x The parsed source(s) as provided by `rock::parse_source` or `rock::parse_sources`.
#' @param unitCols The columns that together define units.
#' @param conversationCols The columns that together define conversations.
#' @param codes The codes to include; by default, takes all codes.
#' @param metadata The columns in the merged source dataframe that contain the
#' metadata. By default, takes all read metadata.
#'
#' @return The result of a call to [rENA::ena.plot.network()].
#' @export
#'
#' @examples ### Add example later!
parsed_sources_to_ena_network <- function(x,
                                          unitCols,
                                          conversationCols,
                                          codes = x$convenience$codings,
                                          metadata = x$convenience$metadataVars) {

  if (!requireNamespace("rENA", quietly = TRUE)) {
    stop("To create ENA networks, the \"rENA\" package is required. ",
         "Please install it using `install.packages('rENA');`.",
         call. = FALSE);
  }

  if (!("rockParsedSource" %in% class(x)) &&
      !("rockParsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{ufs::vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rockParsedSource' ",
                    "or 'rockParsedSources'."));
  }

  allCols <-
    c(unitCols,
      conversationCols,
      codes,
      metadata);

  if (!all(allCols %in% names(x$mergedSourceDf))) {
    stop(glue::glue("Not all columns you specified exist in the 'mergedSourceDf' in the object you provided! Specifically, you provided:\n\n",
                    "unitCols = {ufs::vecTxtQ(unitCols)}\n",
                    "conversationCols = {ufs::vecTxtQ(conversationCols)}\n",
                    "codes = {ufs::vecTxtQ(codes)}\n",
                    "metadata = {ufs::vecTxtQ(metadata)}\n\n",
                    "However, the following columns were not found: {ufs::vecTxtQ(allCols[!(allCols %in% names(x$mergedSourceDf))])}."));
  }

  dat <-
    x$mergedSourceDf[, allCols];

  ENA_accumulated_data <-
    rENA::ena.accumulate.data(
      units = dat[, unitCols, drop=FALSE],
      conversation = dat[, conversationCols, drop=FALSE],
      codes = dat[, codes, drop=FALSE],
      metadata = dat[, metadata, drop=FALSE]
    );

  ENA_set <-
    rENA::ena.make.set(enadata=ENA_accumulated_data);

  ENA_plot <-
    rENA::ena.plot(ENA_set,
                   rotation.by = rENA::ena.rotate.by.mean);

  ENA_network_plot <-
    rENA::ena.plot.network(ENA_plot,
                           network=colSums(ENA_set$line.weights));

  return(ENA_network_plot);

}
