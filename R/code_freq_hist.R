#' Create a frequency histogram for codes
#'
#' @param parsedSource A parsed source object.
#' @param codes A regular expression to select codes to include.
#' @param sortByFreq Whether to sort by frequency decreasingly
#' (`decreasing`, the default), increasingly (`increasing`),
#' or alphabetically (`NULL`).
#'
#' @return a [ggplot2::ggplot()].
#' @export
code_freq_hist <- function(parsedSource,
                           codes = ".*",
                           sortByFreq = "decreasing") {

  parsedSource$countedCodings <-
    parsedSource$countedCodings[grepl(codes,
                                      names(parsedSource$countedCodings))];

  tmpDf <-
    data.frame(Code = names(parsedSource$countedCodings),
               Frequency = parsedSource$countedCodings);

  if ((!is.null(sortByFreq)) && (sortByFreq == "increasing")) {
    sortOrder <- order(tmpDf$Frequency,
                       decreasing = TRUE);
  } else if ((!is.null(sortByFreq)) && (sortByFreq == "decreasing")) {
    sortOrder <- order(tmpDf$Frequency,
                       decreasing = FALSE);
  } else {
    sortOrder <- order(tmpDf$Code,
                       decreasing = TRUE);
  }

  tmpDf$Code <- factor(tmpDf$Code,
                       levels = tmpDf$Code[sortOrder],
                       labels = tmpDf$Code[sortOrder],
                       ordered = TRUE);

  tmpDf$codeNr <- as.numeric(tmpDf$Code);

  res <- ggplot2::ggplot(data=tmpDf,
                         mapping=ggplot2::aes_string(x='codeNr',
                                                     y='Frequency')) +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::scale_x_continuous(name = "Code",
                                breaks=tmpDf$codeNr[sortOrder],
                                labels=tmpDf$Code[sortOrder],
                                sec.axis = ggplot2::dup_axis(labels = tmpDf$Frequency[sortOrder])) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color="#eeeeee",
                                                              size=.1)) +
    ggplot2::coord_flip();

  return(res);

}
