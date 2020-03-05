#' Create a frequency histogram for codes
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include.
#' @param sortByFreq Whether to sort by frequency decreasingly
#' (`decreasing`, the default), increasingly (`increasing`),
#' or alphabetically (`NULL`).
#'
#' @return a [ggplot2::ggplot()].
#' @export
code_freq_hist <- function(x,
                           codes = ".*",
                           sortByFreq = "decreasing") {

  if (!(("rockParsedSources" %in% class(x)) |
        ("rockParsedSource"  %in% class(x)))) {
    stop("As `x`, you must pass either an `rockParsedSource` or ",
         "an `rockParsedSources` object (i.e. either the result ",
         "from a call to `rock::parseSource()` or the result from ",
         "a call to `rock::parseSources()`). However, you ",
         "provided an object of class ", vecTxtQ(x), ".");
  }

  if ("rockParsedSource" %in% class(x)) {

    x$countedCodings <-
      x$countedCodings[grepl(codes,
                             names(x$countedCodings))];

    tmpDf <-
      data.frame(Code = names(x$countedCodings),
                 Frequency = x$countedCodings);

  } else {

    tmpDf <-
      do.call(
        rbind,
        lapply(names(x$parsedSources),
               function(y) {
                 srcObj <- x$parsedSources[[y]];
                 if ('countedCodings' %in% names(srcObj)) {
                   res <-
                     srcObj$countedCodings[grepl(codes,
                                                 names(srcObj$countedCodings))];
                   res <- data.frame(Code = names(res),
                                     Frequency = res,
                                     Source = y);
                   return(res);
                 } else {
                   return(NULL);
                 }
               }));

  }

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

  if ("rockParsedSource" %in% class(x)) {
    res <- ggplot2::ggplot(data=tmpDf,
                           mapping=ggplot2::aes_string(x='codeNr',
                                                       y='Frequency'));
  } else {
    res <- ggplot2::ggplot(data=tmpDf,
                           mapping=ggplot2::aes_string(x='codeNr',
                                                       y='Frequency',
                                                       fill = 'Source')) +
      ggplot2::scale_fill_viridis_d(end=.9);
  }

  res <- res +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::scale_y_continuous(breaks = 0:9999) +
    ggplot2::scale_x_continuous(name = "Code",
                                breaks=tmpDf$codeNr[sortOrder],
                                labels=tmpDf$Code[sortOrder],
                                sec.axis = ggplot2::dup_axis(labels = tmpDf$Frequency[sortOrder],
                                                             name = "Frequency")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color="#eeeeee",
                                                              size=.1)) +
    ggplot2::coord_flip();

  return(res);

}
