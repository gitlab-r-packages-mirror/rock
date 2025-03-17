#' Soft Non-numeric Occurrence Estimation (SNOE) plot
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include, or,
#' alternatively, a character vector with literal code idenfitiers.
#' @param estimateWithin The column specifying within what to count.
#' @param sortByFreq Whether to sort by frequency decreasingly
#' (`decreasing`, the default), increasingly (`increasing`),
#' or alphabetically (`NULL`).
#' @param forceRootStripping Force the stripping of roots, even if they are
#' different.
#' @param ggplot2Theme Can be used to specify theme elements for the plot.
#' @param silent Whether to be chatty or silent.
#'
#' @return a [ggplot2::ggplot()].
#' @export
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' exampleFile <-
#'   readLines(
#'     "https://codeberg.org/explicate/ehps-2024-pluriformity-uniformity-roundtable/raw/branch/main/data-coded/sourceId-20240905T0800Z---coderId-consensus-1.rock"
#'   );
#'
#' ### Load example source
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show code frequencies
#' rock::snoe_plot(
#'   loadedExample
#' );
snoe_plot <- function(x,
                      codes = ".*",
                      matchRegexAgainstPaths = TRUE,
                      estimateWithin = NULL) {

  if ((!inherits(x, "rock_parsedSources")) && (!inherits(x, "rock_parsedSource"))) {

    stop("As `x`, you have to pass an object with one or more parsed sources, ",
         "as produced by a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. This object should have class ",
         "`rock_parsedSource` or `rock_parsedSources`, but the object ",
         "you passed has class(es) ", vecTxtQ(class(x)), ".");

  }

  if (length(codes) > 1) {
    codes <- paste(codes, collapse="|");
  }

  if (matchRegexAgainstPaths) {
    codesToInclude <-
      names(x$convenience$codingPaths)[
        grepl(
          codes,
          x$convenience$codingPaths,
          perl = TRUE
        )
      ];
  } else {
    codesToInclude <-
      x$convenience$codingLeaves[
        grepl(
          codes,
          x$convenience$codingLeaves,
          perl = TRUE
        )
      ];
  }


  browser();


  df |>
    ggplot2::ggplot(ggplot2::aes(x = group, y = value, fill = subgroup)) +
    ggdist::stat_ccdfinterval(ggplot2::aes(slab_alpha = ggplot2::after_stat(f)),
                              thickness = 1, position = "dodge", fill_type = "segments", alpha=0
    )

  ggplot2::ggplot(df_na, ggplot2::aes(x = value, y)) +
    ggplot2::geom_bar(ggplot2::aes(fill = y), stat = "identity") +
    ggplot2::scale_fill_gradient(low = "yellow", high = "red", na.value = NA)



ggplot2::ggplot(df_na, ggplot2::aes(xmax = value, y = y)) +
    ggplot2::geom_ribbon(ggplot2::aes(fill = y, xmin = 0), stat = "identity") +
    ggplot2::scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

### https://stackoverflow.com/questions/53397131/gradient-fill-in-ggplot2

n <- 1169
df22 <- data.frame(x = 1:n, val = seq(0, 0.5, length.out = n), type = 1)


grad_ungroup <- grid::linearGradient(
  c("blue", "red"),
  x1 = grid::unit(0, "npc"), y1 = grid::unit(0, "npc"),
  x2 = grid::unit(0, "npc"), y2 = grid::unit(1, "npc")
)

ggplot2::ggplot(df22, ggplot2::aes(x = x)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymax = val, ymin = 0),
    fill = grad_ungroup
  )


}
