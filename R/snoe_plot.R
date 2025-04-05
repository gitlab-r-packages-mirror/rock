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
#' @param title Title of the plot
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
#'   loadedExample,
#'   codes = "un"
#' );
snoe_plot <- function(x,
                      codes = ".*",
                      matchRegexAgainstPaths = TRUE,
                      estimateWithin = NULL,
                      title = "SNOE plot",
                      ggplot2Theme = ggplot2::theme_minimal(),
                      colors = c("#0072B2", "#E69F00")) {

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

  if (inherits(x, "rock_parsedSource")) {

    counts_total <-
      apply(
        x$qdt[, codesToInclude],
        2,
        sum
      );

    totalUtterances <- nrow(x$qdt);

    totalCodings <- sum(x$qdt[, x$convenience$codingLeaves]);

    totalCodedUtterances <-
      sum(
        as.numeric(
          apply(
            x$qdt[, x$convenience$codingLeaves],
            1,
            function(row) {
              return(any(as.logical(row)));
            }
          )
        )
      );

    proportions_totalCodedUtterances <-
      counts_total / totalCodedUtterances;

    CIs_totalCodedUtterances_objects <-
      lapply(
        counts_total,
        ufs::confIntProp,
        n = totalCodedUtterances
      );

    CIs_totalCodedUtterances_df <-
      rock::rbind_df_list(
        lapply(
          CIs_totalCodedUtterances_objects,
          as.data.frame
        )
      );

    CIs_totalCodedUtterances_df$codeId <- codesToInclude;
    CIs_totalCodedUtterances_df$prop <- proportions_totalCodedUtterances;

    row.names(CIs_totalCodedUtterances_df) <- codesToInclude;

    minProp <- min(c(CIs_totalCodedUtterances_df$ci.lo, CIs_totalCodedUtterances_df$ci.hi));
    maxProp <- max(c(CIs_totalCodedUtterances_df$ci.lo, CIs_totalCodedUtterances_df$ci.hi));

    gradients <-
      lapply(
        codesToInclude,
        function(codeId) {
          return(
            grid::linearGradient(
              colours = colors,
              x1 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.lo"], "npc"),
              y1 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.lo"], "npc"),
              x2 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.hi"], "npc"),
              y2 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.hi"], "npc"),
              extend = "pad"
            )
          )
        }
      );
    names(gradients) <- codesToInclude;


  } else if (inherits(x, "rock_parsedSources")) {
    stop("not implemented yet");
  } else {
    stop("As `x`, you have to pass one or more parsed sources, as ",
         "produced by a call to rock::parse_source() or rock::parse_sources(). ",
         "However, the object you passed has class ", rock::vecTxtQ(class(x)), ".");
  }

  palette_functionFactory <-
    function(lb, ub) {
      return(
        function(x) {
          return(
            ifelse(
              x < lb,
              0,
              ifelse (
                x > ub,
                1,
                (x - lb) / (ub - lb)
              )
            )
          )
        }
      )
    }

  ### It does not seem possible to use this as only one fill scale can exist
  ### for a plot; but keeping it here anyway in case I ever need it again.
  ###
  # palette_transformerFactory <-
  #   function(lb, ub) {
  #     return(
  #       scales::new_transform(
  #         name = "snoe",
  #         transform = palette_functionFactory(lb, ub),
  #         inverse = function(x) {return(x)},
  #         domain = c(0, 1)
  #       )
  #     )
  #   }

  nCodesToInclude <- length(codesToInclude);

  resolution <- 10000;

  estimationScale <-
    seq(
      0,
      maxProp,
      length = resolution
    );

  combinedDf <-
    rock::rbind_df_list(
      lapply(
        codesToInclude,
        function(codeId) {
          res <-
            data.frame(
              codeId = rep(codeId, each = resolution),
              occurrence = estimationScale
            );
          res$estimation <-
            palette_functionFactory(
              lb = CIs_totalCodedUtterances_df[codeId, 'ci.lo', drop=TRUE],
              ub = CIs_totalCodedUtterances_df[codeId, 'ci.hi', drop=TRUE]
            )(res$occurrence);
          return(res);
        }
      )
    );

  combinedDf$codeId <-
    factor(
      combinedDf$codeId,
      levels = sort(unique(codesToInclude), decreasing = TRUE),
      labels = sort(unique(codesToInclude), decreasing = TRUE),
      ordered = TRUE
    );

  res <- list();

  res$plot <-
    ggplot2::ggplot(
      data = combinedDf,
      mapping = ggplot2::aes(
        x = occurrence,
        y = codeId,
        fill = estimation
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_gradient(
      low = colors[1],
      high = colors[2],
      guide = NULL
    ) +
    ggplot2::labs(
      x = "Occurrence estimation",
      y = NULL,
      title = title
    ) +
    ggplot2Theme +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    );

  return(res);

    # lapply(
    #   codesToInclude,
    #   function(codeId) {
    #     return(
    #       ggplot2::geom_col(
    #         data = CIs_totalCodedUtterances_df[codeId, , drop=FALSE],
    #         mapping = ggplot2::aes(
    #           x = codeId,
    #           y = prop
    #         )
    #       ) +
    #         ggplot2::scale_fill_gradient(
    #           name = "occurrence estimation",
    #           low = colors[1],
    #           high = colors[2],
    #           transform =
    #             palette_transformerFactory(
    #               lb = CIs_totalCodedUtterances_df[codeId, 'ci.lo', drop=TRUE],
    #               ub = CIs_totalCodedUtterances_df[codeId, 'ci.hi', drop=TRUE]
    #             )
    #         )
    #     )
    #   }
    # ) +
    # ggplot2::theme_minimal();

}
