#' Expand categorical attribute variables to a series of dichotomous variables
#'
#' @param data The data frame, normally the `$mergedSources` data frame that
#' exists in the object returned by a call to [parse_sources()].
#' @param attributes The name of the attribute(s) to expand.
#' @param prefix,suffix The prefix and suffix to add to the variables names
#' that are returned.
#' @param glue The glue to paste the first part ad the second part of the
#' composite variable name together.
#' @param falseValue,trueValue The values to set for rows that, respectively,
#' do not match and do match an attribute value.
#' @param valueFirst Whether to insert the attribute value first, or the
#' attribute name, in the composite variable names.
#' @param append Whether to append the columns to the supplied data
#' frame or not.
#'
#' @return A data.frame
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Create a categorical attribute column
#' parsedExample$mergedSourceDf$age_group <-
#'   rep(c("18", "18-30", "31-60", ">60"), each=13);
#'
#' ### Expand to four logical columns
#' parsedExample$mergedSourceDf <-
#'   rock::expand_attributes(
#'     parsedExample$mergedSourceDf,
#'     "age_group",
#'     valueFirst = FALSE
#' );
#'
#' ### Show result
#' table(parsedExample$mergedSourceDf$age_group,
#'       parsedExample$mergedSourceDf$age_group__.18);
#' table(parsedExample$mergedSourceDf$age_group,
#'       parsedExample$mergedSourceDf$age_group__18.30);
expand_attributes <- function(data,
                              attributes,
                              prefix="",
                              glue="__",
                              suffix="",
                              falseValue = 0,
                              trueValue = 1,
                              valueFirst = TRUE,
                              append = TRUE) {

  if (!(all(attributes %in% names(data)))) {
    stop("You specified one or more attributes that don't exist in the ",
         "data you passed: ",
         vecTxtQ(attributes[!(attributes %in% names(data))]),
         ".");
  }

  for (currentAttribute in attributes) {
    uniqueValues <-
      sort(unique(data[, currentAttribute]));
    if (valueFirst) {
      newVarNames <-
        paste0(prefix, uniqueValues, glue, currentAttribute, suffix);
    } else {
      newVarNames <-
        paste0(prefix, currentAttribute, glue, uniqueValues, suffix);
    }
    subRes <-
      data.frame(
        stats::setNames(
          lapply(
            uniqueValues,
            function(x) {
              return(ifelse(data[, currentAttribute] == x,
                            trueValue,
                            falseValue));
            }
          ),
          nm = newVarNames
        )
      );
    if (!(exists('res')) || is.null(res)) {
      res <- subRes;
    } else {
      res <-
        cbind(
          res,
          subRes
        );
    }
  }

  if (append) {
    return(cbind(data, res));
  } else {
    return(res);
  }

}
