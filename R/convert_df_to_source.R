#' Convert a data frame to a source
#'
#' @param data
#' @param output The name (and path) of the file in which to save the processed
#' source (if it *is* `NULL`, the result will be returned visibly).
#' @param cols_to_utterances The names of the columns to convert to utterances,
#' as a character vectors.
#' @param cols_to_ciids The names of the columns to convert to class instance
#' identifiers (e.g. case identifiers), as a named character vector, with
#' the values being the column names in the data frame, and
#' @param cols_to_codes The names of the columns to convert to codes (i.e.
#' codes appended to every utterance), as a character vectors.
#' @param cols_to_attributes The names of the columns to convert to attributes,
#' as a named character vector, where each name is the name of the class
#' instance identifier to attach the attribute to. If only one column is passed
#' in `cols_to_ciids`, names can be omitted and a regular unnames character
#' vector can be passed.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param removeNewlines Whether to remove all newline characters from the source before
#' starting to clean them.
#' @param encoding The encoding of the source(s).
#' @param silent Whether to suppress the warning about not editing the cleaned source.
#'
#' @return A source as a character vector.
#' @export
#'
#' @examples ### Get path to example files
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to file with example data frame
#' exampleFile <-
#'   file.path(examplePath, "spreadsheet-import-test.csv");
#'
#' ### Read data into a data frame
#' dat <-
#'   read.csv(exampleFile);
#'
#' ### Convert data frame to a source
#' source_from_df <-
#'   convert_df_to_source(
#'     dat,
#'     cols_to_utterances = c("open_question_1",
#'                            "open_question_2"),
#'     cols_to_ciids = c(cid = "id"),
#'     cols_to_attributes = c("age", "gender"),
#'     cols_to_codes = c("code_1", "code_2"),
#'     ciid_labels = c(cid = "caseId")
#'  );
#'
#' ### Show the result
#' cat(
#'   source_from_df,
#'   sep = "\n"
#' );
convert_df_to_source <- function(data,
                                 output = NULL,
                                 cols_to_utterances = NULL,
                                 cols_to_ciids = NULL,
                                 cols_to_codes = NULL,
                                 cols_to_attributes = NULL,
                                 ciid_labels = NULL,
                                 ciid_separator = "=",
                                 preventOverwriting = rock::opts$get(preventOverwriting),
                                 encoding = rock::opts$get(encoding),
                                 silent = rock::opts$get(silent)) {

  delimiterString <- rock::opts$get(delimiterString);
  attributeContainer <- rock::opts$get(attributeContainers)[1];
  codeDelimiters <- rock::opts$get(codeDelimiters);

  if (!is.data.frame(data)) {
    stop("As `data`, you must pass a data frame!");
  }

  allCols <-
    c(cols_to_utterances,
      cols_to_ciids,
      cols_to_codes,
      cols_to_attributes);

  if (!all(allCols %in% names(data))) {
    stop("Not all columns you specified exist in the data frame ",
         "you passed. Missing the following column(s): ",
         vecTxtQ(allCols[!(allCols %in% names(data))]), ".");
  }

  if (is.null(ciid_labels)) {
    ciid_labels <-
      stats::setNames(
        names(cols_to_ciids),
        nm = names(cols_to_ciids)
      );
  }

  source <- c();
  attributes <- list();

  if (!is.null(cols_to_codes)) {
    codeVector <-
      apply(
        data[, cols_to_codes],
        1,
        function(row) {
          return(
            paste0(
              codeDelimiters[1],
              as.vector(row),
              codeDelimiters[2],
              collapse = " "
            )
          );
        }
      );
    codeVector <- paste0(
      " ",
      codeVector
    );
  } else {
    codeVector <- "";
  }

  for (i in 1:nrow(data)) {

    source <- c(source, "");

    if (!is.null(cols_to_ciids)) {

      for (j in seq_along(cols_to_ciids)) {
        source <-
          c(source,
            paste0(
              codeDelimiters[1],
              names(cols_to_ciids)[j],
              ciid_separator,
              data[i, cols_to_ciids[j]],
              codeDelimiters[2]
            )
          );
      }

      currentAttributes <-
        stats::setNames(
          c(list(as.character(data[i, cols_to_ciids[j]])),
            as.list(as.character(data[i, cols_to_attributes]))
          ),
          nm = c(ciid_labels[names(cols_to_ciids)[j]],
                 cols_to_attributes)
        );

      attributes <-
        c(attributes,
          list(currentAttributes)
        );

      for (j in cols_to_utterances) {
        source <-
          c(source,
            "",
            paste0(data[i, j], codeVector[i])
          );
      }

    }

  }

  if (length(attributes) > 0) {
    attributes <-
      list(attributes);
    names(attributes) <-
      attributeContainer;
    source <-
      c(source,
        "",
        "",
        delimiterString,
        unlist(
          strsplit(
            yaml::as.yaml(
              attributes
            ),
            "\n",
            fixed = TRUE
          )
        ),
        delimiterString,
        ""
      );
  }

  if (is.null(output)) {
    return(source);
  } else {
    if (preventOverwriting && (file.exists(output))) {
      warning("The file you specified to save the output to, '",
              output,
              "', exists, and `preventOverwriting` is set to TRUE, ",
              "so not writing the source to disk!");
    } else {
      writeLines(
        source,
        output,
        encoding = encoding
      );
    }
    return(invisible(source));
  }

}

# dat <- openxlsx::read.xlsx(here::here("inst", "extdata", "spreadsheet-import-test.xlsx"))
# write.csv(dat, here::here("inst", "extdata", "spreadsheet-import-test.csv"), row.names=FALSE)

