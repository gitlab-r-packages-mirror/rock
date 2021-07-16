#' Convert 'rectangular' or spreadsheet-format data to a source
#'
#' These functions first import data from a 'data format', such as spreadsheets
#' in `.xlsx` format, comma-separated values files (`.csv`), or SPSS data
#' files (`.sav`). You can also just use R data frames (imported however you
#' want). These functions then use the columns you specified to convert these
#' data to a `rock` source file, optionally including class instance
#' identifiers (such as case identifiers to identify participants, or location
#' identifiers, or moment identifiers, etc) and using those to link the
#' utterances to attributes from columns you specified. You can also precode
#' the utterances with codes you specify (if you ever would want to for some
#' reason).
#'
#' @param data The data frame containing the data to convert.
#' @param file The path to a file containing the data to convert.
#' @param importArgs Optionally, a list with named elements representing
#' arguments to pass when importing the file.
#' @param output The name (and path) of the file in which to save the processed
#' source (if it *is* `NULL`, the result will be returned visibly instead of
#' invisibly).
#' @param omit_empty_rows Whether to omit rows where the values in the columns
#' specified to convert to utterances are all empty (or contain only
#' whitespace).
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
#'
#' @export
#'
#' @rdname convert_to_source
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
                                 omit_empty_rows = TRUE,
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

  if (omit_empty_rows) {
    oldData <- data;
    rowsWithUtterances <-
      nchar(
        trimws(
          unlist(
            apply(
              oldData[, cols_to_utterances],
              1,
              paste0,
              collapse = "",
              simplify = FALSE
            )
          )
        )
      ) > 0;

    data <- oldData[rowsWithUtterances, ];

    emptyRows <- nrow(oldData) - nrow(data);

    if (emptyRows > 0) {
      msg("Deleted ", emptyRows, " 'empty rows' from the data (rows with no ",
          "data in the columns you specified to convert to utterances), ",
          "leaving ", sum(rowsWithUtterances), " rows with utterances.\n",
          silent=silent);
    } else {
      msg("No 'empty rows' found: every row had data in the columns you ",
          "specified to convert to utterances.\n", silent=silent);
    }
  }

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
    codeVector <- rep("", nrow(data));
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
    msg("Nothing specified as `output`, returning produced source visibly.\n",
        silent=silent);
    return(source);
  } else {
    if (preventOverwriting && (file.exists(output))) {
      warning("The file you specified to save the output to, '",
              output,
              "', exists, and `preventOverwriting` is set to TRUE, ",
              "so not writing the source to disk!");
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=source,
                 con=con);
      close(con);
      msg("Wrote the produced source to the file specified as `output` (",
          output, ").\n",
          silent=silent);
    }
    return(invisible(source));
  }

}

# dat <- openxlsx::read.xlsx(here::here("inst", "extdata", "spreadsheet-import-test.xlsx"))
# write.csv(dat, here::here("inst", "extdata", "spreadsheet-import-test.csv"), row.names=FALSE)

import_and_convert_to_source <- function(file,
                                         importFunction,
                                         importArgs = NULL,
                                         silent = rock::opts$get(silent),
                                         ...) {

  if (file.exists(file)) {

    dat <-
      do.call(
        importFunction,
        c(list(file),
          importArgs)
      )

  } else {
    stop("The file you specified to import (", file, ") does ",
         "not seem to exist.");
  }

  msg("Imported a data frame with ", ncol(dat), " columns and ",
      nrow(dat), " rows. Converting it to a source.\n",
      silent = silent);

  return(
    convert_df_to_source(
      data = dat,
      silent = silent,
      ...
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_csv_to_source <- function(file,
                                  importArgs = NULL,
                                  omit_empty_rows = TRUE,
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

  importFunction <- read.csv;

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_csv2_to_source <- function(file,
                                   importArgs = NULL,
                                   omit_empty_rows = TRUE,
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

  importFunction <- read.csv2;

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}


#' @rdname convert_to_source
#' @export
convert_xlsx_to_source <- function(file,
                                   importArgs = list(overwrite = !preventOverwriting),
                                   omit_empty_rows = TRUE,
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

  if (requireNamespace("openxlsx", quietly = TRUE)) {

    importFunction = openxlsx::read.xlsx;

  } else {
    stop("To import .sav files, you need to have the {openxlsx} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('openxlsx');\n");
  }

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_sav_to_source <- function(file,
                                  importArgs = NULL,
                                  omit_empty_rows = TRUE,
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

  if (requireNamespace("haven", quietly = TRUE)) {

    importFunction = haven::read_sav;

  } else {
    stop("To import .sav files, you need to have the {haven} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('haven');\n");
  }

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      omit_empty_rows = omit_empty_rows,
      importArgs = importArgs,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}
