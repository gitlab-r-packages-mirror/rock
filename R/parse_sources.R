#' Extract DCT specifications from all files in a directory
#'
#' This function processes all DCT specifications in a directory.
#'
#' This function is called by [process_dir()]; it is normally not
#' necessary to call this function directly.
#'
#' @param path The path from where to read the DCT files.
#' @param extension The extension of the file; convenient alternative to
#'   specifying a `regex`.
#' @param regex A regular expression; when provided, overrides the `extension`
#'   argument to guide file selection in the `path`.
#' @param delimiterRegEx The regular expression specifying how the YAML fragments
#'   specifying the constructs are delimited. Should normally never be changed.
#' @param ignoreOddDelimiters Whether to ignore a final odd delimiter, if
#'   encountered.
#' @param silent Whether to provide information on progress.
#'
#' @return An object of class `dctRawSpecListSet` for processing by [parse_dct_specs()].
#' @examples \dontrun{extract_dct_dir("A:/path/to/some/directory");
#' }
#' @export
parse_sources_dir <- function(path,
                              extension = "rock",
                              regex,
                              codeRegex = "\\[\\[([a-zA-Z0-9._-]+)\\]\\]",
                              idRegexes = c(caseId = "\\[\\[(cid)=([a-zA-Z0-9._-]+)\\]\\]",
                                            stanzaId = "\\[\\[(sid)=([a-zA-Z0-9._-]+)\\]\\]"),
                              autoGenerateIds = c('stanzaId'),
                              sectionRegexes = c(paragraphs = "---paragraph-break---",
                                                 secondary = "---<>---"),
                              delimiterRegEx = "^---$",
                              ignoreOddDelimiters = FALSE,
                              encoding="UTF-8",
                              silent=FALSE) {

  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  filelist <- list.files(path,
                         pattern=regex,
                         full.names=TRUE);

  sourceFileList <- lapply(filelist,
                           readLines,
                           encoding=encoding);

  res <- lapply(sourceFileList,
                parse_source,
                codeRegex=codeRegex,
                idRegexes=idRegexes,
                autoGenerateIds=autoGenerateIds,
                sectionRegexes=sectionRegexes,
                delimiterRegEx = "^---$",
                ignoreOddDelimiters = FALSE,
                silent=silent);

  ###--------------------------------------------------------------------------
  ### Now look in the returned objects for generic information and structure
  ### the result better
  ###--------------------------------------------------------------------------

  yamlLineSets <-
    lapply(res,
           function(x) {
             return(x$yamlFragments);
           });

  rawSpecs <- lapply(yamlLineSets,
                     yaml::yaml.load);

  if (!silent) {
    glue::glue("Loaded {length(rawSpecs)} raw metadata specifications.\n");
  }

  metadata <- list();
  for (i in names(idRegexes)) {
    metadata[[i]] <-

  }

  idRegexes

  names(parsedAttributes) <-
    purrr::map(parsedAttributes,
               'cid');





  return(structure(res,
                   class="rockParsedSources"));

}
