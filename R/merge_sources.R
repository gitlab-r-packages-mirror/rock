#' @export
merge_sources <- function(input,
                          output,
                          outputPrefix = "",
                          outputSuffix = "_merged",
                          primarySourcesRegex=".*",
                          primarySourcesPath = input,
                          coderId = "\\[\\[coderId=([a-zA-Z0-9._-]+)\\]\\]",
                          idForOmittedCoderIds = "noCoderId",
                          codeRegexes = c(codes = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                          idRegexes = c(caseId = "\\[\\[cid=([a-zA-Z0-9._-]+)\\]\\]",
                                        stanzaId = "\\[\\[sid=([a-zA-Z0-9._-]+)\\]\\]",
                                        coderId = "\\[\\[coderId=([a-zA-Z0-9._-]+)\\]\\]"),
                          sectionRegexes = c(paragraphs = "---paragraph-break---",
                                             secondary = "---<[a-zA-Z0-9]?>---"),
                          uidRegex = "\\[\\[uid=([a-zA-Z0-9._-]+)\\]\\]",
                          autoGenerateIds = c('stanzaId'),
                          persistentIds = c('caseId', 'coderId'),
                          noCodes = "^uid:|^uid=|^dct:|^ci:",
                          recursive = TRUE,
                          primarySourcesRecursive = recursive,
                          filenameRegex = ".*",
                          delimiterRegEx = "^---$",
                          ignoreRegex = "^#",
                          ignoreOddDelimiters=FALSE,
                          postponeDeductiveTreeBuilding = TRUE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (!dir.exists(primarySourcesPath)) {
    stop("Directory specified to read primary sources from (",
         primarySourcesPath, ")does not exist!");
  }

  ### Store all arguments and delete the ones specific to this function
  args <- as.list(environment());
  args <-
    args[setdiff(names(args),
                 c('primarySourcesRegex',
                   'primarySourcesPath',
                   'primarySourcesRecursive',
                   'output',
                   'outputPrefix',
                   'outputSuffix'))];

  ### Then pass arguments along to extract_codings_by_coderId and store result
  parsedSources <-
    do.call(extract_codings_by_coderId,
            args);

  ### Read primary sources
  primarySources <-
    load_sources(input = primarySourcesPath,
                 encoding=encoding,
                 filenameRegex=primarySourcesRegex,
                 recursive=primarySourcesRecursive,
                 silent=silent);

  res <- list(parsedSources,
              primarySources);

  return(res);

}
