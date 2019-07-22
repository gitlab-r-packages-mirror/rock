#' @rdname parsing_sources_by_coderId
#' @export
parse_sources_by_coderId <- function(input,
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
                                     filenameRegex = ".*",
                                     delimiterRegEx = "^---$",
                                     ignoreRegex = "^#",
                                     ignoreOddDelimiters=FALSE,
                                     postponeDeductiveTreeBuilding = TRUE,
                                     encoding="UTF-8",
                                     silent=TRUE) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  res <- list();
    res <-
      lapply(rawSourceFiles,
             parse_source_by_coderId,
             output=NULL,
             coderId=coderId,
             idForOmittedCoderIds=idForOmittedCoderIds,
             codeRegexes=codeRegexes,
             idRegexes=idRegexes,
             sectionRegexes=sectionRegexes,
             uidRegex=uidRegex,
             autoGenerateIds=autoGenerateIds,
             persistentIds=persistentIds,
             noCodes=noCodes,
             delimiterRegEx=delimiterRegEx,
             ignoreRegex=ignoreRegex,
             ignoreOddDelimiters=ignoreOddDelimiters,
             postponeDeductiveTreeBuilding=FALSE,
             encoding=encoding,
             silent=silent);

  names(res) <-
    unlist(lapply(rawSourceFiles,
                  basename));

  return(res);

}
