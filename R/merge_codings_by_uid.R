merge_codings_by_uid <- function(input,
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

  ### First parse all sources by coderId
  parsedSources <-
    do.call(parse_sources_by_coderId,
            as.list(environment()));

  ### Then construct objects with codes for each utterance

  res <- parsedSources;





  return(res);

}
