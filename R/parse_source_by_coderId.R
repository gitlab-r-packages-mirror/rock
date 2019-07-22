parse_source_by_coderId <- function(x,
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
                                    delimiterRegEx = "^---$",
                                    ignoreRegex = "^#",
                                    ignoreOddDelimiters=FALSE,
                                    postponeDeductiveTreeBuilding = FALSE,
                                    silent=TRUE) {

  ### Get all coders that coded this source
  codersIdAtLines <- grep(coderId,
                          x);
  matchedCoderIds <- regmatches(x,
                                regexpr(coderId, x));

  ### Set 'idForOmittedCoderIds' for lines without coder
  if ((length(codersIdAtLines) == 0) || (min(codersIdAtLines) > 1)) {
    codersIdAtLines <- c(1,
                         codersIdAtLines);
    matchedCoderIds <- c(idForOmittedCoderIds,
                         matchedCoderIds);
  }

  ### Process matchedCoderIds according to regex
  for (i in seq_along(matchedCoderIds)) {
    matchedCoderIds <-
      gsub(coderId,
           "\\1",
           matchedCoderIds);
  }

  ### Set 'subsource' for each coder
  if (length(codersIdAtLines) == 1) {
    subsources <-
      list(x);
    names(subsources) <- matchedCoderIds[1];
  } else {
    subsources <- list();
    for (i in seq_along(codersIdAtLines)) {
      if (i == length(codersIdAtLines)) {
        subsources[[matchedCoderIds[i]]] <-
          x[codersIdAtLines[i]:length(x)];
      } else {
        subsources[[matchedCoderIds[i]]] <-
          x[codersIdAtLines[i]:(codersIdAtLines[i+1]-1)];
      }
    }
  }

  ### Process each subsource
  parsedSubsources <- list();
  for (i in seq_along(codersIdAtLines)) {
    parsedSubsources[[matchedCoderIds[i]]] <-
      rock::parse_source(text = subsources[[i]],
                         codeRegexes=codeRegexes,
                         idRegexes=idRegexes,
                         sectionRegexes=sectionRegexes,
                         autoGenerateIds=autoGenerateIds,
                         persistentIds=persistentIds,
                         noCodes=noCodes,
                         delimiterRegEx=delimiterRegEx,
                         ignoreRegex=ignoreRegex,
                         ignoreOddDelimiters=ignoreOddDelimiters,
                         postponeDeductiveTreeBuilding=TRUE,
                         silent=silent);
  }

  res <- list(subsources = subsources,
              parsedSubsources=parsedSubsources);

  return(res);

}
