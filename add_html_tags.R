add_html_tags <- function(x,
                          codeRegexes = c(codes = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                          idRegexes = c(caseId = "\\[\\[cid=([a-zA-Z0-9._-]+)\\]\\]",
                                        stanzaId = "\\[\\[sid=([a-zA-Z0-9._-]+)\\]\\]"),
                          sectionRegexes = c(paragraphs = "---paragraph-break---",
                                             secondary = "---<[a-zA-Z0-9]?>---"),
                          uidRegex = "\\[\\[uid=([a-zA-Z0-9._-]+)\\]\\]",
                          inductiveCodingHierarchyMarker = ">",
                          codeClass = "code",
                          idClass = "identifier",
                          sectionClass = "sectionBreak",
                          uidClass = "uid") {

  res <- x;

  ### Add html tags
  for (currentCodeRegexName in names(codeRegexes)) {
    currentCodeRegex <- codeRegexes[currentCodeRegexName];
    codeContentMatches <- grepl(currentCodeRegex, res);
    codeContent <-
      ifelse(codeContentMatches,
             gsub(paste0(".*", currentCodeRegex, ".*"),
                  "\\1",
                  res),
             "");
    splitCodeContent <-
      unlist(lapply(strsplit(codeContent,
                             inductiveCodingHierarchyMarker),
                    paste0,
                    collapse=" "));
    splitCodeContent <-
      paste0('<span class="', codeClass,
             ' ', currentCodeRegexName,
             ' ', splitCodeContent, '">');
    res <- gsub(paste0("(", currentCodeRegex, ")"),
                paste0('\\1</span>'),
                res);
    res <- paste0(splitCodeContent,
                  res);
  }

  return(res);

}
