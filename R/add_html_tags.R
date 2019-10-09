add_html_tags <- function(x,
                          codeRegexes = c(codes = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                          idRegexes = c(caseId = "\\[\\[cid[=:]([a-zA-Z0-9._-]+)\\]\\]",
                                        stanzaId = "\\[\\[sid[=:]([a-zA-Z0-9._-]+)\\]\\]"),
                          sectionRegexes = c(paragraphs = "---paragraph-break---",
                                             secondary = "---<[a-zA-Z0-9]?>---"),
                          uidRegex = "\\[\\[uid[=:]([a-zA-Z0-9._-]+)\\]\\]",
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
    if (any(codeContentMatches)) {
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
               #' ', splitCodeContent,
               '">');
      # splitCodeContent <-
      #   ifelse(codeContentMatches,
      #          splitCodeContent,
      #          "");
      # currentCodeRegexMatches <-
      #   gregexpr(paste0("(", currentCodeRegex, ")"),
      #            res);
      res <-
        gsub(paste0("(", currentCodeRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res);
    }
  }

  ### Add break tags
  for (currentBreakRegexName in names(sectionRegexes)) {
    currentBreakRegex <- sectionRegexes[currentBreakRegexName];
    codeContentMatches <- grepl(currentBreakRegex, res);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentBreakRegex, ".*"),
                    "\\1",
                    res),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', sectionClass,
               ' ', currentBreakRegexName,
               '">');
      res <-
        gsub(paste0("(", currentBreakRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res);
    }
  }

  ### Add identifier tags
  for (currentIdRegexName in names(idRegexes)) {
    currentIdRegex <- idRegexes[currentIdRegexName];
    codeContentMatches <- grepl(currentIdRegex, res);
    if (any(codeContentMatches)) {
      res <-
        gsub(paste0("(", currentIdRegex, ")"),
             paste0('<span class="', idClass,
                    ' ', currentIdRegexName,
                    '">\\1</span>'),
             res);
    }
  }

  ### Add UID tags
  res <-
    gsub(paste0("(", uidRegex, ")"),
         paste0('<span class="', uidClass,
                '">\\1</span>'),
         res);

  return(res);

}
