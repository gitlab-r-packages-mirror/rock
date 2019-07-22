#' Parsing sources separately for each coder
#'
#' @param input For `parse_source_by_coderId`, either a character vector
#' containing the text of the relevant source *or* a path to a file that
#' contains the source text; for `parse_sources_by_coderId`, a path to a
#' directory that contains the sources to parse.
#' @param coderId The regular expression designating the coder identifier.
#' @param idForOmittedCoderIds The identifier to use for utterances that have
#' not been marked by a coder identifier.
#' @param codeRegexes,idRegexes,sectionRegexes These are named character vectors with one
#' or more regular expressions. For `codeRegexes`, these specify how to extract the codes
#' (that were used to code the sources). For `idRegexes`, these specify how to extract the
#' different types of identifiers. For `sectionRegexes`, these specify how to extract the
#' different types of sections. The `codeRegexes` and `idRegexes` must each contain one
#' capturing group to capture the codes and identifiers, respectively.
#' @inheritParams parse_source
#' @rdname parsing_sources_by_coderId
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source_by_coderId(exampleFile);
#'
#' @export
parse_source_by_coderId <- function(input,
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
                                    encoding="UTF-8",
                                    silent=TRUE) {

  ### Read input, if it's a file
  if (file.exists(input)) {
    x <- readLines(input,
                   encoding=encoding);
  } else {
    x <- input;
  }

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
