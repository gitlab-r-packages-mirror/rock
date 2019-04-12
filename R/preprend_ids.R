preprend_ids_to_source <- function(input,
                                   output = NULL,
                                   encoding="UTF-8") {
  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    res <- input;
  }


}
