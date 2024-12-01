splitString <- function(x,
                        splittingValuesRegex = zirconia::opts$get("splittingValuesRegex")) {
  
  return(
    strsplit(
      x,
      splittingValuesRegex
    )[[1]]
  );
  
}