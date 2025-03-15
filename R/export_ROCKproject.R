export_ROCKproject <- function(path,
                               output,
                               includeRegex = NULL,
                               excludeRegex = NULL,
                               createDirs = FALSE) {

  outputDir <- dirname(output);

  if (!dir.exist(dirname(output))) {
    if (createDirs) {
      dir.create(outputDir, recursive = TRUE);
    } else {
      stop("The directory where you wanted to create the ROCKproject file, `",
           outputDir,
           "`, does not exist!");
    }
  }

  if (!dir.exists(path)) {

    stop("The directory you specified to export the ROCKproject from, `", path,
         "`, does not exist!");

  }


  #zip(zipfile <-


}
