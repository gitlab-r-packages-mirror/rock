test_that("an RPE coding file can be prepared", {

  ### devtools::load_all("B:/Data/R/limonaid");

  lsFilesPath <- system.file("limesurvey",
                             package="rock");

  lsDat <-
    limonaid::ls_import_data(sid = 795779,
                             path = lsFilesPath);

  labelDf <-
    limonaid::ls_process_labels(
      lsDat,
      lengthToWrap = Inf
    );

  item_varNames <-
    c(item1 = "item1",
      item2 = "item2");

  metaquestionIdentifiers <-
    list(
      item1 = c("item1mq1",
                "item1mq2"),
      item2 = c("item2mq1",
                "item2mq2")
    );

  metaquestionVarNames <-
    c(item1mq1 = "item1mq1",
      item1mq2 = "item1mq2",
      item2mq1 = "item2mq1",
      item2mq2 = "item2mq2");

  item_questionTextMatches <-
    match(labelDf$varNames.raw, item_varNames);
  item_questionTextIndices <-
    which(questionTextMatches %in% na.omit(questionTextMatches));
  item_contents <-
    stats::setNames(
      labelDf[item_questionTextIndices, "questionText"],
      nm = item_varNames
    );

  mq_questionTextMatches <-
    match(labelDf$varNames.raw, metaquestionVarNames);
  mq_questionTextIndices <-
    which(mq_questionTextMatches %in% na.omit(mq_questionTextMatches));
  mq_itemContents <-
    stats::setNames(
      labelDf[mq_questionTextIndices, "questionText"],
      nm = names(metaquestionVarNames)
    );

  itemSource <-
    rpe_create_source_with_items(
      data = lsDat,
      iterationId = "iterationId",
      batchId = "batchId",
      populationId = "populationId",
      itemVarNames = item_varNames,
      metaquestionIdentifiers = metaquestionIdentifiers,
      metaquestionVarNames = metaquestion_VarNames,
      itemContents = item_contents,
      metaquestionContents = mq_itemContents,
      coderId = "coder1"
    );

  cat(itemSource);

});
