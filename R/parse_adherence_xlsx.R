parse.adherence.xlsx <- function(x) {
  adherence <- as.data.frame(readxl::read_excel(
    x,
    n_max = 61,
    na = 'n/a',
    ));

  colnames(adherence) <-  gsub('%', ' percent', tolower(colnames(adherence)));
  colnames(adherence) <-  gsub('#', 'num', tolower(colnames(adherence)));
  colnames(adherence) <- gsub('[ ]+', '.', trimws(colnames(adherence)));

  date.cols <- c('consent.date', 'bl.submax', 'fu.submax');
  adherence[, date.cols] <- lapply(adherence[, date.cols], clean.dates);

  logical.cols <- grepl('y/n', colnames(adherence));
  adherence[, logical.cols] <- lapply(adherence[, logical.cols], function(x) ifelse(tolower(x) == 'y', TRUE, FALSE));

  # Some data validation
  stopifnot(
    all(
      adherence$attendance.percent == adherence$completed.exercise.sessions / adherence$planned.exercise.sessions
    )
  )
  # We are not using this column but study.id == 62 has 100% bitesnap adherence with 3 days?
  # stopifnot(
  #   all(
  #     adherence$bitesnap.adherence == adherence$bitesnap.days / 6, na.rm = TRUE
  #   )
  # )

  return(adherence);
}
