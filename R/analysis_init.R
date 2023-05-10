#' Wrapper to do analysis setup, cleanup and logging
#'
#' @param data.folder Data folder
#' @param script.name Name of the script
#' @param split.stdout Should stdout be written to file and stdout?
#' @param expr R code that we want to execute
#'
#' @export
analysis.init <- function(data.folder, script.name, split.stdout = TRUE, seed = 131313, expr) {
  set.seed(seed);
  run.date <- Sys.Date();

  plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);
  log.path <- file.path(data.folder, 'digIT-EX', 'logs', script.name);
  results.path <- file.path(data.folder, 'digIT-EX', 'results', script.name);

  dir.create(plot.path, showWarnings = FALSE, recursive = TRUE);
  dir.create(log.path, showWarnings = FALSE, recursive = TRUE);
  dir.create(results.path, showWarnings = FALSE, recursive = TRUE);

  cat('Script: ', script.name, '\n')
  cat('Writing log files to: ', log.path, '\n');
  cat('Writing plots to: ', plot.path, '\n');

  # Write output to log file and stdout
  log.file <- file.path(log.path, paste0(run.date, '_', script.name, '.log'))
  sink(file = log.file, split = split.stdout);

  eval(expr);

  session.profile();
  sink();
  }
