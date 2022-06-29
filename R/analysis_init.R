#' Wrapper to do analysis setup, cleanup and logging
#' @export
analysis.init <- function(data.folder, script.name, split.stdout = TRUE, expr) {

  run.date <- Sys.Date();

  plot.path <- file.path(data.folder, 'plots', script.name);
  log.path <- file.path(data.folder, 'logs', script.name);

  dir.create(plot.path, showWarnings = FALSE, recursive = TRUE);
  dir.create(log.path, showWarnings = FALSE, recursive = TRUE);

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
