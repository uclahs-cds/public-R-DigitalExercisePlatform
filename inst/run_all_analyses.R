library(DigITx);
library(BoutrosLab.plotting.general);

run.date <- Sys.Date();

script.name <- 'run_all_analyses';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');

plot.path <- file.path(data.folder, 'plots', 'study_time_costs');
log.path <- file.path(data.folder, 'logs', 'study_time_costs');

dir.create(plot.path, showWarnings = FALSE, recursive = TRUE);
dir.create(log.path, showWarnings = FALSE, recursive = TRUE);

cat('Writing log files to: ', log.path, '\n');
cat('Writing plots to: ', plot.path, '\n');

sink(file = file.path(log.path, paste0(run.date, '_', script.name, '.log')), split = TRUE);

# Cost analysis
cost_data <- read.table(here::here('data/patient_cost_hours.tsv'), sep = '\t', header = TRUE);
cost_hour_analysis(cost_data);

save.session.profile(nullfile(), stdout = TRUE);
