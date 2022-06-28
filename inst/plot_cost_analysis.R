library(DigITx);
library(BoutrosLab.plotting.general);

run.date <- Sys.Date();

script.name <- 'plot_cost_analysis';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');

plot.path <- file.path(data.folder, 'plots', 'study_time_costs');
log.path <- file.path(data.folder, 'logs', 'study_time_costs');

dir.create(plot.path, showWarnings = FALSE, recursive = TRUE);
dir.create(log.path, showWarnings = FALSE, recursive = TRUE);

cat('Writing log files to: ', log.path, '\n');
cat('Writing plots to: ', plot.path, '\n');

## BPG Parameters
points.cex <- 1;
ylab.cex <- 1.75;
ylab.axis.padding <- 3;
xaxis.lab <- c('Traditional', 'DigITx');
extension <- 'png';

sink(file = file.path(log.path, paste0(run.date, '_', script.name, '.log')), split = TRUE);

cost_xlsx <- read.table(here::here('data/patient_cost_hours.tsv'), sep = '\t', header = TRUE);
cost_xlsx$phase0b <- ifelse(as.numeric(cost_xlsx$Patient >= 4), TRUE, FALSE);

digitx_hours <- cbind(cost_xlsx[, c('Patient', 'patient.id', 'Digitx.Time..total.hrs.')], study = 1);
colnames(digitx_hours) <- c('Patient', 'patient.id', 'hours', 'study');
trad_hours <- cbind(cost_xlsx[, c('Patient', 'patient.id', 'Traditional.Time..total.hrs.')], study = 0);
colnames(trad_hours) <- c('Patient', 'patient.id', 'hours', 'study');

time_df <- rbind(
  digitx_hours,
  trad_hours
  );

time_df$phase0b <- ifelse(as.numeric(time_df$Patient >= 4), TRUE, FALSE);

create.boxplot(
  hours ~ as.factor(study),
  data = time_df[time_df$phase0b == 1, ],
  ylab.cex = ylab.cex,
  ylab.axis.padding = ylab.axis.padding,
  add.stripplot = TRUE,
  points.cex = points.cex,
  xaxis.lab = xaxis.lab,
  ylab.label = 'Total Patient Hours',
  xlab.label = '',
  ylimits = c(-10, 175),
  filename = file.path(
    plot.path,
    generate.filename('ExOnc', file.core = 'phase0b_boxplot_study_hours', extension = extension)
    )
  );

phase_0b_costs <- cbind(cost_xlsx[cost_xlsx$phase0b, 'Total.Cost', drop = FALSE], group = 1);
# Add the DigITx costs as '0'
phase_0b_costs <- rbind(phase_0b_costs, c(0, 2));

# phase_0b_costs
create.boxplot(
  Total.Cost ~ as.factor(group),
  data = phase_0b_costs,
  add.stripplot = TRUE,
  ylab.cex = ylab.cex,
  ylab.axis.padding = ylab.axis.padding,
  points.cex = points.cex,
  xaxis.lab = xaxis.lab,
  ylab.label = 'Estimated Patient Cost ($)',
  xlab.label = '',
  filename = file.path(
    plot.path,
    generate.filename('ExOnc', file.core = 'phase0b_boxplot_total_cost', extension = extension)
    )
  );

save.session.profile(nullfile(), stdout = TRUE);
