library(DigITx);
library(BoutrosLab.plotting.general);
library(lme4);
library(lmerTest);

script.name <- 'daily_summary_physiological';
data.folder <- Sys.getenv('DIGITX_HOME');
if (data.folder == '') data.folder <- here::here('results');
plot.path <- file.path(data.folder, 'plots', script.name);
results.path <- file.path(data.folder, 'results', script.name);
extension <- 'png';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    daily.summary <- read.table(
      # TODO: Fix paths
      here::here('inst/data-raw/daily_summary.tsv'),
      sep = '\t',
      header = TRUE
      );

    baseline.data <- read.table(
      here::here('inst/data-raw/baseline_data.tsv'),
      sep = '\t',
      header = TRUE
      );

    moving.avg <- TRUE;

    max.study.day <- 49;

    daily.summary <- merge(daily.summary, baseline.data, by = 'patient', all.x = TRUE);
    daily.summary.phase0b <- daily.summary[! daily.summary$patient %in% c('EX001', 'EX002', 'EX003'),];

    physiological.vars <- c(
      'rest.hr.sleep.mean',
      'rest.cgm.sleep.mean',
      'mass',
      'fat.mass',
      'systolic',
      'diastolic'
      );

    vars.nice.names <- c(
      'Resting HR',
      'Resting glucose',
      'Body mass',
      'Fat mass',
      'Systolic BP',
      'Diastolic BP'
      );

    full.cohort.models <- plot.daily.summary.physiological(
      daily.summary,
      physiological.vars = physiological.vars,
      vars.nice.names = vars.nice.names
      );

    phase0b.models <- plot.daily.summary.physiological(
      daily.summary.phase0b,
      phase0b.only = TRUE,
      physiological.vars = physiological.vars,
      vars.nice.names = vars.nice.names
      );
    }
  );
