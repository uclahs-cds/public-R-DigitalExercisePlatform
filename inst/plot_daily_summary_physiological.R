library(EXONC.DEXP);
library(BoutrosLab.plotting.general);
library(lme4);
library(lmerTest);

script.name <- 'daily_summary_physiological';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
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
      system.file('extdata', 'daily_summary.tsv', package = 'EXONC.DEXP'),
      sep = '\t',
      header = TRUE
      );

    baseline.data <- read.table(
      system.file('extdata', 'baseline_data.tsv', package = 'EXONC.DEXP'),
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

    full.cohort.models <- daily.summary.physiological.plot(
      daily.summary,
      physiological.vars = physiological.vars,
      vars.nice.names = vars.nice.names,
      use.gotham.font = FALSE
      );

    phase0b.models <- daily.summary.physiological.plot(
      daily.summary.phase0b,
      phase0b.only = TRUE,
      physiological.vars = physiological.vars,
      vars.nice.names = vars.nice.names,
      use.gotham.font = FALSE
      );

    lme4::lmer(
      formula = scale(mass) ~ 1 + nday + Age.at.Consent + (1 + scale(nday) || patient),
      data = daily.summary.phase0b,
      na.action = na.omit
      )

    mass.cormod <- nlme::lme(
      fixed = scale(mass) ~ 1 + nday + Age.at.Consent,
      random = ~ 1 + scale(nday) | patient,
      correlation = nlme::corAR1(form = ~ 1 | patient),
      data = daily.summary.phase0b,
      na.action = na.omit
      )

    acf(resid(mass.cormod, type = 'normalized'))

    acf(resid(phase0b.models$models$mass))
    }
  );
