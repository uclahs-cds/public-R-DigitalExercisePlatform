library(EXOC.DPEx);
library(BoutrosLab.plotting.general);
library(lme4);
library(lmerTest);

script.name <- 'daily_summary_mean_diff';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);
results.path <- file.path(data.folder, 'digIT-EX', 'results', script.name);
extension <- 'png';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    daily.summary <- read.table(
      # TODO: Fix paths
      system.file('extdata', 'daily_summary.tsv', package = 'EXOC.DPEx'),
      sep = '\t',
      header = TRUE
      );

    # load dosage file
    dosage <- read.table(
        file.path(data.folder, 'raw_data', 'Phase1', 'Dose_finding', 'PRESTO_Dose_levels.tsv'),
        sep = '\t',
        header = TRUE
        );
    dosage$patient <- sprintf('EX%03d', dosage$patient.identifier);

    # merge with dosage
    daily.summary <- merge(
        x = daily.summary,
        y = dosage[, c('patient', 'dose', 'relative.dose.intensity')],
        by = 'patient',
        all.x = TRUE
        )

    mean.delta.analysis(daily.summary,
      dosage,
      results.path = results.path
      )

    }
  );
