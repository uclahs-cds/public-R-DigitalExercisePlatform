library(EXOC.DPEx);
library(BoutrosLab.plotting.general);

script.name <- 'submax_analysis';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);
results.path <- file.path(data.folder, 'digIT-EX', 'results', script.name);

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {

    submax.long.data <- read.table(
      system.file('extdata', 'submax_long.tsv', package = 'EXOC.DPEx'),
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
    submax.long.data <- merge(
        x = submax.long.data,
        y = dosage[, c('patient', 'dose', 'relative.dose.intensity')],
        by = 'patient',
        all.x = TRUE
        )

    submax.delta.summary(
      submax.long.data = submax.long.data,
      dosage = dosage,
      results.path = results.path
      );
    }
  );
