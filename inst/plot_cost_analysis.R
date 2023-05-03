library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'cost_analysis';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    plot.path <- file.path(data.folder, 'plots', 'study_time_costs');

    time.cost.data <- read.table(
      file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_Time_and_Cost.tsv'),
      header = TRUE
      );
    colnames(time.cost.data) <- gsub('[.]+', '.', tolower(colnames(time.cost.data)))
    colnames(time.cost.data) <- gsub('[.]$', '', colnames(time.cost.data))

    time.cost.data.subset <- time.cost.data[, c('id', 'total.cost', 'digitx.time.total.hrs', 'traditional.time.total.hrs', 'phase')]
    colnames(time.cost.data.subset) <- c('patient.id', 'Total.Cost', 'EXONC.DEXP.Time.total.hrs', 'Traditional.Time.total.hrs', 'phase')
    time.cost.data.subset$Patient <- time.cost.data.subset$patient.id


    phase0b <- time.cost.data.subset[startsWith(time.cost.data.subset$phase, prefix = '0B'), ];
    cost.hour.analysis(
      cost.data = phase0b,
      plot.path = plot.path,
      suffix = 'phase0b',
      main = sprintf('Phase 0b (n = %s)', nrow(phase0b)),
      main.cex = 1.5
      );

    phase1.phase0b.prostate <- time.cost.data.subset[time.cost.data.subset$phase %in% c('1', '0B Prostate'), ];
    cost.hour.analysis(
      cost.data = phase1.phase0b.prostate,
      plot.path = plot.path,
      suffix = 'phase1.phase0b.prostate',
      main = sprintf('Phase 0b Prostate + Phase 1 (n = %s)', nrow(phase1.phase0b.prostate)),
      main.cex = 1.5
      );
    }
  );
