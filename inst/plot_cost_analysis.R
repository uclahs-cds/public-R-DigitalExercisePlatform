library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'cost_analysis';
data.folder <- Sys.getenv('EXONC_DEXP_HOME');
if (data.folder == '') data.folder <- here::here('results');

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    plot.path <- file.path(data.folder, 'plots', 'study_time_costs');

    cost.data <- read.table(
      system.file('extdata', 'patient_cost_hours.tsv', package = 'EXONC.DEXP'),
      sep = '\t',
      header = TRUE
      );

    cost.hour.analysis(
      cost.data = cost.data,
      plot.path = plot.path
      );
    }
  );
