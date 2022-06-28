library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'run_all_analyses';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    plot.path <- file.path(data.folder, 'plots', 'study_time_costs');

    # Cost analysis
    cost.data <- read.table(
      here::here('data/patient_cost_hours.tsv'),
      sep = '\t',
      header = TRUE
    );
    cost_hour_analysis(
      cost.data = cost.data,
      plot.path = plot.path
    );
    }
  )
