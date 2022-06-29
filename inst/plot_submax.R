library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'submax_analysis';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    plot.path <- file.path(data.folder, 'plots', 'submax');

    submax.long.data <- read.table(
      here::here('data/submax_long.tsv'),
      sep = '\t',
      header = TRUE
      );

    submax_analysis(
      submax.long.data = submax.long.data[! submax.long.data$Study.ID %in% c('EX001', 'EX002', 'EX003'), ],
      plot.path = plot.path,
      );
    }
  );
