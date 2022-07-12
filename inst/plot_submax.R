library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'submax.analysis';
data.folder <- Sys.getenv('DIGITX_HOME');
if (data.folder == '') data.folder <- here::here('results');
plot.path <- file.path(data.folder, 'plots', script.name);

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    submax.data <- read.table(
      here::here('inst/data-raw/submax.tsv'),
      sep = '\t',
      header = TRUE
      );

    # submax.data.phase0b <- submax.data[! submax.data$Study.ID %in% c('EX001', 'EX002', 'EX003'), ];

    submax.long.data <- read.table(
      here::here('inst/data-raw/submax_long.tsv'),
      sep = '\t',
      header = TRUE
      );

    # [! submax.long.data$Study.ID %in% c('EX001', 'EX002', 'EX003'), ]
    submax.plot <- submax.analysis(
      submax.long.data = submax.long.data,
      plot.path = plot.path,
      color.points = TRUE,
      extension = 'pdf'
      );
    }
  );
