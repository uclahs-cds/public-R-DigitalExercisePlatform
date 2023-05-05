library(EXONC.DEXP);
library(BoutrosLab.plotting.general);
library(fs);

script.name <- 'run_all_analyses';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    run.scripts <- list.files(
      fs::path_package('EXONC.DEXP'), # Works better with devtools::load_all
      pattern = '*\\.R$',
      full.names = TRUE
      );
    # Remove this script
    run.scripts <- run.scripts[! grepl(script.name, run.scripts)];
    for (s in run.scripts) {
      source(s);
      }

    # plot.path <- file.path(data.folder, 'digIT-EX', 'plots');
    # update.figures.script <- system.file('bash_scripts', 'update_combined_figures.sh', package = 'EXONC.DEXP');
    # update.figures.script <- paste(update.figures.script, plot.path);
    # print(paste0('Running command: ', update.figures.script))
    # system(update.figures.script);
    }
  )
