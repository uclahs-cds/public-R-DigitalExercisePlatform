library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'run_all_analyses';
data.folder <- Sys.getenv('EXONC_DEXP_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    run.scripts <- system.file(
      list.files('inst', pattern = '*\\.R$', full.names = TRUE),
      package = 'EXONC.DEXP'
      );
    # Remove this script
    run.scripts <- run.scripts[! grepl(script.name, run.scripts)];
    for (s in run.scripts) {
      source(s);
      }

    plot.path <- file.path(data.folder, 'plots');
    update.figures.script <- system.file('bash_scripts', 'update_combined_figures.sh', package = 'EXONC.DEXP');
    update.figures.script <- paste(update.figures.script, plot.path);
    print(paste0('Running command: ', update.figures.script))
    system(update.figures.script);
    }
  )
