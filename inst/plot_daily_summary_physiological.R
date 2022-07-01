library(DigITx);
library(BoutrosLab.plotting.general);
library(lme4);
library(lmerTest);

script.name <- 'daily_summary_physiological';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');
plot.path <- file.path(data.folder, 'plots', script.name);
extension <- 'png';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    daily.summary <- read.table(
      # TODO: Fix paths
      here::here('inst/data-raw/daily_summary.tsv'),
      sep = '\t',
      header = TRUE
      );

    phase0b_only <- FALSE;
    moving.avg <- TRUE;

    max.study.day <- 49;

    if(phase0b_only) {
      daily.summary <- daily.summary[! daily.summary$patient %in% c('EX001', 'EX002', 'EX003'),]
    }

    daily.summary.max.study.day <- daily.summary[daily.summary$nday <= max.study.day, ];

    physiological.vars <- c(
      'rest.hr.sleep.mean',
      'rest.cgm.sleep.mean',
      'mass',
      'fat.proportion',
      'systolic',
      'diastolic'
      );

    vars.nice.names <- c(
      'Resting HR',
      'Resting glucose',
      'Body mass',
      '% Fat mass',
      'Systolic BP',
      'Diastolic BP'
      );

    daily.summary.physio <- daily.summary.max.study.day[, physiological.vars];

    physio.mods <- lapply(physiological.vars, function(v) {
      formula <- as.formula(sprintf('scale(%s) ~ nday + (1 | patient)', v))
      lmerTest::lmer(formula, data = daily.summary)
      });
    names(physio.mods) <- physiological.vars;

    physio.mod.summary <- do.call('rbind.data.frame', lapply(physio.mods, function(m) {
      summary(m)$coefficients['nday', c('Estimate', 'Pr(>|t|)')]
      }));
    colnames(physio.mod.summary) <- c('estimate', 'pvalue');

    # Extract the p-values from the lmertest summary for nday coefficient
    physio.mod.pvalues <- physio.mod.summary$pvalue;

    # Scale within each patient
    daily.summary.physio.scaled <- do.call(
      'rbind.data.frame',
      lapply(split(daily.summary.physio, daily.summary.max.study.day$patient), function(x) {
        as.data.frame(lapply(x, function(y) as.numeric(scale(y))));
      }))

    daily.summary.heatmap.mean <- aggregate(
      x = daily.summary.physio.scaled,
      by = list(daily.summary.max.study.day$nday), FUN = function(x) {
      mean(x, na.rm = TRUE)
    })[, -1]

    if(moving.avg) {
      daily.summary.heatmap.mean <- as.data.frame(
        lapply(daily.summary.heatmap.mean, slider::slide_mean, before = 1, after = 1)
        );
    }

    p.value.ordering <- order(physio.mod.pvalues, decreasing = TRUE);

    xat <- c(1, seq(7, 49, by = 7));
    physio.heatmap <- create.heatmap(
      daily.summary.heatmap.mean[, p.value.ordering],
      clustering.method = 'none',
      yaxis.lab = vars.nice.names[p.value.ordering],
      xat = xat,
      xaxis.lab = xat,
      xaxis.rot = 0,
      colourkey.labels.at = c(-0.5, 0, 0.5),
      colourkey.labels = c('-0.5', '0', '0.5'),
      yaxis.tck = c(1, 0),
      xlab.label = 'Study day',
      xlab.cex = 1.5,
      colourkey.cex = 2
      );

    pvalue.barplot.data <- data.frame(
      variable = factor(physiological.vars, levels = physiological.vars[p.value.ordering]),
      pvalue = physio.mod.pvalues,
      effect.size = physio.mod.summary$estimate
      );

    pvalue.barplot.data <- pvalue.barplot.data[p.value.ordering, ];

    # effect.size.scaled <- round(pvalue.barplot.data$effect.size * 1000);
    # effect.size.range <- range(effect.size.scaled);
    #
    # effect.size.colors <- colorRampPalette(c('darkorange1', 'dodgerblue2'))(diff(effect.size.range) + 1);
    # actual.colors <- effect.size.colors[effect.size.scaled + abs(min(effect.size.scaled)) + 1];
    effect.colors <- ifelse(pvalue.barplot.data$effect.size > 0, 'darkorange1', 'dodgerblue2');

    pvalue.barplot <- create.barplot(
      formula = variable ~ -log10(pvalue),
      data = pvalue.barplot.data,
      col = effect.colors,
      plot.horizontal = TRUE,
      yaxis.lab = rep('', length(physiological.vars)),
      xlab.label = expression('-log'['10']~'(p-value)'),
      xlab.cex = 1.5,
      yaxis.tck = 0,
      xaxis.tck = 0,
      abline.v = -log10(0.05),
      abline.lty = 2,
      abline.col = 'darkgrey',
      ylab.label = ''
      );

    filename <- file.path(
      plot.path,
      generate.filename('digIT-EX', file.core = 'daily_percentile_states', extension = extension)
      );
    print(sprintf('Saving daily summary heatmap to: %s', filename));
    create.multipanelplot(
      list(physio.heatmap, pvalue.barplot),
      layout.height = 1,
      layout.width = 2,
      x.spacing = 0,
      plot.objects.widths = c(1, 0.25),
      width = 14,
      height = 8,
      filename = filename
      );
    }
  );
