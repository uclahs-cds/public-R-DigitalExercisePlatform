#' Plot the daily summary heatmap for the physiological variables
#'
#' @param daily.summary daily summary data frame
#' @param max.study.day Maximum study day to analyze
#' @param phase0b.only Should only phase0b cohort be used?
#' @param random.slopes Should random slopes be used in lmer?
#' @param moving.avg Number of points to use in moving average for heatmap. Use 0 for no moving average
#' @param vars.nice.names Vector of nice variable names
#' @param use.gotham.font Should gotham font be used?
#' @param scale.dependent Should dependent variable be scaled via scale(...)
#' @param physiological.vars Names for the physiological variables
#'
#' @return
#' @export
daily.summary.physiological.plot <- function(
  daily.summary,
  max.study.day = 49,
  phase0b.only = FALSE,
  random.slopes = TRUE,
  moving.avg = 1,
  scale.dependent = TRUE,
  use.gotham.font = FALSE,
  physiological.vars = c(
    'rest.hr.sleep.mean',
    'rest.cgm.sleep.mean',
    'mass',
    'fat.mass',
    'systolic',
    'diastolic'
  ),
  vars.nice.names = c(
      'Resting HR',
      'Resting glucose',
      'Body mass',
      'Fat mass',
      'Systolic BP',
      'Diastolic BP'
  )) {

  phase0b.string <- '';
  if (phase0b.only) {
    phase0b.string <- '_phase0b';
    }
  physio.mods <- model.linear.daily.summary(
    daily.summary,
    physiological.vars = physiological.vars,
    random.slopes = TRUE,
    scale.dependent = scale.dependent
    );

  physio.mod.summary <- physio.mods$model.summary[physio.mods$model.summary$var.type == 'Physiological', ];
  rownames(physio.mod.summary) <- physio.mod.summary$variable;
  # Reorder variables
  physio.mod.summary <- physio.mod.summary[physiological.vars, ];

  results.filename <- file.path(
    results.path,
    generate.filename('digIT-EX', file.core = sprintf('physiological_lmm_results%s', phase0b.string), extension = 'tsv')
  );
  print(physio.mods$model.summary);
  print(sprintf('Saving model results to: %s', results.filename));
  write.table(
    x = physio.mods$model.summary,
    file = results.filename,
    sep = '\t',
    row.names = FALSE
    );

  # Extract the p-values from the lmertest summary for nday coefficient
  physio.mod.pvalues <- physio.mod.summary$p.value;
  names(physio.mod.pvalues) <- physio.mod.summary$variable;
  p.value.ordering <- order(physio.mod.pvalues, decreasing = TRUE);

  daily.summary.max.study.day <- daily.summary[daily.summary$nday <= max.study.day, ];
  # Only select the physiological variables
  daily.summary.physio <- daily.summary.max.study.day[, physiological.vars];

  # Scale within each patient for heatmap
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

  if (!is.null(moving.avg) && moving.avg > 0) {
    daily.summary.heatmap.mean <- as.data.frame(
      lapply(daily.summary.heatmap.mean, slider::slide_mean, before = moving.avg, after = moving.avg)
      );
    }

  xat <- c(1, seq(7, max.study.day, by = 7));
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

  pvalue.effectsize.data <- data.frame(
    variable = factor(physiological.vars, levels = physiological.vars[p.value.ordering]),
    pvalue = physio.mod.pvalues,
    effect.size = physio.mod.summary$estimate,
    nday.ci.lower = physio.mod.summary$nday.ci.lower,
    nday.ci.upper = physio.mod.summary$nday.ci.upper
    );

  pvalue.effectsize.data <- pvalue.effectsize.data[p.value.ordering, ];

  pvalue.effectsize.data[, -c(1,2)] <- pvalue.effectsize.data[, -c(1,2)] * 49;
  effect.size.segplot <- create.segplot(
    formula = variable ~ nday.ci.lower + nday.ci.upper,
    data = pvalue.effectsize.data,
    centers = pvalue.effectsize.data$effect.size,
    # symbol.cex = pmax(-log10(pvalue.effectsize.data$pvalue), 0.6),
    yaxis.lab = rep('', length(physiological.vars)),
    segments.lwd = 1.5,
    xlab.cex = 1.5,
    xlab.label = 'Effect Size (Z-scale)',
    xaxis.tck = c(1, 0),
    yaxis.tck = 0,
    xat = seq(-1, 1, by = 0.5),
    xaxis.lab = c(-1, -0.5, 0, 0.5, 1),
    xlimits = c(-1.5, 1.5),
    abline.v = 0,
    abline.lty = 2,
    abline.col = 'darkgrey',
    ylab.label = ''
    );

  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = sprintf('daily_phyisological_heatmap%s', phase0b.string), extension = extension)
    );
  width <- 16;
  height <- 8;

  print(sprintf('Saving daily summary heatmap to: %s', filename));
  daily.summary.multiplot <- create.multipanelplot(
    list(physio.heatmap, effect.size.segplot),
    layout.height = 1,
    layout.width = 2,
    x.spacing = 0,
    plot.objects.widths = c(1, 0.35),
    );

  if (use.gotham.font) {
    daily.summary.multiplot <- replace.font(daily.summary.multiplot, font = gotham.font);
    }


  BoutrosLab.plotting.general::write.plot(
    trellis.object = daily.summary.multiplot,
    filename = filename,
    height = height,
    width = width
    );

  return(physio.mods);
  }
