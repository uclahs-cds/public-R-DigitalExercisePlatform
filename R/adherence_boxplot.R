adherence.boxplot <- function(
  x,
  plot.path,
  extension = 'png',
  phase = c('phase0a', 'phase0b')
  ) {
  phase <- match.arg(phase);

  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = paste0(phase, '_adherence'), extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  yat <- seq(40, 100, by = 10);
  ylimits <- c(35, 105);
  yaxis.lab <- paste0(yat, '%');

  create.boxplot(
    formula = Percent ~ Variable.factor,
    data = x,
    add.stripplot = TRUE,
    ylab.label = 'Adherence',
    yat = yat,
    yaxis.lab = yaxis.lab,
    ylimits = ylimits,
    xlab.label = '',
    points.cex = 1,
    xaxis.cex = 2,
    ylab.cex = 3,
    yaxis.cex = 2.5,
    ylab.axis.padding = 3,
    height = 8,
    width = 12,
    filename = filename
  );
}
