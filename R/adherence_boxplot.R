#' DigIT-x adherence plots
#'
#' @param x Adherence long data.
#' @param plot.path path to save plots
#' @param extension extension to save plots
#' @param variable.names x-axis variable names
#' @param phase either 'phase0a' or 'phase0b'
#' @param use.gotham.font Should Gotham Medium font be used?
#'
#' @return
#' @export
adherence.boxplot <- function(
  x,
  plot.path = NULL,
  extension = c('png', 'pdf'),
  phase = c('phase0a', 'phase0b', 'phase1'),
  variable.names = NULL,
  use.gotham.font = FALSE,
  col = 'transparent',
  points.col = 'darkgrey',
  formula = Percent ~ Variable.factor,
  xlab.label = '',
  main = '',
  ...
  ) {
  phase <- match.arg(phase);
  write.plot <- ! is.null(plot.path);

  yat <- seq(0, 100, by = 20);
  ylimits <- c(-5, 105);
  yaxis.lab <- yat;
  height <- 8;
  width <- 12;
  lab.cex <- 3;

  adherence.plot <- BoutrosLab.plotting.general::create.boxplot(
    formula = formula,
    data = x,
    add.stripplot = TRUE,
    ylab.label = 'Adherence, %',
    yat = yat,
    xaxis.lab = if (!is.null(variable.names)) variable.names else levels(x$Variable.factor),
    yaxis.lab = yaxis.lab,
    ylimits = ylimits,
    col = col,
    points.col = points.col,
    xlab.label = xlab.label,
    xlab.cex = lab.cex,
    points.cex = 1,
    xaxis.cex = 2,
    ylab.cex = lab.cex,
    yaxis.cex = 2.5,
    main = main,
    ylab.axis.padding = 3,
    height = height,
    width = width,
    ...
    );

  if (use.gotham.font) {
    adherence.plot <- replace.font(adherence.plot, font = gotham.font);
    }

  if (write.plot) {
    filename <- file.path(
      plot.path,
      generate.filename('digIT-EX', file.core = paste0(phase, '_adherence'), extension = extension)
      );
    print(sprintf('Plotting to: %s', filename));
    BoutrosLab.plotting.general::write.plot(
      trellis.object = adherence.plot,
      filename = filename,
      height = height,
      width = width
      );

    invisible(adherence.plot);
    } else {
    return(adherence.plot);
    }
  }
