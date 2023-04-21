#' Creates the estimated study hours plot for EXONC.DEXP

#' @param cost.data the cost data frame.
#' @param plot.path path to the plot output
#' @param extension the extension for the plot
#' @param add.t.test.text Should t-test text results be added?
#' @param use.gotham.font Should Gotham font be used?
#' @param ... extra arguments for create.boxplot
#'
#' @export
study.hours.plot <- function(
  cost.data,
  plot.path = NULL,
  extension = c('png', 'pdf'),
  add.t.test.text = TRUE,
  use.gotham.font = FALSE,
  suffix = '',
  ...) {
  write.plot <- ! is.null(plot.path);
  EXONC.DEXP.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'EXONC.DEXP.Time.total.hrs')], study = 1);
  colnames(EXONC.DEXP.hours) <- c('Patient', 'patient.id', 'hours', 'study');
  trad.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'Traditional.Time.total.hrs')], study = 0);
  colnames(trad.hours) <- c('Patient', 'patient.id', 'hours', 'study');

  time.df <- rbind(
    EXONC.DEXP.hours,
    trad.hours
    );

  height <- 6;
  width <- 6;

  hours.plot <- BoutrosLab.plotting.general::create.boxplot(
    formula = hours ~ as.factor(study),
    data = time.df,
    add.stripplot = TRUE,
    ylab.label = 'Estimated Time Commitment, Hours',
    xlab.label = '',
    ylimits = c(-10, 175),
    ...
    );

  if (use.gotham.font) {
    hours.plot <- replace.font(hours.plot, font = gotham.font);
    }

  if (add.t.test.text) {
    panel.x <- 1.9;
    panel.y <- 155;
    t.test.layer <- ttest.plot.text(
      x = cost.data$EXONC.DEXP.Time.total.hrs,
      y = cost.data$Traditional.Time.total.hrs,
      panel.x = panel.x,
      panel.y = panel.y,
      use.gotham.font,
      paired = FALSE
      );

    hours.plot <- hours.plot + t.test.layer;
    }

  if (!is.null(suffix) && suffix != '') {
    suffix <- paste0('_', suffix)
    }

  if(write.plot) {
    filename <- file.path(
      plot.path,
      generate.filename(
        'digIT-EX',
        file.core = paste0('phase0b_boxplot_study_hours', suffix),
        extension = extension
        )
      );
    print(sprintf('Plotting to: %s', filename));

    BoutrosLab.plotting.general::write.plot(
      trellis.object = hours.plot,
      filename = filename,
      height = height,
      width = width
      );
    invisible(hours.plot);
    } else {
    return(hours.plot);
    }
  }

#' Creates the estimated total cost boxplot plot for EXONC.DEXP

#' @param cost.data the cost data frame.
#' @param extension the extension for the plot
#' @param plot.path output plot path
#' @param use.gotham.font Should Gotham font be used?
#' @param ... extra arguments to create.boxplot
#'
#' @export
total.cost.plot <- function(
  cost.data,
  plot.path = NULL,
  extension = c('png', 'pdf'),
  use.gotham.font = FALSE,
  height = 6,
  width = 6,
  suffix = '',
  ...) {
  write.plot <- ! is.null(plot.path);
  cost.data$phase0b <- ifelse(cost.data$Patient <= 3, FALSE, TRUE);
  phase.0b.costs <- cbind(cost.data[cost.data$phase0b, 'Total.Cost', drop = FALSE], group = 1);
  # Add the EXONC.DEXP costs as '0'
  phase.0b.costs <- rbind(phase.0b.costs, c(0, 2));

  # phase.0b.costs
  cost.plot <- BoutrosLab.plotting.general::create.boxplot(
    Total.Cost ~ as.factor(group),
    data = phase.0b.costs,
    add.stripplot = TRUE,
    ylab.label = 'Estimated Cost, $',
    xlab.label = '',
    ...
    );

  if (use.gotham.font) {
    cost.plot <- replace.font(cost.plot, font = gotham.font);
    }

  if (!is.null(suffix) && suffix != '') {
    suffix <- paste0('_', suffix)
    }

  if (write.plot) {
    filename <- file.path(
      plot.path,
      generate.filename(
        'digIT-EX',
        file.core = paste0('phase0b_boxplot_total_cost', suffix),
        extension = extension
        )
      );
    print(sprintf('Plotting to: %s', filename));

    BoutrosLab.plotting.general::write.plot(
    trellis.object = cost.plot,
    filename = filename,
    height = height,
    width = width
    );

    invisible(cost.plot);
    } else {
    return(cost.plot);
    }

  }

#' Full cost/hour analysis
#' @param cost.data hours and cost data
#' @param plot.path output plot path
#' @param extension plot extension
#' @param color.points Should plots be colored by `plot.color` column?
cost.hour.analysis <- function(
    cost.data,
    plot.path,
    extension = 'png',
    color.points = FALSE,
    suffix = '',
    ...
    ) {
  # BPG Parameters
  bpg.shared <- list(
    points.cex = 1,
    ylab.cex = 1.7,
    ylab.axis.padding = 3,
    points.col = if (color.points) cost.data$plot.color else 'darkgrey',
    xaxis.lab = c('Virtual\nTwin', 'DPEx'),
    extension = extension,
    suffix = suffix,
    ...
    );

  bpg.shared$cost.data <- cost.data;
  bpg.shared$plot.path <- plot.path;

  # Plot the study hours boxplot
  do.call(study.hours.plot, bpg.shared);

  # Plot the total study cost boxplot
  do.call(total.cost.plot, bpg.shared);
  }
