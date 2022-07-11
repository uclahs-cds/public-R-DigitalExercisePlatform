#' Creates the estimated study hours plot for DigITx

#' @param cost.data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot.study.hours <- function(
  cost.data,
  plot.path,
  extension,
  gotham.font = TRUE,
  ...) {
  digitx.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'Digitx.Time.total.hrs')], study = 1);
  colnames(digitx.hours) <- c('Patient', 'patient.id', 'hours', 'study');
  trad.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'Traditional.Time.total.hrs')], study = 0);
  colnames(trad.hours) <- c('Patient', 'patient.id', 'hours', 'study');

  time.df <- rbind(
    digitx.hours,
    trad.hours
    );

  time.df$phase0b <- ifelse(as.numeric(time.df$Patient >= 4), TRUE, FALSE);

  height <- 6;
  width <- 6;
  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = 'phase0b_boxplot_study_hours', extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  hours.plot <- create.boxplot(
    formula = hours ~ as.factor(study),
    data = time.df[time.df$phase0b == 1, ],
    add.stripplot = TRUE,
    ylab.label = 'Estimated Time Commitment, Hours',
    xlab.label = '',
    ylimits = c(-10, 175),
    ...
    );

  if (gotham.font) {
    hours.plot <- replace.font(hours.plot, font = 'iCiel Gotham Medium');
    }

  BoutrosLab.plotting.general::write.plot(
    trellis.object = hours.plot,
    filename = filename,
    height = height,
    width = width
    );
  }

#' Creates the estimated total cost boxplot plot for DigITx

#' @param cost.data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot.total.cost <- function(
  cost.data,
  plot.path,
  extension,
  gotham.font = TRUE,
  ...) {
  phase.0b.costs <- cbind(cost.data[cost.data$phase0b, 'Total.Cost', drop = FALSE], group = 1);
  # Add the DigITx costs as '0'
  phase.0b.costs <- rbind(phase.0b.costs, c(0, 2));

  height <- 6;
  width <- 6;

  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = 'phase0b_boxplot_total_cost', extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  # phase.0b.costs
  cost.plot <- create.boxplot(
    Total.Cost ~ as.factor(group),
    data = phase.0b.costs,
    add.stripplot = TRUE,
    ylab.label = 'Estimated Cost, $',
    xlab.label = '',
    ...
    );

  if (gotham.font) {
    cost.plot <- replace.font(cost.plot, font = 'iCiel Gotham Medium');
    }

  BoutrosLab.plotting.general::write.plot(
    trellis.object = cost.plot,
    filename = filename,
    height = height,
    width = width
    );
  }

#' Full cost/hour analysis
#' @param cost.data hours and cost data
cost.hour.analysis <- function(cost.data, plot.path, extension = 'png', color.points = FALSE) {
  # BPG Parameters
  bpg.shared <- list(
    points.cex = 1,
    ylab.cex = 1.75,
    ylab.axis.padding = 3,
    points.col = if (color.points) cost.data$plot.color else 'darkgrey',
    xaxis.lab = c('Traditional', 'DigITx'),
    extension = extension
    );

  cost.data$phase0b <- ifelse(as.numeric(cost.data$Patient >= 4), TRUE, FALSE);

  bpg.shared$cost.data <- cost.data;
  bpg.shared$plot.path <- plot.path;

  # Plot the study hours boxplot
  do.call(plot.study.hours, bpg.shared);

  # Plot the total study cost boxplot
  do.call(plot.total.cost, bpg.shared);
  }
