plot.delta.waterfall <- function(
    x,
    width = 12,
    height = 10,
    resolution = 500,
    variable = c('PSA', 'ki67', 'adherence'),
    filename = NULL,
    ...) {
    variable <- match.arg(variable);
    x$dose.fct <- factor(x$dose, levels = c(
      'control',
      setdiff(
        sort(unique(x$dose)),
        'control'
        )
      ))
    if (variable == 'adherence') {
      x <- x[order(-as.integer(x$dose.fct), x$delta), ]
      x$y <- 1:nrow(x) + (7 - as.numeric(x$dose.fct))
    } else {
      x <- x[order(as.integer(x$dose.fct), -x$delta), ]
      x$y <- 1:nrow(x) + (as.numeric(x$dose.fct) - 1)
    }
    rownames(x) <- NULL

    if (! 'col' %in% names(x)) {
      x$col <- dose.colors[as.character(x$dose)];
      }

    # Dummy data to insert between groups
    dummy.data <- data.frame(
      y = setdiff(seq(1, max(x$y, na.rm = T)), x$y),
      delta = NA,
      col = 'transparent'
    )

    x.dummy <- plyr::rbind.fill(
      x,
      dummy.data
    )

    xlimits <- range(x$delta) + c(-0.05, 0.05);

    if (variable == 'adherence') {
      by <- 10;
      round.max <- ceiling(max(x$delta) / 10) * 10;
      xlimits <- c(0, round.max + 0.05);
      xat <- seq(0, round.max, by = by);
      ylab.label <- 'Exercise sessions';
    } else {
      if (variable == 'PSA') by <- 2;
      if (variable == 'ki67') by <- 4;

      xat <- c(
        seq(0, floor(xlimits[1]), by = -by),
        seq(0, ceiling(xlimits[2]), by = by)
        );

      ylab.label <- bquote(bold(.(variable)~Delta));
    }

    if (variable == 'adherence') {
      waterfall.grouped.plot <- create.barplot(
        y ~ delta,
        data = x.dummy,
        col = if ('col' %in% names(x.dummy)) x.dummy$col else 'black',
        yat = seq(1, max(x.dummy$y)),
        plot.horizontal = TRUE,
        ylab.label = 'Patient',
        xlab.label = ylab.label,
        yaxis.lab = rep('', nrow(x.dummy)),
        disable.factor.sorting = TRUE,
        yaxis.tck = 0,
        xaxis.tck = c(1, 0),
        xlimits = xlimits,
        xat = xat,
        ...
        );

      waterfall.grouped.plot <- remove.axis(waterfall.grouped.plot, side = c('left', 'right', 'top'))
    } else {
      waterfall.grouped.plot <- create.barplot(
        delta ~ y,
        data = x.dummy,
        col = if ('col' %in% names(x.dummy)) x.dummy$col else 'black',
        xat = seq(1, max(x.dummy$y)),
        plot.horizontal = FALSE,
        xlab.label = 'Patient',
        ylab.label = ylab.label,
        xaxis.lab = rep('', nrow(x.dummy)),
        disable.factor.sorting = TRUE,
        xaxis.tck = 0,
        yaxis.tck = c(1, 0),
        ylimits = xlimits,
        yat = xat,
        ...
        );

      waterfall.grouped.plot <- remove.axis(waterfall.grouped.plot, side = c('bottom', 'right', 'top'))
    }


    if (!is.null(filename)) {
      write.plot(
        waterfall.grouped.plot,
        width = width,
        height = height,
        resolution = resolution,
        filename = filename
        )
      return(invisible(waterfall.grouped.plot));
    } else {
      return(waterfall.grouped.plot);
    }

}
