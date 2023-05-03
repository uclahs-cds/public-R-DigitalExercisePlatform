plot.delta.waterfall <- function(
    x,
    width = 12,
    height = 10,
    resolution = 500,
    variable = c('PSA', 'ki67'),
    filename = NULL,
    ...) {
    x$dose.fct <- factor(x$dose, levels = c('control', unique(psa.dose$dose)))
    x <- x[order(as.integer(x$dose.fct), -x$delta), ]
    rownames(x) <- NULL
    # psa.data$y <- 1:nrow(psa.data) + (7 - as.numeric(psa.data$dose.fct))
    x$y <- 1:nrow(x) + (as.numeric(x$dose.fct) - 1)

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
    if (variable == 'PSA') by <- 2;
    if (variable == 'ki67') by <- 4;

    xat <- c(
      seq(0, floor(xlimits[1]), by = -by),
      seq(0, ceiling(xlimits[2]), by = by)
      );

    waterfall.grouped.plot <- create.barplot(
        delta ~ y,
        data = x.dummy,
        col = if ('col' %in% names(x.dummy)) x.dummy$col else 'black',
        xat = seq(1, max(x.dummy$y)),
        plot.horizontal = FALSE,
        xlab.label = 'Patient',
        ylab.label = bquote(bold(.(variable)~Delta)),
        xaxis.lab = rep('', nrow(x.dummy)),
        disable.factor.sorting = TRUE,
        xaxis.tck = 0,
        yaxis.tck = c(1, 0),
        ylimits = xlimits,
        yat = xat,
        ...
        );

    waterfall.grouped.plot <- remove.axis(waterfall.grouped.plot, side = c('bottom', 'right', 'top'))

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
