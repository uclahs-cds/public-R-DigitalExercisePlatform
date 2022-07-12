submax.analysis <- function(
  submax.long.data,
  plot.path,
  extension = 'png',
  color.points = FALSE,
  use.gotham.font = TRUE,
  ...) {
  t.test.results <- t.test(
    x = submax.long.data$Time.to.submax[submax.long.data$Session == 'BL'],
    y = submax.long.data$Time.to.submax[submax.long.data$Session == 'FU'],
    paired = TRUE,
    alternative = 'two.sided'
    );
  test.text.labels <- paste0('t-test p = ', round(t.test.results$p.value, 3));

  submax.long.data$Time.to.submax.minutes <- submax.long.data$Time.to.submax / 60;

  fontfamily <- if (use.gotham.font) gotham.font else BoutrosLab.plotting.general::get.defaults(property = 'fontfamily');
  key <- NULL;
  if (color.points) {
    key <- list(
      text =  list(
        lab = names(cancer.type.colors)
        ),
      lines = list(
        pch = 19,
        type = 'b',
        size = 3,
        cex = 0.75,
        col = unlist(cancer.type.colors)
        ),
      fontfamily = fontfamily,
      border = TRUE,
      divide = 1,
      corner = c(0.05, 0.05)
      );
    }

  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = 'full_cohort_submax', extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  submax.plot <- create.scatterplot(
    Time.to.submax.minutes ~ as.factor(Session),
    data = submax.long.data,
    groups = submax.long.data$Study.ID,
    col = if (color.points) submax.long.data$plot.color[submax.long.data$Session == 'BL'] else 'black',
    group.specific.colouring = color.points,
    type = 'b',
    main.cex = 1.75,
    ylimits = c(4, 15),
    ylab.cex = 1.5,
    xaxis.lab = c('Baseline', 'Follow-up'),
    ylab.label = 'Time to Submax (minutes)',
    xaxis.cex = 1.25,
    ylab.axis.padding = 3,
    xlab.label = '',
    resolution = 200,
    key = key,
    ...
    );

  t.test.text <- latticeExtra::layer(
    panel.text(
      x = 2,
      y = 5,
      labels = labels,
      cex = 1,
      fontface = 'bold',
      fontfamily = fontfamily
      ),
      # Needs to be passed in via data rather than directly to panel
      data = list(
        labels = test.text.labels,
        fontfamily = fontfamily
        ),
      ...
    );

  if (use.gotham.font) {
    submax.plot <- replace.font(submax.plot, font = gotham.font);
    }

  height <- 6;
  width <- 6;

  submax.plot <- submax.plot + t.test.text;
  BoutrosLab.plotting.general::write.plot(
    trellis.object = submax.plot,
    filename = filename,
    height = height,
    width = width
    );

  invisible(submax.plot);
  }
