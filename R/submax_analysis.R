submax.analysis <- function(submax.long.data, plot.path, extension = 'png', color.points = FALSE) {
  t.test.results <- t.test(
    x = submax.long.data$Time.to.submax[submax.long.data$Session == "BL"],
    y = submax.long.data$Time.to.submax[submax.long.data$Session == "FU"],
    paired = TRUE,
    alternative = "two.sided"
    );
  test.text.labels <- paste0('t-test p = ', round(t.test.results$p.value, 3));

  submax.long.data$Time.to.submax.minutes <- submax.long.data$Time.to.submax / 60;

  create.scatterplot(
    Time.to.submax.minutes ~ as.factor(Session),
    data = submax.long.data,
    groups = submax.long.data$Study.ID,
    col = submax.long.data$plot.color[submax.long.data$Session == "BL"],
    type = 'b',
    main.cex = 1.75,
    ylimits = c(4, 15),
    ylab.cex = 1.5,
    xaxis.lab = c('Baseline', 'Follow-up'),
    ylab.label = 'Time to submax (minutes)',
    xaxis.cex = 1.25,
    ylab.axis.padding = 3,
    xlab.label = '',
    add.text = TRUE,
    text.x = 1.5,
    text.y = 5,
    text.cex = 1,
    resolution = 200,
    text.labels = test.text.labels,
    filename = file.path(
      plot.path,
      generate.filename('digIT-EX', file.core = 'phase0b_submax', extension = extension)
      )
    );
  }
