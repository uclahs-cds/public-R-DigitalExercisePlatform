adherence.to.long <- function(x, scale.percent = TRUE) {
  x.percent <- x[, grepl('Percent', colnames(x))];

  x.percent.long <- do.call(
    what = 'rbind.data.frame',
    args = lapply(names(x.percent), function(cname) {
      perc.col <- x.percent[[cname]];
      new.cname <- gsub('\\.Percent', '', cname);
      cbind(
        Percent = perc.col,
        Variable = new.cname,
        Patient.ID = x$Patient.ID
      )
    })
  );
  x.percent.long$Percent <- as.numeric(x.percent.long$Percent);
  if(scale.percent) {
    x.percent.long$Percent <- x.percent.long$Percent * 100;
    }

  rownames(x.percent.long) <- NULL;
  x.percent.long;
  }
