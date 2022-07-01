#' Converts the adherence wide data into long format
#' @param x Adherence data frame with percent columns indicated by <Variable>.Percent
#' @param scale.percent Should the proportion be multiplied by 100?
#' @export
adherence.to.long <- function(x, scale.percent = TRUE) {
  x.percent <- x[, grepl('Percent', colnames(x))];

  x.percent.long <- do.call(
    what = 'rbind.data.frame',
    args = lapply(names(x.percent), function(cname) {
      perc.col <- x.percent[[cname]];
      if(!all(perc.col >= 0 & perc.col <= 1)) {
        warning(sprintf('The value in percent column: %s are not all in the interval [0,1] ', cname));
        }
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
