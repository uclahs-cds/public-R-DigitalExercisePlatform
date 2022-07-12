#' Named Gotham font
gotham.font <- 'iCiel Gotham Medium';

#' Replaces the font in a BPG object with Gotham Medium font
#'
#' @param x BPG plot
#' @return Same BPG plot but with all fonts replaced with Gotham Medium
replace.font <- function(x, font = 'iCiel Gotham Medium') {
  .replace.single <- function(y) {
    # Get the same default font
    default.fontfamily <- BoutrosLab.plotting.general::get.defaults(property = 'fontfamily');
    if (length(y) == 1 && typeof(y) == 'character' && y == default.fontfamily) {
      return(gotham.font);
      }
    else {
      return(y);
      }
    }

  # Apply recursively
  rapply(x, .replace.single, class = 'character', how = 'replace');
  }
