
################
### packages ###
################

library(tidyverse)
library(splines)
library(latex2exp)
library(RColorBrewer)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

stroke_grp_levels = c("Driving", "Approaching", "Putting")

