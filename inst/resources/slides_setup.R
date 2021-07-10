knitr::opts_chunk$set(echo = FALSE, global.par = TRUE, warning=FALSE, message=FALSE, fig.height = 3, dpi = 300)
knitr::opts_hooks$set(
  panel = function(options) {
    if (isTRUE(options$panel)) {
      options$eval = TRUE
      options$echo = TRUE
      options
    }
  }
)

knitr::knit_hooks$set(
  panel = function(before, options, envir){
    if (isTRUE(options$panel)) {
      options$echo=TRUE
      if (before) {
        '.codePanel['
      } else ']'
    }
  }
)

options(scipen=999)
set.seed(420)

bg_col <- "#fdfdfd"
theme_col <- "#52006f"
default_col <- '#464b61'
second_col <- "#00979f"

### plot theme for teaching materials
library(ggplot2)

point_col <- paste0(default_col, "88")

ggplot2::theme_set(cowplot::theme_cowplot() +
                     theme(line = element_line(colour = default_col),
                           plot.background = element_rect(fill = bg_col),
                           legend.background = element_rect(fill= bg_col),
                           panel.background = element_rect(fill = bg_col),
                           text = element_text(colour = default_col),
                           title = element_text(colour = default_col),
                           axis.line = element_line(colour = default_col),
                           axis.ticks = element_line(colour = default_col),
                           axis.text = element_text(colour = default_col),
                           axis.title = element_text(colour = default_col),
                     )
)
ggplot2::update_geom_defaults("bar", list(fill = bg_col, colour = default_col))

