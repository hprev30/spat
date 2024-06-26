# loading packages

# # be sure to check for packages conflicts!
# 01 import/export ----
library(readxl) # read excel files
library(janitor) # simple tools to clean dirty data
library(here) # a simpler way to find your files
library(SWMPr) # working with SWMP data from the NERRS
library(SWMPrExtension) # expanded plotting and analysis of SWMP data from NERRS
# library(xlsx) # to export df as .xlsx files

# 02 tidy and wrangle ----
library(tidyverse) # because...tidyverse (ggplot2, tidyr, dplyr)
library(lubridate) # dates and times
library(weathermetrics) # functions to convert between weather metrics

# 03 pulling information and statistics ----
library(broom) # convert statistical analysis objects into tidy tibbles
library(psych)
# library(wql)
library(glmmTMB)
library(AICcmodavg)
library(bbmle)
library(DHARMa)
library(car)
library(emmeans)
library(performance)

# 04 markdown ----
# library(knitr)
# library(kableExtra) # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html 

# 05 graphics ----
library(patchwork) # grid graphics
library(scales) # scale functions for visualization
library(plotly) # create interactive web graphics - use for html output files
library(gganimate) # create animated web graphics
library(gghighlight) # allows for highlighting flexibility in ggplot2
library(ggthemes) # colorblind palettes
library(ggrepel)
