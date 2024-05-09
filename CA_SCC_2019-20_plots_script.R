# Plots



library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
source("H:/My R functions/lintestOR.R")
source("H:/My R functions/tidyoutput.R")
source("H:/My R functions/niceN.R")
source("H:/My R functions/niceP.R")
# library(janitor)
# library(officer)
# library(flextable)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(lme4)
'%!in%' <- function(x,y)!('%in%'(x,y))
library(car)
library(extrafont)
loadfonts()
fonts()
library(forcats)


dat <- readRDS("Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/tidyData/CA_SCC_2019_clean_data_2020-04-15.RDS")




# 3.4.3 KM curve
# remove Scotland

dat$arrival_to_b2a_minutes

datKMBA <- filter(dat, !is.na(arrival_to_b2a_minutes)) # %>% filter(country != "Scotland")
datKMBA$seen <- 1

# Optionally we can replicate this to give all, but it basically just follows the English curve
# sccKMall <- sccKM
# sccKMall$country <- "All"
# Now we bind it together
# sccKM <- rbind(sccKM, sccKMall)


# in minutes
# survfit(Surv(datKMBA$arrivaltob2agonists*24*60, datKMBA$seen) ~ datKMBA$country, data = datKMBA) %>% 
#   plot_survfit(ci = TRUE, legend.title = "Country", xmax = 360, xbreaks = seq(0, 360, 30)) + 
#   labs(x = "Time (minutes)", y = "Percentage of patients who have received PEF within 48 hours (%)")

# in hours
survfit(Surv(datKMBA$arrival_to_b2a_minutes/60, datKMBA$seen) ~ datKMBA$country, data = datKMBA) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 12, xbreaks = seq(0, 12, 1)) + 
  labs(x = "Time (hours)", 
       y = expression(paste("Percentage of patients who have received ", beta[2]," agonists (%)")))

# Bit of a nightmare trying to get the beta and subscript 2, but managed it in 
# the end.

