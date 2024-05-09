#-----------------------------------------------------------------------------------------#
# c h i l d   a s t m a   s c c   a n l y s i s   s c r i p t                             #
#                                                                                         #
# Author: a l e x                                                                         #
# Date created:  1 4 t h   a p r i l   2 0 2 0                                            #
#-----------------------------------------------------------------------------------------#




# sink() # Just putting this here so that if I run it over again it doesn't create more and more sinks...
# 
# filename <- "Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/R/logs/CA_clinical_2019_analysis_log_"
# filedate <- Sys.Date()
# 
# sink(file = paste(filename, filedate, ".txt", sep = ""),
#      append = FALSE,
#      split = TRUE)
# 
# cat("\n START \n") # This means that every time I run it it restarts the document instead of getting an
# # unuseable document at the end
# 
# sink()
# 
# sink(file = paste(filename, filedate, ".txt", sep = ""),
#      append = TRUE,
#      split = TRUE)



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

tablex <- function(x, y, z) { x %>% select(!!y, !!z) %>% table(useNA = "ifany") }

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

medTableforadmiss <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- sum(eng, na.rm = TRUE)
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- sum(wal, na.rm = TRUE)
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- sum(scot, na.rm = TRUE)
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- sum(all, na.rm = TRUE)
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  # 
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
  # 
  ret <- as.data.frame(ret)
  
  return(ret)
}


meanSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, mean, sd)
  varcol[ ,3:4] <- format(round(varcol[ ,3:4], roundno), nsmall = roundno)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
  
}

mediSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, median, lo.quart, hi.quart)
  # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno), nsmall = roundno) # otherwise use 'roundno'
  
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  
#   if(nrow(gen) == 0) {return(var_N)}
  
#  else {
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
    # gen.E2 <- select(gen.E2, Var1, England)
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      var_N <- cbind(var_N, gen3[ ,2:3])
    }
    return(var_N)
    
 # }
}



medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  
  # NOTE!!! Medians all rounded to 0dp
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
 
  ret <- as.data.frame(ret)
  
  return(ret)
}



# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
#  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
#  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
 # print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  # print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  # print(gen.A2)

  gen.table <- inner_join(gen.A2, gen.E2, by = "Var1") %>% inner_join(gen.S2, by = "Var1") %>%
    inner_join(gen.W2, by = "Var1")
  
  # Changed order to suit what they want. Need to change column names as well.  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
  #   inner_join(gen.A2, by = "Var1")

  
    colnames(gen.table) <- c(varname, 
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}




histnorm <- function(g) {
  
  h <- hist(g, breaks = 10, density = 10,
            col = "lightgray", xlab = "Accuracy", main = "Overall") 
  xfit <- seq(min(g, na.rm = TRUE), max(g, na.rm = TRUE), length = 40) 
  yfit <- dnorm(xfit, mean = mean(g, na.rm = TRUE), sd = sd(g, na.rm = TRUE)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  
  plot(h, ylim = c(0, max(yfit)))
  lines(xfit, yfit, col = "black", lwd = 2)
}


nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}


makeFlatNPercInf <- function(subana.N) {
  
  
  colnames(subana.N) <- gsub(" ", "_", colnames(subana.N))
  rownames(subana.N) <- gsub(" ", "_", rownames(subana.N))
  subana.perc <- round(prop.table(subana.N, 1)*100, 1)  # create the prop table
  # totals <- matrix(margin.table(subana.N, 2), nrow = 1, ncol = 2)
  # colnames(totals) <- paste0(colnames(subana.N), "_N")
  
  
  subana.N_flat <- matrix(subana.N, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.N)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.N)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_N")
  
  colnames(subana.N_flat) <- cols
  subana.N_flat <- as.data.frame(subana.N_flat)
  
  subana.perc_flat <- matrix(subana.perc, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.perc)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.perc)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_perc")
  
  colnames(subana.perc_flat) <- cols
  subana.perc_flat <- as.data.frame(subana.perc_flat)
  
  # subana.flat <- cbind(totals, subana.N_flat, subana.perc_flat)
  subana.flat <- cbind(subana.N_flat, subana.perc_flat)
  
  return(subana.flat)
}





# Now let's put this into a function to make it easier

WTmed <- function(x, variable) {
  print(medTable(x, variable))
  write.table(medTable(x, variable), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}

WTfreq <- function(x, variable) {
  print(myFreqTable(x, variable))
  write.table(myFreqTable(x, variable), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}




# When we read this in, if we use stringsasfactors = true then we can say that blank strings count as missing,
# without needing to recode them all. Then we can change all the necessary factors back to characters if 
# need be.



# Okay let's go!

#old # dat <- readRDS("Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/tidyData/CA_SCC_2019_clean_data_2020-07-30.RDS")

dat <- readRDS("Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/tidyData/CA_SCC_2019_clean_data_2020-09-23.RDS")


# Need the median number of hospital admissions for this one, which requires a seperate dataframe:

admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])



# If you don't want to overwrite, change reporttabs to something invalid.

# reporttabs <- paste0("Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/tidyData/CA_SCC_2019_report_tables_",
#                      Sys.Date(), ".csv")

reporttabs <- ""

write.table(medTable(dat, "age"), 
            file = reporttabs, sep = "\t", # append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)

# Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
# and instead I need to open Excel first and then go on 'import data'

WTfreq(dat, "gender")

WTfreq(dat, "IMD_quintile_Eng")
WTfreq(dat, "IMD_quintile_Scot")
WTfreq(dat, "IMD_quintile_Wal")

       

write.table(medTableforadmiss(admissmeds, "admisscount"), 
            file = reporttabs, sep = "\t", append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)

print(medTableforadmiss(admissmeds, "admisscount"))

dat$arrival2hourtimes <- cut(dat$arrival_time_mins_from_midnight, breaks = seq(-0.5, 1439.5, 120),
                             labels = paste0("lessthan", seq(2, 24, 2)))

# Day of the week N
admisstimedow.N <- table(dat$arrival2hourtimes, dat$arrival_day_of_week)
sum(admisstimedow.N)

admisstimedow.N.all <- admisstimedow.N

# Day of the week %
admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)

summary(admisstimedow.perc)


togeth <- paste(niceN(admisstimedow.N), " (", niceP(admisstimedow.perc), "%)", sep = "")
togeth <- matrix(togeth, nrow = 12, ncol = 7)

row.names(togeth) <- row.names(admisstimedow.N)

togethcolnamesprep <- as.data.frame(margin.table(admisstimedow.N, 2))

togethcolnames <- paste0(togethcolnamesprep$Var1, " (N=", 
                         format(togethcolnamesprep$Freq, big.mark=",", trim=TRUE), ")")


colnames(togeth) <- togethcolnames
togeth <- rownames_to_column(as.data.frame(togeth))

str(togeth)

write.table(togeth, 
            file = reporttabs, sep = "\t", append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)

WTfreq(dat, "smoke_status")
WTfreq(dat, "SH_smoke")
WTmed(dat, "LOS_hours")
WTfreq(dat, "life_status")
WTmed(dat, "heart_rate")
WTmed(dat, "resp_rate")
WTfreq(dat, "oxygen_sat_recorded")
WTmed(dat, "oxygen_sat_value")
WTfreq(dat, "oxygen_sat92")
WTfreq(dat, "oxygen_sat_measurement_type")
WTfreq(dat, "oxygen_supp_hypoxic_only")
WTmed(dat, "PEF_init_value")
WTfreq(dat, "PEF_init_recorded")
WTmed(dat, "PEF_prev_value")
WTfreq(dat, "PEF_prev_recorded")
WTmed(dat, "PEF_predict_value")
WTfreq(dat, "PEF_predict_recorded")
WTmed(dat, "PEF_percent_pred")
WTfreq(dat, "PEF_percpred_75")
WTfreq(dat, "asthma_sev")
WTfreq(dat, "RSR")
WTfreq(dat, "steroids_pre_arrival")
WTfreq(dat, "steroids_admin")
WTmed(dat, "arrival_to_steroids_hours")
WTfreq(dat, "steroids_1hour_alt")
WTfreq(dat, "steroids_1hour_alt_1_to_5_years")
WTfreq(dat, "steroids_1hour_alt_6_plus_years")


# Need another datset for just those who received steroids


dat$arrival4hourtimes <- cut(dat$arrival_time_mins_from_midnight, breaks = seq(-0.5, 1439.5, 240),
                             labels = paste0("lessthan", seq(4, 24, 4)))

table(dat$arrival4hourtimes, dat$arrival_day_of_week)

# Create a little mini dataset that is just for the numerator for those who received steroids within 1 hour

summary(dat$steroids_1hour)
summary(dat$steroids_pre_arrival)

datster <- filter(dat, steroids_1hour == "<1 hour")

# And another for the denominator

summary(dat$steroids_pre_arrival)

dat %>% filter(steroids_pre_arrival != "Yes") %>% filter(steroids_admin == "Yes") %>% select(steroids_admin_DT) %>% summary()

datsterdenom <- dat %>% filter(steroids_pre_arrival != "Yes")

dat %>% select(steroids_admin, steroids_1hour) %>% table()

table(dat$steroids_1hour)




# Day of the week N for the 4 hour cats
admisstimedow.N.ster.denom <- table(datsterdenom$arrival4hourtimes, datsterdenom$arrival_day_of_week)


# Day of the week N
admisstimedowforsteroids.N <- table(datster$arrival4hourtimes, datster$arrival_day_of_week)


# Day of the week %
admisstimedowforsteroids.perc <- round((admisstimedowforsteroids.N/admisstimedow.N.ster.denom)*100, 1)




summary(admisstimedowforsteroids.perc)

# Put them together into a matrix that can then be copy and pasted

togeth <- paste(niceN(admisstimedowforsteroids.N), " (", niceP(admisstimedowforsteroids.perc), "%)", sep = "")
togeth <- matrix(togeth, nrow = 6, ncol = 7)

# We need to know how many admissions there were for each day as well

tot.admiss <- margin.table(admisstimedow.N.ster.denom, 2)

row.names(togeth) <- row.names(admisstimedowforsteroids.N)

togethcolnamesprep <- as.data.frame(margin.table(admisstimedow.N.ster.denom, 2))

togethcolnames <- paste0(togethcolnamesprep$Var1, " (N=", 
                         format(togethcolnamesprep$Freq, big.mark=",", trim=TRUE), ")")


colnames(togeth) <- togethcolnames
togeth <- rownames_to_column(as.data.frame(togeth))
togeth



write.table(togeth, 
            file = reporttabs, sep = "\t", append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)



WTfreq(dat, "b2a_pre_arrival")
WTfreq(dat, "b2a_admin")
WTmed(dat, "arrival_to_b2a_minutes")
WTfreq(dat, "IV_med_aminophylline")

# # # # Converting this to a factor so that it works
dat$IV_med_ketamine <- factor(dat$IV_med_ketamine)

WTfreq(dat, "IV_med_ketamine")

# Converting back for consistency

dat$IV_med_ketamine <- as.integer(as.character((dat$IV_med_ketamine)))


WTfreq(dat, "IV_med_mag_sulphate")
WTfreq(dat, "IV_med_b2a")
WTfreq(dat, "IV_med_none")

WTfreq(dat, "crit_care_total")

WTfreq(dat, "discharge_day_of_week")

WTfreq(dat, "discharge_bundle")
WTfreq(dat, "transferred")
WTfreq(dat, "discharge_day_of_week")

attributes(table(dat$discharge_day_of_week, dat$discharge_bundle))$dimnames[[1]]
dis.bun.by.day.N <- as.data.frame(matrix(table(dat$discharge_day_of_week, dat$discharge_bundle), 
                                         nrow = 7, ncol = 3))

row.names(dis.bun.by.day.N) <- attributes(table(dat$discharge_day_of_week, dat$discharge_bundle))$dimnames[[1]]
colnames(dis.bun.by.day.N) <-  attributes(table(dat$discharge_day_of_week, dat$discharge_bundle))$dimnames[[2]]
dis.bun.by.day.N <- rownames_to_column(dis.bun.by.day.N)


dis.bun.by.day.perc <- as.data.frame(matrix(round(prop.table(table(dat$discharge_day_of_week, dat$discharge_bundle), 1)*100,
                                                  1), nrow = 7, ncol = 3))

row.names(dis.bun.by.day.perc) <- attributes(table(dat$discharge_day_of_week, dat$discharge_bundle))$dimnames[[1]]
colnames(dis.bun.by.day.perc) <-  attributes(table(dat$discharge_day_of_week, dat$discharge_bundle))$dimnames[[2]]
dis.bun.by.day.perc <- rownames_to_column(dis.bun.by.day.perc)

write.table(dis.bun.by.day.N, 
            file = reporttabs, sep = "\t", append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)


write.table(dis.bun.by.day.perc, 
            file = reporttabs, sep = "\t", append = TRUE, 
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)



WTfreq(dat, "DB_inhaler")
WTfreq(dat, "DB_maintenance")
WTfreq(dat, "DB_adherence")
WTfreq(dat, "DB_PAAP")
WTfreq(dat, "DB_triggers")

# When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.

dat$DB_smoke <- factor(dat$DB_smoke)
WTfreq(dat, "DB_smoke")
dat$DB_smoke <- as.integer(as.character(dat$DB_smoke))

table(dat$DB_smoke)

WTfreq(dat, "DB_parent_smoke")
WTfreq(dat, "DB_comm_FU_2_days")
WTfreq(dat, "DB_asthma_clinic_4_weeks")
WTfreq(dat,"DB_none")

WTfreq(dat, "inhaled_steroids_dis")
WTfreq(dat, "oral_steroids_dis")
WTfreq(dat, "oral_steroids_rescue_history")
WTfreq(dat, "referred_for_FU")


# Sub analyses

# MDT / RSR vs discharge bundle, inhaler check, PAAP issued

# All people who were not transferred.

RSRmargins <- dat %>% select(discharge_bundle_yes_no, RSR) %>% table() %>% margin.table(., 2) %>% as.data.frame()


# Receipt of discharge bundle


# Basic table and prop table and margin table to look at
dat %>% select(discharge_bundle_yes_no, RSR) %>% table()
dat %>% select(discharge_bundle_yes_no, RSR) %>% table() %>% prop.table(., 2) %>% `*`(100)  %>% round(1)
dat %>% select(discharge_bundle_yes_no, RSR) %>% table() %>% margin.table(., 2)

# To save, easier to just convert to data frame and save
RSRDB <- dat %>% select(discharge_bundle_yes_no, RSR) %>% table() %>% as.data.frame()
RSRDB$Perc <-dat %>% select(discharge_bundle_yes_no, RSR) %>% table() %>% prop.table(., 2) %>% `*`(100)  %>% 
  round(1) %>% as.data.frame() %>% pull(Freq)
RSRDB$RSRmargins <- c(NA, RSRmargins$Freq[1], NA, RSRmargins$Freq[2]) 

write.table(RSRDB, file = reporttabs, sep = "\t", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)

# We're now doing mixed effects models


library(lme4)

m1form <- "discharge_bundle_yes_no ~ RSR + (1 | hosp_code)"
m1 <- glmer(as.formula(m1form), family=binomial(link='logit'), data = dat)
summary(m1)
m1pres <- tidyoutput(m1, MEM = TRUE, meth = "Wald") %>% rownames_to_column()
m1pres

# detach("package:lmtest", unload=TRUE)

str(m1)

cat("(Mixed effects) Odds ratio function and output:\n", file=reporttabs, append=TRUE)
# For some reason, need to use the 'format' function rather than the 'as.character' function
cat(format(m1form), file=reporttabs, append=TRUE)
cat("\n", file=reporttabs, append=TRUE)

write.table(m1pres, file = reporttabs, sep = "\t", append = TRUE,
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)
cat("\n", file=reporttabs, append=TRUE)



# Receipt of inhaler technique checked

# to look at

dat %>% select(DB_inhaler, RSR) %>% table()
dat %>% select(DB_inhaler, RSR) %>% table() %>% prop.table(., 2)
dat %>% select(DB_inhaler, RSR) %>% table() %>% margin.table(., 2)
# m2 <- glm(DB_inhaler ~ RSR, family=binomial(link='logit'), data = dat)
# tidyoutput(m2, MEM = FALSE)

m2form <- "DB_inhaler ~ RSR + (1 | hosp_code)"
m2 <- glmer(as.formula(m2form), family=binomial(link='logit'), data = dat)
summary(m2)
tidyoutput(m2, MEM = TRUE, meth = "Wald")


# to save

# To save, easier to just convert to data frame and save
RSRinhaler <- dat %>% select(DB_inhaler, RSR) %>% table() %>% as.data.frame()
RSRinhaler$Perc <-dat %>% select(DB_inhaler, RSR) %>% table() %>% prop.table(., 2) %>% `*`(100)  %>% 
  round(1) %>% as.data.frame() %>% pull(Freq)
RSRinhaler$RSRmargins <- c(NA, RSRmargins$Freq[1], NA, RSRmargins$Freq[2]) 
RSRinhaler

write.table(RSRinhaler, file = reporttabs, sep = "\t", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)

m2form <- "DB_inhaler ~ RSR + (1 | hosp_code)"
m2 <- glmer(as.formula(m2form), family=binomial(link='logit'), data = dat)
m2pres <- tidyoutput(m2, MEM = TRUE, meth = "Wald") %>% rownames_to_column()

cat("(Mixed effects) Odds ratio function and output:\n", file=reporttabs, append=TRUE)
# For some reason, need to use the 'format' function rather than the 'as.character' function
cat(format(m2form), file=reporttabs, append=TRUE)
cat("\n", file=reporttabs, append=TRUE)

write.table(m2pres, file = reporttabs, sep = "\t", append = TRUE,
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)
cat("\n", file=reporttabs, append=TRUE)


# Receipt of PAAP

# to look at

dat %>% select(DB_PAAP, RSR) %>% table()
dat %>% select(DB_PAAP, RSR) %>% table() %>% prop.table(., 2)
dat %>% select(DB_PAAP, RSR) %>% table() %>% margin.table(., 2)
# m3 <- glm(DB_PAAP ~ RSR, family=binomial(link='logit'), data = dat)
# tidyoutput(m3, MEM = FALSE)

m3form <- "DB_PAAP ~ RSR + (1 | hosp_code)"
m3 <- glmer(as.formula(m3form), family=binomial(link='logit'), data = dat)
summary(m3)
tidyoutput(m3, MEM = TRUE, meth = "Wald")



# to save

# To save, easier to just convert to data frame and save
RSRPAAP <- dat %>% select(DB_PAAP, RSR) %>% table() %>% as.data.frame()
RSRPAAP$Perc <-dat %>% select(DB_PAAP, RSR) %>% table() %>% prop.table(., 2) %>% `*`(100)  %>% 
  round(1) %>% as.data.frame() %>% pull(Freq)
RSRPAAP$RSRmargins <- c(NA, RSRmargins$Freq[1], NA, RSRmargins$Freq[2]) 
RSRPAAP

write.table(RSRPAAP, file = reporttabs, sep = "\t", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

cat("\n", file=reporttabs, append=TRUE)


m3form <- "DB_PAAP ~ RSR + (1 | hosp_code)"
m3 <- glmer(as.formula(m3form), family=binomial(link='logit'), data = dat)
m3pres <- tidyoutput(m3, MEM = TRUE, meth = "Wald") %>% rownames_to_column()

cat("(Mixed effects) Odds ratio function and output:\n", file=reporttabs, append=TRUE)
# For some reason, need to use the 'format' function rather than the 'as.character' function
cat(format(m3form), file=reporttabs, append=TRUE)
cat("\n", file=reporttabs, append=TRUE)

write.table(m3pres, file = reporttabs, sep = "\t", append = TRUE,
            quote = FALSE,
            col.names = TRUE, row.names = FALSE)
cat("\n", file=reporttabs, append=TRUE)





# # # # # # # # # # # # # # # END OF WRITTEN REPORT TABLES # # # # # # # # # # # # # # # #



# START OF CSV WRITING


# First up, need to make sure all those binary variables that are currently
# coded as numeric are classed as factors. I think this is just for the IV and DB variables.


dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")), 
                         .funs = ~factor(.)) #%>% str()

dat <- dat %>% mutate_at(.vars = vars(starts_with("IV")), 
                         .funs = ~factor(.)) #%>% str()


# Now we should be fine to get on with what we're doing.

# Second, create our 'psychic' data frame for the medians

psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)

# We need to create a new row in psychic for the admissions IQR.

admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                   mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                   sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                   se = NA,
                                   lo.quart = round(quantile(admissmeds$admisscount, 
                                                    probs = 0.25, na.rm = TRUE), 0),
                                   median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                   hi.quart = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.75, na.rm = TRUE), 0))

row.names(admissmedsforpsychic) <- "admissions"

psychic <- rbind(psychic, admissmedsforpsychic)


# makeFlatNPercInf(testtable)



flat <- data.frame(country = "All")

flat <- cbind(flat,
              
             mediSumRound(dat, "age", 0),

              
              
              # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
              # and instead I need to open Excel first and then go on 'import data'
              
              FreqSum(dat, "gender"),
              
              FreqSum(dat, "IMD_quintile_Eng"),
              FreqSum(dat, "IMD_quintile_Scot"),
              FreqSum(dat, "IMD_quintile_Wal"),
              
              
              mediSumRound(dat, "admissions", 0))


# Now create the 2 hour table and bind it in

admisstimedow.N <- table(dat$arrival2hourtimes, dat$arrival_day_of_week)
rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
# bind this

flat <- cbind(flat, admisstime_flat)


admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                    rownames(admisstimedow.perc)[1:12], "admiss_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)

# bind this


flat <- cbind(flat, admisstime_flat_perc)

# admisstimedow.N.all <- admisstimedow.N


# bind these below
flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]

# Then carry on as normal:
       
              
              
         
flat <- cbind(flat,
              FreqSum(dat, "smoke_status"),
              FreqSum(dat, "SH_smoke"),
              mediSumRound(dat, "LOS_hours", 0),
              FreqSum(dat, "life_status"),
              mediSumRound(dat, "heart_rate", 0),
              mediSumRound(dat, "resp_rate", 0),
              FreqSum(dat, "oxygen_sat_recorded"),
              mediSumRound(dat, "oxygen_sat_value", 0),
              FreqSum(dat, "oxygen_sat92"),
              FreqSum(dat, "oxygen_sat_measurement_type"),
              FreqSum(dat, "oxygen_supp_hypoxic_only"),
              mediSumRound(dat, "PEF_init_value", 0),
              FreqSum(dat, "PEF_init_recorded"),
              mediSumRound(dat, "PEF_prev_value", 0),
              FreqSum(dat, "PEF_prev_recorded"),
              mediSumRound(dat, "PEF_predict_value", 0),
              FreqSum(dat, "PEF_predict_recorded"),
              mediSumRound(dat, "PEF_percent_pred", 0),
              FreqSum(dat, "PEF_percpred_75"),
              FreqSum(dat, "asthma_sev"),
              FreqSum(dat, "RSR"),
              FreqSum(dat, "steroids_pre_arrival"),
              FreqSum(dat, "steroids_admin"),
              mediSumRound(dat, "arrival_to_steroids_hours", 0),
              FreqSum(dat, "steroids_1hour_alt"),
              FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
              FreqSum(dat, "steroids_1hour_alt_6_plus_years"))
              
# I think this all works now

# steroids one hour

admisstimedow.N <- table(dat$arrival4hourtimes[dat$steroids_1hour == "<1 hour"], 
                         dat$arrival_day_of_week[dat$steroids_1hour == "<1 hour"])
rownames(admisstimedow.N) <- paste0(seq(0, 20, 4), ".00to", seq(3, 23, 4), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 42, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 6),
                rownames(admisstimedow.N)[1:6], "admiss_with_1hour_steroids_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)

flat <- cbind(flat, admisstime_flat)


admisstimedow.N.steroid.denom <- table(dat$arrival4hourtimes[!is.na(dat$steroids_1hour)],
                                       dat$arrival_day_of_week[!is.na(dat$steroids_1hour)])


admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
# rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 42, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 6),
                    rownames(admisstimedow.perc)[1:6], "admiss_with_1hour_steroids_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
flat <- cbind(flat, admisstime_flat_perc)




flat$Monday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
flat$Tuesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
flat$Wednesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
flat$Thursday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
flat$Friday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
flat$Saturday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
flat$Sunday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]


flat <- cbind(flat,
              FreqSum(dat, "b2a_pre_arrival"),
              FreqSum(dat, "b2a_admin"),
              mediSumRound(dat, "arrival_to_b2a_minutes", 0),
              FreqSum(dat, "IV_med_aminophylline"),
              FreqSum(dat, "IV_med_ketamine"),
              FreqSum(dat, "IV_med_mag_sulphate"),
              FreqSum(dat, "IV_med_b2a"),
              FreqSum(dat, "IV_med_none"),
              
              FreqSum(dat, "crit_care_total"),
              
              FreqSum(dat, "discharge_day_of_week"),
              
              FreqSum(dat, "discharge_bundle"),
              FreqSum(dat, "transferred"),
              FreqSum(dat, "discharge_day_of_week"),
              
              makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
              
              FreqSum(dat, "DB_inhaler"),
              FreqSum(dat, "DB_maintenance"),
              FreqSum(dat, "DB_adherence"),
              FreqSum(dat, "DB_PAAP"),
              FreqSum(dat, "DB_triggers"),
              
              # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
              
              FreqSum(dat, "DB_smoke"),
              FreqSum(dat, "DB_parent_smoke"),
              FreqSum(dat, "DB_comm_FU_2_days"),
              FreqSum(dat, "DB_asthma_clinic_4_weeks"),
              FreqSum(dat,"DB_none"),
              
              FreqSum(dat, "inhaled_steroids_dis"),
              FreqSum(dat, "oral_steroids_dis"),
              FreqSum(dat, "oral_steroids_rescue_history"),
              FreqSum(dat, "referred_for_FU"))

flat.all <- flat
dat.save <- dat



for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)

  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  
  flat <- data.frame(country = i)
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", 0),
                
                
                
                # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
                # and instead I need to open Excel first and then go on 'import data'
                
                FreqSum(dat, "gender"),
                
                FreqSum(dat, "IMD_quintile_Eng"),
                FreqSum(dat, "IMD_quintile_Scot"),
                FreqSum(dat, "IMD_quintile_Wal"),
                
                
                mediSumRound(dat, "admissions", 0))
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival2hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                  rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                      rownames(admisstimedow.perc)[1:12], "admiss_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # bind these below
  flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # Then carry on as normal:
  
  
  
  
  flat <- cbind(flat,
                FreqSum(dat, "smoke_status"),
                FreqSum(dat, "SH_smoke"),
                mediSumRound(dat, "LOS_hours", 0),
                FreqSum(dat, "life_status"),
                mediSumRound(dat, "heart_rate", 0),
                mediSumRound(dat, "resp_rate", 0),
                FreqSum(dat, "oxygen_sat_recorded"),
                mediSumRound(dat, "oxygen_sat_value", 0),
                FreqSum(dat, "oxygen_sat92"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                FreqSum(dat, "oxygen_supp_hypoxic_only"),
                mediSumRound(dat, "PEF_init_value", 0),
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "PEF_prev_value", 0),
                FreqSum(dat, "PEF_prev_recorded"),
                mediSumRound(dat, "PEF_predict_value", 0),
                FreqSum(dat, "PEF_predict_recorded"),
                mediSumRound(dat, "PEF_percent_pred", 0),
                FreqSum(dat, "PEF_percpred_75"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "RSR"),
                FreqSum(dat, "steroids_pre_arrival"),
                FreqSum(dat, "steroids_admin"),
                mediSumRound(dat, "arrival_to_steroids_hours", 0),
                FreqSum(dat, "steroids_1hour_alt"),
                FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
                FreqSum(dat, "steroids_1hour_alt_6_plus_years"))
  
  # I think this all works now
  
  # steroids one hour
  
  admisstimedow.N <- table(dat$arrival4hourtimes[dat$steroids_1hour == "<1 hour"], 
                           dat$arrival_day_of_week[dat$steroids_1hour == "<1 hour"])
  rownames(admisstimedow.N) <- paste0(seq(0, 20, 4), ".00to", seq(3, 23, 4), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 42, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 6),
                  rownames(admisstimedow.N)[1:6], "admiss_with_1hour_steroids_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.N.steroid.denom <- table(dat$arrival4hourtimes[!is.na(dat$steroids_1hour)],
                                         dat$arrival_day_of_week[!is.na(dat$steroids_1hour)])
  
  
  admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
  # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 42, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 6),
                      rownames(admisstimedow.perc)[1:6], "admiss_with_1hour_steroids_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  flat <- cbind(flat, admisstime_flat_perc)
  
  
  
  
  flat$Monday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
  flat$Tuesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
  flat$Wednesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
  flat$Thursday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
  flat$Friday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
  flat$Saturday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
  flat$Sunday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]
  
  
  flat <- cbind(flat,
    FreqSum(dat, "b2a_pre_arrival"),
                FreqSum(dat, "b2a_admin"),
                mediSumRound(dat, "arrival_to_b2a_minutes", 0),
                FreqSum(dat, "IV_med_aminophylline"),
                FreqSum(dat, "IV_med_ketamine"),
                FreqSum(dat, "IV_med_mag_sulphate"),
                FreqSum(dat, "IV_med_b2a"),
                FreqSum(dat, "IV_med_none"),
                
                FreqSum(dat, "crit_care_total"),
                
                FreqSum(dat, "discharge_day_of_week"),
                
                FreqSum(dat, "discharge_bundle"),
                FreqSum(dat, "transferred"),
                FreqSum(dat, "discharge_day_of_week"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                
                # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
                
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_parent_smoke"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_asthma_clinic_4_weeks"),
                FreqSum(dat,"DB_none"),
                
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                FreqSum(dat, "referred_for_FU"))
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save

# write.csv(flat.all,
#           "Z:/Group_work/PS_AA/Child Asthma/SCC 2019/Data/tidyData/CA_SCC_2019_report_data_2020-09-23.csv",
#           row.names = FALSE)





# # # # # # # # And now we do the site-level csv # # # # # # # # # #

dat$hosp_code

for (i in unique(dat.save$hosp_code)) {
  
  dat <- filter(dat.save, hosp_code == i)

  # From the adult asthma script, let's hope it works
  
  # (From here...)
  
  flat <- data.frame(hosp_code = i)
  flat$hosp_name <- as.character(dat$hosp_name[1])
  flat$trust_code <- as.character(dat$trust_code[1])
  flat$trust_name <- as.character(dat$trust_name[1])
  flat$country <- as.character(dat$country[1])
  flat$record_N <- nrow(dat)
  
  # (... to here)
  
    
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", 0),
                
                
                
                # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
                # and instead I need to open Excel first and then go on 'import data'
                
                FreqSum(dat, "gender"),
                
                FreqSum(dat, "IMD_quintile_Eng"),
                FreqSum(dat, "IMD_quintile_Scot"),
                FreqSum(dat, "IMD_quintile_Wal"),
                
                
                mediSumRound(dat, "admissions", 0))
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival2hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                  rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                      rownames(admisstimedow.perc)[1:12], "admiss_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # bind these below
  flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # Then carry on as normal:
  
  
  
  
  flat <- cbind(flat,
                FreqSum(dat, "smoke_status"),
                FreqSum(dat, "SH_smoke"),
                mediSumRound(dat, "LOS_hours", 0),
                FreqSum(dat, "life_status"),
                mediSumRound(dat, "heart_rate", 0),
                mediSumRound(dat, "resp_rate", 0),
                FreqSum(dat, "oxygen_sat_recorded"),
                mediSumRound(dat, "oxygen_sat_value", 0),
                FreqSum(dat, "oxygen_sat92"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                FreqSum(dat, "oxygen_supp_hypoxic_only"),
                mediSumRound(dat, "PEF_init_value", 0),
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "PEF_prev_value", 0),
                FreqSum(dat, "PEF_prev_recorded"),
                mediSumRound(dat, "PEF_predict_value", 0),
                FreqSum(dat, "PEF_predict_recorded"),
                mediSumRound(dat, "PEF_percent_pred", 0),
                FreqSum(dat, "PEF_percpred_75"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "RSR"),
                FreqSum(dat, "steroids_pre_arrival"),
                FreqSum(dat, "steroids_admin"),
                mediSumRound(dat, "arrival_to_steroids_hours", 0),
                FreqSum(dat, "steroids_1hour_alt"),
                FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
                FreqSum(dat, "steroids_1hour_alt_6_plus_years"))
  
  # I think this all works now
  
  # steroids one hour
  
  admisstimedow.N <- table(dat$arrival4hourtimes[dat$steroids_1hour == "<1 hour"], 
                           dat$arrival_day_of_week[dat$steroids_1hour == "<1 hour"])
  rownames(admisstimedow.N) <- paste0(seq(0, 20, 4), ".00to", seq(3, 23, 4), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 42, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 6),
                  rownames(admisstimedow.N)[1:6], "admiss_with_1hour_steroids_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.N.steroid.denom <- table(dat$arrival4hourtimes[!is.na(dat$steroids_1hour)],
                                         dat$arrival_day_of_week[!is.na(dat$steroids_1hour)])
  
  
  admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
  # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 42, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 6),
                      rownames(admisstimedow.perc)[1:6], "admiss_with_1hour_steroids_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  flat <- cbind(flat, admisstime_flat_perc)
  
  
  
  
  flat$Monday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
  flat$Tuesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
  flat$Wednesday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
  flat$Thursday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
  flat$Friday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
  flat$Saturday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
  flat$Sunday_admit_for_1hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]
  
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_pre_arrival"),
                FreqSum(dat, "b2a_admin"),
                mediSumRound(dat, "arrival_to_b2a_minutes", 0),
                FreqSum(dat, "IV_med_aminophylline"),
                FreqSum(dat, "IV_med_ketamine"),
                FreqSum(dat, "IV_med_mag_sulphate"),
                FreqSum(dat, "IV_med_b2a"),
                FreqSum(dat, "IV_med_none"),
                
                FreqSum(dat, "crit_care_total"),
                
                FreqSum(dat, "discharge_day_of_week"),
                
                FreqSum(dat, "discharge_bundle"),
                FreqSum(dat, "transferred"),
                FreqSum(dat, "discharge_day_of_week"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                
                # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
                
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_parent_smoke"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_asthma_clinic_4_weeks"),
                FreqSum(dat,"DB_none"),
                
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                FreqSum(dat, "referred_for_FU"))
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save

nrow(flat.all)
unique(flat.all$hosp_code)
head(flat.all$country)

colnames(flat.all)

# change to appropriate order and remove unnecessary 'median admissions' columns and heat map columns,
flat.all <- flat.all %>% select(hosp_code:record_N, 
                                country:referred_for_FU_Already_being_seen_in_secondary_care_clinic_perc) %>% 
  select(-admissions_N, -admissions_median, -admissions_lo.quart, -admissions_hi.quart) %>%
  select(-c(Monday_0.00to3.59_admiss_with_1hour_steroids_n:Sunday_20.00to23.59_admiss_with_1hour_steroids_perc))
# remove the 'all', 'england', 'wales', and 'scotland' rows

summary(flat.all$record_N)

# they are the ones without a 'record_N' variable.

flat.all <- flat.all %>% filter(!is.na(record_N))

nrow(flat.all)
length(unique(flat.all$hosp_code))
length(unique(dat.save$hosp_code))

# write.csv(flat.all,
#           "Z:/Group_work/PS_AA/Child Asthma/SCC 2019/Data/tidyData/CA_SCC_2019_hospital_level_data_2020-10-23.csv",
#           row.names = FALSE)

