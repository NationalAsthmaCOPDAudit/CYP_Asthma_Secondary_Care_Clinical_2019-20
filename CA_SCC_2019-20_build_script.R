#-----------------------------------------------------------#
# c h i l d   a s t m a   s c c   b u i l d   s c r i p t   #
#                                                           #
# Author: a l e x                                           #
# Date created:  3 1 s t   m a r c h   2 0 2 0              #
#-----------------------------------------------------------#




# sink() # Just putting this here so that if I run it over again it doesn't create more and more sinks...
# 
# filename <- "Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/R/logs/CA_clinical_2019_cleaning_log_"
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
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
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
  
  if(nrow(gen) == 0) {return(var_N)}
  
  else {
    
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
    
  }
}



medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
  return(ret)
}

# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  print(gen.A2)
  
  gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
    inner_join(gen.A2, by = "Var1")
  colnames(gen.table) <- c(varname, paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
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

# When we read this in, if we use stringsasfactors = true then we can say that blank strings count as missing,
# without needing to recode them all. Then we can change all the necessary factors back to characters if 
# need be.

dat <- read.csv(
  "Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/rawData/CYPA-Extract-20190601-20200131-IMP-v102.csv",
  header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

# write.csv(dat, "Z:/Group_work/Philip/CYP SCC/CYPA-Extract-20190601-20200131-IMP-v102.csv",
#           row.names = FALSE, na = "")


summary(dat)

# Rename the columns so that they are easier to read and if possible match up to the adult asthma column
# names:


dat1 <- dat

dat2 <- read.csv("Z:/Group_work/Alex/Encrypted/NACAP-MAY22/CYPA-2104-2203-v203-Imperial.csv",
                 header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

colnames(dat1)
colnames(dat2)

# IT's exactly the same.


namescomp <- data.frame(old = rep(NA, 69), new = rep(NA, 69))
namescomp$old[1:67] <- colnames(dat1)
namescomp$new <- colnames(dat2)

View(namescomp)


dat <- dat %>% select(study_ID = STUDYID,
                      patient_ID = PATIENTID,
                      LSOA = lsoa11,
                      hosp_code = Hospital,
                      trust_code = TrustCode,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                      ambulance = X1.2.Arrive.by.Ambulance,
                      age = X.1.2a.Age.At.Arrival,
                      gender = X2.3.Gender,
                      smoke_status = X3.1.Smoking,
                      SH_smoke = X3.2.Smoking.exposure,
                      heart_rate = X4.1.Heart.Rate..BPM.,
                      resp_rate = X4.2.Respiratory.Rate..BPM.,
                      oxygen_sat_value = X4.3.Oxygen.Saturation....,
                      oxygen_sat_recorded = X4.3.1.OxygenSaturation.NR,
                      oxygen_sat_measurement_type = X4.3a.Oxygen.Measurement,
                      oxygen_supp = X4.3b.Supplementary.oxygen,
                      PEF_init_value = X4.4.Peak.Flow..Post.Arrival.,
                      PEF_init_recorded = X4.4.1.Peak.Flow.NR,
                      PEF_prev_value = X4.4a.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X4.4a.1.Previous.Best.Peak.Flow.NR,
                      PEF_predict_value = X4.4b.Predicted.Peak.Flow,
                      PEF_predict_recorded = X4.4b.1.Predicted.Peak.Flow.NR,
                      RSR = X5.1.MDT.Specialist.Review,
                      steroids_pre_arrival = X5.2.Steroids.Pre.Arrival,
                      steroids_admin = X5.3.Steroids,
                      steroids_admin_date = X5.3a.Steroids.Date,
                      steroids_admin_time = X5.3b.Steroids.Time,
                      b2a_pre_arrival = X5.4.Agonists.B2.Pre.Arrival,
                      b2a_admin = X5.5.Agonists.B2.On.Arrival,
                      b2a_admin_date = X5.5a.Agonists.B2.Date,
                      b2a_admin_time = X5.5b.Agonists.B2.Time,
                      IV_med_total = X5.6.Intravenous.Medications,
                      IV_med_aminophylline = X5.6.Intravenous.Medications...Aminophylline,
                      IV_med_ketamine = X5.6.Intravenous.Medications...Ketamine,
                      IV_med_mag_sulphate = X5.6.Intravenous.Medications...Magnesium.sulphate,
                      IV_med_b2a = X5.6.Intravenous.Medications...B2.agonists,
                      IV_med_none = X5.6.Intravenous.Medications...No,
                      crit_care_total = X5.7.Critical.Care.Transfer,
                      crit_care_HDU = X5.7.Critical.Care.Transfer...Yes...HDU,
                      crit_care_ICU = X5.7.Critical.Care.Transfer...Yes...ICU,
                      crit_care_none = X5.7.Critical.Care.Transfer...No,
                      life_status = X6.1.Discharge.Life.Status,
                      discharge_date = X6.2a.Discharge.Date,
                      discharge_time = X6.2b.Discharge.Time,
                      discharge_bundle = X6.3.Discharge.Bundle,
                      discharge_elements_all = X6.4.Discharge.Elements, # completely useless: drop.
                      DB_inhaler = X6.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X6.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X6.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X6.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X6.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X6.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_parent_smoke = X6.4.Discharge.Elements...Parent.carer.tobacco.dependency.addressed,
DB_comm_FU_2_days =  X6.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
DB_asthma_clinic_4_weeks = X6.4.Discharge.Elements...Paediatric.asthma.clinic.requested.within.4.weeks,
DB_RSR_if_LT = X6.4.Discharge.Elements...Paediatric.respiratory.specialist.review.if.there.have.been.life.threatening.features,
                      DB_none = X6.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X7.1.Inhaled.Steroids.At.Discharge,
                      oral_steroids_dis = X7.2.Oral.Steroids.at.Discharge,
                      oral_steroids_rescue_history = X7.3.Rescue.Oral.Steroids...2,
                      referred_for_FU = X7.4.Referred.for.Followup,
                      overseas = Overseas,
                      completion_status = Completion..)
                      # Dataset.Version   # useless - drop

# dat %>% filter(discharge_bundle == "Patient transferred to another hospital") %>% 
#   select(discharge_elements_all) %>% summary()

# Those transferred can't have anything inputted for the discharge bundle

# Immediately drop all draft records before doing anything else because they will just mess up the dataset
# otherwise.

dat %>% select(hosp_code, overseas) %>% table()

colnames(dat)

nlc("No. records in original dataset:")
nrow(dat)

nlc("No. records that are drafts or test hospitals:")
dat %>% filter(completion_status != "100%" | hosp_code == "YYY") %>% nrow()


dat <- dat %>% filter(completion_status == "100%" & hosp_code != "YYY")
nlc("No. records after draft records and test hospitals removed:")
nrow(dat)

# remove that column because we don't need it any more
dat <- dat %>% select(-completion_status)


# Remove overseas patients
nlc("Number of overseas patients:") 

dat %>% filter(overseas == 1) %>% nrow()

dat <- dat %>% filter(overseas != 1)
nlc("No. records after overseas removed:")
nrow(dat)


# remove that column because we don't need it any more
dat <- dat %>% select(-overseas)


table(dat$oxygen_supp, useNA = "ifany")
table(dat$oxygen_sat_measurement_type, useNA = "ifany")




# Need to add in the empty gender factors and smoking status factors, and while we're doing it we might as well
# put it in the correct order

summary(dat$smoke_status)


dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Other", "Not recorded"))
dat$smoke_status <- factor(dat$smoke_status,
                           levels = c("Never smoked", "Ex-smoker", "Current smoker", "Ex-smoker and current vaper",
                                      "Never smoked and current vaper", "Not recorded"))
summary(dat$smoke_status)
# Now we do more building and cleaning:

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))

# Everything seems to be alright at this point.

# Now would probably be a good point to link to the LSOAs.

# Read in the IMD quintiles


IMDeng <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2019_England.csv",
                   header = TRUE, stringsAsFactors = FALSE)
IMDwales <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2019_Wales.csv",
                     header = TRUE, stringsAsFactors = FALSE)
IMDscot <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2016_Scotland.csv",
                    header = TRUE, stringsAsFactors = FALSE)


# Create the quintiles for the English IMD data

IMDeng$IMD_quintile <- NA

IMDeng$IMD_quintile[IMDeng$IMD_decile == 1] <- 1
IMDeng$IMD_quintile[IMDeng$IMD_decile == 2] <- 1
IMDeng$IMD_quintile[IMDeng$IMD_decile == 3] <- 2
IMDeng$IMD_quintile[IMDeng$IMD_decile == 4] <- 2
IMDeng$IMD_quintile[IMDeng$IMD_decile == 5] <- 3
IMDeng$IMD_quintile[IMDeng$IMD_decile == 6] <- 3
IMDeng$IMD_quintile[IMDeng$IMD_decile == 7] <- 4
IMDeng$IMD_quintile[IMDeng$IMD_decile == 8] <- 4
IMDeng$IMD_quintile[IMDeng$IMD_decile == 9] <- 5
IMDeng$IMD_quintile[IMDeng$IMD_decile == 10] <- 5


IMDeng <- IMDeng %>% select(LSOA = LSOA_code_2011, IMD_quintile_Eng = IMD_quintile)
IMDwales <- IMDwales %>% select(LSOA = LSOA_Code, IMD_quintile_Wal = WIMD_2019_Overall_Quintile)
IMDscot <- IMDscot %>% select(LSOA = LSOA_Code, IMD_quintile_Scot = IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng, by = "LSOA")
dat <- left_join(dat, IMDwales, by = "LSOA")
dat <- left_join(dat, IMDscot, by = "LSOA")


dat <- dat %>% mutate(IMD_quintile_Eng = factor(IMD_quintile_Eng),
                      IMD_quintile_Wal = factor(IMD_quintile_Wal),
                      IMD_quintile_Scot = factor(IMD_quintile_Scot))


# create a variable to represent any IMD present

dat <- dat %>% mutate(anyIMD = factor(ifelse(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Wal) &
                                               is.na(IMD_quintile_Scot), "No IMD", "IMD present")))

# (Or:)
dat %>% filter(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Wal) & is.na(IMD_quintile_Scot)) %>% nrow()

# Add country variable

country <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_CYP_asthma_R_format.csv",
                    stringsAsFactors = FALSE)

# country <- country %>% rename(country = Country, hosp_code = HospCode, trust_code = TrustCode)

# Drop the provided trust code because the trust code in Tim's provided spreadsheet should be the most
# up-to-date

dat <- dat %>% select(-trust_code)

dat <- left_join(dat, country, by = "hosp_code")

# The trust codes differ, however this seems to be a constant issue. Hospitals details are linked by 
# hospital name rather than trust though, so I'm just going to go by trust details given in the most 
# up-to-date spreadsheet given by Tim. If you hash out the dropping of the trust code above, you can
# try this for yourself by running the code below

# dat %>% select(trust_code.x, trust_code.y) %>% table() %>% sum()
# dat %>% select(trust_code.x, trust_code.y) %>% table() %>% diag() %>% sum()
# 
# dat %>% select(hosp_name) %>% filter(is.na(hosp_name))
 





# # This code gives the hospitals for which we are missing the country link up or hospital details
# unique(dat$hosp_code)[!(unique(dat$hosp_code) %in% country$hosp_code)]
# 
# data.frame(hosp_code = unique(dat$hosp_code)[!(unique(dat$hosp_code) %in% country$hosp_code)]) %>% CP()
# 
# 
# 
# dat %>% filter(country == "") %>% nrow()
# dat %>% filter(is.na(country)) %>% nrow()
# table(dat$country)
# 
# dat %>% filter(HospCode == "") %>% nrow()

# We can try it for trust codes as well

# dat <- left_join(dat, country, by = "trust_code")
# 
# table(dat$trust_code)
# 
# summary(factor(dat$country))





dat$country <- factor(dat$country, levels = c("England", "Scotland", "Wales"), ordered = FALSE)
levels(dat$country)


table(dat$arrival_date)
# Okay now we've added IMD quintile and additional hospital details... let's do some cleaning.


# Let's convert all our dates to dates and times to times (as they're currently factors)

dat$arrival_date <- as.Date(dat$arrival_date, "%d/%m/%Y")
dat$steroids_admin_date <- as.Date(dat$steroids_admin_date, "%d/%m/%Y")
dat$b2a_admin_date <- as.Date(dat$b2a_admin_date, "%d/%m/%Y")
dat$discharge_date <- as.Date(dat$discharge_date, "%d/%m/%Y")

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)

summary(dat$arrival_date)
summary(dat$arrival_time)
summary(dat$steroids_admin_date)
summary(dat$steroids_admin_time)
summary(dat$b2a_admin_date)
summary(dat$b2a_admin_time)
summary(dat$discharge_date)
summary(dat$discharge_time)

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)

# dat$arrival_time[dat$arrival_time != ""] <- paste0(dat$arrival_time[dat$arrival_time != ""]
#                                                    
#                                                    
#                                                    
# dat$steroids_admin_time[dat$steroids_admin_time)
# dat$b2a_admin_time[dat$b2a_admin_time)
# dat$discharge_time[dat$discharge_time)
# 
# 
head(dat$arrival_time)



dat <- dat %>% mutate(arrival_time = ifelse(arrival_time == "", "", 
                                     paste0(arrival_time, ":00")),
                      steroids_admin_time = ifelse(steroids_admin_time == "", "", 
                                            paste0(steroids_admin_time, ":00")),
                      b2a_admin_time = ifelse(b2a_admin_time == "", "", 
                                            paste0(b2a_admin_time, ":00")),
                      discharge_time = ifelse(discharge_time == "", "", 
                                            paste0(discharge_time, ":00")))


head(dat$steroids_admin_time)
head(dat$arrival_time)
head(dat$arrival_date)
head(dat$discharge_time)
head(dat$discharge_date)

# We can try converting times to 'difftime' when we try and create the table of when people are given
# particular things.


dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M:%S"),
                      steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time), 
                                                     format="%Y-%m-%d %H:%M:%S"),
                      b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M:%S"),
                      discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M:%S"))

head(dat$arrival_DT)
head(dat$steroids_admin_DT)
summary(dat$steroids_admin_DT)

# Life status

dat <- dat %>% mutate(life_status = recode_factor(life_status, Yes = "Alive"))
dat$life_status <- factor(dat$life_status, levels = c(levels(dat$life_status), "Died as inpatient"))

head(dat)


# Time to b2a minutes

dat <- dat %>% mutate(arrival_to_b2a_minutes = difftime(b2a_admin_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_b2a_minutes)
dat$arrival_to_b2a_minutes <- as.integer(dat$arrival_to_b2a_minutes)

# Time to steroids in hours

dat <- dat %>% mutate(arrival_to_steroids_hours = difftime(steroids_admin_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_steroids_hours)
dat$arrival_to_steroids_hours <- as.numeric(dat$arrival_to_steroids_hours)

summary(dat$arrival_to_steroids_hours)

# length of stay hours

dat <- dat %>% mutate(LOS_hours = difftime(discharge_DT, arrival_DT, units = "hours"))
head(dat$LOS_hours)
dat$LOS_hours <- as.numeric(dat$LOS_hours)

# Need to remove those who were transferred from this

dat <- dat %>% mutate(LOS_hours = ifelse(discharge_bundle == "Patient transferred to another hospital", 
                                         NA, LOS_hours))


# day of arrival and day of discharge


dat <- dat %>% mutate(arrival_day_of_week = weekdays(arrival_date, abbreviate = FALSE))

dat$arrival_day_of_week <- ordered(dat$arrival_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

table(dat$arrival_day_of_week)


dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))

dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                     "Friday", "Saturday", "Sunday"))


# Those who transferred need to be removed from the discharge day of week

# Using ifelse here changes them to factor levels so I'm doing it the old school method

# dat <- dat %>% mutate(discharge_day_of_week = ifelse(discharge_bundle == "Patient transferred to another hospital", 
#                                          NA, discharge_day_of_week))

dat$discharge_day_of_week[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA

summary(dat$discharge_day_of_week)

# time of arrival for the time of arival table.
# basically times are so annoying to work with, so what I've done is given each time an associated date of 
# 1970-01-01 (the common origin date) and then subtracted the time from 1970-01-01 00:00:00 to give minutes
# into the day. Seems to have worked well

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.POSIXct(paste("1970-01-01", arrival_time))) %>%
  mutate(arrival_time_mins_from_midnight = difftime(arrival_time_mins_from_midnight, 
                                                    "1970-01-01 00:00:00", units = "mins"))
head(dat$arrival_time_mins_from_midnight)

dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(arrival_time_mins_from_midnight) %>% head(10)
dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(desc(arrival_time_mins_from_midnight)) %>% head(10)

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.integer(arrival_time_mins_from_midnight))


# Peak flow things



dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% filter(!is.na(PEF_prev_value)) %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% 
  table(useNA = "ifany")

# Invalid PEF values? No.
dat %>% filter(PEF_init_value < 30) %>% nrow()
dat %>% filter(PEF_init_value > 800) %>% nrow()

dat %>% filter(PEF_prev_value < 30) %>% nrow()
dat %>% filter(PEF_prev_value > 800) %>% nrow()

dat %>% filter(PEF_predict_value < 30) %>% nrow()
dat %>% filter(PEF_predict_value > 800) %>% nrow()

# PEF / recorded inconsistencies? No

dat %>% filter(!is.na(PEF_init_value)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & !is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% select(PEF_init_recorded) %>% table()

dat %>% filter(!is.na(PEF_prev_value)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_prev_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & is.na(PEF_prev_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_predict_value)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & !is.na(PEF_predict_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & is.na(PEF_predict_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_predict_value)) %>% nrow()

summary(dat$PEF_init_recorded)

# Make the 'recorded' variable clearer, by adding in 'recorded' for people that have an associated value

dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)

dat$PEF_init_recorded[(!is.na(dat$PEF_init_value))] <- "Recorded"
dat$PEF_prev_recorded[(!is.na(dat$PEF_prev_value))] <- "Recorded"
dat$PEF_predict_recorded[(!is.na(dat$PEF_predict_value))] <- "Recorded"

dat$PEF_init_recorded <- factor(dat$PEF_init_recorded)
dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded)
dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded)

summary(dat$PEF_init_recorded)
summary(dat$PEF_prev_recorded)
summary(dat$PEF_predict_recorded)


dat %>% filter(age > 5) %>% select(PEF_init_recorded) %>% table(useNA = "ifany")
dat %>% filter(age < 6) %>% select(PEF_init_recorded) %>% table(useNA = "ifany")


dat %>% filter(age > 5) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(age > 5) %>% filter(PEF_prev_recorded == "Not recorded") %>% 
  select(PEF_predict_recorded) %>% table(useNA = "ifany")

# No one over the age of 5 has missing data for this question

dat %>% filter(age > 5) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(age > 5) %>% filter(PEF_prev_recorded == "Not recorded") %>% 
  select(PEF_predict_recorded) %>% table(useNA = "ifany")

# No invalid PEFs! Just doing it here while I sort out that variable


# Now to make the % predicted

dat <- dat %>% mutate(PEF_percent_pred = round(ifelse(!is.na(PEF_prev_value), PEF_init_value/PEF_prev_value,
                                                PEF_init_value/PEF_predict_value)*100, 0))

summary(dat$PEF_percent_pred)
dat %>% filter(PEF_init_recorded == "Recorded" & 
                 (PEF_prev_recorded == "Recorded" | PEF_predict_recorded == "Recorded")) %>% nrow()

# Split into <75% and => 75%

dat <- dat %>% mutate(PEF_percpred_75 = factor(ifelse(PEF_percent_pred < 75, "<75%",
                                       ifelse(PEF_percent_pred >= 75, ">= 75%", NA))))

# Seems to work
summary(dat$PEF_percpred_75)

# Matches the expected nummber

# Cleaning

# No inconsistent steroids or beta agonists administration

dat %>% filter(is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_time)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% nrow()
dat %>% select(steroids_admin) %>% table()
dat %>% filter(steroids_admin == "Yes") %>% filter(is.na(steroids_admin_DT)) %>% nrow()


dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()



summary(dat)

# Discharge bundle consistency check

dat %>% select(discharge_bundle, DB_none) %>% table(useNA = "ifany") 
table(dat$DB_none)

# 144 people received a discharge bundle but were marked as 'none' for each element.

# Filter out invalid dates (N = 0)

dat %>% filter(arrival_date < "2019-06-01") %>% nrow()
dat %>% filter(arrival_date > "2020-01-31") %>% nrow()

# Was anyone discharged before they arrived, or received medication before they arrived, or received medication
# after they left? No.

dat %>% filter(discharge_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(b2a_admin_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(steroids_admin_DT - arrival_DT < 0) %>% nrow()

dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - steroids_admin_DT < 0) %>% nrow()


# Filter out invalid heart rates (0-250) or resp rates (0-80) or oxygen sats (60-100). None present.

dat %>% filter(heart_rate < 0) %>% nrow()
dat %>% filter(heart_rate > 250) %>% nrow()

dat %>% filter(resp_rate < 0) %>% nrow()
dat %>% filter(resp_rate > 80) %>% nrow()

dat %>% filter(oxygen_sat_value < 60) %>% nrow()
dat %>% filter(oxygen_sat_value > 100) %>% nrow()


dat %>% filter(is.na(oxygen_sat_value)) %>% nrow()
dat %>% select(oxygen_sat_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(oxygen_sat_value) & is.na(oxygen_sat_recorded)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value) & !is.na(oxygen_sat_recorded)) %>% nrow()

summary(dat$oxygen_sat_recorded)

str(dat$oxygen_sat_recorded)

dat <- dat %>% mutate(oxygen_sat_recorded = fct_explicit_na(oxygen_sat_recorded, "Recorded"))

summary(dat$PEF_prev_recorded)


dat$oxygen_sat_recorded[is.na(dat$oxygen_sat_recorded)] <- "Recorded"



# Need to classify the oxygen saturation
head(dat$oxygen_sat_value)

dat <- dat %>% mutate(oxygen_sat92 = ifelse(oxygen_sat_value < 92, "<92",
                                            ifelse(oxygen_sat_value > 91, ">=92", NA)))


table(dat$oxygen_sat92, useNA = "ifany")
table(dat$oxygen_sat_recorded)

# It's worked

# Also need to create a new variable for the hypoxic children

table(dat$oxygen_supp, useNA = "ifany")

dat <- dat %>% mutate(oxygen_supp_hypoxic_only = oxygen_supp)
dat$oxygen_supp_hypoxic_only[dat$oxygen_sat92 == ">=92"] <- NA

dat %>% select(oxygen_sat92, oxygen_supp_hypoxic_only) %>% table(useNA = "ifany")

# More on steroids

summary(dat$steroids_pre_arrival)

# We need to remove data to account for the people who received steroids pre=arrival.
# However, don't want to simply remove it entirely. So, I'm going to create a new variable for all the steroids
# variables to represent what they were before I deleted the necessary things from them.

dat <- dat %>% mutate(steroids_admin_inc_pre_arrival_steroids = steroids_admin,
                      steroids_admin_date_inc_pre_arrival_steroids = steroids_admin_date,
                      steroids_admin_time_inc_pre_arrival_steroids = steroids_admin_time,
                      steroids_admin_DT_inc_pre_arrival_steroids = steroids_admin_DT,
                      arrival_to_steroids_hours_inc_pre_arrival_steroids = arrival_to_steroids_hours)


dat$steroids_admin[dat$steroids_pre_arrival == "Yes"] <- NA
dat$steroids_admin_date[dat$steroids_pre_arrival == "Yes"] <- NA
dat$steroids_admin_time[dat$steroids_pre_arrival == "Yes"] <- NA
dat$steroids_admin_DT[dat$steroids_pre_arrival == "Yes"] <- NA
dat$arrival_to_steroids_hours[dat$steroids_pre_arrival == "Yes"] <- NA
summary(dat$arrival_to_steroids_hours)




dat <- dat %>% mutate(steroids_1hour = factor(ifelse(arrival_to_steroids_hours >= 1 | 
                                                       is.na(arrival_to_steroids_hours), 
                                                     ">= 1 hour OR not given OR not recorded", "<1 hour")))
summary(dat$steroids_1hour)
dat$steroids_1hour[dat$steroids_pre_arrival == "Yes"] <- NA
summary(dat$steroids_1hour)


# need to create another variable for the updated steroids 1 hour variable. Just going to create a new variable
# rather than replace it because I'm not sure if the standard variable is used for other things.


summary(dat$steroids_admin)
summary(dat$steroids_pre_arrival)


dat <- dat %>% mutate(steroids_1hour_alt = factor(ifelse(is.na(arrival_to_steroids_hours), as.character(steroids_admin),
                                                     ifelse(arrival_to_steroids_hours < 1, "<1 hour",
                                                            ">= 1 hour"))))
summary(dat$steroids_1hour_alt)

dat %>% select(steroids_1hour_alt, steroids_admin) %>% table(useNA = "ifany")
dat %>% select(steroids_1hour_alt, steroids_1hour) %>% table(useNA = "ifany")
dat %>% select(steroids_1hour_alt, steroids_pre_arrival) %>% table(useNA = "ifany")


# Sorted. For some reason, NA aren't counted as not being <1.

summary(dat$steroids_pre_arrival)
summary(dat$arrival_to_steroids_hours)
summary(dat$arrival_to_steroids_hours_inc_pre_arrival_steroids)
summary(dat$steroids_admin)

dat %>% select(steroids_pre_arrival, steroids_1hour, steroids_admin) %>% table(useNA = "ifany")

# And now we sort out the discharge bundle variables, with inappropriate tobacco smoking ones recoded to missing.
# We also have to remove those who transferred to another hospital.

dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")),
                         .funs = ~ifelse(discharge_bundle == "Patient transferred to another hospital", NA, .))

colnames(dat)

dat %>% select(SH_smoke) %>% table(useNA = "ifany")
dat %>% select(DB_parent_smoke, SH_smoke) %>% table()
dat %>% select(smoke_status, DB_smoke) %>% table(useNA = "ifany")

# Does anyone under the age of 11 have smoking status as anything other than missing? No. 
dat %>% filter(age < 11) %>% select(smoke_status) %>% table(useNA = "ifany")
dat %>% filter(age < 11) %>% nrow()

dat %>% filter(age > 10) %>% select(smoke_status) %>% table(useNA = "ifany")
dat %>% filter(age > 10) %>% nrow()

summary(dat$DB_smoke)

# Going to save a copy of this original variable because removing 18 people whose smoking status was not
# recorded, but did receive smoking cessation advice. Same for parent smoking.

dat <- dat %>% mutate(DB_smoke_NR_included = DB_smoke,
                      DB_parent_smoke_NR_included = DB_parent_smoke)


# And now we make all those who aren't current smokers missing for 'DB_smoke':

dat$DB_smoke[dat$smoke_status != "Current smoker" | is.na(dat$smoke_status)] <- NA
dat %>% select(smoke_status, DB_smoke) %>% table(useNA = "ifany")

# And we do the same for parents' smoking status

dat$DB_parent_smoke[dat$SH_smoke != "Yes"] <- NA
dat %>% select(SH_smoke, DB_parent_smoke) %>% table(useNA = "ifany")


# Going to create a new variable for transferred and not transferred, and then recode transferred as missing
# for the discharge bundle variable. Doing it right at the end because I know I use that variable previously.

# No missing so doing this is fine. But maybe need to watch out if I re-use this if someone dies.

dat <- dat %>% mutate(transferred = factor(ifelse(
                                           discharge_bundle == "Patient transferred to another hospital", "Yes",
                                    ifelse(discharge_bundle != "Patient transferred to another hospital", "No",
                                                  NA))))

# And now we change discharge bundle so that the transferreds are missing

dat$discharge_bundle[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA
dat$discharge_bundle <- factor(dat$discharge_bundle)

str(dat$transferred)

dat %>% select(transferred, discharge_bundle) %>% table(useNA = "ifany")

# I also need to create a variable for discharge bundle yes/no for the subanalysis section

dat <- dat %>% mutate(discharge_bundle_yes_no = discharge_bundle)
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Parental/carer/self-discharge"] <- "No"
dat$discharge_bundle_yes_no <- factor(dat$discharge_bundle_yes_no, levels = c("No", "Yes"))

summary(dat$discharge_bundle_yes_no)

colnames(dat)               
summary(dat)

# Okay and now I think we're ready to go!

# I'm also going to put the levels so that they're in the same order as the report.




dat$SH_smoke <- factor(dat$SH_smoke, levels = c("Yes", "No", "Not recorded"))
dat$oxygen_sat_recorded <- factor(dat$oxygen_sat_recorded, levels = c("Recorded", "Not recorded"))
dat$oxygen_sat_measurement_type <- factor(dat$oxygen_sat_measurement_type, levels = c("Yes", "No - room air",
                                                                                      "Not recorded"))
dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded",
                                                                  "Patient too unwell", "Not recorded"))
dat$steroids_pre_arrival <- factor(dat$steroids_pre_arrival, levels = c("Yes", "Not recorded", "No"))


dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered", "Not recorded"))

summary(dat$b2a_admin)

dat$b2a_pre_arrival <- factor(dat$b2a_pre_arrival, levels = c("Yes", "Not recorded", "No"))
dat$b2a_admin <- factor(dat$b2a_admin, levels = c("Yes", "Not recorded", "Not administered"))

dat$crit_care_total <- factor(dat$crit_care_total, levels = c("Yes - ICU", "Yes - HDU", 
                                                              "Yes - HDU,Yes - ICU", "No"))
dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Parental/carer/self-discharge"))

dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis, 
                                   levels = c("Yes", "No - not medically indicated", "No - reason not given",
                                              "Offered but patient/parent/carer declined"))

dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No - not medically indicated", 
                                "No - reason not given"))
summary(dat$oral_steroids_rescue_history)

dat$oral_steroids_rescue_history <- factor(dat$oral_steroids_rescue_history, levels = c("Yes", "No", 
                                                                                        "Not recorded"))

summary(dat$referred_for_FU)
dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No - not medically indicated", 
                                                              "Not recorded", 
                              "Patient/parent/carer declined", "Already being seen in secondary care clinic"))



nrow(dat)
nrow(unique(dat)) # No duplicates


# Also, need to split steroids into 1-5 and 6+

summary(dat$steroids_admin)
summary(dat$steroids_admin_inc_pre_arrival_steroids)

dat %>% select(steroids_admin, steroids_admin_inc_pre_arrival_steroids) %>%
  table(useNA = "ifany")

dat %>% select(steroids_admin, steroids_pre_arrival) %>%
  table(useNA = "ifany")

dat %>% select(steroids_1hour, steroids_pre_arrival) %>% table(useNA = "ifany")

# Now we split it into 1-5 and 6+

dat <- dat %>% mutate(steroids_1hour_1_to_5_years = steroids_1hour,
                      steroids_1hour_6_plus_years = steroids_1hour)

dat$steroids_1hour_1_to_5_years[dat$age > 5] <- NA
dat$steroids_1hour_6_plus_years[dat$age < 6] <- NA

dat %>% select(steroids_1hour_1_to_5_years, age) %>% table(useNA = "ifany")
dat %>% select(steroids_1hour_6_plus_years, age) %>% table(useNA = "ifany")

summary(dat$steroids_1hour_1_to_5_years)


# and the same for the alt steroids:

dat <- dat %>% mutate(steroids_1hour_alt_1_to_5_years = steroids_1hour_alt,
                      steroids_1hour_alt_6_plus_years = steroids_1hour_alt)

dat$steroids_1hour_alt_1_to_5_years[dat$age > 5] <- NA
dat$steroids_1hour_alt_6_plus_years[dat$age < 6] <- NA

dat %>% select(steroids_1hour_alt_1_to_5_years, age) %>% table(useNA = "ifany")
dat %>% select(steroids_1hour_alt_6_plus_years, age) %>% table(useNA = "ifany")

summary(dat$steroids_1hour_alt_1_to_5_years)


# We need to create some asthma severity levels. Depends on age.
# Using BTS / SIGN guidelines 2009 Annex 8
# Cannot assess severity in children <2 years of age.
# (Note - is it strange that we don't have any children aged 0 in our dataset?
# Going to assume that everyone is treated according to the child guidelines, as this is a paediatric audit.
# However, worth pointing out that there are 14 18-year-olds.


# - - - - - - - - - - - - - -#
#  Defining asthma severity  #
# - - - - - - - - - - - - - -#

# More difficult because different definitions of severity according to the different ages.
# <2 not included

# For 2-5:
# Acute severe asthma
# . SpO2 <92%  <<<<<
# .                                    (Too breathless to talk or eat)
# . Heart rate >140/min  <<<<<<<
# . Respiratory rate >40/min  <<<<<<<
# .                                    (Use of accessory neck muscles)

# Life-threatening asthma
# SpO2 <92% plus any of:
#   . Silent chest
# . Poor respiratory effort
# . Agitation
# . Confusion
# . Cyanosis



# For 6+
# Acute severe asthma
# . SpO2 <92%  <<<<<<<<<<
# . PEF 33-50% best or predicted  <<<<<<<<<
# . Heart rate >125/min   <<<<<<<
# . Respiratory rate >30/min   <<<<<<<<<
# .                                       (Use of accessory neck muscles)


# Life-threatening asthma
# SpO2 <92% plus any of:
#   . PEF <33% best or predicted
# . Silent chest
# . Poor respiratory effort
# . Confusion
# . Cyanosis



# So, we use oxygen saturation, heart rate, respiratory rate, and PEF if >5

# For oxygen saturation, it's the same for all ages (apart from <2)

# Also applying Jenni's criteria of heart rate < 30 or resp rate < 10
# (Note - this changes 4-5 people into the severe asthma group)


table(dat$oxygen_sat92, useNA = "ifany")

dat <- dat %>% mutate(oxygen_sat_sev = fct_recode(oxygen_sat92, Normal = ">=92",
                                                  Low = "<92"))

dat$oxygen_sat_sev[dat$age < 2] <- NA

table(dat$oxygen_sat_sev, useNA = "ifany")


# For PEF it's quite straight forward as those under 6 didn't get any PEF

dat %>% select(PEF_init_recorded, age) %>% table()



# Don't need to worry about floating point rounding errors for this 
# But can't use cut because of the definitions the left and the right are both < and >. 

dat <- dat %>% mutate(PEF_percent_pred_sev = NA)


dat$PEF_percent_pred_sev[dat$PEF_percent_pred > 50] <- "Moderate"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred <= 50] <- "Severe and Life-threatening"
dat$PEF_percent_pred_sev[dat$PEF_init_recorded == "Patient too unwell"] <- "Severe and Life-threatening"
dat$PEF_percent_pred_sev <- factor(dat$PEF_percent_pred_sev)

summary(dat$PEF_percent_pred_sev) # All adds up

# Resp rate rate
# Let's create it in the old school way - probably easier

dat <- dat %>% mutate(resp_rate_sev = NA)

# For age 2-5


# resp rate > 40 for ages 2-5

dat$resp_rate_sev[dat$resp_rate > 40 & dat$age %in% (2:5)] <- "High"
dat$resp_rate_sev[dat$resp_rate <= 40 & dat$age %in% (2:5)] <- "Normal"

# resp rate > 30 for 6+

dat$resp_rate_sev[dat$resp_rate > 30 & dat$age %in% (6:18)] <- "High"
dat$resp_rate_sev[dat$resp_rate <= 30 & dat$age %in% (6:18)] <- "Normal"

# Resp rate < 10 for all is low
dat$resp_rate_sev[dat$resp_rate < 10 & dat$age %in% (2:18)] <- "Low"

dat$resp_rate_sev <- factor(dat$resp_rate_sev, levels = c("Low", "Normal", "High"))

dat %>% select(resp_rate_sev, age) %>% table(useNA = "ifany")
dat %>% select(resp_rate_sev, resp_rate) %>% table(useNA = "ifany")


# Heart rate

dat <- dat %>% mutate(heart_rate_sev = NA)

# Heart rate >140 in those aged 2-5

dat$heart_rate_sev[dat$heart_rate > 140 & dat$age %in% (2:5)] <- "High"
dat$heart_rate_sev[dat$heart_rate <= 140 & dat$age %in% (2:5)] <- "Normal"

# Heart rate >125 in those aged 6+

dat$heart_rate_sev[dat$heart_rate > 125 & dat$age %in% (6:18)] <- "High"
dat$heart_rate_sev[dat$heart_rate <= 125 & dat$age %in% (6:18)] <- "Normal"

# Heart rate < 30
dat$heart_rate_sev[dat$heart_rate < 30 & dat$age %in% (2:18)] <- "Low"

dat$heart_rate_sev <- factor(dat$heart_rate_sev, levels = c("Low", "Normal", "High"))


dat %>% select(heart_rate_sev, age) %>% table(useNA = "ifany")
dat %>% select(heart_rate_sev, heart_rate) %>% table(useNA = "ifany")


# I'm just going to define asthma severity as one thing now, which doesn't account for whether a measurement it missing.


dat$asthma_sev <- "Moderate"
dat$asthma_sev[dat$age < 2] <- NA
dat$asthma_sev[dat$oxygen_sat_sev == "Low"] <- "Severe and Life-threatening"
dat$asthma_sev[dat$resp_rate_sev == "High"] <- "Severe and Life-threatening"
dat$asthma_sev[dat$resp_rate_sev == "Low"] <- "Severe and Life-threatening"
dat$asthma_sev[dat$heart_rate_sev == "High"] <- "Severe and Life-threatening"
dat$asthma_sev[dat$heart_rate_sev == "Low"] <- "Severe and Life-threatening"
dat$asthma_sev[dat$PEF_percent_pred_sev == "Severe and Life-threatening"] <- "Severe and Life-threatening"
dat$asthma_sev <- factor(dat$asthma_sev, levels = c("Moderate", "Severe and Life-threatening"))



dat <- dat %>% filter(country != "Scotland")

# saveRDS(dat, "Z:/Group_work/Alex/Encrypted/Alex/Child Asthma/data/tidyData/CA_SCC_2019_clean_data_2020-09-23.RDS")

