#Install package
#install.packages("data.table")
#install.packages("forestplot")

#Load package
library(data.table)
library(dplyr)
library(forestplot)

#STEP 1 - Look at inclusion and exclusion criteria
#Open the data and make sure missing data is imported accordingly In the case of observation_Periods
observation_table <- fread("OBSERVATION_PERIODS.csv")
observation_table <- observation_table[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))]

#Study period is set on 01/01/2017 - 31/12/2020
#Filter data on inclusion and exclusion criteria
#1. Have at least 1 year of follow up before start of study date, hence 2016/01/01
OP_followup <- observation_table[op_start_date<20160101]

#2. Have at least 1 day of follow up after start of study date
minIday_OP_followup <- OP_followup[op_end_date>20170101]

#Print the remaining number of observations
cat('The original dataset contained', nrow(observation_table), 'observations')
cat('After selection on inclusion criteria', nrow(minIday_OP_followup), 'observations remained')

#STEP 2 - Age and age_bands
#Open the data of person and make sure missing data is imported accordingly.
persons_table <- fread("PERSONS.CSV")
persons_table <- persons_table[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))]
persons_table$birth_date <- gsub("\\s", "", paste(persons_table$year_of_birth,persons_table$month_of_birth, persons_table$day_of_birth))

#Create column which indicates the age of the person
persons_table$age <- 2016- as.numeric(persons_table$year_of_birth)

#Create age bands
persons_table[age < 160, ageband := "80+"]
persons_table[age < 81, ageband := "61-80"]
persons_table[age < 61, ageband := "41-60"]
persons_table[age < 41, ageband := "21-40"]
persons_table[age < 21, ageband := "0-20"]

#STEP 3 - Merge
#Merge the data tables OBSERVATION_PERIODS and PERSON on person_id
#Keep only individuals who are in OBSERVATION_PERIODS by all.y=T
observed_persons <- merge(x=persons_table, y=minIday_OP_followup, by = 'person_id', all.y = T) 

#Calculate the number of persons lost to follow up
cat('Total number of unique persons within dataset', length(unique(observed_persons$person_id)))
cat('Number of persons lost to follow up', sum(observed_persons$op_end_date < 20210000))

#STEP 4 - Create Concept sets
#Create a data table with the Diagnosis event codes and vocabulary
Vocabulary = data.table(
  Diagnosis = c("GBS", "GBS", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible"),
  event_code = c("357.0", "G61.0", "999.4", "T80.5", "T88.6", "T78.00", "995.60", "T78.05", "995.64", "T78.0", "995.5", "995.6", "T78.01", "995.61", "995.62", "T78.04", "995.63", "995.65", "T78.06", "995.66", "995.67", "T78.08", "995.68", "T78.02", "T78.03", "T78.07", "T78.09", "T78.2XXA", "T78.2XXD", "T78.2XXS", "995.0", "995.69", "T78.3", "995.1", "R23.0", "782.5", "R60.9", "782.3", "T78.4", "T78.40", "I95", "I95.9", "458", "458.9", "R57.9", "785.50", "L50.0", "708.0", "995.3", "995.4", "995.27", "995.2"),
  vocabulary = c("ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD9", "ICD9", "ICD9")
)

#STEP 5 - Load the EVENTS table
#Load EVENTS table from .csv file
events_table <- fread("EVENTS.csv")

#Merge EVENT table with the Vocabulary data on event_code
merge_events <- merge(x = events_table, y = Vocabulary, by = 'event_code', all.x = T)

#Count the number of different diagnoses
diagnosis_count <- merge_events[, .N, by=.(Diagnosis)] 

#Subset the events to be within the time frame of the study period
merge_events <- merge_events[start_date_record>20169999]
merge_events <- merge_events[end_date_record<20210000]

#Remove repeated events, only 1 event for the same individual 
merge_events_order<-merge_events[order(person_id, start_date_record),]
merge_events_unique<-merge_events_order[!duplicated(merge_events_order$person_id),]

#Print the number of deleted events
cat('The original dataset contained', nrow(merge_events), 'events within study period')
cat('The new dataset contains', nrow(merge_events_unique), 'when only 1 event per individual')

#STEP 6 - Censoring
#Fix the end of observation date to the censored date if beyond study period
observed_persons$end_date <- replace(observed_persons$op_end_date, observed_persons$op_end_date > 20201231, '20201231')

#Fix the start of observation date to the censored date if before study period
observed_persons$start_date <- replace(observed_persons$op_start_date, observed_persons$op_start_date < 20170101, '20170101')

#STEP 7 - Merge the 2 data sets
data <- merge(x = observed_persons, y = merge_events_unique, by = 'person_id', all.x = T)

#Create column end_date which is the date of event (start_date_record) 
#If no event, end_date is the op_end_date
data$end_date <- ifelse(!is.na(data$Diagnosis), data$start_date_record, data$end_date)

#Convert dates to date data type
data$end_date <- as.Date(data[["end_date"]], "%Y%m%d")
data$start_date <- as.Date(data[['start_date']], "%Y%m%d")

#Calculate the total person years
data$years <- as.numeric(((data$end_date - data$start_date)/365))

#STEP 8 - Calculation of Incidence rates overall
#Create function that calculates the IR for each diagnosis
IR_overall <- function(diagnosis, formula, full_name) {
  cases = length(which(formula))
  years = sum(data$years)
  IR_rate = cases/years
  IR_rate_1000 = IR_rate * 1000
  CI_low = IR_rate - (1.96 * IR_rate / sqrt(cases))
  CI_high = IR_rate + (1.96 * IR_rate / sqrt(cases))
  IR_overall_table <- data.table(
    Diagnosis = c(full_name),
    Cases = c(cases),
    Years = c(years),
    IR = c(IR_rate),
    IR_CI_low = c(CI_low),
    IR_CI_high = c(CI_high),
    IR_per_1000 = c(IR_rate_1000)
  )
  return(IR_overall_table)
}

#Create table that combines calculations for each diagnosis
gbs = IR_overall('GBS', data$Diagnosis== 'GBS', 'Guillain Barre Syndrome')
ana_narrow = IR_overall('narrow', data$Diagnosis== 'narrow', 'Anaphylaxis Narrow')
ana_broad = IR_overall('possible', data$Diagnosis== 'narrow' | data$Diagnosis== 'possible', 'Anaphylaxis Broad')

#Combine the calculation tables 
table_overall_IR <- rbind(gbs, ana_narrow, ana_broad)

#Create a forest plot 
forestplot(matrix(table_overall_IR$Diagnosis), table_overall_IR$IR, table_overall_IR$IR_CI_low, table_overall_IR$IR_CI_high, align = 'l', xlab = 'Incidence Rate', zero=1)


#STEP 9 - Calculation of Incidence rates by year by age bands
#Function that subsets data based on age_band
select_age <- function(age_criteria) {
  data = age[age$ageband == age_criteria,]
  age_gbs = length(which(data$Diagnosis=='GBS'))
  age_ana_narrow = length(which(data$Diagnosis=='narrow'))
  age_ana_broad = length(which(data$Diagnosis=='possible')) + age_ana_narrow
  age_years = sum(data$years)
  
  age_gbs_overall = age_gbs/age_years
  age_gbs_overall_1000 = age_gbs_overall * 1000
  age_gbs_CI_low_n = age_gbs_overall - (1.96 * age_gbs_overall / sqrt(age_gbs))
  #CI cannot be negative so replace with zero if negative
  if (age_gbs_CI_low_n <0) {
    age_gbs_CI_low = 0
  } else {
    age_gbs_CI_low = age_gbs_CI_low_n
  }
  age_gbs_CI_high = age_gbs_overall + (1.96 * age_gbs_overall / sqrt(age_gbs))
  
  age_ana_narrow_overall = age_ana_narrow/age_years
  age_ana_narrow_overall_1000 = age_ana_narrow_overall * 1000
  age_ana_narrow_CI_low_n = age_ana_narrow_overall - (1.96 * age_ana_narrow_overall / sqrt(age_ana_narrow))
  #CI cannot be negative so replace with zero if negative
  if (age_ana_narrow_CI_low_n <0) {
    age_ana_narrow_CI_low = 0
  } else {
    age_ana_narrow_CI_low = age_ana_narrow_CI_low_n
  }
  age_ana_narrow_CI_high = age_ana_narrow_overall + (1.96 * age_ana_narrow_overall / sqrt(age_ana_narrow))
  
  age_ana_broad_overall = age_ana_broad/age_years
  age_ana_broad_overall_1000 = age_ana_broad_overall * 1000
  age_ana_broad_CI_low_n = age_ana_broad_overall - (1.96 * age_ana_broad_overall / sqrt(age_ana_broad))
  #CI cannot be negative so replace with zero if negative
  if (age_ana_broad_CI_low_n <0) {
    age_ana_broad_CI_low = 0
  } else {
    age_ana_broad_CI_low = age_ana_broad_CI_low_n
  }
  age_ana_broad_CI_high = age_ana_broad_overall + (1.96 * age_ana_broad_overall / sqrt(age_ana_broad))
  
  #Create table that combines calculation for the 3 diagnosis
  list = c(age_gbs_overall, age_ana_narrow_overall, age_ana_broad_overall)
  table = data.table(
    Diagnosis = c("Guillain Barre Syndrome", "Anaphylaxis Narrow", "Anaphylaxis Broad"),
    Age_Bands = c(age_criteria, age_criteria, age_criteria),
    Cases = c(age_gbs, age_ana_narrow, age_ana_broad),
    IR = c(age_gbs_overall, age_ana_narrow_overall, age_ana_broad_overall),
    IR_per_1000 = c(age_gbs_overall_1000, age_ana_narrow_overall_1000, age_ana_broad_overall_1000),
    CI_low = c(age_gbs_CI_low, age_ana_narrow_CI_low, age_ana_broad_CI_low),
    CI_high = c(age_gbs_CI_high, age_ana_narrow_CI_high, age_ana_broad_CI_high)
  )
  return(table)
}

#Run function for each age band
age0_20 <- select_age('0-20')
age21_40 <- select_age('21-40')
age41_60 <- select_age('41-60')
age61_80 <- select_age('61-80')
#age80 <- select_age('80+')

#Combine the results 
age_table <- rbind(age0_20, age21_40, age41_60, age61_80)

#Subset on diagnosis
GBS_forest <- age_table[age_table$Diagnosis == 'Guillain Barre Syndrome',]
ANA_N_forest <- age_table[age_table$Diagnosis == 'Anaphylaxis Narrow',]
ANA_B_forest <- age_table[age_table$Diagnosis == 'Anaphylaxis Broad',]

#Create forest plot for each age band for each diagnosis
forestplot(matrix(GBS_forest$Age_Bands), GBS_forest$IR, GBS_forest$CI_low, 
           GBS_forest$CI_high, align = 'l', xlab = 'Incidence Rate', zero = 1,
           title= 'Icidence Rate for Guillain Barre Syndrome per age band',
           grid=TRUE)

forestplot(matrix(ANA_N_forest$Age_Bands), ANA_N_forest$IR, ANA_N_forest$CI_low,
           ANA_N_forest$CI_high, align = 'l', xlab = 'Incidence Rate', zero = 1,
           title= 'Icidence Rate for Anaphylaxis Narrow per age band')

forestplot(matrix(ANA_B_forest$Age_Bands), ANA_B_forest$IR, ANA_B_forest$CI_low,
           ANA_B_forest$CI_high, align = 'l', xlab = 'Incidence Rate', zero = 1,
           title= 'Icidence Rate for Anaphylaxis Broad per age band')

observed_persons <- merge(x=persons_table, y=minIday_OP_followup, by = 'person_id', all.y = T) 
observed_persons$end_date <- replace(observed_persons$op_end_date, observed_persons$op_end_date > 20201231, '20201231')
observed_persons$start_date <- replace(observed_persons$op_start_date, observed_persons$op_start_date < 20170101, '20170101')
data <- merge(x = observed_persons, y = merge_events_unique, by = 'person_id', all.x = T)
data$end_date <- ifelse(!is.na(data$Diagnosis), data$start_date_record, data$end_date)

#Create a function that subsets data on year based on observation time when diagnosed with one of the two
year_select <- function(data, data_start, data_end) {
  data_na = data[is.na(data$start_date_record),]
  data_year = data[data$start_date_record > data_start & data$start_date_record < data_end,]
  data_combined = rbind(data_na, data_year)
  data_combined_end = data_combined[data_combined$op_end_date > data_start]
  data_full = data_combined_end
  data_full$end_date <- ifelse(!is.na(data_full$Diagnosis), data_full$start_date_record, data_full$end_date)
  data_full$end_date <- ifelse(data_full$end_date > data_end, data_end, data_full$end_date)
  data_full$start_date <- data_start
  data_full$end_date <- as.Date(data_full[["end_date"]], "%Y%m%d")
  data_full$start_date <- as.Date(data_full[['start_date']], "%Y%m%d")
  data_full$years <- as.numeric(((data_full$end_date - data_full$start_date)/365))
  return(data_full)
}

#Run year select function for the four years within the study period
data_2017 <- year_select(data, 20170101, 20171231)
data_2018 <- year_select(data, 20171231, 20181231)
data_2019 <- year_select(data, 20181231, 20191231)
data_2020 <- year_select(data, 20191231, 20201231)

IR_year <- function(diagnosis, formula, full_name, data) {
  cases = length(which(formula))
  years = sum(data$years)
  IR_rate = cases/years
  IR_rate_1000 = IR_rate * 1000
  CI_low_n = IR_rate - (1.96 * IR_rate / sqrt(cases))
  if (CI_low_n <0 | is.na(CI_low_n)) {
    CI_low = 0
  } else {
    CI_low = CI_low_n
  }
  CI_high_n = IR_rate + (1.96 * IR_rate / sqrt(cases))
  if (is.na(CI_high_n)) {
    CI_high = 0
  } else {
    CI_high = CI_high_n
  }
  IR_overall_table <- data.table(
    Diagnosis = c(full_name),
    Cases = c(cases),
    Person_years = c(years),
    IR = c(IR_rate),
    IR_CI_low = c(CI_low),
    IR_CI_high = c(CI_high),
    IR_per_1000 = c(IR_rate_1000)
  )
  return(IR_overall_table)
}

#Create table that combines calculations for each diagnosis
gbs_2017 = IR_year('GBS', data_2017$Diagnosis== 'GBS', 'Guillain Barre Syndrome', data_2017)
ana_narrow_2017 = IR_year('narrow', data_2017$Diagnosis== 'narrow', 'Anaphylaxis Narrow', data_2017)
ana_broad_2017 = IR_year('possible', data_2017$Diagnosis== 'narrow' | data_2017$Diagnosis== 'possible', 'Anaphylaxis Broad', data_2017)

gbs_2018 = IR_year('GBS', data_2018$Diagnosis== 'GBS', 'Guillain Barre Syndrome', data_2018)
ana_narrow_2018 = IR_year('narrow', data_2018$Diagnosis== 'narrow', 'Anaphylaxis Narrow', data_2018)
ana_broad_2018 = IR_year('possible', data_2018$Diagnosis== 'narrow' | data_2018$Diagnosis== 'possible', 'Anaphylaxis Broad', data_2018)

gbs_2019 = IR_year('GBS', data_2019$Diagnosis== 'GBS', 'Guillain Barre Syndrome', data_2019)
ana_narrow_2019 = IR_year('narrow', data_2019$Diagnosis== 'narrow', 'Anaphylaxis Narrow', data_2019)
ana_broad_2019 = IR_year('possible', data_2019$Diagnosis== 'narrow' | data_2019$Diagnosis== 'possible', 'Anaphylaxis Broad', data_2019)

gbs_2020 = IR_year('GBS', data_2020$Diagnosis== 'GBS', 'Guillain Barre Syndrome', data_2020)
ana_narrow_2020 = IR_year('narrow', data_2020$Diagnosis== 'narrow', 'Anaphylaxis Narrow', data_2020)
ana_broad_2020 = IR_year('possible', data_2020$Diagnosis== 'narrow' | data_2020$Diagnosis== 'possible', 'Anaphylaxis Broad', data_2020)

#Combine the calculation tables 
table_IR_2017 <- rbind(gbs_2017, ana_narrow_2017, ana_broad_2017)
table_IR_2017$year <- 2017
table_IR_2018 <- rbind(gbs_2018, ana_narrow_2018, ana_broad_2018)
table_IR_2018$year <- 2018
table_IR_2019 <- rbind(gbs_2019, ana_narrow_2019, ana_broad_2019)
table_IR_2019$year <- 2019
table_IR_2020 <- rbind(gbs_2020, ana_narrow_2020, ana_broad_2020)
table_IR_2020$year <- 2020
table_per_year <- rbind(table_IR_2017, table_IR_2018, table_IR_2019, table_IR_2020)
write.csv(table_per_year, 'table.csv')
#Create a forest plot 
forestplot(matrix(table_IR_2017$Diagnosis), table_IR_2017$IR, table_IR_2017$IR_CI_low, table_IR_2017$IR_CI_high, align = 'l', xlab = 'Incidence Rate', zero=1)
forestplot(matrix(table_IR_2018$Diagnosis), table_IR_2018$IR, table_IR_2018$IR_CI_low, table_IR_2018$IR_CI_high, align = 'l', xlab = 'Incidence Rate', zero=1)
forestplot(matrix(table_IR_2019$Diagnosis), table_IR_2019$IR, table_IR_2019$IR_CI_low, table_IR_2019$IR_CI_high, align = 'l', xlab = 'Incidence Rate', zero=1)
forestplot(matrix(table_IR_2020$Diagnosis), table_IR_2020$IR, table_IR_2020$IR_CI_low, table_IR_2020$IR_CI_high, align = 'l', xlab = 'Incidence Rate', zero=1)
