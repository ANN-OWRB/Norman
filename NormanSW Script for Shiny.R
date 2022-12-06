#install all packages if not already installed 
install.packages("readxl")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("dplyr")

#download the packages that are installed
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)
library(magrittr)

# Original Import 
# Import raw dowloaded dataset from AWQMS
# master <- read_excel("C:\\Users\\353937\\OneDrive - State of Oklahoma\\R\\NormanSW\\NormanSW\\raw2022122.xlsx", 
#                  col_types = c("text", "text", "text", 
#                                      "text", "text", "numeric", "numeric", 
#                                      "text", "text", "date", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "text", "numeric", "text", 
#                                      "text", "text", "text", "text", "text", 
#                                      "text", "text", "skip", "skip", "skip", 
#                                      "skip", "skip", "text", "text", "numeric", 
#                                      "text", "skip", "skip", "skip", "text", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "text", "text", "text", "numeric", 
#                                      "numeric"), skip = 1)
#-----
# Import raw dowloaded dataset from AWQMS
master <- read_excel("C:\\Users\\353937\\OneDrive - State of Oklahoma\\R\\NormanSW\\NormanSW\\rawabbv2022122.xlsx" ,
                  col_types = c("text", "text", "text", 
                                      "text", "text", "numeric", "numeric", 
                                      "text", "text", "date", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "skip", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "skip", "skip", "skip", 
                                      "skip", "skip", "text", "text", "numeric", 
                                      "text", "skip", "skip", "skip", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "text", "text", "text", "numeric", 
                                      "numeric"), skip = 1)


#RDS conversion
#saveRDS(master,"C:master.rds")
saveRDS(master,"C:/Users/353937/OneDrive - State of Oklahoma/R/NormanSW/NormanSW/rawabbv2022122.rds")

# Convert non-detects to 1/2 the detection limit, and copy over detection limit maximums
master$`Result Value` <- as.numeric(master$`Result Value`)
master$`Detection Limit Value1` <- as.numeric(master$`Detection Limit Value1`)
master<- master %>%
  mutate(`Result Value` = case_when((is.na(`Result Value`) & `Detection Condition` == "Present Below Quantification Limit" ~ `Detection Limit Value1`/2),
                                    is.na(`Result Value`) & `Detection Condition` == "Present Above Quantification Limit" ~ `Detection Limit Value1`,
                                    TRUE ~ `Result Value`))

#TsS has both calculated values and multiprobe values; remove multiprobe values
TSS_subset <- subset(master, master$`Characteristic Name` == "Total suspended solids")
TSS_subset <- subset(TSS_subset, TSS_subset$`Result Value Type` == "Actual")
raw <- rbind(TSS_subset, subset(master, master$`Characteristic Name` != "Total suspended solids"))

#Rename columns to remove spaces, prepare for new dataset. Functions make new columns
raw$Site_Name <- gsub(".*\\, ","",raw$'Monitoring Location Name')
raw$Date <- raw$'Activity Start Date'
raw$ID <- raw$'Monitoring Location ID'
raw$Latitude <- raw$'Monitoring Location Latitude'
raw$Longitude <- raw$'Monitoring Location Longitude'
raw$Constituent <- raw$'Characteristic Name'
raw$Value <- raw$'Result Value'
raw$Unit <- raw$'Result Unit'
raw$Detection <- raw$'Detection Condition'
raw$Detection_Value <- raw$'Detection Limit Value1'

# Make new tibble based on renamed values in 70-79
raw_working <- raw[,c(37,38,39,40,41,42,43,44,45,46)]

# Rename constituents to add units
# raw_working <- raw_working %>%
raw_working<-raw_working%>%
  mutate(Constituent = case_when(Constituent== "pH" ~ "pH",
                               Constituent== "Total Nitrogen, mixed forms" ~ "TN_mgL",
                               Constituent== "Total hardness" ~ "Hardness_mgL",
                               Constituent== "Phosphorus" ~ "TP_mgL",
                               Constituent== "Total suspended solids" ~ "TsS_mgL",
                               Constituent== "Turbidity" ~ "Turbidity_NTU",
                               Constituent== "Dissolved oxygen (DO)" ~ "DO_mgL",
                               Constituent== "Specific conductance" ~ "SpC_uScm",
                               Constituent== "Temperature, water" ~ "Temp_C",
                               Constituent== "Kjeldahl nitrogen" ~ "Kjel_N_mgL" ,
                               Constituent== "Nitrate + Nitrite" ~ "NO2NO3_mgL" ,
                               TRUE ~ Constituent
)) %>%
  

# Select columns of interest
# raw_working <- raw_working %>%
  select(Site_Name, Date, Constituent, Value)

# There are a few issues with turbidity; let's force those to a number that makes sense (either 1 or 1000, nothing outside)
raw_working<-raw_working%>%
  mutate(Value = case_when(Value < 1 & Constituent == "Turbidity_NTU" ~ 1,
                           Value > 1000 & Constituent == "Turbidity_NTU" ~ 1000,
                           TRUE ~ Value)) %>%
  
# Group data for summarise function
# raw_working <- raw_working %>%  
  group_by(Site_Name, Date, Constituent, Value) %>%
  summarise(Value = mean(Value, na.rm = TRUE))

# Check for which rows are equal for site, date, and constituent so we don't have errors in the wide set
raw_working$fixer <- (duplicated(raw_working[,1:3]))
raw_working <- subset(raw_working, fixer != TRUE)

#make rows into columns based on values in "Constituent"
raw_wide <- raw_working %>%
  pivot_wider(names_from = "Constituent", values_from = "Value")

#Make new tibble based on columns we want to keep.  
raw_wide <- raw_wide %>%
  select(Site_Name, Date, Temp_C, 
         DO_mgL, Kjel_N, NO2NO3, pH,
         SpC_uScm, TN_mgL, TP_mgL, TsS_mgL,
         Turbidity_NTU)
#saveRDS
saveRDS(raw_wide, "C:/Users/353937/OneDrive - State of Oklahoma/R/NormanSW/NormanSW/NORMANPOR.rds" )




