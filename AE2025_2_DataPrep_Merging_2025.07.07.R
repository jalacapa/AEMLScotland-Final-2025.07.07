####################################################
# --- R CODE 2: DATA PREPARATION AND DATA MERGING (WIDE) ---
####################################################

# ==================================================
# SETUP: Package Installations
# ==================================================

## Install required packages
packages <- c("readxl", "tidyr", "dplyr", "purrr", "mice")
installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

## Load the libraries
library(readxl)
library(tidyr)
library(dplyr)
library(purrr)
library(mice)

# ==================================================
# SETUP: Working Directory and Datasets
# ==================================================

# Set working directory
setwd("/cloud/project")
if (!dir.exists("outputs")) dir.create("outputs")

## Load files
monthlyAE<- read.csv("2025-05-06-ae-monthly-attendance-and-waiting-times-data.csv")
ageGroup <- read_excel("2025-05-06-whoattends-agegroup.xlsx", sheet = "Health Board")
dayOfWeek <- read_excel("2025-05-06-whenpeopleattend-dayofweek.xlsx", sheet = "Health Board")
deprivation <- read_excel("2025-05-06-whoattends-deprivation.xlsx", sheet = "Health Board")
dischargeDestination <- read_excel("2025-05-06-dischargedestination.xlsx", sheet = "Health Board")
arrivalHour <- read_excel("2025-05-06-whenpeopleattend-arrivalhour.xlsx", sheet = "Health Board")
inOut <- read_excel("2025-05-06-whenpeopleattend-inoutofhours.xlsx", sheet = "Health Board")
referralSource <- read_excel("2025-05-06-referralsource.xlsx", sheet = "Health Board")
sex <- read_excel("2025-05-06-whoattends-sex.xlsx", sheet = "Health Board")

# ==================================================
# Data Cleaning: Age Group (ageGroup)
# ==================================================

## Recode healthboards
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Borders"] <- "B"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Fife"] <- "F"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Forth Valley"] <- "V"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Grampian"] <- "N"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Highland"] <- "H"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Lanarkshire"] <- "L"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Lothian"] <- "S"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Orkney"] <- "R"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Shetland"] <- "Z"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Tayside"] <- "T"
ageGroup$HealthBoard[ageGroup$HealthBoard == "NHS Western Isles"] <- "W"

## Drop Type and Rate columns
ageGroup$Type <- NULL
ageGroup$`Rate/100,000` <- NULL

## Aggregate attendances per healthboard
ageGroup <- as.data.frame(ageGroup)
ageGroup <- aggregate(Attendances ~ HealthBoard + Month + Age, data = ageGroup, sum)

## Transform month variable
ageGroup$Month <- format(as.Date(ageGroup$Month), "%Y_%m")

## Create unique id
ageGroup$uniqueID <- paste(ageGroup$HealthBoard, ageGroup$Month, sep = "_")
ageGroup$HealthBoard <- NULL
ageGroup$Month <- NULL

## Move uniqueID to the beginning
ageGroup <- ageGroup[c("uniqueID", "Age", "Attendances")]

## Create wide table
ageGroup <- pivot_wider(ageGroup,names_from = Age,values_from = Attendances, names_prefix = "age_")

## Reorder columns
ageGroup <- ageGroup[c("uniqueID", "age_Under 18", "age_18-24", "age_25-39", "age_40-64", "age_65-74",
                       "age_75 plus", "age_Unknown")]

# ==================================================
# Data Cleaning: Day of the Week (dayOfWeek)
# ==================================================

## Recode healthboards
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Borders"] <- "B"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Fife"] <- "F"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Forth Valley"] <- "V"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Grampian"] <- "N"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Highland"] <- "H"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Lanarkshire"] <- "L"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Lothian"] <- "S"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Orkney"] <- "R"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Shetland"] <- "Z"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Tayside"] <- "T"
dayOfWeek$HealthBoard[dayOfWeek$HealthBoard == "NHS Western Isles"] <- "W"

## Drop Type and Rate columns
dayOfWeek$Type <- NULL

## Aggregate attendances per healthboard
dayOfWeek <- as.data.frame(dayOfWeek)
dayOfWeek <- aggregate(Average ~ HealthBoard + Month + Day, data = dayOfWeek, sum)

## Transform month variable
dayOfWeek$Month <- format(as.Date(dayOfWeek$Month), "%Y_%m")

## Create unique id
dayOfWeek$uniqueID <- paste(dayOfWeek$HealthBoard, dayOfWeek$Month, sep = "_")
dayOfWeek$HealthBoard <- NULL
dayOfWeek$Month <- NULL

## Move uniqueID to the beginning
dayOfWeek <- dayOfWeek[c("uniqueID", "Day", "Average")]

## Rename Average column to Attendances
dayOfWeek  <- dayOfWeek  %>% rename(Attendances = Average)

## Create wide table
dayOfWeek <- pivot_wider(dayOfWeek,names_from = Day,values_from = Attendances, names_prefix = "day_")

## Reorder columns
dayOfWeek <- dayOfWeek[c("uniqueID", "day_Monday", "day_Tuesday","day_Wednesday",
                         "day_Thursday","day_Friday", "day_Saturday","day_Sunday")]

# ==================================================
# Data Cleaning: SIMD Deprivation (deprivation)
# ==================================================

## Recode healthboards
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Borders"] <- "B"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Fife"] <- "F"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Forth Valley"] <- "V"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Grampian"] <- "N"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Highland"] <- "H"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Lanarkshire"] <- "L"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Lothian"] <- "S"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Orkney"] <- "R"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Shetland"] <- "Z"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Tayside"] <- "T"
deprivation$HealthBoard[deprivation$HealthBoard == "NHS Western Isles"] <- "W"

## Drop Type and Rate columns
deprivation$Type <- NULL
deprivation$`Rate/100,000` <- NULL

## Aggregate attendances per healthboard
deprivation <- as.data.frame(deprivation)
deprivation <- aggregate(Attendances ~ HealthBoard + Month + Deprivation, data = deprivation, sum)

## Transform month variable
deprivation$Month <- format(as.Date(deprivation$Month), "%Y_%m")

## Create unique id
deprivation$uniqueID <- paste(deprivation$HealthBoard, deprivation$Month, sep = "_")
deprivation$HealthBoard <- NULL
deprivation$Month <- NULL

## Move uniqueID to the beginning
deprivation <- deprivation[c("uniqueID", "Deprivation", "Attendances")]

## Create wide table
deprivation <- pivot_wider(deprivation,names_from = Deprivation,values_from = Attendances,
                           names_prefix = "deprivation_")

# ==================================================
# Data Cleaning: Discharge Destination (dischargeDestination)
# ==================================================

## Recode healthboards
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Borders"] <- "B"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Fife"] <- "F"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Forth Valley"] <- "V"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Grampian"] <- "N"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Highland"] <- "H"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Lanarkshire"] <- "L"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Lothian"] <- "S"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Orkney"] <- "R"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Shetland"] <- "Z"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Tayside"] <- "T"
dischargeDestination$HealthBoard[dischargeDestination$HealthBoard == "NHS Western Isles"] <- "W"

## Drop Type and Rate columns
dischargeDestination$Type <- NULL

## Aggregate attendances per healthboard
dischargeDestination <- as.data.frame(dischargeDestination)
dischargeDestination <- aggregate(Attendances ~ HealthBoard + Month + Discharge + Age, data = dischargeDestination, sum)

## Transform month variable
dischargeDestination$Month <- format(as.Date(dischargeDestination$Month), "%Y_%m")

## Create unique id
dischargeDestination$uniqueID <- paste(dischargeDestination$HealthBoard,
                                       dischargeDestination$Month, sep = "_")
dischargeDestination$HealthBoard <- NULL
dischargeDestination$Month <- NULL

## Create Destination-Age
dischargeDestination$destinationAge <- paste(dischargeDestination$Discharge,
                                             dischargeDestination$Age, sep = "-")
## Move uniqueID to the beginning
dischargeDestination <- dischargeDestination[c("uniqueID", "destinationAge", "Discharge", "Age",
                                               "Attendances")]

## Create wide table
wDischargeDestination <- dischargeDestination
wDischargeDestination$Discharge <- NULL
wDischargeDestination$Age <- NULL
wDischargeDestination <- pivot_wider(wDischargeDestination,names_from = destinationAge,
                                     values_from = Attendances, names_prefix = "dischargeAge_")

# ==================================================
# Data Cleaning: Arrival Hour (arrivalHour)
# ==================================================

## Recode healthboards
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Borders"] <- "B"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Fife"] <- "F"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Forth Valley"] <- "V"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Grampian"] <- "N"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Highland"] <- "H"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Lanarkshire"] <- "L"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Lothian"] <- "S"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Orkney"] <- "R"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Shetland"] <- "Z"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Tayside"] <- "T"
arrivalHour$HealthBoard[arrivalHour$HealthBoard == "NHS Western Isles"] <- "W"

## Recode Hour
arrivalHour$Hour <- gsub("^(\\d{2}):00 to \\d{2}:59$", "\\1", arrivalHour$Hour)

## Drop Type and Rate columns
arrivalHour$Type <- NULL

## Aggregate attendances per healthboard
arrivalHour <- as.data.frame(arrivalHour)
arrivalHour <- aggregate(Attendances ~ HealthBoard + Month + Hour + Week,
                         data = arrivalHour, sum)

## Transform month variable
arrivalHour$Month <- format(as.Date(arrivalHour$Month), "%Y_%m")

## Create unique id
arrivalHour$uniqueID <- paste(arrivalHour$HealthBoard, arrivalHour$Month, sep = "_")
arrivalHour$HealthBoard <- NULL
arrivalHour$Month <- NULL

## Create HourWeek before reordering
arrivalHour$HourWeek <- paste(arrivalHour$Hour, arrivalHour$Week, sep = "-")

## Move uniqueID to the beginning (include HourWeek)
arrivalHour <- arrivalHour[c("uniqueID", "Hour", "Week", "HourWeek", "Attendances")]

## Create wide table
wArrivalHour <- arrivalHour
wArrivalHour <- pivot_wider(wArrivalHour,
                            names_from = HourWeek,
                            values_from = Attendances,
                            names_prefix = "hourWeek_")

## Clean up raw columns
wArrivalHour$Hour <- NULL
wArrivalHour$Week <- NULL
# ==================================================
# Data Cleaning: In / Out of Office Hours (inOut)
# ==================================================

## Recode healthboards
inOut$HealthBoard[inOut$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
inOut$HealthBoard[inOut$HealthBoard == "NHS Borders"] <- "B"
inOut$HealthBoard[inOut$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
inOut$HealthBoard[inOut$HealthBoard == "NHS Fife"] <- "F"
inOut$HealthBoard[inOut$HealthBoard == "NHS Forth Valley"] <- "V"
inOut$HealthBoard[inOut$HealthBoard == "NHS Grampian"] <- "N"
inOut$HealthBoard[inOut$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
inOut$HealthBoard[inOut$HealthBoard == "NHS Highland"] <- "H"
inOut$HealthBoard[inOut$HealthBoard == "NHS Lanarkshire"] <- "L"
inOut$HealthBoard[inOut$HealthBoard == "NHS Lothian"] <- "S"
inOut$HealthBoard[inOut$HealthBoard == "NHS Orkney"] <- "R"
inOut$HealthBoard[inOut$HealthBoard == "NHS Shetland"] <- "Z"
inOut$HealthBoard[inOut$HealthBoard == "NHS Tayside"] <- "T"
inOut$HealthBoard[inOut$HealthBoard == "NHS Western Isles"] <- "W"

## Recode InOut
inOut$InOut[inOut$InOut == "In Hours"] <- "In"
inOut$InOut[inOut$InOut == "Out of Hours"] <- "Out"

## Drop Type and Rate columns
inOut$Type <- NULL

## Aggregate attendances per healthboard
inOut <- as.data.frame(inOut)
inOut <- aggregate(Attendances ~ HealthBoard + InOut + Month,
                   data = inOut, sum)

## Transform month variable
inOut$Month <- format(as.Date(inOut$Month), "%Y_%m")

## Create unique id
inOut$uniqueID <- paste(inOut$HealthBoard, inOut$Month, sep = "_")
inOut$HealthBoard <- NULL
inOut$Month <- NULL

## Move uniqueID to the beginning
inOut <- inOut[c("uniqueID", "InOut", "Attendances")]

## Create wide table
inOut <- pivot_wider(inOut,names_from = InOut,values_from = Attendances,
                     names_prefix = "InOut_")

# ==================================================
# Data Cleaning: Referral Source (referralSource)
# ==================================================

## Recode healthboards
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Borders"] <- "B"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Fife"] <- "F"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Forth Valley"] <- "V"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Grampian"] <- "N"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Highland"] <- "H"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Lanarkshire"] <- "L"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Lothian"] <- "S"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Orkney"] <- "R"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Shetland"] <- "Z"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Tayside"] <- "T"
referralSource$HealthBoard[referralSource$HealthBoard == "NHS Western Isles"] <- "W"

## Drop Type and Rate columns
referralSource$Type <- NULL

## Aggregate attendances per healthboard
referralSource <- as.data.frame(referralSource)
referralSource <- aggregate(Attendances ~ HealthBoard + Month + Referral + Age, data = referralSource, sum)

## Transform month variable
referralSource$Month <- format(as.Date(referralSource$Month), "%Y_%m")

## Ccreate unique id
referralSource$uniqueID <- paste(referralSource$HealthBoard,
                                 referralSource$Month, sep = "_")
referralSource$HealthBoard <- NULL
referralSource$Month <- NULL

## Create Referral-Age
referralSource$referralAge <- paste(referralSource$Referral,
                                    referralSource$Age, sep = "-")

## Move uniqueID to the beginning
referralSource <- referralSource[c("uniqueID", "referralAge", "Referral", "Age",
                                   "Attendances")]

##create wide table
wReferralSource <- referralSource
wReferralSource$Referral <- NULL
wReferralSource$Age <- NULL
wReferralSource <- pivot_wider(wReferralSource,names_from = referralAge,
                               values_from = Attendances, names_prefix = "referralAge_")
# ==================================================
# Data Cleaning: Sex (sex)
# ==================================================
##recode healthboards
sex$HealthBoard[sex$HealthBoard == "NHS Ayrshire & Arran"] <- "A"
sex$HealthBoard[sex$HealthBoard == "NHS Borders"] <- "B"
sex$HealthBoard[sex$HealthBoard == "NHS Dumfries & Galloway"] <- "Y"
sex$HealthBoard[sex$HealthBoard == "NHS Fife"] <- "F"
sex$HealthBoard[sex$HealthBoard == "NHS Forth Valley"] <- "V"
sex$HealthBoard[sex$HealthBoard == "NHS Grampian"] <- "N"
sex$HealthBoard[sex$HealthBoard == "NHS Greater Glasgow & Clyde"] <- "G"
sex$HealthBoard[sex$HealthBoard == "NHS Highland"] <- "H"
sex$HealthBoard[sex$HealthBoard == "NHS Lanarkshire"] <- "L"
sex$HealthBoard[sex$HealthBoard == "NHS Lothian"] <- "S"
sex$HealthBoard[sex$HealthBoard == "NHS Orkney"] <- "R"
sex$HealthBoard[sex$HealthBoard == "NHS Shetland"] <- "Z"
sex$HealthBoard[sex$HealthBoard == "NHS Tayside"] <- "T"
sex$HealthBoard[sex$HealthBoard == "NHS Western Isles"] <- "W"

##drop Type and Rate columns
sex$Type <- NULL
sex$`Rate/100,000` <- NULL

##aggregate attendances per healthboard
sex <- as.data.frame(sex)
sex <- aggregate(Attendances ~ HealthBoard + Month + Sex, data = sex, sum)

##transform month variable
sex$Month <- format(as.Date(sex$Month), "%Y_%m")

##create unique id
sex$uniqueID <- paste(sex$HealthBoard, sex$Month, sep = "_")
sex$HealthBoard <- NULL
sex$Month <- NULL

##move uniqueID to the beginning
sex <- sex[c("uniqueID", "Sex", "Attendances")]
##create wide table
sex <- pivot_wider(sex,names_from = Sex,values_from = Attendances, names_prefix = "sex_")

# ==================================================
# Data Cleaning: Monthly A&E Attendances (monthlyAE)
# ==================================================

## Recode healthboards (NHSBoardName)
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Ayrshire & Arran"] <- "A"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Borders"] <- "B"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Dumfries & Galloway"] <- "Y"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Fife"] <- "F"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Forth Valley"] <- "V"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Grampian"] <- "N"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Greater Glasgow & Clyde"] <- "G"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Highland"] <- "H"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Lanarkshire"] <- "L"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Lothian"] <- "S"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Orkney"] <- "R"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Shetland"] <- "Z"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Tayside"] <- "T"
monthlyAE$NHSBoardName[monthlyAE$NHSBoardName == "NHS Western Isles"] <- "W"

## Drop other columns
monthlyAE$LocationCode <- NULL
monthlyAE$LocationName <- NULL
monthlyAE$DepartmentType <- NULL
monthlyAE$NumberOfAttendancesAll <- NULL
monthlyAE$PercentageWithin4HoursAll <- NULL
monthlyAE$NumberOfAttendancesEpisode <- NULL
monthlyAE$NumberWithin4HoursEpisode <- NULL
monthlyAE$NumberOver4HoursEpisode <- NULL
monthlyAE$PercentageWithin4HoursEpisode <- NULL
monthlyAE$NumberOver8HoursEpisode <- NULL
monthlyAE$PercentageOver8HoursEpisode <- NULL
monthlyAE$NumberOver12HoursEpisode <- NULL
monthlyAE$PercentageOver12HoursEpisode <- NULL

## Aggregate attendances per healthboard
monthlyAE <- monthlyAE %>%
  group_by(NHSBoardName, MonthEndingDate) %>%
  summarise(
    adherence_0 = sum(NumberWithin4HoursAll, na.rm = TRUE),
    adherence_1   = sum(NumberOver4HoursAll, na.rm = TRUE),
    .groups = 'drop'
  )

## Transform month variable
monthlyAE$MonthEndingDate <- format(as.Date(monthlyAE$MonthEndingDate), "%Y_%m")

# ==================================================
# Creating a Unique ID (uniqueID)
# ==================================================

## Create unique id
monthlyAE$uniqueID <- paste(monthlyAE$NHSBoardName, monthlyAE$MonthEndingDate, sep = "_")
monthlyAE$NHSBoardName <- NULL
monthlyAE$MonthEndingDate <- NULL

## Move uniqueID to the beginning
monthlyAE <- monthlyAE[c("uniqueID", "adherence_0", "adherence_1")]

# ==================================================
# Creating the Final Merged Dataset
# ==================================================

## List dataframes
dataFrames <- list(monthlyAE, ageGroup, dayOfWeek, deprivation, wDischargeDestination,
                   wArrivalHour, inOut, wReferralSource, sex)

## Merged Table: Wide and Unmatched (Full Outer)
wideMonthlyAE <- reduce(dataFrames, full_join, by = "uniqueID")

## Limiting the dataset to January 2018 to December 2023
wideMonthlyAE <- wideMonthlyAE %>%
  separate(uniqueID, into = c("HealthBoard", "Year", "Month"), sep = "_")
wideMonthlyAE <- wideMonthlyAE %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )
wideMonthlyAE <- wideMonthlyAE %>%
  filter(Year >= 2018 & Year <= 2023)

# ==================================================
# Pandemic Coding following Teoh et al. (2024)
# ==================================================

## Ensure Year and Month are numeric
wideMonthlyAE$Year <- as.numeric(wideMonthlyAE$Year)
wideMonthlyAE$Month <- sprintf("%02d", as.numeric(wideMonthlyAE$Month))  # pad with 0 if needed

## Create YYYYMM numeric variable
wideMonthlyAE$year_month <- as.numeric(paste0(wideMonthlyAE$Year, wideMonthlyAE$Month))

## Create COVID phase coding as a new numeric variable
wideMonthlyAE$covid_phase <- with(wideMonthlyAE, case_when(
  year_month >= 201801 & year_month <= 202002 ~ 0,  # Pre-Pandemic
  year_month >= 202003 & year_month <= 202005 ~ 1,  # Peak 1
  year_month >= 202006 & year_month <= 202009 ~ 2,  # Adaptive 1
  year_month >= 202010 & year_month <= 202102 ~ 3,  # Peak 2 (Alpha)
  year_month >= 202103 & year_month <= 202105 ~ 4,  # Adaptive 2
  year_month >= 202106 & year_month <= 202109 ~ 5,  # Peak 3 (Delta)
  year_month >= 202110 & year_month <= 202111 ~ 6,  # Adaptive 3
  year_month >= 202112 & year_month <= 202202 ~ 7,  # Peak 4 (Omicron)
  year_month >= 202203 & year_month <= 202209 ~ 8,  # Post-Omicron Recovery
  year_month >= 202210 & year_month <= 202312 ~ 9,  # Endemic Transition
  TRUE ~ NA_real_
))

#==================================================
# Final Cleaning for Modeling
#==================================================

## Ensure adherence_0 and adherence_1 are not NA
wideMonthlyAE <- wideMonthlyAE %>%
  filter(!is.na(adherence_0), !is.na(adherence_1))

## Replace NA in predictors with 0 (assume no attendances reported): Identify predictors (exclude identifiers and targets)
predictor_vars <- setdiff(names(wideMonthlyAE), c("HealthBoard", "Year", "Month", "adherence_0", "adherence_1", "year_month", "covid_phase"))
wideMonthlyAE[predictor_vars] <- lapply(wideMonthlyAE[predictor_vars], function(x) ifelse(is.na(x), 0, x))

## Create binary target variable: 1 if at least 95% seen within 4 hours, 0 otherwise
wideMonthlyAE <- wideMonthlyAE %>%
  mutate(
    total_attendances = adherence_0 + adherence_1,
    adherence_binary = ifelse(adherence_0 / total_attendances >= 0.95, 1, 0)
  ) %>%
  select(-total_attendances)  # drop temp col

## Ensure correct data types
wideMonthlyAE$HealthBoard <- as.factor(wideMonthlyAE$HealthBoard)
wideMonthlyAE$Year <- as.integer(wideMonthlyAE$Year)
wideMonthlyAE$Month <- as.integer(wideMonthlyAE$Month)
wideMonthlyAE$covid_phase <- as.factor(wideMonthlyAE$covid_phase)
wideMonthlyAE$adherence_binary <- as.integer(wideMonthlyAE$adherence_binary)

#==================================================
# Export Tables
#==================================================

write.csv(wideMonthlyAE, "wideMonthlyAE_Merged.csv", row.names = FALSE)
write.csv(wideMonthlyAE, "wideMonthlyAE_Merged_ForModeling.csv", row.names = FALSE)

#==================================================
# --- DONE ---
#==================================================

