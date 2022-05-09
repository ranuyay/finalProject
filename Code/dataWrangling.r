# Load libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)

# import data

individual2016 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/individual2016.csv")
individual2017 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/individual2017.csv")
individual2018 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/individual2018.csv")

adult18 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/adult18.csv")
adult19 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/adult19.csv")
adult20 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/adult20.csv")

child18 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/child18.csv")
child19 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/child19.csv")
child20 <- read.csv("C:/Users/Rania/Documents/Woz-U/finalProject/Data/child20.csv")

View(individual2016)

# Use Codebook to subset demographics and ADHD diagnoses

adult18keeps <- c("SRVY_YR", "REGION", "SEX", "RACERPI2", "HISPAN_I", "AGE_P", "DOINGLWA", "WHYNOWKA", "EVERWRK", "YRSWRKPA", "WRKLONGH", "AFLHCA17", "AFLHC30_", "AFLHCA90", "AFLHCA91", "ALDURB17", "ALCHRC17", "ALTIME30", "ALDURA30", "ALDURB30", "ALCHRC30")
adult19keeps <- c("SRVY_YR", "URBRRL", "REGION", "AGEP_A", "SEX_A", "EDUC_A", "HISPALLP_A", "PCNTFAM_A", "PCNTKIDS_A", "OVER65FLG_A", "MLTFAMFLG_A", "MAXEDUC_A", "PHSTAT_A", "ANXFREQ_A", "ANXMED_A", "ANXLEVEL_A", "DEPFREQ_A", "DEPMED_A", "DEPLEVEL_A", "MHRX_A", "MHTHRPY_A", "MHTPYNOW_A", "MHTHDLY_A", "MHTHND_A")
adult20keeps <- c("SRVY_YR", "URBRRL", "REGION", "AGEP_A", "SEX_A", "EDUC_A", "HISPALLP_A", "PCNTFAM_A", "PCNTKIDS_A", "OVER65FLG_A", "MLTFAMFLG_A", "MAXEDUC_A", "PHSTAT_A", "ANXFREQ_A", "ANXMED_A", "ANXLEVEL_A", "DEPFREQ_A", "DEPMED_A", "DEPLEVEL_A", "MHRX_A", "MHTHRPY_A", "MHTPYNOW_A", "MHTHDLY_A", "MHTHND_A")

child18keeps <- c("SRVY_YR", "AGE_P", "REGION", "SEX", "HISPAN_I", "RACERPI2", "ADD2", "LEARND", "RSCL2_C2", "RSCL2_E2", "RSCL3_E3", "RSCL5_H5", "RSCL6")
child19keeps <- c("SRVY_YR", "URBRRL", "REGION", "AGEP_C", "SEX_C", "HISPALLP_C", "RACEALLP_C", "ADHDEV_C", "LDEV_C", "LEARNDF_C", "REMEMBERDF_C", "ANXFREQ_C", "DEPFREQ_C", "MHRX_C", "MHTHRPY_C", "MHTHDLY_C", "MHTHND_C")
child20keeps <- c("SRVY_YR", "URBRRL", "REGION", "AGEP_C", "SEX_C", "HISPALLP_C", "RACEALLP_C", "ADHDEV_C", "LDEV_C", "LEARNDF_C", "REMEMBERDF_C", "ANXFREQ_C", "DEPFREQ_C", "MHRX_C", "MHTHRPY_C", "MHTHDLY_C", "MHTHND_C")

adult2018 <- adult18[adult18keeps]
adult2019 <- adult19[adult19keeps]
adult2020 <- adult20[adult20keeps]

child2018 <- child18[child18keeps]
child2019 <- child19[child19keeps]
child2020 <- child20[child20keeps]

adult2018r <- na.omit(adult2018)
# too many missing values for occupational questions; will remove from dataset 

adult18keeps2 <- c("SRVY_YR", "REGION", "SEX", "RACERPI2", "HISPAN_I", "AGE_P", "AFLHCA17", "AFLHC30_", "ALTIME30", "ALDURB30", "ALCHRC30")
adult2018.2 <- adult18[adult18keeps2]
adult2018.2r <- na.omit(adult2018.2)
adult2019r <- na.omit(adult2019)
adult2020r <- na.omit(adult2020)

child2018r <- na.omit(child2018)
child2019r <- na.omit(child2019)
chidl2020r <- na.omit(child2020)

# intial child data exploration

summary(child2018r)

