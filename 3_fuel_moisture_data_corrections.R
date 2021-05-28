#Authors: Deborah Nemens and Jim Cronan
#Organization: Pacific Wildland Fire Sciences Laboratory
#Address: 
#400 North 34th Street
#Suite 201
#Seattle, WA 98103
#Contact:
#Main: 206-732-7800
#Fax: 206-732-7801
#Emnail: james.cronan@usda.gov
#Date:
#7-May-2021
#NOTE:
#Purpose of this script is to extrapolate fuel moisture weights that were erroneously entered. These are instances identified in
#https://docs.google.com/document/d/1Wfz_-kPP7OyUzXntZV5X2nLIN4CZP4Ak/edit. This document was created by reviewing plots of
#absolute weights and caluclated fuel moisture values for each sample over the course of the experiment. Because each sample is
#weighed at each of the four locations (i.e., 3, 6, 9, and 12 meters) during a scan time (e.g., 12 hours)
#there are essentially 4 independent measurements of each sample at each scan time that can be used as checks on each other.
#Sample weights should only change by a small amount during a scan time so any deviation from that small range indicates that
#the person recording the weight either incorrectly recorded the value, the sample was not properly placed on the scale (e.g., 
#it was leaning on an adjacent object) or the scale was not functionaing correctly.
#To correct erroneous values we will extrapolate from the three correct values. Weights decline predictably by a small amount 
#over the course of the 4 scans during a scan time so we can extrapolate values with a high degree of confidence (within 1
#gram of the true weight). 

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#Libraries
library(tidyverse)
library(readxl)
library(googledrive)#used to access files in Google Drive
library(lubridate)
library(stats)

#Drive authorization -- provide authorization to access Google Drive
drive_auth()
1#jcronan@uw.edu

#Set working drive
setwd("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/")


#Fuel Moisture Weights
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1mnouEvWTbH8AFeanQcH9e4qmcAJymQlKN-qYy5LQRVU/edit#gid=1007445331", 
                    team_drive = "2021 Lidar fuel moisture")

#Download a copy of the fuel moisture file
drive_download(target, 
               type = "xlsx", 
               path = "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/Fuel_moisture_weights.xlsx", 
               overwrite = T)

#load data
weights <- read_excel("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/Fuel_moisture_weights.xlsx")
weights <- write_csv(weights, "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/Fuel_moisture_weights.csv")
weights <- read.csv("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/Fuel_moisture_weights.csv")

#Tray Weights
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1qsmEyXoUtkw29mrA6s1WLTebi-vYt9qj/edit#gid=1294211753", 
                    team_drive = "2021 Lidar fuel moisture")

#Download a copy of the fuel moisture file
drive_download(target, 
               type = "xlsx", 
               path = "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/drying_trays.xlsx", 
               overwrite = T)

#load data
tare <- read_excel("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/drying_trays.xlsx")
tare <- write_csv(tare, "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/drying_trays.csv")
tare <- read.csv("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/drying_trays.csv")

#Add column to weights object that explains modifications to data
weights.mod <- data.frame(weights,
                          cor_type = vector(mode = "numeric", length = length(weights[,1])), 
                          correction_notes = vector(mode = "character", length = length(weights[,1])))
#correction types
#0 - no modification made
#1 - extrapolated: value changed based on neighboring values
#2 - inferred: value changed based on perceived data entry mistake

#Error numbers refer to numbers listed in column one of:
#https://docs.google.com/document/d/1Wfz_-kPP7OyUzXntZV5X2nLIN4CZP4Ak/edit

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     PINE BOARDS          ##################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 1
#Sample Type
sm <- "PB"
#Sample Number
sn <- 2
#Scan Time
st <- "0.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value.
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 2
#Sample Type
sm <- "PB"
#Sample Number
sn <- 2
#Scan Time
st <- "4.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Extrapolated. Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 3
#Sample Type
sm <- "PB"
#Sample Number
sn <- 6
#Scan Time
st <- "0.0"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "PB"
#Sample Number
cor.sn <- 8
#Scan Time
cor.st <- "0.0"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
              weights$Sample == cor.sn & 
              weights$scan == cor.st & 
              weights$location == cor.el] 
  
weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Inferred correction. Gross weight (915.72) for sample 6, 0-hr scan, 3-meter location was accidentally recorded as sample 8, 0-hr scan, 3-meter location and gross weight (906.12) for sample 8, 0-hr scan, 3-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 4
#Sample Type
sm <- "PB"
#Sample Number
sn <- 6
#Scan Time
st <- "0.0"
#Error Location
el <- 12

#Correct value location
#Sample Type
cor.sm <- "PB"
#Sample Number
cor.sn <- 8
#Scan Time
cor.st <- "0.0"
#Error Location
cor.el <- 12

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - Inferred correction. Gross weight (919.2) for sample 6, 0-hr scan, 12-meter location was accidentally recorded as sample 8, 0-hr scan, 12-meter location and gross weight (908.9) for sample 8, 0-hr scan, 12-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 5
#Sample Type
sm <- "PB"
#Sample Number
sn <- 8
#Scan Time
st <- "0.0"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "PB"
#Sample Number
cor.sn <- 6
#Scan Time
cor.st <- "0.0"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan: Inferred correction. Gross weight (906.12) for sample 8, 0-hr scan, 3-meter location was accidentally recorded as sample 6, 0-hr scan, 3-meter location and gross weight (915.72) for sample 6, 0-hr scan, 3-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 6
#Sample Type
sm <- "PB"
#Sample Number
sn <- 8
#Scan Time
st <- "0.0"
#Error Location
el <- 12

#Correct value location
#Sample Type
cor.sm <- "PB"
#Sample Number
cor.sn <- 6
#Scan Time
cor.st <- "0.0"
#Error Location
cor.el <- 12

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan: Inferred correction. Gross weight (908.9) for sample 8, 0-hr scan, 12-meter location was accidentally recorded as sample 6, 0-hr scan, 12-meter location and gross weight (919.2) for sample 6, 0-hr scan, 12-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 7
#Sample Type
sm <- "PB"
#Sample Number
sn <- 6
#Scan Time
st <- "2.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Errors 8
#This error was caused by incorrect transcription of data from paper data sheet to Google spreadhseet.
#Error was corrected on spreadsheet.

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     CHEESE CLOTH          ##################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 9
#Sample Type
sm <- "CC"
#Sample Number
sn <- 3
#Scan Time
st <- "8.0"
#Error Location
el <- 3

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 10
#Sample Type
sm <- "CC"
#Sample Number
sn <- 7
#Scan Time
st <- "4.0"
#Error Location
el <- 3

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 11
#Sample Type
sm <- "CC"
#Sample Number
sn <- 10
#Scan Time
st <- "2.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 12
#Sample Type
sm <- "CC"
#Sample Number
sn <- 10
#Scan Time
st <- "24.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 13
#Sample Type
sm <- "CC"
#Sample Number
sn <- 10
#Scan Time
st <- "36.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 14
#Sample Type
sm <- "CC"
#Sample Number
sn <- 10
#Scan Time
st <- "48.0"
#Error Location
el <- 6

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     DOUGLAS-FIR          ##################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 15
#This error was caused by incorrect transcription of data from paper data sheet to Google spreadhseet.
#Error was corrected on spreadsheet.

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 16
#This error was caused by incorrect transcription of data from paper data sheet to Google spreadhseet.
#Error was corrected on spreadsheet.

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 17
#Sample Type
sm <- "DF"
#Sample Number
sn <- 5
#Scan Time
st <- "48.0"
#Error Location
el <- 3

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 18
#Sample Type
sm <- "DF"
#Sample Number
sn <- 7
#Scan Time
st <- "od"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "DF"
#Sample Number
cor.sn <- 8
#Scan Time
cor.st <- "od"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan: Inferred correction. Gross weight (596.3) for sample 7, oven-dry scan, 3-meter location was accidentally recorded as sample 8, oven-dry scan, 3-meter location and gross weight (579.42) for sample 8, oven-dry scan, 3-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 19
#Sample Type
sm <- "DF"
#Sample Number
sn <- 8
#Scan Time
st <- "od"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "DF"
#Sample Number
cor.sn <- 7
#Scan Time
cor.st <- "od"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan: Inferred correction. Gross weight (579.42) for sample 7, oven-dry scan, 3-meter location was accidentally recorded as sample 8, oven-dry scan, 3-meter location and gross weight (596.3) for sample 8, oven-dry scan, 3-meter location was accidentally recorded here. Switched values."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 20
#Sample Type
sm <- "DF"
#Sample Number
sn <- 9
#Scan Time
st <- "od"
#Error Location
el <- 9

#Correct value
correct.value <- 589.32

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - I changed value from 584.32 to 589.32. Paper data sheet could be either value, but sample 9 weights at other locations all fall betwen 589.0 and 589.9."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 21-26
#Missing fuel moisture values for all 9-meter oven-dry scans for Douglas-fir samples.
#This error was caused by a scripting type and was corrected.

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     PONDEROSA PINE      ###################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 37
#Sample Type
sm <- "PP"
#Sample Number
sn <- 26
#Scan Time
st <- "48.0"
#Error Location
el <- 12

#Correct value
correct.value <- 628.2

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - changed value from 638.2 to 628.2. The other three values for this sample at the 48-hr scan time are between 628.0 and 628.5. Highly likely person entering data recorded a three instead of a two."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     SOUTHERN RED OAK      #################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 38
#Sample Type
sm <- "SRO"
#Sample Number
sn <- 6
#Scan Time
st <- "2.0"
#Error Location
el <- 3

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 39
#Sample Type
sm <- "SRO"
#Sample Number
sn <- 13
#Scan Time
st <- "8.0"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "SRO"
#Sample Number
cor.sn <- 15
#Scan Time
cor.st <- "8.0"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - I switched this value (521.09) with the value for sample 15 (514.86) at the 8-hr scan/3-meter location. Values line up much better with other locations at this scan for this sample when they are switched. Highly likely staff recording data for samples got tray 13 and 15 mixed up."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 40
#Sample Type
sm <- "SRO"
#Sample Number
sn <- 15
#Scan Time
st <- "8.0"
#Error Location
el <- 3

#Correct value location
#Sample Type
cor.sm <- "SRO"
#Sample Number
cor.sn <- 13
#Scan Time
cor.st <- "8.0"
#Error Location
cor.el <- 3

#Correct value
correct.value <- weights$gross.weight[weights$sample.type == cor.sm & 
                                        weights$Sample == cor.sn & 
                                        weights$scan == cor.st & 
                                        weights$location == cor.el] 

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - I switched this value (514.86) with the value for sample 13 (521.09) at the 8-hr scan/3-meter location. Values line up much better with other locations at this scan for this sample when they are switched. Highly likely staff recording data for samples got tray 13 and 15 mixed up."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 41
#Sample Type
sm <- "SRO"
#Sample Number
sn <- 14
#Scan Time
st <- "od"
#Error Location
el <- 3

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################J. Cronan - I switched this value (514.86) with the value for sample 13 (521.09) at the 8-hr scan/3-meter location. Values line up much better with other locations at this scan for this sample when they are switched. Highly likely staff recording data for samples got tray 13 and 15 mixed up.
#Error 42
#Sample Type
sm <- "SRO"
#Sample Number
sn <- 15
#Scan Time
st <- "24.0"
#Error Location
el <- 9

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################J. Cronan - I switched this value (514.86) with the value for sample 13 (521.09) at the 8-hr scan/3-meter location. Values line up much better with other locations at this scan for this sample when they are switched. Highly likely staff recording data for samples got tray 13 and 15 mixed up.
#Error 43
#Not an error

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#################################################     LONGLEAF PINE      ####################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 44-59
#Scan values were incorrectly transcribed (as 36-hr instead of 48-hr) from the data sheets to the Google spreadsheet. 
#I corrected scan time values on spreadsheet so they are the same as paper data sheets.

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 60
#Sample Type
sm <- "LLP"
#Sample Number
sn <- 21
#Scan Time
st <- "od"
#Error Location
el <- 3

#Correct value
correct.value <- 598.25

weights.mod$gross.weight[weights.mod$sample.type == sm & 
              weights.mod$Sample == sn & 
              weights.mod$scan == st & 
              weights.mod$location == el] <- correct.value

weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                       weights.mod$location == el] <- 2
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                 weights.mod$Sample == sn & weights.mod$scan == st & 
                                 weights.mod$location == el] <- 
  "J. Cronan - Negative fuel moisture. The original value was 593.25, but this yields a negative fuel moisture. Highly likely that person recording data misread the eight as a three."

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Error 61
#Sample Type
sm <- "LLP"
#Sample Number
sn <- 30
#Scan Time
st <- "od"
#Error Location
el <- 9

#Isolate sample for specified scane time.
error <- weights[weights$sample.type == sm & weights$Sample == sn & weights$scan == st,]
error[order(error$time),]

#Remove erroneous weight
error$gross.weight[error$location == el] <- NA 

#Re-class time in minutes from midnight to create a linear model
error$time <- sprintf("%04d", error$time)
error$time <- format(strptime(error$time, format = "%H%M"), format = "%H:%M")
error$time <- as.numeric(hm(error$time))/60

#Generate a linear model of gross weight against time
lr <- lm(gross.weight ~ time, error)
plot(error$time, error$gross.weight)
abline(lr)
error[order(error$time),]

#Predict gross weight for erroneous value.
ev <- round(as.vector(predict(lr, data.frame(time = error$time[is.na(error$gross.weight) == T]))),1)

#Replace erroneous value with extrapolated value
#Add 1 to extrapolated column (0 = no & 1 = yes if data is extrapolated.
#Add text explaining how data was extrapolated.
weights.mod$gross.weight[weights.mod$sample.type == sm & weights.mod$Sample == sn & 
                           weights.mod$scan == st & weights.mod$location == el] <- ev
weights.mod$cor_type[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st & 
                           weights.mod$location == el] <- 1
weights.mod$correction_notes[weights.mod$sample.type == sm & 
                                      weights.mod$Sample == sn & weights.mod$scan == st & 
                                      weights.mod$location == el] <- 
  "J. Cronan - Gross weight extrapolated based on linear model of three weights at other sampling locations."
show <- weights.mod[weights.mod$sample.type == sm & weights.mod$Sample == sn & weights.mod$scan == st,]
show[order(show$time),]

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#Save table of sample weights with values extrapolated in this script.
setwd("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc")
write.csv(weights.mod, "fuel_moisture_corrected_weights.csv", row.names = F)

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#End
