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
#8-Mar-2021
#NOTE:
#Purpose of this script is to do an initial review of fuel moisture samples that were collected as part of a 3D Fuels SERDP
#research project to test if terrestrial scanning Lidar can detect fuel moisure of dead fuels in a lab setting. This script
#prduces graphics, summary stats, and runs QAQC checks on the data.
#Data resides on a University of Washington shared Google Drive.

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#Libraries
library(tidyverse)
library(readxl)
library(googledrive)#used to access files in Google Drive

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

###############################################################################################################################
#################################################     PINE BOARDS          ####################################################
###############################################################################################################################
weights.pb <- filter(weights, sample.type=="PB")

#adds dry weights from earliest set of oven dry scans
dry.p <- weights.pb %>%
  filter(scan=="od" & time == 1104) %>%
  select(Sample, gross.weight) %>%
  mutate(dry.wt = gross.weight) %>%
  select(Sample, dry.wt)

#add dry weights for each board
weights.pb <- inner_join(weights.pb, dry.p, by = "Sample")

#calculate fuel moisture
pb.fm <- mutate(weights.pb, fm = ((gross.weight-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
pb.fm2 <- pb.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.pb <- pb.fm$Sample
scan.pb <- factor(pb.fm$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))

PB <- ggplot(data=pb.fm, aes(x=scan.pb, y=fm))+
  geom_point(color=sample.pb, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle ("Pine boards")+
  # scale_y_continuous(breaks=seq(0,100,10))+
  xlab("Scan time (hour)")
PB 

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#Manually enter each of the sample numbers, 1-16, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <- 8
data <- pb.fm[pb.fm$Sample == sn,]
data.pb <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.pb, fm, color = location)) +
  geom_point(aes(fill=data.pb),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))
#Lines connected weights at locations should be parallel. If they are not samples were weighed out of order or there
#was an error.

#Flags
#Sample 2 - sample weights do not fall into expected order for the 6-m location with the 0-hr and 4-hr scan.
data[data$scan == "0.0",]
data[data$scan == "4.0",]

#Flags
#Sample 6 - sample weights do not fall into expected order for the 6-m location with the 0-hr and 2-hr scan.
x1 <- data[data$scan == "0.0",]
x2 <- data[data$scan == "2.0",]
x3 <- data[data$scan == "4.0",]
x4 <- data[data$scan == "8.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]
x4[order(x4$time),]

#Flags
#Sample 8 - sample weights do not fall into expected order for the 9-m location with the 0-hr scan.
x1 <- data[data$scan == "0.0",]
x2 <- data[data$scan == "2.0",]
x3 <- data[data$scan == "4.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Flags
#Sample 8 - sample weights are outside of the expected range for the 12-m location with the 120-hr scan.
x1 <- data[data$scan == "48.0",]
x2 <- data[data$scan == "72.0",]
x3 <- data[data$scan == "120.0",]
x4 <- data[data$scan == "od",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]
x4[order(x4$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.pb <- vector()
if(length(weights.pb[,1]) == length(unique(weights.pb$scan)) * 64)
  check.weights.pb <- "OKAY" else
    check.weights.pb <- "ERROR"
check.weights.pb

check.fm.pb <- vector()
if(length(pb.fm[,1]) == length(unique(pb.fm$scan)) * 64)
  check.fm.pb <- "OKAY" else
    check.fm.pb <- "ERROR"
pb.ex.w <- length(unique(weights.pb$scan)) * 64
pb.ac.w <- length(weights.pb[,1])
pb.ex.m <- length(unique(pb.fm$scan)) * 64
pb.ac.m <- length(pb.fm[,1])
pb.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                     Value = c(pb.ex.w, pb.ac.w, check.weights.pb, pb.ex.m, pb.ac.m, check.fm.pb))
pb.rep

###############################################################################################################################
#################################################     CHEESE CLOTH         ####################################################
###############################################################################################################################
weights.cc <- weights %>%
  filter(sample.type == "CC") 

#adds tray tare weights to data file
tare.cc <- tare %>%
  filter (tare.staple > 0) %>%
  select (Sample, tare.staple)

weights.cc <- inner_join(weights.cc, tare.cc, by = "Sample")

weights.cc$net.wt <- weights.cc$gross.weight - weights.cc$tare.staple

#adds dry weights from earliest set of oven dry scans
dry.cc <- weights.cc %>%
  filter(scan=="od" & time == "842") %>%
  select(Sample, net.wt) %>%
  mutate(dry.wt = net.wt) %>%
  select(Sample, dry.wt)

weights.ccd <- inner_join(weights.cc, dry.cc, by = "Sample")

cc.fm <- weights.ccd %>% mutate(fm = ((net.wt-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
cc.fm2 <- cc.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.c <- cc.fm$Sample
scan.c <- factor(cc.fm$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))

CC <- ggplot(data=cc.fm, aes(x=scan.c, y=fm))+
  geom_point(color = sample.c, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Cheesecloth")+
  # scale_y_continuous(breaks=seq(0,320,50))+
  xlab("Scan time (hour)")

CC 

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#manually enter each of the sample numbers, 1-16, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <- 12
data <- cc.fm[cc.fm$Sample == sn,]
data.cc <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)
x2 <- data[data$scan == "36.0",]
x2[order(x2$time),]

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.cc, fm, color = location)) +
  geom_point(aes(fill=data.cc),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

#Sample 3
#Flags
#Details - sample weights do not fall into expected order for the 8-hr scan.
#The weight for the 12-meter location is less than would be expected and is probably incorrect.
x1 <- data[data$scan == "2.0",]
x2 <- data[data$scan == "4.0",]
x3 <- data[data$scan == "8.0",]
x4 <- data[data$scan == "12.0",]
x5 <- data[data$scan == "24.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]
x4[order(x4$time),]
x5[order(x5$time),]

#Sample 7
#Flags
#Details - sample weights do not fall into expected order for the 4-hr scan.
#The weight for the 3-meter location is greater than would be expected and is probably incorrect.
x1 <- data[data$scan == "2.0",]
x2 <- data[data$scan == "4.0",]
x3 <- data[data$scan == "8.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 10
#Flags
#Details - sample weights do not fall into expected order for the 2-hr scan.
#The weight for the 6-meter location is far greater than would be expected and is probably incorrect.
x1 <- data[data$scan == "0.0",]
x2 <- data[data$scan == "2.0",]
x3 <- data[data$scan == "4.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 10
#Flags
#Details - sample weights and fuel moistures are higher than expected range for the 24-hr, 36-hr, 
#and 48-hr scans.
#The weight for the 6-meter location is far greater than would be expected and is probably incorrect.
x1 <- data[data$scan == "12.0",]
x2 <- data[data$scan == "24.0",]
x3 <- data[data$scan == "36.0",]
x4 <- data[data$scan == "48.0",]
x5 <- data[data$scan == "od",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]
x4[order(x4$time),]
x5[order(x5$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.cc <- vector()
if(length(weights.cc[,1]) == length(unique(weights.cc$scan)) * 64)
  check.weights.cc <- "OKAY" else
    check.weights.cc <- "ERROR"
check.weights.cc

check.fm.cc <- vector()
if(length(cc.fm[,1]) == length(unique(cc.fm$scan)) * 64)
  check.fm.cc <- "OKAY" else
    check.fm.cc <- "ERROR"
cc.ex.w <- length(unique(weights.cc$scan)) * 64
cc.ac.w <- length(weights.cc[,1])
cc.ex.m <- length(unique(cc.fm$scan)) * 64
cc.ac.m <- length(cc.fm[,1])
cc.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                     Value = c(cc.ex.w, cc.ac.w, check.weights.cc, cc.ex.m, cc.ac.m, check.fm.cc))
cc.rep

###############################################################################################################################
#################################################     DOUGLAS-FIR          ####################################################
###############################################################################################################################
weights.df <- filter(weights, sample.type == "DF")

tare.df <- tare %>%
  filter (tare.wt > 0 & Sample < 17) %>%
  select (Sample, tare.wt)

weights.df <- inner_join(weights.df, tare.df, by = "Sample")

#calculates net wt of each sample by subtracting tare wt (= tray weight)
weights.df$net.wt <- weights.df$gross.weight-weights.df$tare.wt

#adds dry weights from earliest set of oven dry scans
dry.df <- weights.df %>%
  filter(scan=="od" & time == "1015") %>%
  select(Sample, gross.weight, tare.wt)%>%
  mutate(dry.wt = gross.weight - tare.wt) %>%
  select(Sample, dry.wt)

#joins dry weight and gross weights tables
weights.df <- inner_join(weights.df, dry.df, by = "Sample")

#calculates fuel moistures
df.fm <- weights.df %>% mutate(fm =((net.wt-dry.wt)*100/dry.wt)) 

#df sample name/number/scan/angle
df.fm2 <- df.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.d <- df.fm$Sample
scan.d <- factor(df.fm$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))

DF <- ggplot(data=df.fm, aes(x=scan.d, y=fm))+
  geom_point(color = sample.d, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Douglas Fir")+
  #scale_y_continuous(breaks=seq(0,320,50))+
  xlab("Scan time (hours)")

DF

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#Manually enter each of the sample numbers, 1-16, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <-2
data <- df.fm[df.fm$Sample == sn,]
data.df <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)
x1 <- data[data$scan == "od",]
x1[order(x1$Sample),]

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.df, fm, color = location)) +
  geom_point(aes(fill=data.df),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

#Sample 1
#Flags
#Details - the 3m location for the oven-dry scan has a fuel moisture that is higher than expected.
x1 <- data[data$scan == "48.0",]
x2 <- data[data$scan == "od",]
x1[order(x1$time),]
x2[order(x2$time),]

#Sample 2
#Flags
#Details - the 12m location for the 2-hr scan was outside of the expected range.
x1 <- data[data$scan == "1.0",]
x2 <- data[data$scan == "2.0",]
x3 <- data[data$scan == "4.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 5
#Flags
#Details - the 3m location for the 48-hr scan has a gross weight/fuel moisture that is lower than expected.
x1 <- data[data$scan == "36.0",]
x2 <- data[data$scan == "48.0",]
x3 <- data[data$scan == "od",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]


#Sample 7
#Flags
#Details - the 3m location for the oven-dry scan has a gross weight/fuel moisture that is higher than expected.
x1 <- data[data$scan == "48.0",]
x2 <- data[data$scan == "od",]
x1[order(x1$time),]
x2[order(x2$time),]

#Sample 7
#Flags
#Details - the 3m location for the oven-dry scan has a gross weight/fuel moisture that is higher than expected.
x2 <- weights.df[weights.df$Sample == 7,]
x1[order(x1$time),]
x2[order(x2$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.df <- vector()
if(length(weights.df[,1]) == length(unique(weights.df$scan)) * 64)
  check.weights.df <- "OKAY" else
    check.weights.df <- "ERROR"
check.weights.df

check.fm.df <- vector()
if(length(df.fm[,1]) == length(unique(df.fm$scan)) * 64)
  check.fm.df <- "OKAY" else
    check.fm.df <- "ERROR"
df.ex.w <- length(unique(weights.df$scan)) * 64
df.ac.w <- length(weights.df[,1])
df.ex.m <- length(unique(df.fm$scan)) * 64
df.ac.m <- length(df.fm[,1])
df.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                     Value = c(df.ex.w, df.ac.w, check.weights.df, df.ex.m, df.ac.m, check.fm.df))
df.rep

###############################################################################################################################
#################################################     PONDEROSA PINE       ####################################################
###############################################################################################################################
weights.pp <- filter(weights, sample.type == "PP")

weights.pp <- inner_join(weights.pp, tare, by = "Sample")

#calculates net weight by subtracting tray weight
weights.pp$net.wt <- weights.pp$gross.weight-weights.pp$tare.wt

#adds dry weights from earliest set of oven dry scans
dry.pp <- weights.pp %>%
  filter(scan=="od" & time == "841") %>%
  select(Sample, gross.weight, tare.wt)%>%
  mutate(dry.wt = gross.weight - tare.wt) %>%
  select(Sample, dry.wt)

weights.pp <- inner_join(weights.pp, dry.pp, by = "Sample")

pp.fm <- weights.pp %>% mutate(fm =((net.wt-dry.wt)*100/dry.wt)) 

#df sample name/number/scan/angle
pp.fm2 <- pp.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.p <- pp.fm$Sample
scan.p <- factor(pp.fm$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))

PP <- ggplot(data=pp.fm, aes(x=scan.p, y=fm))+
  geom_point(color = sample.p, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Pondo Pine")+
  #scale_y_continuous(breaks=seq(0,320,50))+
  xlab("Scan time (hours)")+
  theme(legend.position = "right")

PP

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#Manually enter each of the sample numbers, 17-32, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <- 28
data <- pp.fm[pp.fm$Sample == sn,]
data.pp <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)
x1 <- data[data$scan == "48.0",]
x1[order(x1$time),]

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.pp, fm, color = location)) +
  geom_point(aes(fill=data.pp),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

#Sample 26
#Flags
#Details - the 12m location for the 48-hr scan has a gross weight/fuel moisture that is higher than expected.
x1 <- data[data$scan == "36.0",]
x2 <- data[data$scan == "48.0",]
x3 <- data[data$scan == "120.0",]
x4 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]
x4[order(x4$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.pp <- vector()
if(length(weights.pp[,1]) == length(unique(weights.pp$scan)) * 64)
  check.weights.pp <- "OKAY" else
    check.weights.pp <- "ERROR"
check.weights.pp

check.fm.pp <- vector()
if(length(pp.fm[,1]) == length(unique(pp.fm$scan)) * 64)
  check.fm.pp <- "OKAY" else
    check.fm.pp <- "ERROR"
pp.ex.w <- length(unique(weights.pp$scan)) * 64
pp.ac.w <- length(weights.pp[,1])
pp.ex.m <- length(unique(pp.fm$scan)) * 64
pp.ac.m <- length(pp.fm[,1])
pp.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                     Value = c(pp.ex.w, pp.ac.w, check.weights.pp, pp.ex.m, pp.ac.m, check.fm.pp))
pp.rep
#There are 705 samples and there should be 704. Where is the extra sample weight?
sns <- 17:32
check <- vector()
for(i in 1:16)
  {
  check[i] <- length(weights.pp[,1][weights.pp$Sample == sns[i]])
}
check
#sample 24 has the extra value.
weights.pp[weights.pp$Sample == 24,]
#There are 2 rows for the 6 meter location in the 2-hr scan. They both have the same exact values
#One row was deleted from the source data.


###############################################################################################################################
#################################################     SOUTHERN RED OAK    #####################################################
###############################################################################################################################
weights.sro <- filter(weights, sample.type == "SRO")

tare.sro <- tare %>%
  filter(tare.wt2 != "NA")%>%
  select(Sample, tare.wt2)

weights.sro <- inner_join(weights.sro, tare.sro, by = "Sample")

#CALCULATES net wt by subtracting tray wt
weights.sro$net.wt <- weights.sro$gross.weight-weights.sro$tare.wt

#adds dry weights from first set of oven dry scans
dry.sro <- weights.sro %>%
  filter(scan=="od" & time == "1014") %>%
  select(Sample, gross.weight, tare.wt2)%>%
  mutate(dry.wt = gross.weight - tare.wt2) %>%
  select(Sample, dry.wt)

weights.sro <- inner_join(weights.sro, dry.sro, by = "Sample")

#calculates fuel moisture
sro.fm <- weights.sro %>% mutate(fm =((net.wt-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
sro.fm2 <- sro.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.s <- sro.fm$Sample
scan.s <- factor(sro.fm$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))

SRO <- ggplot(data=sro.fm, aes(x=scan.s, y=fm))+
  geom_point(color = sample.s, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Southern red oak")+
  #scale_y_continuous(breaks=seq(-150,320,50))+
  xlab("Scan time (hours)")

SRO

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#Manually enter each of the sample numbers, 1-16, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <- 16
data <- sro.fm[sro.fm$Sample == sn,]
data.sro <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)
x1 <- data[data$scan == "od",]
x1[order(x1$time),]

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.sro, fm, color = location)) +
  geom_point(aes(fill=data.sro),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

#Sample 6
#Flags
#Details - the 3m location for the 2-hr scan has a gross weight/fuel moisture that is higher than expected.
x1 <- data[data$scan == "12.0",]
x2 <- data[data$scan == "24.0",]
x3 <- data[data$scan == "36.0",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 13
#Flags
#Details - the 3m location for the 8-hr scan has a gross weight/fuel moisture that is higher than expected.
x1 <- data[data$scan == "4.0",]
x2 <- data[data$scan == "8.0",]
x3 <- data[data$scan == "12.0",]
x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 14
#Flags
#Details - the gross weight and fuel moisture at 3m location for the oven-dry scan is lower than expected.
x1 <- data[data$scan == "24.0",]
x2 <- data[data$scan == "36.0",]
x3 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 15
#Flags
#Details - the gross weight and fuel moisture at 3m location for the 8-hr scan is lower than expected.
x1 <- data[data$scan == "4.0",]
x2 <- data[data$scan == "8.0",]
x3 <- data[data$scan == "12.0",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Sample 15
#Flags
#Details - the gross weight and fuel moisture at 3m location for the oven-dry scan is lower than expected.
x1 <- data[data$scan == "24.0",]
x2 <- data[data$scan == "36.0",]
x3 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.sro <- vector()
if(length(weights.sro[,1]) == length(unique(weights.sro$scan)) * 64)
  check.weights.sro <- "OKAY" else
    check.weights.sro <- "ERROR"
check.weights.sro

check.fm.sro <- vector()
if(length(sro.fm[,1]) == length(unique(sro.fm$scan)) * 64)
  check.fm.sro <- "OKAY" else
    check.fm.sro <- "ERROR"
sro.ex.w <- length(unique(weights.sro$scan)) * 64
sro.ac.w <- length(weights.sro[,1])
sro.ex.m <- length(unique(sro.fm$scan)) * 64
sro.ac.m <- length(sro.fm[,1])
sro.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                     Value = c(sro.ex.w, sro.ac.w, check.weights.sro, sro.ex.m, sro.ac.m, check.fm.sro))
sro.rep

###############################################################################################################################
#################################################     LONGLEAF PINE    ########################################################
###############################################################################################################################
weights.llp <- filter(weights, sample.type == "LLP")

weights.llp <- inner_join(weights.llp, tare, by = "Sample")

#adds dry weights from first set of oven dry scans
dry.llp <- weights.llp %>%
  filter(scan=="od" & time == "1026") %>%
  select(Sample, gross.weight, tare.wt)%>%
  mutate(dry.wt = gross.weight - tare.wt) %>%
  select(Sample, dry.wt)

weights.llp <- inner_join(weights.llp, dry.llp, by = "Sample")

weights.llp$net.wt <- weights.llp$gross.weight-weights.llp$tare.wt

#calculate fuel moisture
llp.fm <- weights.llp %>% mutate(fm =((net.wt-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
llp.fm2 <- llp.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = paste(scan, "H", sep ="")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)


sample.l <- llp.fm$Sample
scan.l <- factor(weights.llp$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))

LLP <- ggplot(data=llp.fm, aes(x=scan.l, y=fm))+
  geom_point(color = sample.l, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Long leaf pine")+
  #scale_y_continuous(breaks=seq(0,320,50))+
  xlab("Scan time (hours)")

LLP

##########################################
#What kind of errors can I look for in this data?
#1 - plot out changes in weight and fuel moisture for each sample.
#Seperate out by location to see if individual scales are having an impact.
#Manually enter each of the sample numbers, 17-32, and review graphs. Lines should be mostly parallel
#Run review for gross weight and fuel moisture.

sn <- 32
data <- llp.fm[llp.fm$Sample == sn,]
data.llp <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)
x1 <- data[data$scan == "48.0",]
x1[order(x1$time),]

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.llp, fm, color = location)) +
  geom_point(aes(fill=data.llp),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))


#Samples 17-32
#Flags
#Details - the 3m location for the 48-hr scan is missing weight.
x1 <- data[data$scan == "36.0",]
x2 <- data[data$scan == "48.0",]
x3 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Samples 21
#Flags
#Details - the 3m location for the oven-dry scan is lower than expected.
x1 <- data[data$scan == "36.0",]
x2 <- data[data$scan == "48.0",]
x3 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Samples 30
#Flags
#Details - the 9m location for the oven-dry scan is lower than expected.
x1 <- data[data$scan == "36.0",]
x2 <- data[data$scan == "48.0",]
x3 <- data[data$scan == "od",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Samples 30
#Flags
#Details - the 9m location for the oven-dry scan is lower than expected.
x1 <- data[data$scan == "4.0",]
x2 <- data[data$scan == "8.0",]
x3 <- data[data$scan == "12.0",]

x1[order(x1$time),]
x2[order(x2$time),]
x3[order(x3$time),]

#Check the number of weights in weight and fuel moisture tables.
#The number should equal the number of scans times 64 (16 samples x 4 scan locations)
check.weights.llp <- vector()
if(length(weights.llp[,1]) == length(unique(weights.llp$scan)) * 64)
  check.weights.llp <- "OKAY" else
    check.weights.llp <- "ERROR"
check.weights.llp

check.fm.llp <- vector()
if(length(llp.fm[,1]) == length(unique(llp.fm$scan)) * 64)
  check.fm.llp <- "OKAY" else
    check.fm.llp <- "ERROR"
llp.ex.w <- length(unique(weights.llp$scan)) * 64
llp.ac.w <- length(weights.llp[,1])
llp.ex.m <- length(unique(llp.fm$scan)) * 64
llp.ac.m <- length(llp.fm[,1])
llp.rep <- data.frame(Item = c("Weights-Expected", "Weights-Actual", "Result", "FM-Expected", "FM-Actual", "Result"),
                      Value = c(llp.ex.w, llp.ac.w, check.weights.llp, llp.ex.m, llp.ac.m, check.fm.llp))
llp.rep

############################################################################################################################
############################################################################################################################
############################################################################################################################


############
#create df of all sample names and fuel moistures
mashup <- rbind(pb.fm2, cc.fm2, df.fm2, pp.fm2, llp.fm2, sro.fm2)

write.csv(mashup, "G:/Shared drives/2021 Lidar fuel moisture/Data_results/FinalFuelMoistureData.csv", row.names = F)

##############stack multiple ggplots
library("gridExtra")
all <- grid.arrange (PB, CC, DF, PP, SRO, LLP, nrow = 3)

ggsave(all, filename = "G:/Shared drives/2021 Lidar fuel moisture/Data_results/FM.Plots.pdf", device = "pdf", width = 7.7, height = 8, dpi = 300)




###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
#QAQC - flag gross weight outliers among scan positions (3m , 6m, 9m, and 12m) for each sample at each scan time.

#Calculate standard deviation, number of samples, minimum value, and max value for gross weights by sample type, sample number, and
#scan time.
sstats.1 <- as.data.frame(weights %>%
  group_by(sample.type, Sample, scan) %>%
  summarise(sd = round(sd(gross.weight),2), n = n(), min = min(gross.weight), max = max(gross.weight)))

#Calculate range between min and max values for gross weight.
diff.1 <- sstats.1$max - sstats.1$min

#Combine with earlier object.
sstats.2 <- data.frame(sstats.1, data.range = diff.1)

#Breaks for histogram
bs <- seq(0,65,1)

#Do a histogram to see how gross weight spread 
nf <- layout(matrix(c(1,2,3,4,5,6),3,2,byrow = TRUE), c(1,1,1), c(1,1), TRUE)
layout.show(nf)
hist(sstats.2$data.range[sstats.2$sample.type == "PB"], breaks = bs, main = "Pine Board")
hist(sstats.2$data.range[sstats.2$sample.type == "CC"], breaks = bs, main = "Cheese Cloth")
hist(sstats.2$data.range[sstats.2$sample.type == "DF"], breaks = bs, main = "Douglas-fir")
hist(sstats.2$data.range[sstats.2$sample.type == "PP"], breaks = bs, main = "Ponderosa Pine")
hist(sstats.2$data.range[sstats.2$sample.type == "SRO"], breaks = bs, main = "Southern Red Oak")
hist(sstats.2$data.range[sstats.2$sample.type == "LLP"], breaks = bs, main = "Longleaf Pine")

sstats_noZero <- sstats.2[sstats.2$scan != "0",]

#Do a histogram to see how gross weight spread 
nf <- layout(matrix(c(1,2,3,4,5,6),3,2,byrow = TRUE), c(1,1,1), c(1,1), TRUE)
layout.show(nf)
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "PB"], breaks = bs, main = "Pine Board")
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "CC"], breaks = bs, main = "Cheese Cloth")
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "DF"], breaks = bs, main = "Douglas-fir")
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "PP"], breaks = bs, main = "Ponderosa Pine")
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "SRO"], breaks = bs, main = "Southern Red Oak")
hist(sstats_noZero$data.range[sstats_noZero$sample.type == "LLP"], breaks = bs, main = "Longleaf Pine")


#How many samples exceed a gram difference
yellow.flags <- sstats.2[sstats.2$data.range > 2,]

#How many samples have yellow flags
length(yellow.flags[,1])

#Percent of samples with yellow flags
length(yellow.flags[,1])/length(sstats.2[,1])

#Samples for the zero hour scan can reasonably be expected to have different weights over the four scan locations because
#water was dripping from sample trays. Remove them
#How many samples exceed a gram difference
blue.flags <- sstats_noZero[sstats_noZero$data.range > 5,]

#How many samples have yellow flags
length(blue.flags[,1])

#Percent of samples with yellow flags
length(blue.flags[,1])/length(sstats.2[,1])

#Worst offenders: samples after 0-hr scan with range of weights exceeding 10 grams
red.flags <- sstats_noZero[sstats_noZero$data.range > 10,]

#How many samples have yellow flags
length(red.flags[,1])

#Percent of samples with yellow flags
length(red.flags[,1])/length(sstats.2[,1])
#3.3% of samples are in this category

#Start reviewing them
red.flags

setwd("C:/Users/jcronan/Box/FERA-UW/Research/3DFuels/Lidar_Fuel_Moisture/QAQC_Fuel_Moisture")
write.csv(red.flags, file = "red_flags.csv")
write.csv(sstats.2, file = "fuel_moisture_sample_group_qaqc.csv")

###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
#Error checking
#Diagnose and fix errors in red flags object

##### 1
#Biggest source of errors is LLP 12-hr scan.

#First, sample 29 has 3 samples for the 12 hour scan and 5 samples in the 8-hr scan. Highly likely one of the 8hrs
#was mislabeled and should be a 12-hr sample.
weights.llp[weights.llp$Sample == 29 & weights.llp$scan %in% c(8,12),]
#Yep, there are two sample 29s in the 8-hr scan at the 3 hr position.
#Based on record time and weight the sample with weight 635.82 should be in the 12-hr scan group.
weights.llp$scan[561] <- "12"

weights.12 <- weights.llp[weights.llp$scan == "12",]

#Okay, now that's fixed let's figure out all the samples that are outliers.
er <- 2#error range in grams

sstats.llp <- as.data.frame(weights.12 %>%
                            group_by(Sample) %>%
                            summarise(median = round(median(gross.weight),2)))

llp.median <- expand.grid(location = c(3,6,9,12), median = sstats.llp$median)
diff.12 <- weights.12$gross.weight - llp.median$median
error.12 <- weights.12[diff.12 > er,]
error.12 <- data.frame(error.12, dev = diff.12[diff.12 > er])

os.1 <- sstats.llp[order(sstats.llp$median),]
oe.1 <- error.12[order(error.12$gross.weight),]

matches <- list()
for(i in 1:length(oe.1$date))
{
  matches[[i]] <- os.1[os.1$median >= (oe.1$gross.weight[i] - er) & os.1$median <= (oe.1$gross.weight[i] + er),]
}



