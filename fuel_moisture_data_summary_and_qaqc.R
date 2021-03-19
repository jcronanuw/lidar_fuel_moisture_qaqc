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
target <- drive_get("https://docs.google.com/spreadsheets/d/1PFLwhnQT0MWADytQnyVDfg7utLOEv0DiczoX4micFGQ/edit#gid=1004095978", 
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

#Dry Weights
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1QJgsKv3ArD5V7lIuMS77XX83NShDouah/edit#gid=698536073", 
                    team_drive = "2021 Lidar fuel moisture")

#Download a copy of the fuel moisture file
drive_download(target, 
               type = "xlsx", 
               path = "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/dry_weights.xlsx", 
               overwrite = T)

#load data
dry.wts <- read_excel("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/dry_weights.xlsx")
dry.wts <- write_csv(dry.wts, "C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/dry_weights.csv")
dry.wts <- read.csv("C:/Users/jcronan/Documents/GitHub/lidar_fuel_moisture_qaqc/GoogleDrive_TempDownloads/dry_weights.csv")

###############################################################################################################################
#################################################     PINE BOARDS          ####################################################
###############################################################################################################################
weights.pb <- filter(weights, sample.type=="PB")

#dry.p <- dry.wts %>%
# filter(Sample.Type == "PB") %>%
#  rename(dry.wt = "Dry.Weight", Sample = "Sample.Number") %>%
#  select(Sample, dry.wt)

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

sn <- 16
data <- pb.fm[pb.fm$Sample == sn,]
data.pb <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.pb, fm, color = location)) +
  geom_point(aes(fill=data.pb),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

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

#dry.cc <- dry.wts %>%
# filter(Sample.Type=="CC") %>%
#  rename(dry.wt="Dry.Weight", Sample="Sample.Number")%>%
# select(Sample, dry.wt)

#adds dry weights from earliest set of oven dry scans
dry.cc <- weights.cc %>%
  filter(scan=="od" & time == "842") %>%
  select(Sample, net.wt) %>%
  mutate(dry.wt = net.wt) %>%
  select(Sample, dry.wt)

weights.ccd <- inner_join(weights.cc, dry.cc, by = "Sample")

cc.fm <- weights.ccd %>%
  mutate(fm = ((net.wt-dry.wt)*100/dry.wt))

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

sn <- 16
data <- cc.fm[cc.fm$Sample == sn,]
data.cc <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.cc, gross.weight, color = location)) +
  geom_point(aes(fill=data.cc),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

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

#creates table of dry weights for each sample
#dry.df <- dry.wts %>%
#  filter(Sample.Type=="DF") %>%
#  rename(dry.wt="Dry.Weight", Sample="Sample.Number")%>%
#  select(Sample, dry.wt)

#adds dry weights from earliest set of oven dry scans
dry.df <- weights.df %>%
  filter(scan=="od" & time == "1015") %>%
  select(Sample, gross.weight, tare.wt)%>%
  mutate(dry.wt = gross.weight - tare.wt) %>%
  select(Sample, dry.wt)

#joins dry weight and gross weights tables
weights.df <- inner_join(weights.df, dry.df, by = "Sample")

#calculates fuel moistures
df.fm <- weights.df %>%
  mutate(fm =((net.wt-dry.wt)*100/dry.wt)) %>%
  filter(fm>0) 

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

sn <- 16
data <- df.fm[df.fm$Sample == sn,]
data.df <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.df, gross.weight, color = location)) +
  geom_point(aes(fill=data.df),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

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

pp.fm <- weights.pp %>%
  mutate(fm =((net.wt-dry.wt)*100/dry.wt)) 

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

sn <- 32
data <- pp.fm[pp.fm$Sample == sn,]
data.pp <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.pp, fm, color = location)) +
  geom_point(aes(fill=data.pp),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

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
sro.fm <- weights.sro %>%
  mutate(fm =((net.wt-dry.wt)*100/dry.wt))

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

sn <- 6
data <- sro.fm[sro.fm$Sample == sn,]
data.sro <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "od"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.sro, fm, color = location)) +
  geom_point(aes(fill=data.sro),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

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
llp.fm <- weights.llp %>%
  mutate(fm =((net.wt-dry.wt)*100/dry.wt))

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
data.llp <- factor(data$scan, ordered = T, levels = c("0.0", "1.0", "2.0", "4.0", "8.0", "12.0", "24.0", "36.0", "48.0", "72.0", "120.0", "OD"))
data$location <- as.factor(data$location)

# Plot
#"fm" | "gross.weight"
sp <- data %>%
  ggplot(aes(data.llp, fm, color = location)) +
  geom_point(aes(fill=data.llp),size=3) +
  geom_line(aes(group = location),color="black")
sp + scale_color_manual(values=c("green", "blue", "orange", "purple"))

llp.fm[llp.fm$scan == "48.0",]
str(data)


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



