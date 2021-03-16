library(tidyverse)

#load data

weights <- read.csv("G:/Shared drives/2021 Lidar fuel moisture/Data_results/Fuel moisture_weights.csv", header=T)

tare <- read.csv("G:/Shared drives/2021 Lidar fuel moisture/Methods_Study Plan/drying.trays.csv", header=T)

dry.wts <- read.csv("G:/Shared drives/2021 Lidar fuel moisture/Methods_Study Plan/Dry Weights.csv", header = T)

############ Pine boards
weights.pb <- filter(weights, sample.type=="PB")

#dry.p <- dry.wts %>%
 # filter(Sample.Type == "PB") %>%
#  rename(dry.wt = "Dry.Weight", Sample = "Sample.Number") %>%
#  select(Sample, dry.wt)

#adds dry weights from earliest set of oven dry scans
dry.p <- weights.pb %>%
  filter(scan=="od" & time == "1104") %>%
  select(Sample, gross.weight) %>%
  mutate(dry.wt = gross.weight) %>%
  select(Sample, dry.wt)
  
#add dry weights for each board
weights.pb <- inner_join(weights.pb, dry.p, by = "Sample")

#calculate fuel moisture
pb.fm <- mutate(weights.pb, fm = ((gross.weight-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
#pb.fm <- is.numeric(pb.fm$scan)

pb.fm2 <- pb.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)
  
sample.pb <- pb.fm$Sample
scan.pb <- factor(pb.fm$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "72", "120", "od"))

PB <- ggplot(data=pb.fm, aes(x=scan.pb, y=fm))+
  geom_point(color=sample.pb, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle ("Pine boards")+
  #scale_y_continuous(breaks=seq(0,100,10))+
  ylim(0,300)+
  xlab("Scan time (hour)")
PB 

######## Cheese cloth
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
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.c <- cc.fm$Sample
scan.c <- factor(cc.fm$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "72", "od"))

CC <- ggplot(data=cc.fm, aes(x=scan.c, y=fm))+
  geom_point(color = sample.c, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Cheesecloth")+
  scale_y_continuous(breaks=seq(0,320,50))+
  ylim(0,300)+
  xlab("Scan time (hour)")

CC 

##############DF
weights.df <- filter(weights, sample.type == "DF")

tare.df <- tare %>%
  filter (tare.wt > 0 & Sample < 16) %>%
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
  mutate(fm =((net.wt-dry.wt)*100/dry.wt))

#df sample name/number/scan/angle
df.fm2 <- df.fm %>%
  mutate(name = paste(sample.type, Sample, sep = "")) %>%
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.d <- df.fm$Sample
scan.d <- factor(df.fm$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "od"))

DF <- ggplot(data=df.fm, aes(x=scan.d, y=fm))+
  geom_point(color = sample.d, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Douglas Fir")+
   ylim(0,300)+
  #scale_y_continuous(breaks=seq(0,300,50))+
   xlab("Scan time (hours)")

DF

#######PP
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
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.p <- pp.fm$Sample
scan.p <- factor(pp.fm$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "120", "od"))

PP <- ggplot(data=pp.fm, aes(x=scan.p, y=fm))+
  geom_point(color = sample.p, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Pondo Pine")+
  #scale_y_continuous(breaks=seq(0,320,50))+
  #ylim(0,300)+
  xlab("Scan time (hours)")+
  theme(legend.position = "right")

PP

#######SRO
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
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.s <- sro.fm$Sample
scan.s <- factor(sro.fm$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "od"))

SRO <- ggplot(data=sro.fm, aes(x=scan.s, y=fm))+
  geom_point(color = sample.s, size=3)+
  ylab("Fuel moisture (%)")+
  ggtitle("Southern red oak")+
  scale_y_continuous(breaks=seq(0,300,50))+
  xlab("Scan time (hours)")

SRO

#######LLP
weights.llp <-   filter(weights, sample.type == "LLP")

weights.llp <- read.csv("C:/Users/dnemens/Documents/3D Fuels/llp.oops.csv")

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
  mutate(scanh = if_else(scan!="od", paste(scan, "H", sep =""), "od")) %>%
  mutate(locationm = paste(location, "M", sep ="")) %>%
  mutate(name2 = paste(name, scanh, locationm, sep = "-")) %>%
  select(name2, fm)

sample.l <- weights.llp$Sample
scan.l <- factor(weights.llp$scan, ordered = T, levels = c("0", "1", "2", "4", "8", "12", "24", "36", "48", "od"))

LLP <- ggplot(data=llp.fm, aes(x=scan.l, y=fm))+
  geom_point(color = sample.l, size=4)+
  ylab("Fuel moisture (%)")+
  ggtitle("Long leaf pine")+
 #scale_y_continuous(breaks=seq(0,320,50))+
  ylim(0,300)+
  xlab("Scan time (hours)")

LLP

############ 
#create df of all sample names and fuel moistures
mashup <- rbind(pb.fm2, cc.fm2, df.fm2, pp.fm2, llp.fm2, sro.fm2)

write.csv(mashup, "G:/Shared drives/2021 Lidar fuel moisture/Data_results/FinalFuelMoistureData.csv", row.names = F)

##############stack multiple ggplots
library("gridExtra")
all <- grid.arrange (PB, CC, DF, PP, SRO, LLP, nrow = 3)

ggsave(all, filename = "G:/Shared drives/2021 Lidar fuel moisture/Data_results/FM.Plots.pdf", device = "pdf", width = 7.7, height = 8, dpi = 300)

