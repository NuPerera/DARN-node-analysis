#Edited by Nu Perera
#email: nuperera@ou.edu

install.packages('rgdal', type = "source", configure.args=c(
  '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config', 
  '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers', 
  '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

install.packages("raster","datasets", "optimx", "dfoptim", "mgcv", "arm","segmented")
install.packages("boot")
install.packages("quantreg")
install.packages("rgdal")
update.packages("rgdal")
install.packages("rsq")
install.packages("MumIn")
install.packages("rgl")
library(raster)
library(sp)
library(rgdal)
library(rgl)
library(sf)
library(ggplot2)
library(geosphere)
library(dplyr)
library(lubridate)
library(sf)
library(maptools)
library(suncalc) 
library(tidyverse)
library(scales)
library(tidyr)
library(motus)
library(motusData)
library(boot)
library(lme4)
library(quantreg)
library(cowplot)
library(car)
library(MASS)
library(datasets)
library(lme4)
library(optimx)
library(dfoptim)
library(mgcv)
library(arm)
library(nlme)
library(segmented)
library(rsq)
library(MuMIn)
library(lattice)
library(okmesonet)
library(data.table)


setwd("/Users/gamageperera/Desktop/Motus/Motus")
source("data_tools_master/functions/data_manager.R")
source("data_tools_master/functions/localization.R")


###EDIT THESE VALUES
infile <- "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/Winterbirds/tag_detections/Sensorstations/Tumbleweed_working/F21E1A341DB5"
outpath <- "./node_output/"

all_data <- load_data(infile)
beep_data <- all_data[[1]][[1]]
beep_data <- beep_data %>% 
  filter (!is.na(NodeId))
saveRDS(beep_data,file = "./beep_data.rds" )

###looking for a file with the column names NodeId, lat, lng IN THAT ORDER
nodes <- node_file(all_data[[2]][[1]])

nodes$NodeId <- toupper(nodes$NodeId)
write.csv(nodes,file = "./nodes.csv" )
#nodes <- read.csv("./nodes.csv", as.is=TRUE, na.strings=c("NA", ""), strip.white=TRUE) #IF NEEDED
str(nodes)
head(nodes)

beep_data <- beep_data[beep_data$NodeId %in% nodes$NodeId,] #c("326317", "326584", "3282fa", "3285ae", "3288f4") #make sure Node list is consistent
beep_data<- readRDS("beep_data")
unique(beep_data$TagId)
beep_data[!complete.cases(beep_data),]


tags <- read.csv("./data_tools_master/CSV_files_for_node_data_analysis/tags-to-analyze.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters
#filter(Validated ==1)

#tags<-tags[!duplicated(tags$TagId),]
#tags<-tags %>%
#filter(Validated==1)

#EXAMPLE POSSIBLE VALUES
tag_id <- tags$TagId

freq <- c("3 min", "10 min")
#freq<- c("1 min")

max_nodes <- 0 #how many nodes should be used in the localization calculation?
df <- merge_df(beep_df=beep_data, node_df=nodes, tag_id=tag_id, latlng = TRUE)

unique(tags$TagId)
unique(df$TagId)

resampled <- advanced_resampled_stats(beeps = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
#removing N/A tagRSSI
resampled<- resampled %>%
  filter(!is.na(TagRSSI_sd))
resampled
p1 = ggplot(data=resampled, aes(x=freq, y=TagRSSI_max, group=NodeId, colour=NodeId)) +
  geom_line()
p1

#total number of observations for each bird
LOdf<- resampled %>%
  filter(!is.na(TagRSSI_sd)) %>%
  group_by(TagId) %>%
  summarise(number = n())
#three birds with most observations
LOdf_subset<-filter(resampled, TagId %in% c('61780778', '614B7878', '19784C52'))

write.csv(LOdf_subset,file = "./LO_nodes.csv" )

#Activity budget
#If you want to change the date, make sure to have consecutive dates (not just the min and max time)
actbud<- dplyr::select(resampled, TagId, freq, NodeId, TagRSSI_sd, beep_count)
node_sum<-actbud %>%
  group_by(NodeId)%>%
  summarise(number = n())
write.csv(actbud,file = "./actbud.csv" )

#Renaming nodes
actbud$NodeId=gsub("3287DE","W1",actbud$NodeId)
actbud$NodeId=gsub("328C94","W2",actbud$NodeId)
actbud$NodeId=gsub("328C26","W3",actbud$NodeId)
actbud$NodeId=gsub("328591","W4",actbud$NodeId)
actbud$NodeId=gsub("328497","W5",actbud$NodeId)
actbud$NodeId=gsub("32624B","N1",actbud$NodeId)
actbud$NodeId=gsub("328A8E","N2",actbud$NodeId)
actbud$NodeId=gsub("328147","N3",actbud$NodeId)
actbud$NodeId=gsub("325D6E","N4",actbud$NodeId)
actbud$NodeId=gsub("32926A","N5",actbud$NodeId)
actbud$NodeId=gsub("328E58","E1",actbud$NodeId)
actbud$NodeId=gsub("329306","E2",actbud$NodeId)
actbud$NodeId=gsub("325D80","E3",actbud$NodeId)
actbud$NodeId=gsub("325EAA","E4",actbud$NodeId)
actbud$NodeId=gsub("328D8E","E5",actbud$NodeId)
actbud$NodeId=gsub("328B4C","S1",actbud$NodeId)
actbud$NodeId=gsub("325CF3","S2",actbud$NodeId)
actbud$NodeId=gsub("327C23","S3",actbud$NodeId)
actbud$NodeId=gsub("3263FF","S4",actbud$NodeId)
actbud$NodeId=gsub("329934","S5",actbud$NodeId)

actbud

head(resampled)
str(resampled)

datetime1 <- as.POSIXct("2021-01-22 14:45:23", tz="UTC")
actbud.sub<-resampled%>%
  mutate(ts=as.numeric(freq))

#Time to sunriseset
actbud.sub <- actbud.sub %>%
  arrange(TagId, ts) %>%
  mutate(ts = as_datetime(ts + 0, tz = "UTC")) %>%
  dplyr::select (TagId, ts, NodeId, node_lat_mode, node_lng_mode, beep_count) %>%
  filter(!is.na(node_lat_mode), !is.na(node_lng_mode))%>%
  mutate(timeToSunriset(data = actbud.sub, lat="node_lat_mode", lon="node_lng_mode", ts="ts", units="hours")) %>%
  collect() %>%
  as.data.frame()
str(actbud.sub)
write.csv(actbud.sub,file = "./actbud.sub.csv" )

#Time since sunrise
hist(actbud.sub$ts_since_rise[ !actbud.sub$ts_since_rise>12 ], breaks = 24, xlab = "Time since sunrise (h)", 
     ylab = "Frequency", cex.axis=1.5,cex.lab=1.5,  main = "" )

#Time until sunset
hist(actbud.sub$ts_to_set[ !actbud.sub$ts_to_set>12 ], breaks = 24, xlab = "Time till sunset (h)", 
     ylab = "Frequency", cex.axis=1.5,cex.lab=1.5,  main = "" )

hist(actbud.sub$ts_since_rise[!actbud.sub$TagId == 61780778], breaks =24)
plot(actbud.sub$ts_since_rise,actbud.sub$beep_count)

#Plotting each individual separately
ggplot(data = actbud.sub, aes(x=ts, y = beep_count, color=beep_count)) +
  geom_point() +
  facet_wrap(facets = vars(TagId))

actbud.sub$NodeId=gsub("3287DE","W1",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328C94","W2",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328C26","W3",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328591","W4",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328497","W5",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("32624B","N1",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328A8E","N2",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328147","N3",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("325D6E","N4",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("32926A","N5",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328E58","E1",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("329306","E2",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("325D80","E3",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("325EAA","E4",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328D8E","E5",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("328B4C","S1",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("325CF3","S2",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("327C23","S3",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("3263FF","S4",actbud.sub$NodeId)
actbud.sub$NodeId=gsub("329934","S5",actbud.sub$NodeId)

#looking at beep count with sunrise and sunset
ggplot(data = filter(actbud.sub, TagId == 61780778, 
                     ts > ("2021-01-20 15:06:00"),
                     ts < ("2021-03-28 14:39:000")),
       aes(x = ts, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .) 

ggplot(data = filter(actbud.sub, TagId == 61780778), 
       aes(x = ts, y = beep_count)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Beep count") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

#time to sunrise and sunset each month
p2<- ggplot(data = filter(actbud.sub, TagId == 61780778,
                     ts > ("2021-01-20 15:06:00"),
                     ts < ("2021-01-31 22:39:00")), 
       aes(x = ts, y = beep_count)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Beep count") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

p3<- ggplot(data = filter(actbud.sub, TagId == 61780778,
                     ts > ("2021-02-01 14:39:00"),
                     ts < ("2021-02-28 23:21:00")), 
       aes(x = ts, y = beep_count)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Beep count") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

p4<- ggplot(data = filter(actbud.sub, TagId == 61780778,
                     ts > ("2021-03-01 14:57:00"),
                     ts < ("2021-03-28 14:39:00")), 
       aes(x = ts, y = beep_count)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Beep count") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

plot_grid(p2, p3, p4, labels = "AUTO")

#beep counts from sunrise and sunset
ggplot(data=actbud.sub, aes(x = ts_since_rise, y = beep_count, color = beep_count)) +
  geom_line() +
  facet_wrap(facets = vars(TagId))

unique(actbud.sub$TagId)
unique(actbud$TagId)
unique(resampled$TagId)
unique(df$TagId)
unique(tags$TagId)


#beep counts since sunrise and time to sunset
sp<- ggplot(data=actbud.sub, aes(x = ts_since_rise, y = beep_count, color = beep_count))+
  geom_point() +
  facet_wrap(facets = vars(TagId))
sp+scale_color_gradient(low = "blue", high = "orange")
ggsave("Pond visitations since sunrise.png")

sp2<-ggplot(data=actbud.sub, aes(x = ts_to_set, y = beep_count, color = beep_count)) +
  geom_point() +
  facet_wrap(facets = vars(TagId))
sp2+scale_color_gradient(low = "cyan3", high = "coral")

#sum of beep count across all nodes vs time
actbud.sub1<- actbud.sub %>%
  group_by(ts_since_rise)%>%
  summarise(count=n(), beep_count=sum(beep_count))

ggplot(data = filter(actbud.sub1, 
                     ts_since_rise > ("0.161282443"),
                     ts_since_rise < ("11.6068066")),
       aes(x = ts_since_rise, y = count, col = as.factor(count))) + #beep_count instead?
  theme(legend.position="none") + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time since sunrise", y = "Beep count")

#Add day length to the data set
daylength(lat=36.48946, doy= 2021-01-20) #14.65317
daylength(lat=36.48946, doy= 2021-02-20) #14.65447
daylength(lat=36.48946, doy= 2021-03-20) #14.6549


#Generalized additive mixed model
#s specifies the smoother
#bs is basis, bs=tp low rank isotropic smoother
actbud.sub$TagId<-as.factor(actbud.sub$TagId)
actbud.sub.gamm<- gamm(beep_count ~ s(ts_since_rise, fx= FALSE, bs = "tp")+ s(TagId, bs ="re"),
          family = poisson,
          data=actbud.sub)
summary(actbud.sub.gamm$gam)
summary(actbud.sub.gamm$lme)
plot(actbud.sub.gamm$gam)
gam.check(actbud.sub.gamm$gam)
plot(actbud.sub.gamm$gam, rug= TRUE)
plot(actbud.sub.gamm$gam, shade = TRUE)
plot.gam(actbud.sub.gamm$gam)


#using REML (Restricted Maximum Likelihood) method
actbud.sub.gamm1<- gamm(beep_count ~ s(ts_since_rise, fx= FALSE, bs = "tp")+ s(TagId, bs ="re"),
                       data = actbud.sub, method = "REML")
actbud.sub.gamm1<- gamm(beep_count ~ s(ts_since_rise, fx= FALSE, bs = "tp")+ s(TagId, bs ="re"),
                       data = actbud.sub, method = "negative.binomial")
summary(actbud.sub.gamm1$gam)
plot(actbud.sub.gamm1$gam, shade = TRUE)

intervals(actbud.sub.gamm1$lme, which="var-cov")

E.actbud<- resid(actbud.sub.gamm1$gam)
fit.E<-fitted(actbud.sub.gamm1$gam)
plot(x=fit.E, y=E.actbud, xlab="Fitted values", ylab="Residuals")

### Generalized Linear Mixed Model (GLMM)
actbud.sub.GLMM <- glmer(beep_count ~ scale(ts_since_rise) + (1|TagId), data = actbud.sub, 
                         family = MASS::negative.binomial(theta=1.75))
summary(actbud.sub.GLMM)
r.squaredGLMM(actbud.sub.GLMM)

#GLMM with negative binomial
actbud.sub.GLMM1 <- glmer.nb(beep_count ~ scale(ts_since_rise) + (1|TagId), 
                         data = actbud.sub, family = MASS::negative.binomial(theta=1.75))
summary(actbud.sub.GLMM1)


md_temp <- actbud.sub
md_gp <- as.data.frame(predict(actbud.sub.gamm$gam, re.form = TRUE,
                               se = TRUE, type = "response", exclude = s(TagId)))
md_pred <- cbind(md_temp, md_gp)

panal <- ggplot(md_pred) +
  geom_line(aes(ts_since_rise, fit)) +
  geom_ribbon(data = md_pred, aes(x = ts_since_rise, ymin = (fit - 2*se.fit), ymax = (fit + 2*se.fit)), linetype = 2, alpha = 0.2) +
  theme_bw() +
  labs(x = "Time since sunrise", y = "Beep count", title = "Activity since sunrise") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))
panal


ggplot(md_pred) +
  geom_line(aes(ts_since_rise, fit)) +
  geom_ribbon(data = md_pred, aes(x = ts_since_rise, 
                                  ymin = (fit - 2*se.fit), ymax = (fit + 2*se.fit)),
              linetype = 2, alpha = 0.2) +
  theme_bw() +
  labs(x = "Time since sunrise", y = "Activity (Beep count)") +
  facet_wrap("TagId", ncol = 2) +
  theme(strip.text.x = element_text(size = 16), strip.background = element_blank(),
        strip.placement = "outside", axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))

anova(actbud.sub.gamm1$gam, actbud.sub.GLMM1, test="F")


####################################################################################
## Weather patterns, using data from ERIC Station year 2021 on dates 1/19/21-3/28/21

beginTime = paste0(i,"-05-01 00:00:00", sep = "")
endTime = paste0(i,"-06-30 23:55", sep = "")


years = c(2021)
wdsum_total = NULL
mass = 16
tuc = (6.130*(log10(mass))) + 28.328 #upper critical limit
w=NULL

for(i in years){ #beginning of weather data loop
  beginTime = paste0(i,"-01-19 00:00:00", sep = "")
  endTime = paste0(i,"-03-28 23:55", sep = "")
  
  stid <- "KENT"
  
  #Obtain data from OK Mesonet website
  updatestn() #get latest information on mesonet stations
  okstations = updatestn() #save latest information into okstations
  wok <- okmts(begintime=beginTime,
               endtime=endTime, 
               variables = c("TAIR", "RELH","PRES"),
               station="KENT",  
               localtime = TRUE, missingNA = TRUE, mcores = FALSE) #Need to download the data into UTC
  w = rbind(wok,w)
}

w$DT = as.POSIXct(w$TIME, tz = "UTC")
w$DTL = w$DT #Saves datetime into a new vector for local datetime
w$DATE = as.POSIXct(substr(w$TIME,0,10))
w$MONTH = as.factor(substr(w$TIME,6,7))
w$DAY = as.factor(substr(w$TIME,9,10))
w$YMD = w$DATE
attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
w$YMDL <- as_date(w$DTL) #gives local (Oklahoma) ymd date


actbud$freq <- as.POSIXct(actbud$freq, tz="UTC")  #Make Posix time-CHECK TIME ZONE!!!

#Make a dataframe to define time bins.

bins <- data.frame( bin1 = seq.POSIXt(from = min(actbud$freq)-1*60, to = max(actbud$freq)-1*60, by = 5*60),
                    bin2 = seq.POSIXt(from = min(actbud$freq)+4*60, to = max(actbud$freq)+4*60, by = 5*60) )


#Make a new column for time bin

actbud$timeBin <- actbud$freq

#loop through each individual and each data line.

for(id in unique(actbud$TagId)) {
  
  subId <- subset(actbud, actbud$TagId==id)
  
  for(i in nrow(subId)) {
    
    b1 <- which(bins$bin1 <= subId$freq[i] & bins$bin2 > subId$freq[i])
    
    subId$timeBin[i] <- bins$bin1[b1]
    
  }
  
  summed <- aggregate(subId$beep_count, by=list(subId$timeBin), FUN=sum)
  
  summed$ID = id
  
  if(id == unique(actbud$TagId)[1]){
    
    output = summed
    
  } else {
    
    output = rbind(output, summed)
    
  }
  
}


#with actbud.sub

actbud.sub$freq <- as.POSIXct(actbud.sub$freq, tz="UTC")  #Make Posix time-CHECK TIME ZONE!!!

#Make a dataframe to define time bins.

bins <- data.frame( bin1 = seq.POSIXt(from = min(actbud.sub$freq)-1*60, to = max(actbud.sub$freq)-1*60, by = 5*60),
                    
                    bin2 = seq.POSIXt(from = min(actbud.sub$freq)+4*60, to = max(actbud.sub$freq)+4*60, by = 5*60) )

#make a dataframe with 30mins
bins_30 <- data.frame( bin30 = seq.POSIXt(from = min(actbud.sub$freq)-1*60, to = max(actbud.sub$freq)-1*60, by = 30*60))

#Make a new column for time bin
actbud.sub$timeBin <- actbud.sub$freq

actbud.sub$bins <- cut(actbud.sub$ts, breaks= bins$bin1)
#bin with 30mins
actbud.sub$bin_30<- cut(actbud.sub$ts, breaks= bins_30$bin30)

actbud.sub2<- actbud.sub%>%
  group_by(bins)%>%
  summarise(beep_count=sum(beep_count))
    
actbud.sub3 <- merge(actbud.sub, actbud.sub2, by="bins" )

#merging weather data to actbud.sub3
wok<- rename(wok, bins=TIME)
actbud.sub4<-merge(actbud.sub3, wok, by=c("bins"), all.x =TRUE)

actbud.sub3$bins <- as.POSIXct(actbud.sub3$bins, tz="UTC")
actbud.sub4<-left_join(x=actbud.sub3, y=wok)

#Generalized additive mixed model for weather
#s specifies the smoother
#bs is basis, bs=tp low rank isotropic smoother
actbud.sub4$TagId<-as.factor(actbud.sub4$TagId)

actbud.sub4.gamm<- gamm(beep_count.y ~ s(ts_since_rise, fx= FALSE, bs = "tp")+ s(TAIR, bs="tp")+ s(RELH, bs="tp") +s(TagId, bs ="re"),
                        data = actbud.sub4, method = "REML")

summary(actbud.sub4.gamm$gam)
summary(actbud.sub.gamm$lme)
intervals(actbud.sub4.gamm$lme, which="var-cov")
plot(actbud.sub4.gamm$gam, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)

anova(actbud.sub4.gamm$gam)

#gam.check
#my models are not fitting the data well.  
gam.check(actbud.sub4.gamm$gam)



##check
#Colinearity
#with(actbud.sub4, cor(TAIR, RELH))
#cor_matrix<- cor(actbud.sub4)
vif(actbud.sub4.gamm$gam)
vif_values<- vif(actbud.sub4.gamm$gam)
barplot(vif_values, main = "VIF Values",col = 'green',ylim = c(0.0,8.0))
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
interaction(actbud.sub4.gamm$ts_since_rise, actbud.sub4$TAIR)

#sum of activity throughout the day
actbud_split<- actbud.sub4 %>%
  separate (freq, c("Date", "Time"), " ")


actbud.sub5<- actbud_split%>%
  group_by(TagId, Date)%>%
  summarise(beep_count.y=sum(beep_count.y))

#need to calculate mean temp over night
wea <- wok %>%
  mutate(Hour = hour(bins)) %>%
  mutate(Date = date(bins)) %>%
  filter(Hour >= 6 & Hour <= 11) 

wea<- wea%>% group_by(Date)%>%
  summarise_all("mean")%>%
  filter(!is.na(TAIR))

actbud.sub5$Date <- as.Date(actbud.sub5$Date)
actbud.sub6<-left_join(x=actbud.sub5, y=wea)
actbud.sub6<-actbud.sub6%>%
  filter(!is.na(Hour))

actbud.sub6$TagId <- as.factor(actbud.sub6$TagId)


actbud.sub6.gamm<- gamm(beep_count.y ~ s(RELH, fx= FALSE, bs = "tp")+ s(TagId, bs ="re"),
                        data = actbud.sub6, method = "REML")
actbud.sub6.gamm<- gamm(beep_count.y ~ s(TAIR, fx= FALSE, bs = "tp")+ s(RELH, bs ="tp")+ s(TagId, bs ="re"),
                        data = actbud.sub6, method = "REML")

summary(actbud.sub6.gamm$gam)
summary(actbud.sub.gamm$lme)
intervals(actbud.sub.gamm1$lme, which="var-cov")
plot(actbud.sub6.gamm$gam, shade = TRUE)


# Plot
install.packages("plotly")
library(plotly)
plot_ly(x=actbud.sub4$ts_since_rise, y=actbud.sub4$beep_count.y, z=actbud.sub4$TAIR, 
        type="scatter3d", mode="markers", color=actbud.sub4$TagId)

plot_ly(x=actbud.sub4$ts_since_rise, y=actbud.sub4$TAIR, z=actbud.sub4$beep_count.y) %>%
  add_lines(color = actbud.sub$TagId) %>%
  layout(scene = list(
    xaxis = list(title = "Time since sunrise"),
    yaxis = list(title = "Air Temperature"),
    zaxis = list(title = "Beep count")))
    

###################################
actbud.sub$beep_count.t<-actbud.sub$beep_count +1
qqp(actbud.sub$beep_count.t, "norm")
qqp(actbud.sub$beep_count.t, "lnorm") #log normal

#qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. 
nbinom <- fitdistr(actbud.sub$beep_count.t, "Negative Binomial")
qqp(actbud.sub$beep_count.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

gamma <- fitdistr(actbud.sub$beep_count.t, "gamma")
qqp(actbud.sub$beep_count.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#bootstrapping method
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lmer(formula, data=d)
  return(fixef(fit)[2])
}
# bootstrapping with 1000 replications
results <- boot(data=actbud.sub, statistic=rsq,
                R=1000, formula=beep_count~ts_since_rise+(1|TagId))

test<-lme4::lmer(formula=beep_count~ts_since_rise+(1|TagId), data=actbud.sub)
summary(test)
str(test)
fixef(test)[2]

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

#Poisson regression modeling for count data
hist(actbud.sub$beep_count)
mean(actbud.sub$beep_count) #calculate mean #9.24028208
var(actbud.sub$beep_count) #calculate variance #60.5009591
#since the variance is much greater than mean, it suggest that data has an over-dispersion in the model

# extract coefficients from first model using 'coef()'
coef1 = coef(poisson.model)

# extract coefficients from second model
coef2 = coef(poisson.model2)

# extract standard errors from first model using 'se.coef()'
se.coef1 = se.coef(poisson.model)

# extract standard errors from second model
se.coef2 = se.coef(poisson.model2)

# use 'cbind()' to combine values into one dataframe
models.both<- cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1))

# show dataframe
models.both

#I cannot use linear mixed model fit by maximum likelihood because my data is not distributed normally
#therefore, first I need to test whether I can penalized quasilikelihood (PQL) or not
#PQL is a flexible technique that can deal with non-normal data, unbalanced design and crossed random effects.


PQL <- glmmPQL(beep_count.t ~ ts_since_rise + TagId, ~1 | NodeId/TagId, family = gaussian(link = "log"),
               data = actbud.sub, verbose = FALSE)



#determining the threshold of the ts_since_sunrise which birds start to be active
fit<-lm(beep_count~ts_since_rise, data = actbud.sub)
plot(fit, which=1, add.smooth= FALSE) #which=1 argument tells to produce residuals vs fitted values plot
plot(fit, which=1, add.smooth= TRUE) #add.smooth add a line called "loess smooth"
plot(fit, which=2) #plot a normal probability diagnostic
plot(fit, add.smooth=FALSE, which=3) #assess the constant variance assumption

#bartlett.test(beep_count~ts_since_rise, data=actbud.sub)

segmented.ft<-segmented(fit, seg.Z = ~ ts_since_rise, fixed.psi = NULL)
summary(segmented.ft)
plot(actbud.sub$ts_since_rise, actbud.sub$beep_count)
#add segmented regression model
plot(segmented.ft, add=T, col = "red")


###############################
#Not accurate to split freq column to date and time, it split as a chr
#Splitting date and time
actbud_split<- actbud %>%
  separate (freq, c("Date", "Time"), " ")
actbud_split

actbud_split<-resampled%>%
  mutate(ts=as.numeric(freq))
str(actbud_split$Time)

freq_sum2<- actbud_split %>%
  filter(TagId==61780778) %>%
  group_by(Time)


ggplot(data = filter(actbud_split, TagId == 61780778, 
                     Time > ("13:00:00"),
                     Time < ("23:58:00")),
       aes(x = Time, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") + theme(axis.text.x = element_text(angle = 90))
  facet_grid(NodeId ~ .) 

#sum of beep count across all nodes vs time
actbud1<- actbud_split %>%
  group_by(Time)%>%
  summarise(count=n(), beep_count=sum(beep_count))

# actbud1 <- filter(actbud1, 
#        Time > ("13:00:00"),
#        Time < ("23:58:00"))
# actbud1$Time <- hour(hms(actbud1$Time)) #Trying to plot the distribution with more sensible series of tickmarks


ggplot(data = filter(actbud1, 
                     Time > ("13:00:00"),
                     Time < ("23:58:00")),
       aes(x = Time, y = count, col = as.factor(count))) + #beep_count instead?
  theme(legend.position="none") + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count")

actbud2<-actbud1%>%
  filter(Time>"13:00:00")
str(actbud2$Time)

ggplot(data=actbud2, mapping = aes(x=Time, y=beep_count))+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
  scale_x_continuous(breaks = as)
  geom_point()

#Plotting each individual separately
ggplot(data = actbud_split, aes(x=Time, y = beep_count, color = beep_count)) +
  geom_point() +
  facet_wrap(facets = vars(TagId))


#-------------------------------------------------------------------------------------------------------------

#bird 61780778 was pinged at 3287DE the most, which is the closest west node. 32624B (north) and 328A8E(south) are in second

#grouping by freq
freq_sum<- actbud %>%
  filter(TagId==61780778) %>%
  group_by(freq)

ggplot(data = filter(actbud, TagId == 61780778, 
                     freq > ("2021-01-19 19:33:00"),
                     freq < ("2021-03-26 19:18:00")),
       aes(x = freq, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .) 


#for one tagged bird
resampled_bird <- advanced_resampled_stats(beeps = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
#removing N/A tagRSSI
resampled_bird<- resampled_bird %>%
  filter(!is.na(TagRSSI_sd)) %>%
  filter(TagId==52196133) %>%
  filter(freq<"2021-01-22 19:48:00" & freq>"2021-01-21 21:03:00")
resampled_bird
p5 = ggplot(data=resampled_bird, aes(x=freq, y=TagRSSI_max, group=NodeId, colour=NodeId)) +
  geom_line()
p5

##### LOCATION METHODS########
###Example 1: Weighted Average### 

#code from tutorial
locations<- weighted_average(freq[1], beep_data, nodes, node_health=NULL, MAX_NODES=0, tag_id=NULL, 
                             calibrate = NULL, keep_cols = NULL, latlng = TRUE, minRSSI = 0)

#code from script
locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 0, tag_id=tag_id, minRSSI = -105)



#locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 0, tag_id, minRSSI = -123)
#multi_freq <- lapply(freq, weighted_average, beeps=beep_data, node=nodes) 
#export_locs(freq, beep_data, nodes, tag_id, outpath)
######################



###Example 2: Triangulation###
#calibration data frame needs column names: pt, session_id, start, end, TagId, TagLat, TagLng
#start and end need to be in UTC
calibration <- read.csv("/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/node_calibration.csv")
calibration<-subset(calibration, select=c(pt,session_id,start, end, TagId, TagLat, TagLng))
sapply(calibration,class)
#calibration$start<-as.numeric(as.factor(calibration$start))
#calibration$end<-as.numeric(as.factor(calibration$end))
calibration$start <- as.POSIXct(calibration$start, tz="UTC")
calibration$end <- as.POSIXct(calibration$end, tz="UTC")
calibrated <- calibrate(beep_data, calibration, nodes, calibrate = TRUE)
all_data <- calibrated[[1]]
relation <- relate(calibrated[[2]], calibrated[[3]], calibrated[[4]])
out <- triangulate(all_data, distance = relation)
##############################

#this is an example of filtering out locations based on a minimum number of 3 nodes
n <- 1
locations <- locations[locations$unique_nodes > n,]
#now convert to a .csv
locations <- cbind(locations, locations@coords)
CCLO_motus<-locations@data
#change path name where you want .csv to be saved
write.csv(CCLO_motus,file = "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/CCLO_motus.csv" )
library(readr)
CCLO_motus <- read_csv("data_tools_master/CCLO_motus.csv")

time <- "1 day"
move <- as.data.table(locations)[, .(Lat = mean(avg_y), Lon = mean(avg_x), std_lat = sd(avg_y), 
                                     std_lon = sd(avg_x), .N), by = .(cut(freq, time),TagId)] #V = mean(SolarVolts), , 
move$lowlat <- move$Lat - move$std_lat
move$uplat <- move$Lat + move$std_lat
move$lowlon <- move$Lon - move$std_lon
move$uplon <- move$Lon + move$std_lon
move$d <- distVincentyEllipsoid(cbind(move$lowlon, move$lowlat), cbind(move$uplon, move$uplat))
move$d <- (move$d)/1000

nodes_spatial <- nodes
coordinates(nodes_spatial) <- 3:2
crs(nodes_spatial) <- CRS("+proj=longlat +datum=WGS84") 

#boulder_df <- locations[,c("TagId","avg_x","avg_y")]
#coordinates(boulder_df) <- 2:3
#utm <- CRS(paste0("+proj=utm +zone=", locations$zone[1], "+datum=WGS84"))
#crs(boulder_df) <- utm
#boulder_df_geog <- spTransform(locations, proj4string(nodes_spatial))
my_locs <- locations[,1]
locs <- st_as_sf(my_locs)
my_nodes <- st_as_sf(nodes_spatial)

p5 <- ggplot() + 
  #geom_point(data=my_locs, aes(x=long,y=lat))
  #ggmap(ph_basemap) +
  geom_sf(data = locs, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = my_nodes) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 4)
p5




#one tag
locs2<- locs %>%
  filter(TagId == "19552D55")

p6 <- ggplot() + 
  geom_sf(data = locs2, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = my_nodes) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 4)
p6

