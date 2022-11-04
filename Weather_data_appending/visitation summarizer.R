#### Creating summary file of individual visits to node array around pond ####

install.packages("rattle")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("rpart")
install.packages("gganimate")
library(dplyr)
library(lubridate)
library(data.table)
library(lme4)
library(nlme)
library(arm)
library(lattice)
library(effects)
library(okmesonet)
library(suncalc)
library(scales)
library(vioplot)
library(randomForest)
library(party)
library(weathermetrics)
library(ggplot2)
library(rpart.plot)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(gganimate)

actbud.summary <- read_csv("/Users/gamageperera/Desktop/Motus/Motus/actbud.summary.csv")
View(actbud.summary)
setwd("/Users/gamageperera/Desktop/Motus/Motus")
weather.data <- read_csv("/Users/gamageperera/Desktop/Motus/Motus/weather_data.csv")
weather.data

### Open activity budget file ###

actbud <- read.csv("./actbud.csv")
###NEED to filter out all incomplete/incorrect TagIds
tags <- actbud %>% group_by(TagId) %>% tally() %>% filter(n > 55) #filter error or rare detections
actbud.clean <- filter(actbud, actbud$TagId %in% tags$TagId)


#Add a local time and date column for each bin
actbud.clean$freq <- as.POSIXct(actbud.clean$freq, tz="UTC")  #Make Posix time-CHECK TIME ZONE!!!
actbud.clean$DTL = actbud.clean$freq
attributes(actbud.clean$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
actbud.clean$YMDL <- as_date(actbud.clean$DTL) #gives local (Oklahoma) ymd date

#make sure the list is sorted by TagId and THEN date
actbud.clean <- actbud.clean[with(actbud.clean,order(TagId,freq)),]

actbud.summary <- actbud.clean[1,c("TagId","DTL")]
actbud.summary$freq <- NA ## add 3rd column for "end", rename later

j = 1
for (row in 1:nrow(actbud.clean)){
  if (actbud.clean[row,"TagId"] == actbud.clean[row+1,"TagId"]){
    if ((as.numeric(actbud.clean[row+1,3])-as.numeric(actbud.clean[row,3]))>(9*60)){ #need to make numeric, otherwise it reduces it to days rather than minutes
      actbud.summary <- rbind(actbud.summary,actbud.clean[row+1,c("TagId","DTL","freq")])
      if((actbud.clean[row-1,"TagId"] == actbud.clean[row,"TagId"])){
        actbud.summary[j,3] <- actbud.clean[row,c("DTL")]} #this avoids the problem of transitions between individuals 
      j = j + 1
    } #else {print (paste(row, "no"))}
  } else {actbud.summary[j,3] <- actbud.clean[row,c("DTL")]
  actbud.summary <- rbind(actbud.summary,actbud.clean[row+1,c("TagId","DTL","freq")])
  j = j + 1
  print (paste(row, "next individual"))}
}

colnames(actbud.summary) <- c("TagId","visit_start","visit_end")
actbud.summary$visit_end <- as.POSIXct(actbud.summary$visit_end, origin = "1970-01-01", tz = "America/Chicago")

actbud.summary$duration <- actbud.summary$visit_end - actbud.summary$visit_start
actbud.summary$YMDL <- as_date(actbud.summary$visit_start) #gives local (Oklahoma) ymd date #could probably just keep YMDL from earlier

actbud.summary$span <- NA
actbud.summary$visit_num <- NA

k = 1
for (row in 1:nrow(actbud.summary)){
  actbud.summary[row,"visit_num"] <- k
  if (actbud.summary[row,"TagId"] == actbud.summary[row+1,"TagId"]){
    if (actbud.summary[row,"YMDL"] == actbud.summary[row+1,"YMDL"]){
      actbud.summary[row+1,6] <- as.numeric(actbud.summary[row+1,"visit_start"]) - as.numeric(actbud.summary[row,"visit_end"])
      actbud.summary[row+1,7] <- k + 1
      k = k + 1
    } else { k = 1}
  } else { k = 1}
}

#write.csv(actbud.summary,file = "./actbud.summary.csv")

#actbud.summary <- read.csv("./actbud.summary.csv")[,2:8] #excludes the artefact column of row numbers

#### Append weather data to summary file ####

#make sure the list is sorted by TIME, since the matching loop requires an ascending series to work properly
w <- w[with(w,order("TIME")),]

actbud.summary$TAIR <- NA
actbud.summary$RELH <- NA
actbud.summary$PRES <- NA
actbud.summary$WSPD <- NA
actbud.summary$SRAD <- NA


for (row in 1:nrow(actbud.summary)){  #This is clunky, but for the size of data we have, it gets the job done in ~10min
  for (q in 1:nrow(w)){
    if ((as.numeric(actbud.summary[row,"visit_start"]) - as.numeric(w[q,"TIME"]))<300){
      print(paste(row,q))
      actbud.summary[row,"TAIR"] <- w[q,"TAIR"]
      actbud.summary[row,"RELH"] <- w[q,"RELH"]
      actbud.summary[row,"PRES"] <- w[q,"PRES"]
      actbud.summary[row,"WSPD"] <- w[q,"WSPD"]
      actbud.summary[row,"SRAD"] <- w[q,"SRAD"]
      break
    }
  }
}

#Add sunrise sunset to the data set

sundata <-
  getSunlightTimes(
    date = seq.Date(as.Date("2021-01-19"), as.Date("2021-03-28"), by = 1),
    keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
    lat = 36.4957,
    lon = -102.6495,
    tz = "America/Chicago"
  )

actbud.summary <- read_csv("/Users/gamageperera/Desktop/Motus/Motus/actbud.summary.csv")
actbud.summary$visit_start <- as.POSIXct(actbud.summary$visit_start, tz = "America/Chicago") #Need to set time zone if reading file back in
actbud.summary$visit_end <- as.POSIXct(actbud.summary$visit_end, tz = "America/Chicago") 

###Append sundata to summary file###
#left join
actbud.summary<-left_join(x=actbud.summary, y= sundata, by=c("YMDL"="date"))

## Weather patterns, using data from Boise City (BOIS) Station year 2021 on dates 1/19/21-3/28/21

beginTime = "2021-01-19 00:00:00"
endTime = "2021-03-28 23:55"


years = c(2021)
wdsum_total = NULL
mass = 16
tuc = (6.130*(log10(mass))) + 28.328 #upper critical limit
w=NULL

for(i in years){ #beginning of weather data loop
  beginTime = paste0(i,"-01-19 00:00:00", sep = "")
  endTime = paste0(i,"-03-28 23:55", sep = "")
  
  stid <- "BOIS"
  
  #Obtain data from OK Mesonet website
  #DAVG- average dew point temperature, HAVG - Average relative humidity, WSPD - Average wind speed
  updatestn() #get latest information on mesonet stations
  okstations = updatestn() #save latest information into okstations
  wok <- okmts(begintime=beginTime,
               endtime=endTime, 
               variables = c("TAIR", "RELH","PRES", "WSPD", "SRAD"),
               station="BOIS",  
               localtime = TRUE, missingNA = TRUE, mcores = FALSE) #Need to download the data into UTC
  w = rbind(wok,w)
}

w$TIME <- as.POSIXct(w$TIME, tz = "America/Chicago")

w$DATE = as.POSIXct(substr(w$TIME,0,10))
w$MONTH = as.factor(substr(w$TIME,6,7))
w$DAY = as.factor(substr(w$TIME,9,10))
w$YMDL <- as_date(w$TIME) #gives local (Oklahoma) ymd date
w$UTC <- as.POSIXct(w$TIME, tz = "UTC")
attributes(w$UTC)$tzone = "UTC" #provides datetime as UTC, just in case

# Save the file for future opening
write.csv(w,file = "./weather_data.csv")

w <- read.csv("./weather_data.csv")[,2:14] #excludes the artefact column of row numbers
w$TIME <- as.POSIXct(w$TIME, tz = "America/Chicago") #Need to set time zone if reading file back in

#function lag and summary functions to make the new column for yesterday sunset
sundata$sunset.yesterday<-lag(sundata$sunset)

#time has to be greater than yesterday's sunset, smaller than today's sunrise
#arbitrary number for night times because you cannot use the date for grouping and summarizing
#actual grouping and summarizing by minimum temperature

#left_join weather and sundata
w$YMDL<-as_date(w$YMDL, tz=NULL)
w.test<-left_join(x=w, y=sundata, by=c("YMDL"="date"))
w.test<-w.test%>%
  filter(!is.na(sunset.yesterday))

# getting nighttime values only
#w.test$timefromsunset<- (as.numeric(w.test$TIME)- as.numeric(w.test$sunset.yesterday))/(60*60*24)
w.test<-w.test%>% 
  group_by(DATE)%>%
  filter(TIME<sunrise |
           TIME >sunset)

#break is day time arbitrary number
w.test$night.visit <- cumsum(!c(TRUE, diff(w.test$TIME)<=5)) #set threshold to <5mins 

#minimum temp by group_by
w.test2<-w.test%>%
  group_by(night.visit)%>%
  summarise(mintemp=min(TAIR), minRELH=min(RELH), maxRELH=max(RELH), maxtemp=max(TAIR),
            DATE=max(DATE))

actbud.summary.test<-left_join(x=actbud.summary, y=w.test2, by=c("YMDL"="DATE"))
write.csv(actbud.summary.test,file = "/Users/gamageperera/Desktop/Motus/Motus/actbud.summary.test.csv")

#Check time zones
attr(actbud.summary.test$visit_start, "tzone")
attr(actbud.summary.test$sunrise, "tzone")
attr(actbud.summary.test$visit_end, "tzone")

#changing timezones
actbud.summary.test$visit_start <- force_tz(actbud.summary.test$visit_start, tzone = "America/Chicago")
actbud.summary.test$visit_end <- force_tz(actbud.summary.test$visit_end, tzone = "America/Chicago")

#Creating a column for time since sunrise
actbud.summary.test$tsinceRise <- (as.numeric(actbud.summary.test$visit_start) - as.numeric(actbud.summary.test$sunrise))/(60*60)

#remove column
actbud.summary.test <- actbud.summary.test[,!names(actbud.summary.test) %in% c("tsincerise")]


# Save the file for future opening
write.csv(actbud.summary.test,file = "./actbud.summary.test.csv")

#Creating a column for time since sunrise
#actbud.summary.test$tsincerise<- with(actbud.summary.test, difftime(sunrise, visit_start, units="hours"))
#actbud.summary.test$tsinceRise <- (as.numeric(actbud.summary.test$sunrise) - as.numeric(actbud.summary.test$visit_start))/(60*60)

#### Append sun data to summary file #### Might not need this loop.
#make sure the list is sorted by date, since the matching loop requires an ascending series to work properly
#sundata <- sundata[with(sundata,order(sunrise)),]
#actbud.summary <- actbud.summary[with(actbud.summary,order(visit_start)),]

#actbud.summary$sunrise <- NA
#actbud.summary$sunriseEnd <- NA
#actbud.summary$sunset <- NA
#actbud.summary$sunsetStart <- NA

#for (row in 1:nrow(actbud.summary)){  
  #for (q in 1:nrow(sundata)){
   # if ((as.numeric(actbud.summary[row, "visit_start"]) - as.numeric(sundata[q, "sunrise"]))<(60*60*24)){
      #print(paste(row,q))
     # actbud.summary[row,"sunrise"] <- sundata[q,"sunrise"]
     # actbud.summary[row,"sunriseEnd"] <- sundata[q,"sunriseEnd"]
      #actbud.summary[row,"sunset"] <- sundata[q,"sunset"]
      #actbud.summary[row,"sunsetStart"] <- sundata[q,"sunsetStart"]
     # break
   # }
 # }
#}

#actbud.summary$sunrise <- as.POSIXct(actbud.summary$sunrise, tz = "America/Chicago", origin = "1970-01-01")
#actbud.summary$sunriseEnd <- as.POSIXct(actbud.summary$sunriseEnd, tz = "America/Chicago", origin = "1970-01-01")
#actbud.summary$sunset <- as.POSIXct(actbud.summary$sunset, tz = "America/Chicago", origin = "1970-01-01")
#actbud.summary$sunsetStart <- as.POSIXct(actbud.summary$sunsetStart, tz = "America/Chicago", origin = "1970-01-01")

#actbud.summary$tsinceRise <- (as.numeric(actbud.summary$visit_start) - as.numeric(actbud.summary$sunrise))/(60*60)

#Plotting first visit over the study period
str(actbud.summary.test)
ggplot(data = actbud.summary.test, TagId == 61780778, aes(x=YMDL, y = visit_start, color=visit_start)) +
  geom_point() +
  facet_wrap(facets = vars(TagId))
ggplot(data=actbud.summary.test, aes(x=sunrise, y=visit_start, group=TagId, colour=TagId)) +
  geom_line()
ggplot(data=actbud.summary2, aes(x=sunrise, y=Time, group=TagId, colour=TagId)) +
  geom_violin()
ggplot(data=actbud.summary2, aes(x=mintemp, y=duration, group=TagId, colour=TagId)) +
  geom_point()
actbud.summary2$duration<-as.factor(actbud.summary2$duration)
ggplot(actbud.summary2, aes(x=sunrise, y=Time))+ geom_violin()
#GAMM
actbud.summary.test$TagId<-as.factor(actbud.summary.test$TagId)
actbud.summary.gamm <- gamm(visit_num ~ s(tsinceRise, k=10), family = nb, data=actbud.summary.test, random=list(TagId=~1))
summary(actbud.summary.gamm$gam)
summary(actbud.summary.gamm$lme)
gam.check(actbud.summary.gamm$gam)
plot(actbud.summary.gamm$gam, shade = TRUE, shade.col = "lightgreen", seWithMean = TRUE, 
     ylab= "Pond Visitation", xlab= "Time since sunrise", shift = coef(actbud.summary.gamm$gam)[1], pch=50)

#pond visitation with min temp
actbud.summary.test$TagId<-as.factor(actbud.summary.test$TagId)
actbud.summary.gamm2 <- gamm(visit_num ~ s(mintemp, k=50), family = nb, data=actbud.summary.test, random=list(TagId=~1))
plot(actbud.summary.gamm2$gam, shade = TRUE, shade.col = "lightgreen", seWithMean = TRUE, 
     ylab= "Nb of visists", xlab= "Overnight minimum temperature", shift = coef(actbud.summary.gamm2$gam)[1], pch=50)

actbud.summary.gamm3 <- gamm(duration ~ s(mintemp, k=50), family = nb, data=actbud.summary2, random=list(TagId=~1))
summary(actbud.summary.gamm3$gam)
actbud.summary.glmm<-lmer(duration~mintemp+TAIR, actbud.summary2, random=list(TagId))
#Linear model
plot(actbud.summary.gamm3$gam, shade = TRUE, shade.col = "lightgreen", seWithMean = TRUE, 
     ylab= "Duration of the first visit", xlab= "Overnight minimum temperature", shift = coef(actbud.summary.gamm2$gam)[1], pch=50)

#max temp on the day
#just keep the maximum number of visit e.g: 4th visit or 5th visit or 1st visit
#temp at time of arrival
#min temp vs time of arrival of the first visit
#3D plotting : relationship between overnight temp (1st variable) and time of first visit (2nd variable) and temp at the first visit (3rd variable)

#selecting only the first visit
actbud.summary2<-actbud.summary.test%>%
  filter(visit_num==1)

#Extracting Time from visit_start
actbud.summary2$Time<-format(actbud.summary2$visit_start, format="%H:%M:%S")
actbud.summary2$visit<-as.numeric(actbud.summary2$visit_start)
str(actbud.summary$YMDL)

#adding dates that birds were not present
ggplot(data=actbud.summary2, aes(x = YMDL, y = duration, color = duration))+
  geom_point() +
  facet_wrap(facets = vars(TagId))
sp+scale_color_gradient(low = "blue", high = "orange")

#mean duration of birds
statvisits<-actbud.summary.test%>%
  group_by(TagId, visit_num)%>%
  summarise(
    count=n(),
    meanduration=mean(duration, na.rm=TRUE),
    sdduration=sd(duration, na.rm=TRUE),
    seduration=sdduration/sqrt(count),
    ci95lower=meanduration-seduration*1.96,
    ci95upper=meanduration+seduration*1.96
  )
statvisits$meandurationmin<-(statvisits$meanduration)/60
#mean profiles in the same graphs
mp <- ggplot(statvisits, aes(x=visit_num, y=meandurationmin, color=TagId, shape=TagId)) + 
  geom_line() +
  geom_point(size = 2.5) + 
  labs(color="Tag ID", x="Visit number", y = "Mean visit duration (min)") +
  scale_x_continuous(breaks=seq(1,10,1), limits = c(0.5,10.5)) +
  ylim(0, 35) + 
  scale_shape_manual(values=c(8,9,10,11,12,13,14,15,16, 17,18,19,20))+
  theme_bw()
mp + geom_errorbar(aes(ymin = ci95lower, ymax = ci95upper), width = 0.5)
mp

mp+
  geom_point(aes(group=seq_along(visit_num)))+
  transition_reveal(visit_num)
anim_save("Mean visit duration at different visits")
#mean duration of each day for each bird(duration/nb of visits)
durationstats<-actbud.summary.test%>%
  group_by(TagId, YMDL)%>%
  summarise(
    count=n(),
    meanduration=mean(duration, na.rm=TRUE),
    sdduration=sd(duration, na.rm=TRUE),
    seduration=sdduration/sqrt(count),
    ci95lower=meanduration-seduration*1.96,
    ci95upper=meanduration+seduration*1.96
  )
durationstats$meandurationmin<-(durationstats$meanduration)/60
#mean profiles in the same graphs
mp1 <- ggplot(durationstats, aes(x=YMDL, y=meandurationmin, color=TagId, shape=TagId)) + 
  geom_line() +
  geom_point(size = 2.5) + 
  labs(color="Tag ID", x="Date", y = "Mean visit duration (min)") +
  ylim(0, 35) + 
  scale_shape_manual(values=c(8,9,10,11,12,13,14,15,16, 17,18,19,20))+
  theme_bw()
mp + geom_errorbar(aes(ymin = ci95lower, ymax = ci95upper), width = 0.5)
mp1
mp1+
  geom_point(aes(group=seq_along(YMDL)))+
  transition_reveal(YMDL)
anim_save("Mean visit duration over winter")

#for TagId==61780778
stay<-durationstats%>%
  filter(TagId==61780778)
mp2 <- ggplot(stay, aes(x=YMDL, y=meandurationmin, color=TagId, shape=TagId)) + 
  geom_line() +
  geom_point(size = 2.5) + 
  labs(color="Tag ID", x="Date", y = "Mean visit duration (sec)") +
  ylim(0, 30) + 
  scale_shape_manual(values=c(8,9,10,11,12,13,14,15,16, 17,18,19,20))+
  theme_bw()
mp2 + geom_errorbar(aes(ymin = ci95lower, ymax = ci95upper), width = 0.5)
mp2
mp2+
  geom_point(aes(group=seq_along(YMDL)))+
  transition_reveal(YMDL)
anim_save("mean visit duration of TagId 61780778 over winter")

#mean duration of visits by day
durationstats2<-actbud.summary.test%>%
  group_by(YMDL)%>%
  summarise(
    count=n(),
    meanduration=mean(duration, na.rm=TRUE),
    sdduration=sd(duration, na.rm=TRUE),
    seduration=sdduration/sqrt(count),
    ci95lower=meanduration-seduration*1.96,
    ci95upper=meanduration+seduration*1.96
  )
durationstats2$meandurationmin<-(durationstats2$meanduration)/60
boxplot(meandurationmin~YMDL, data= durationstats2, notch=TRUE,main= "mean duration of birds", xlab="Date", ylab="mean duration(min)")
mp3 <- ggplot(durationstats2, aes(x=YMDL, y=meandurationmin, color=YMDL, shape=YMDL)) + 
  geom_line() +
  geom_point(size = 2.5) + 
  labs(color="YMDL", x="Date", y = "Mean visit duration (min)") +
  ylim(0, 25) + 
  scale_shape_manual(values=c(8,9,10,11,12,13,14,15,16, 17,18,19,20))+
  theme_bw()
mp3
mp3+
  geom_point(aes(group=seq_along(YMDL)))+
  transition_reveal(YMDL)
anim_save("Mean visit duration over winter")




m<-glmer(duration~visit_num+TAIR+RELH+mintemp+minRELH+(1|TagId), data=actbud.summary.test, family = poisson, nAGQ=0)
summary(m)
anova(m, type=3)

fit<- lmer(duration~visit_num+(1|TagId), data=actbud.summary.test)
summary(fit)


modelo<-glmer(duration ~ (1|TagId) + offset(log(visit_num)), family = poisson, data = actbud.summary.test)
ranef(modelo)
randoms <- ranef(modelo, condVar = TRUE)$TagId
variances <- as.numeric(attr(randoms, "postVar")) #We'll use the variances for estimating the confidence intervals. 
res <- data.frame(duration = rownames(randoms), mean_effect = randoms$`(Intercept)`+coef(summary(modelo))[,"Estimate"])
coef(summary(modelo))
res$lower <- res$mean_effect - 2* sqrt(variances) # 2 standard deviation
res$upper <- res$mean_effect + 2* sqrt(variances)
exp(res$mean_effect)
res$mean_effect <- exp(res$mean_effect)*1e6  
res$lower <- exp(res$lower)*1e6
res$upper <- exp(res$upper)*1e6
res$duration <- reorder(res$duration, res$mean_effect, mean)
ggplot(data = res, aes(x=duration, y=mean_effect)) + geom_point() + 
  geom_errorbar(width=.1, aes(ymin=lower, ymax=upper), col="blue") + 
  labs(title="Tag Id", y="duration of the visit", 
       x=NULL, caption="Source: Department of Transport") + theme(axis.text.x = element_text(angle = 20))
### Generalized Linear Mixed Model (GLMM)
glmer(duration ~ visit_num+ (1|TagId), data = actbud.summary.test, 
                         family = MASS::negative.binomial(theta=1.75))
ggplot


#GLMM with negative binomial

actbud.summary.GLMM1 <- glmer(tsinceRise ~ mintemp+ minRELH+ DP+WSPD + (1|TagId), data = actbud.summary.test, 
                         family = MASS::negative.binomial(theta=1.75))
summary(actbud.summary.GLMM1)
r.squaredGLMM(actbud.sub.GLMM)

#decision trees
#relationship between air temperature and relative humidity
correlation2<- cor.test(actbud.summary.test$TAIR, actbud.summary.test$RELH, method = "pearson")
correlation2
cor(na.omit(actbud.summary.test[,c("TAIR", "RELH", "WSPD")]))
cor(actbud.summary.test$TAIR, actbud.summary.test$RELH, method = "pearson", na.rm=TRUE)

#making a column for dew point temperature
actbud.summary.test$DP<- humidity.to.dewpoint(t=actbud.summary.test$TAIR, rh=actbud.summary.test$RELH,temperature.metric = 'celsius')

fit<-ctree(tsinceRise~mintemp+DP+minRELH+WSPD+TagId, data=actbud.summary2, controls = ctree_control(testtype = c("Bonferroni"), mincriterion = 0.85))
plot(fit, main="Conditional Inference Tree for the first visit")

#with mincriterion=
fit2<-ctree(tsinceRise~mintemp+DP+minRELH+WSPD+TagId, data=actbud.summary2, controls = ctree_control(testtype = c("Bonferroni"), mincriterion = 0.9))
plot(fit2, main="Conditional Inference Tree for the first visit")


actbud.summary3<-actbud.summary2%>%
  filter(TagId==61780778)
fit3<-ctree(tsinceRise~mintemp+DP+minRELH+WSPD, data=actbud.summary3, controls = ctree_control(testtype = c("Bonferroni"), mincriterion = 0.5))
plot(fit3, main="Conditional Inference Tree for the first visit for 61780778 ")


ggplot(data=actbud.summary2, aes(x=TAIR, y=mintemp)) +
  geom_point()

mytree<-rpart(duration~mintemp+TAIR+tsinceRise+RELH, data= actbud.summary2, method='class')
mytree

mytree2<-rpart(visit_num~mintemp+TAIR+tsinceRise+RELH, data= actbud.summary.test, method='class')
mytree2
 # plot mytree
 fancyRpartPlot(mytree2, caption = NULL)
 
 #adding missing dates
 ts <- seq.POSIXt(as.POSIXct("2001-09-01 0:00",'%m/%d/%y %H:%M'), as.POSIXct("2001-09-01 0:07",'%m/%d/%y %H:%M'), by="min")
 
 ts <- seq.POSIXt(as.POSIXlt("2001-09-01 0:00"), as.POSIXlt("2001-09-01 0:07"), by="min")
 ts <- format.POSIXct(ts,'%m/%d/%y %H:%M')
 
 df <- data.frame(timestamp=ts)
 
 data_with_missing_times <- full_join(df,original_data)
