#first visit for the day
actbud_split2<- actbud.sub4 %>%
  separate (freq, c("Date", "Time"), " ")

actbud.sub7<- actbud_split2%>%
  group_by(Date)%>%
  slice(1)%>%
  ungroup()

actbud.sub7$TagId<-as.factor(actbud.sub7$TagId)
str(actbud.sub7$TAIR)

actbud.sub7.gamm <- gamm(beep_count.y ~ s(TAIR, k=20), family = nb, data=actbud.sub7, random=list(TagId=~1))
plot(actbud.sub7.gamm$gam, shade = TRUE, shade.col = "yellow", seWithMean = TRUE,
     ylab= "Pond Visitation", xlab= "Air Temperature", shift = coef(actbud.sub7.gamm$gam)[1])
summary(actbud.sub7.gamm$gam)


#First visit of the bird on each day
resampled2<- resampled%>%
  separate(freq, c("Date", "Time")," ") %>%
  group_by(TagId, Date)%>%
  arrange(Time)%>%
  slice(1)%>%
  ungroup()

# https://stackoverflow.com/questions/52644455/group-variable-based-on-continuous-values

dat <- data.frame(time = c(1950:1955, 1986:1988, 1990:1992))  #your part needs to be in posixct

dat$visiting_period <- cumsum(!c(TRUE, diff(dat$time)<=15)) #set threshold to <30 or whatever

dat %>%
  
  group_by(visiting_period) %>%
  
  summarize(time_duration = max(time)-min(time))

resampled$freq<- as.POSIXct(resampled$freq , format="%Y-%m-%d %H:%M:%S")
resampled$visiting_period <- cumsum(!c(TRUE, diff(resampled$freq)<=900)) #threshold 900second (15mins)
resampled%>%
  group_by(visiting_period) %>%
  summarize(time_duration = max(freq)-min(freq))





