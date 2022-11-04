#### Creating summary file of individual visits to node array around pond ####

actbud <- read.csv("./actbud.csv")
###NEED to filter out all incomplete/incorrect TagIds
tags <- actbud.sub %>% group_by(TagId) %>% tally() %>% filter(n > 55) #filter error or rare detections
actbud.clean <- filter(actbud.sub, actbud$TagId %in% tags$TagId)


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
    if ((as.numeric(actbud.clean[row+1,3])-as.numeric(actbud.clean[row,3]))>(6*60)){ #need to make numeric, otherwise it reduces it to days rather than minutes
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

write.csv(actbud.summary,file = "./actbud.summary.csv")

unique(actbud_summary$TagId)
