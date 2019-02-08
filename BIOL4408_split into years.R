# BIOL4408 Marine Ecology field trip

###### Import lobster density data from gsheet ######
# Clear the workspace--
rm(list=ls())

# load librarys----
library(googlesheets) 
library(tidyr) 
library(dplyr) 
library(forcats) 
library(readr) 
library(ggplot2) 
library(stringr)
library(purrr)

# Set name for study--
study<-"Rottnest.sanctuaries_SVC"

# Set work directory----
work.dir<-"C:/GitHub/BIOL4408"

# Read in the data from gsheet and check it----
gs_ls() #list gsheets you have access to

dat <- gs_title("BIOL4408.lobster.density")%>% #select the gsheet
  gs_read_csv(ws = "lobster.density")%>% #select the worksheet within the workbook
  glimpse()

loc<-gs_title("BIOL4408.lobster.density")%>% #select the gsheet
  gs_read_csv(ws = "locations")%>% #select the worksheet within the workbook
  glimpse()

# Make corrections and re-formating----
dat1<-dat %>%
  dplyr::mutate(status = fct_recode(status,"No-take" = "No-Take"))%>%
  dplyr::mutate(sanctuary = fct_recode(sanctuary,"Parker Point" = "Parker_Pt","Green Island" = "Green_Island","Armstrong Bay" = "Armstrong"))%>%
  dplyr::mutate(site = fct_recode(site,"West Salmon"="West_Salmon_Bay","Salmon Bay" = "eastsalmon","City of York" = "City of York Bay","Ricey Beach" = "Ricey_Bay","Salmon Bay" = "East_Salmon","Little Salmon" = "Little_Salmon", "Parker Point" = "Poc_Reef", "Green Island" = "Green_Island","Parakeet Bay" = "Parakeet_Bay","Little Armstrong" = "Little_Armstrong_Bay","Geordie Bay" = "Geordie_Bay","Mary Cove" = "Mary_Cove","Strickland Bay" = "East_Strickland"))%>%
  filter(!site%in%c("Armstrong Point","Longreach","Rocky Bay","Stark Bay"))%>%
  #filter(!year==2017)%>%#filter out suspicous year  
  droplevels()%>%#remove the levels for the filtered facotrs
  dplyr::mutate(site.new=paste(site,status,sep="."))%>%  # Make a new unique Site namename
  mutate(sample=paste(1:nrow(.)))%>%
  glimpse()

metadata<-dat1%>%
  distinct(sample,year,date,sanctuary,status,site,group,replicate,sampling.location,depth,gps,way.point,time,complexity,algal.cover)%>%
  left_join(loc)

dat.size.class<-dat1%>%
  #mutate_at(vars(starts_with("x")), funs(ifelse(is.na(.),0,.)))%>%
  select(year,date,sanctuary,status,site,group,replicate,sampling.location,depth,gps,way.point,time,latitude,longitude,complexity,algal.cover,site.new,everything())%>%
  gather(key="size.class",value="count",x25,x30,x35,x40,x45,x50,x55,x60,x65,x70,x75,x80,x85,x90,x95,x100,x105,x110,x115,x120,x125,x130,x135,x140,x145,x150)%>%
  select(-c(unsized,legal.unsized,sublegal.unsized))%>%
  mutate(size.class=str_replace_all(.$size.class,c("x"="")))%>%
  mutate(size.class=as.numeric(size.class))%>%
  mutate(Legal.Sublegal=if_else(size.class>75,"Legal","Sublegal"))%>%
  mutate(size.class=as.character(size.class))%>%
  filter(count>0)

dat.unknowns<-dat1%>%
  select(sample,year,date,sanctuary,status,site,group,replicate,sampling.location,depth,gps,way.point,time,latitude,longitude,complexity,algal.cover,site.new,unsized,legal.unsized,sublegal.unsized)%>%
  gather(key="Legal.Sublegal",value="count",unsized,legal.unsized,sublegal.unsized)%>%
  mutate(Legal.Sublegal=str_replace_all(.$Legal.Sublegal,c("legal.unsized"="Legal",
                                                           "subLegal"="Sublegal",
                                                           "unsized"="Unsized")))%>%
  mutate(size.class="Unsized")%>%
  filter(count>0)

dat.lob<-bind_rows(dat.size.class,dat.unknowns)

# Format metadata names for GA ----
names(metadata)<-str_to_title(names(metadata))

# Format metadata for GA ----
metadata.new<-metadata%>%
  rename(Observer=Group)%>%
  mutate(Successful.count="Yes",Successful.length="Yes")%>%
  #mutate(Sample=paste(Year,Date,Sanctuary,Status,Site,Replicate,Observer,sep="."))%>%
  separate(Date,into=c("Year1","Month","Day"))%>%
  mutate(Date=paste(Year1,Month,Day,sep=""))%>%
  mutate(Location="")%>%
  select(-c(Year1,Month,Day))%>%
  separate(Time,into=c("D","Time"),sep=" ")%>%
  replace_na(list(Depth=1,Time="12:00:00"))

# Check for duplicates in metadata ----
duplicate.samples<-metadata.new%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%
  filter(n>1)

# Export Metadata files ----
uniq <- unique(unlist(metadata.new$Year))
for (i in 1:length(uniq)){
  temp <- subset(metadata.new, Year == uniq[i])
  id<-temp$Year
  # Remove columns where all are NA
  temp1 <- temp[,which(unlist(lapply(temp, function(x)!all(is.na(x)))))]
  
  # write file
  write.csv(temp1, file=paste(unique(id),"-01","_",study,"_Metadata.csv",sep=""), quote=FALSE,row.names = FALSE)
}

## Count Formats for GA ----
names(dat.lob)<-str_to_title(names(dat.lob))

count<-dat.lob%>%
  rename(Observer=Group)%>%
  #mutate(Sample=paste(Year,Date,Sanctuary,Status,Site,Replicate,Observer,sep="."))%>%
  group_by(Year,Sample,Legal.sublegal)%>%
  summarise(Count=sum(Count))%>%
  mutate(Family="Palinuridae",Genus="Panulirus",Species="cygnus")%>%
  ungroup()

## Length Formats for GA ----
names(dat.size.class)<-str_to_title(names(dat.size.class))

length<-dat.size.class%>%
  rename(Observer=Group)%>%
  #mutate(Sample=paste(Year,Date,Sanctuary,Status,Site,Replicate,Observer,sep="."))%>%
  rename(Length=Size.class)%>%
  select(Year,Sample,Count,Length,Legal.sublegal)%>%
  mutate(Family="Palinuridae",Genus="Panulirus",Species="cygnus")

## Save Length files ----
uniq <- unique(unlist(length$Year))
for (i in 1:length(uniq)){
  temp <- subset(length, Year == uniq[i])
  id<-temp$Year
  # Remove columns where all are NA
  temp1 <- temp[,which(unlist(lapply(temp, function(x)!all(is.na(x)))))]
  temp1<-temp1%>%
    select(-c(Year))
  # write file
  write.csv(temp1, file=paste(unique(id),"-01","_",study,"_Length.csv",sep=""), quote=FALSE,row.names = FALSE)
}

## Save Count files ----
uniq <- unique(unlist(count$Year))
for (i in 1:length(uniq)){
  temp <- subset(count, Year == uniq[i])
  id<-temp$Year
  # Remove columns where all are NA
  temp1 <- temp[,which(unlist(lapply(temp, function(x)!all(is.na(x)))))]
  temp1<-temp1%>%
    select(-c(Year))
  # write file
  write.csv(temp1, file=paste(unique(id),"-01","_",study,"_Count.csv",sep=""), quote=FALSE,row.names = FALSE)
}

