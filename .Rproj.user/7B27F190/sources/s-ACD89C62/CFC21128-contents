rm(list=ls()) # Clear memory

library(httr)
library(jsonlite)
library(RCurl)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(plyr)

### Source functions----
galib <- getURL("https://raw.githubusercontent.com/UWAMEGFisheries/globalarchive-api/master/R/galib.R", ssl.verifypeer = FALSE)
eval(parse(text = galib))

functions <-getURL("https://raw.githubusercontent.com/GlobalArchiveManual/globalarchive-query/master/Functions.R", ssl.verifypeer = FALSE)
eval(parse(text = functions))

### Set your working directory ----
work.dir<-"C:/GitHub/BIOL4408"
setwd(work.dir)

## Save directory names ----
download.dir<-paste(work.dir,"Downloads",sep="/")
tidy.dir<-paste(work.dir,"Tidy data",sep="/")

### Setup your query ----
# API
API_USER_TOKEN <- "fba67680725035ebaf72e60db290933dc878454e2dd8506ec75085b1"

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Project (space replaced with +) ----
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"BIOL4408+Marine+Ecology+field+trip"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Metadata files ----
metadata <-list.files(path=download.dir,
                      recursive=T,
                      pattern="Metadata.csv",
                      full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  glimpse()

## Count fles ----
count <-list.files(path=download.dir,
                   recursive=T,
                   pattern="Count.csv",
                   full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  glimpse()

## Length files ----
length <-list.files(path=download.dir,
                    recursive=T,
                    pattern="Length.csv",
                    full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  glimpse()

# Add metadata to count ----
dat<-as.data.frame(count)%>%
  tidyr::complete(nesting(project,campaignid,sample,family,genus,species),legal.sublegal,fill = list(count = 0))%>%
  left_join(.,metadata)

## Save maxn and length files ----
setwd(tidy.dir)
write.csv(maxn,paste(name,"maxn.csv",sep="_"),row.names = FALSE)