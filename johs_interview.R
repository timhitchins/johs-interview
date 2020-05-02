## Assessing Racial Equity in the HMIS
###################################
## This script is a prep helper for 
## the creation of the accompanying 
## R Markdown in this directory
## 
## Date 5/6/2020
## Contact: Tim Hitchins
## Email tim@mappingaction.org
##################################

######################
## Options & Installs
#####################
options(stringsAsFactors = FALSE)

## Package installs 
if(!require(pacman)) install.packages(pacman); library(pacman)
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(lubridate)) install.packages("lubridate")
p_load(tidyverse, readxl, lubridate)

################
## Read Phase
###############
path <- "./data.xlsx" ##realtive to current wd 
sheet_names <- excel_sheets(path)

##read in the sheets
sheets <- lapply(sheet_names, read_excel, path = path)
client <- sheets[[1]]
ee <- sheets[[2]]
gc(rm(sheets))##clean-up

################
## Wrangle Phase
###############

##function to recode to NA
createNAs <- function(val){
 toNAs <- c("Client refused", "Data not collected", "Client doesn't know") 
  if(val %in% toNAs) return(NA)
  else return(val)
}

## convert vals to NAs over entire df /
## convert to tibble
client <- client %>% 
  apply(MARGIN = c(1, 2), FUN = createNAs) %>%
  as_tibble()

ee  <- ee %>% 
  apply(MARGIN = c(1, 2), FUN = createNAs) %>%
  as_tibble()


##create age variable
client$client_age <- ymd_hms(client$`Date of Birth`)

######################
## Visulaization Phase
#####################

##visualize data

