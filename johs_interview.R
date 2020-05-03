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

###############################
## INSTALL & OPTIONS PHASE
##############################
options(stringsAsFactors = FALSE)

## Package installs
if (!require(pacman))
  install.packages(pacman)
library(pacman)
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(readxl))
  install.packages("readxl")
if (!require(lubridate))
  install.packages("lubridate")
if (!require(plotly))
  install.packages("plotly")
if (!require(janitor))
  install.packages("janitor")
if (!require(scales))
  install.packages("scales")
p_load(tidyverse, readxl, lubridate, plotly, janitor, scales)

################
## READ PHASE
###############
path <- "./data.xlsx" ##realtive to current wd
sheet_names <- excel_sheets(path)

##read in the sheets
##standardize names
sheets <- lapply(sheet_names, read_excel, path = path)
client <- clean_names(sheets[[1]])
ee <- clean_names(sheets[[2]])
gc(rm(sheets))##clean-up

################
## WRANGLE PHASE
###############
PULL_DATE <-
  "2019-06-30 00:00:00" ## this is not clear in the instructions

## STEP 1: CREATE HELPER FUNCTIONS
##################################
##function to recode to NA
createNAs <- function(val) {
  toNAs <-
    c("Client refused", "Data not collected", "Client doesn't know")
  if (val %in% toNAs)
    return(NA)
  else
    return(val)
}

##helper for handling dates var
dateTrimmer <- function(x) {
  if (is.na(x)) {
    return(x)
  }
  else {
    date <- str_split(x, " ")
    return(date[[1]][1])
  }
}

##calculate the year interval between two dates
calcInterval <- function(date1, date2, type, digits) {
  period <- interval(ymd(unlist(lapply(
    date1, dateTrimmer
  ))), ymd(unlist(lapply(
    date2, dateTrimmer
  )))) / years(1)
  
  if (type == "floor") {
    return(floor(period))
  } else if (type == "round") {
    return(round(period, digits))
  }
  
}

createPOC <- function(x) {
  poc <-
    c(
      "Asian",
      "Black or African American",
      "American Indian or Alaska Native",
      "Native Hawaiian or Other Pacific Islander"
    )
  if (x %in% poc) {
    return("Person of Color")
  } else{
    return(x)
  }
}

## heler to fix exit date NA vals in ee
## to correspond to 2019-06-2019 00:00:00
exitDateFixer <- function(x) {
  if (is.na(x)) {
    return(PULL_DATE) ##see constant
  } else{
    return(x)
  }
}

### STEP 2: CLEAN DATA
###########################
## convert "Client refused",
## "Data not collected",
## "Client doesn't know" vals
## to NAs over entire dfs /
## then convert to tibble
client <- client %>%
  apply(MARGIN = c(1, 2), FUN = createNAs) %>%
  as_tibble()

ee  <- ee %>%
  apply(MARGIN = c(1, 2), FUN = createNAs) %>%
  as_tibble()

## PREP CLIENT DATA COLS
#########################
##calculate age of client
client$client_age  <-
  calcInterval(client$date_of_birth, PULL_DATE, "floor")


client$race2 <- unlist(lapply(client$race, createPOC))

## PREP EE DATA COLS
#####################
##fix the exit NAs to reflect the PULL_DATE
ee$exit_date_recode <- unlist(lapply(ee$exit_date, exitDateFixer))

##create duration var in ee table
ee$duration <-
  calcInterval(ee$entry_date, ee$exit_date_recode, "round", 2)

## JOIN TABLES
######################
## PK in client is client_id
## PK in ee is ee_id

## new joined table
client_ee <- client %>% inner_join(ee, by = c("client_id"))


######################
## Visulaization Phase
#####################
##basic analysis
##NOTE approx 70% mulco is white 


##Demographic data
## age
agePlot <- ggplot(client, aes(client_age)) +
  geom_histogram() +
  ggtitle("Client Age Distribution on 6/30/2019" ) +
  xlab("Age") +
  ylab("Count")

## add median age vline
medianAge <- median(client$client_age, na.rm = TRUE)
## add median v-line
agePlot +
  geom_vline(xintercept = medianAge, col = "black", size=2)+
  geom_text(aes(label=medianAge, y=20, x=medianAge), color="white", vjust=-1)


##race
racePlot <- ggplot(client, aes(race)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = race)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Client Race by Percentage") +
  ylab("percentage") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  guides(fill=guide_legend(title="Race"))
racePlot

##white vs POC
racePlot2 <- ggplot(client, aes(race2)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = race2)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Client Race by Percentage") +
  ylab("percentage") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  guides(fill=guide_legend(title="Race"))
racePlot2

##race gender
raceGender <- ggplot(client, aes(race2)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = gender), position = "dodge") +
  scale_y_continuous(labels = percent) +
  ggtitle("Client Gender by Race") +
  ylab("percentage") +
  xlab("race") +
  # theme(
  #   axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.title.x = element_blank()
  #   ) +
  guides(fill=guide_legend(title="gender"))
raceGender

##age race plot here
ageRace <- ggplot(client, aes())





###testing
x <- lubridate::interval(ymd("2005-11-01"), ymd("2016-10-31"))
