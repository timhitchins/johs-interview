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
if (!require(janitor))
  install.packages("janitor")
if (!require(scales))
  install.packages("scales")
if(!require(psych))
  install.packages("psych")
if(!require(pander))
  install.packages("pander")
p_load(tidyverse, readxl, lubridate, janitor, scales, psych, pander)

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

##ANSWERING SPECIFIC QUESTION:
##1. Is there a difference between POC and White clients in duration of enrollment
##create data
duration_race <-
  client_ee %>% group_by(client_id, race2) %>%
  summarise(avg_duration = mean(duration), num_enrollments = n())

#simple stats
IQR(duration_race$avg_duration) ##difference of its upper and lower quartiles. It is a measure of how far apart the middle portion of data spreads in value.
mean(duration_race$avg_duration)
median(duration_race$avg_duration)
sd(duration_race$avg_duration)

## t-test and confidence intervals
w <- duration_race[duration_race$race2=="White",]
poc <-duration_race[duration_race$race2=="Person of Color",]
t.test(w$avg_duration, poc$avg_duration) ##no difference

## confidence intervals white
describe(w$avg_duration)
n_w <- 21981
xbar_w <- 0.8
sd_w <- 1.89
alpha <- 0.05 ##Confidence of 95%

(xbar_w + qnorm(alpha/2)*(sd_w/sqrt(n_w))) #0.7750146
(xbar_w - qnorm(alpha/2)*(sd_w/sqrt(n_w))) #0.8249854

## confidence intervals poc
describe(poc$avg_duration)
n_poc <- 16810
xbar_poc <- 0.63
sd_poc <- 1.29
alpha <- 0.05 ##Confidence of 95%
(xbar_poc + qnorm(alpha/2)*(sd_poc/sqrt(n_poc))) #0.6104991
(xbar_poc - qnorm(alpha/2)*(sd_poc/sqrt(n_poc))) #0.6495009

##visualize it
durationRacePlot <- ggplot(duration_race[duration_race$avg_duration < 2.27,], aes(avg_duration)) +
  geom_density(aes(group=race2, colour=race2, fill=race2), alpha=0.3) +
  geom_vline(xintercept = .18, col = "coral4", size=1) + # median
  ggtitle("Avg. Duration of Enrollment by Client Race") +
  ylab("density") +
  xlab("duration in years") +
  geom_text(aes(label=".18", y=1, x=.18), color="black", vjust=-1)
durationRacePlot 
##

##only using completed enrollment cycles
duration_race_filt <-
  client_ee %>% filter(!is.na(exit_date)) %>%
  filter(!is.na(race2)) %>%
  group_by(client_id, race2) %>%
  summarise(avg_duration = mean(duration), num_enrollments = n())


duration_race_filt$log10_duration <- log10(duration_race_filt$avg_duration)
#simple stats
IQR(duration_race_filt$avg_duration) ##difference of its upper and lower quartiles. It is a measure of how far apart the middle portion of data spreads in value.
mean(duration_race_filt$avg_duration)
median(duration_race_filt$avg_duration)
sd(duration_race_filt$avg_duration)

## t-test and confidence intervals
duration_race_filt
t.test(log10_duration ~ race2, data=duration_race_filt, alternative="two.sided") ##no difference
aov(log10_duration ~ race2, data=duration_race_filt)
# ## confidence intervals white
# describe(w$avg_duration)
# n_w <- 21981
# xbar_w <- 0.8
# sd_w <- 1.89
# alpha <- 0.05 ##Confidence of 95%
# 
# (xbar_w + qnorm(alpha/2)*(sd_w/sqrt(n_w))) #0.7750146
# (xbar_w - qnorm(alpha/2)*(sd_w/sqrt(n_w))) #0.8249854
# 
# ## confidence intervals poc
# describe(poc$avg_duration)
# n_poc <- 16810
# xbar_poc <- 0.63
# sd_poc <- 1.29
# alpha <- 0.05 ##Confidence of 95%
# (xbar_poc + qnorm(alpha/2)*(sd_poc/sqrt(n_poc))) #0.6104991
# (xbar_poc - qnorm(alpha/2)*(sd_poc/sqrt(n_poc))) #0.6495009

durationRacePlotFilt <-  ggplot(duration_race_filt, aes(log10(avg_duration))) +
  geom_density(aes(group=race2, colour=race2, fill=race2), alpha=0.3) +
  # geom_vline(xintercept = .18, col = "coral4", size=1) + # median
  ggtitle("Avg. Duration of Enrollment by Client Race") +
  ylab("density") +
  xlab("log10 of duration in years for completed enrollments") 
# + geom_text(aes(label=".18", y=1, x=.18), color="black", vjust=-1)
durationRacePlotFilt
## 2. Is there a diffence between white and poc exit_reason
##Chi Sq Tests
reasons <- unique(client_ee$exit_reason)
reasons <- reasons[!is.na(reasons)]
client_ee %>%  select(race2, exit_reason) %>%
  table() %>%
  chisq.test()


##find sig differences
for(i in reasons){
  print(i)
  result <- client_ee %>% filter(exit_reason == i) %>% 
    select(race2, exit_reason) %>%
    table() %>%
    chisq.test() %>%
    pander()
  print(result$p.value)
}




###testing
x <- lubridate::interval(ymd("2005-11-01"), ymd("2016-10-31"))
names(ee)
unique(ee$exit_reason)
