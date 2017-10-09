### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###            PLAYTIMES AND FRUSTRATIONS               ###
###       A SAD CHRONICLE OF GRADUATE STUDENT LIFE      ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



#### MISC STUFF THAT'S ACTUALLY HELPFUL ####

#pull random cells from a df column to play with ####
test <- locs$Date[sample(1:length(locs$Date), 50,
                         replace=FALSE)]
test


#### ADEHABITAT ####

# checking out how kuds and overlap are calcd and stored
data(puechabonsp)
View(puechabonsp)
hm <- puechabonsp$relocs; View(hm)
ud <- kernelUD(hm[,1])    
ver <- getverticeshr(ud, percent=95)
verdf <- as.data.frame(ver)  



#### APPLY FUNCTION ####

# playstuff from gist.github.com/dsparks/3706541

# Generate random data:
allCountries <- LETTERS[1:10] # these are just characters, 10 of them
allYears <- 1990:2012 # and these are just integers, 23 of them

# oh cool
# this makes a df of all posible combinations, in longform
# for all letters for 1990, all letters and 1991...
myData <- expand.grid(allCountries, allYears)
str(myData)
colnames(myData) <- c("Country", "Year")
myData$variable1 <- rnorm(nrow(myData))
myData$variable2 <- rnorm(nrow(myData))
head(myData)



# OLD CODE - defining seasons ####    
alllocs <- rawlocs %>%
  # format date
  within(Date <- as.Date(Date, "%Y-%m-%d")) %>%
  # add month, year, season, and HRYear (match Dec from previous calendar year with subsequent Jan and Feb)
  mutate(Month = months(Date, abbreviate = TRUE),
         Year = format(as.Date(Date, format = "%Y-%m-%d"), "%Y"),
         Season = ifelse(Month == "Dec" | Month == "Jan" | Month == "Feb", "Winter", 
                         ifelse(Month == "Mar" | Month == "Apr" | Month == "May", "Spring",
                                ifelse(Month == "Jun" | Month == "Jul" | Month == "Aug", "Summer",
                                       "Fall"))),
         HRYear = ifelse(Month == "Dec", as.numeric(Year)+1, as.numeric(Year)))
#### REMOVING LOCS DURING CAPTURE DATES ####

# cut this code because there was some issue with missing() in max()
# and in invesstigating it i realized it made more sense to remove the
# smaller number of locations than to retain the larger number, obvs


# remove locations during captures and indivs with <1yr of data
for(i in 1:nindivs) {
  elk <- indivs[[i]]
  elklocs <- subset(allindivlocs, AnimalID == elk) %>%
    filter(Date > CaptureDate+1)
  if(max(elklocs$Month == 12)) {modlocs <- rbind(modlocs, elklocs)}
}



# figuring out whether i need to give a shit about the above warning    
f <- function(x) {
  if(missing(x)) {
    print("yup")
  }
}

# it should have found... let's see how many...

allindivlocs$Date[1]
any(allindivlocs$Date == allindivlocs$CaptureDate)
length(which(allindivlocs$Date == allindivlocs$CaptureDate))
any(allindivlocs$Date < allindivlocs$CaptureDate+1)
length(which(allindivlocs$Date < allindivlocs$CaptureDate+1))    
length(which(allindivlocs$Date < allindivlocs$CaptureDate))  
2283075-2279847
# ...yeah, 2,283,075 locations
# ok that worked.
# so it's probably easier to remove those than to retain the others, duh...



#### IDENTIFYING POPULATIONS, YEARS, AND INDIVIDUALS OF INTEREST ####

# i.e., remember that time when you fucked it all up and had to start over
# and you kept your failings here for posterity
# and so you could paste out the few things that were good?


#### RAW DATA FROM ACCESS DATABASE ####

collardays <- sqlQuery(channel, paste("select * from GPS_Herd_TimePeriod"))
rawcap <- sqlQuery(channel, paste("select * from CaptureInfo_allelk"))
rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))


#### DATA PER POPULATION ####


# herds that have at least 1 and 2 yrs of collar data ####

n365 <- filter(collardays, CollarDays >= 365)
length(unique(n365$Herd)) # 19 herds with at least 1 year of data for at least 1 elk

n730 <- filter(collardays, CollarDays >= 730)
length(unique(n730$Herd)) # 10 herds with at least 2 yrs of data for at least 1 elk
# since moving to 2 yrs of data cuts number of herds nearly in half, prob will only use 1 yr


# indivduals in each herd with 1 or 2 years of data ####

i365 <- n365 %>%
  group_by(Herd) %>%
  count(nIndiv = n_distinct(AnimalID)) %>%
  ungroup() 
# range 1 - 48 (11 have >= 10)

i730 <- n730 %>%
  group_by(Herd) %>%
  count(nIndiv = n_distinct(AnimalID)) %>%
  ungroup()     
# yeeeeah, definitely not getting more than 1 year per herd


# herds with at least 1 yr of data (to filter from larger db) ####

herds <- data.frame(Herd = unique(n365$Herd))


# check whether we're losing herds with collardays in the 300s (but below 365)

hm <- collardays %>%
  filter(CollarDays >299 & CollarDays <= 365) %>%
  anti_join(hm, n365, by = "Herd")
# excellent, not losing any herds in the 300s 
# making 315 the generic cutoff because that's the fewest days between
# a capture event and the subsequent winter
# (so all indivs should have a chance to return to winter range)


# populations and years of interest ####

popnyrs <- collardays %>%
  filter(CollarDays >= 315) %>%
  group_by(Herd) %>%
  summarise(FirstDate = min(MinOfDate),
            LastDate = max(MaxOfDate),
            nIndiv = n_distinct(AnimalID)) %>%
  ungroup() %>%
  filter(nIndiv >= 10) %>%
  mutate(Yr1 = as.numeric(substr(FirstDate, 0, 4)),
         Yr2 = Yr1+1,
         YrEnd = as.numeric(substr(LastDate, 0, 4))) %>%
  arrange(nIndiv) %>%
  filter(YrEnd > 2006) # no NDVI data prior to 2005
write.csv(popnyrs, file = "popns-yrs.csv", row.names = F)

herds <- data.frame(Herd = popnyrs$Herd)
write.csv(herds, file = "herds.csv", row.names = F)

sum(yrs$nIndiv)
nrow(yrs)
# 457 individuals across 15 populations :) (but incls males etc)


# summary capture information per population of interest ####

# capture dates per popn (to define start date of analysis for indivs)
capdates <- rawcap %>%
  filter(!is.na(CaptureDate),
         Sex == "F") %>%
  semi_join(herds, by = "Herd") %>%
  mutate(CapYr = substr(CaptureDate, 0, 4)) %>%
  group_by(Herd, CapYr) %>%
  summarise(FirstCapDate = min(CaptureDate),
            LastCapDate = max(CaptureDate),
            nIndiv = n_distinct(AnimalID)) %>%
  ungroup()
write.csv(capdates, file = "popn-capdates.csv", row.names=F)

# capture dates only for indivs of interest in popns of interest
capindivs <- rawcap %>%
  filter(!is.na(CaptureDate)) %>%
  semi_join(herds, by = "Herd") %>%
  filter(Sex == "F") %>%
  select(AnimalID, CollarID, Herd, CaptureArea, CaptureDate, Age, Age_Type, GPSdata)
write.csv(capindivs, file = "prelim-capindivs.csv", row.names=F)


#### DATA PER INDIV ####


# read in & format data created above (if skipping from raw data to here) #

popnyrs <- read.csv("popns-yrs.csv")
herds <- read.csv("herds.csv")
capindivs <- read.csv("prelim-capindivs.csv") %>%
  within(CaptureDate <- as.Date(CaptureDate)) %>%
  mutate(CapYr = as.integer(substr(CaptureDate, 0, 4)))
capdates <- read.csv("popn-capdates.csv") %>%
  within(FirstCapDate <- as.Date(FirstCapDate)) %>%
  within(LastCapDate <- as.Date(LastCapDate))
capdata <- left_join(capindivs, capdates, by = c("Herd", "CapYr"))
rawlocs <- rawlocs %>%
  within(Date <- as.Date(rawlocs$Date),
         Time <- strftime(Time, format="%H:%M:%S"))

# only consider females in popns of interest #
allindivlocs <- rawlocs %>%
  semi_join(herds, by = "Herd") %>% # only popns of interest
  filter(Sex == "F") %>% # only females
  select(-c(Herd, CollarID)) %>% # avoid duplicate columns
  right_join(capdata, by = "AnimalID") %>% # add capture info
  mutate(CapYr = substr(CaptureDate, 0, 4)) %>% # capture yr
  filter(Date <= paste0(CapYr, "-12-31")) %>%
  mutate(Month = as.numeric(substr(Date, 6, 7)))


# only keep data from females with full year of locations
# (i.e., at least one location in  the winter following capture)
# and remove locations during and 1 day after capture

# pre-loop setup
indivs <- as.factor(unique(allindivlocs$AnimalID))
nindivs <- length(indivs)
rmlocs <- allindivlocs[0,]



#### * in progress * ####

# identify locations to remove
# retain all from capture year at least 1 day post-capture
# 
rmlocs <- allindivlocs[0,]    
for(i in 1:nindivs) {
  elk <- indivs[[i]]
  elklocs <- subset(allindivlocs, AnimalID == elk) %>%
    filter(Date < CaptureDate+1)   
  rmlocs <- rbind(rmlocs, elklocs)
}

rmindivs <- data.frame(AnimalID = NA)
for(i in 1:nindivs) {
  elk <- indivs[[i]]
  elklocs <- subset(allindivlocs, AnimalID == elk) 
  if(max(elklocs$Month == 12) & n_distinct(elklocs$Month > 8)) {rmindivs[i] <- elk}
}
# need to fix above - it'll get confused about rmindivs[i] if criteria not met



#### figuring out Madison herd had a FUCKING TRAILING WHITESPACE ####
unique(capdates$Herd)
wtf <- filter(capdates, Herd == "Madison")
a <- unique(capdates$Herd)
a[5]
wtfff <- filter(capdates, Herd == a[5])
test <- capdates 
test$Herd <- trimws(test$Herd)
eh <- filter(test, Herd == "Madison")



#### CHECKING THINGS OUT ####

# making sure you're only keeping one year of locs per indiv
# sanity check
sc <- locs %>%
  group_by(AnimalID) %>%
  mutate(plz = n_distinct(Year))
unique(sc$plz)


# capture dates and number individuals per herd per year

hm <- capdates %>%
  mutate(nCapDays = LastCapDate-FirstCapDate)
efk <- subset(capdata, Herd == "East Fork")
wfk <- subset(capdata, Herd == "West Fork")

# testcode: pulling month from posix date [in dplyr pipe]
dats <- sample(indivs$Date, 10)
mos <- dats$month

# any december captures? (to define year1 properly)
unique(as.Date(collardays$MinOfDate)) # whoa, yep, even november.


#### duplicated animalids in capture data ####

test <- data.frame(AnimalID = cap$AnimalID[duplicated(cap$AnimalID)])
hm <- semi_join(cap, test, by = "AnimalID")
# oh duh, some individuals wer recaptured
# just keep first instance then

test <- cap[order(cap$AnimalID, cap$CaptureDate),]
test2 <- test[!duplicated(test$AnimalID),]


#### playing with subsetting dates ####

# determining whether to use posixct or as.date

test <- data.frame(Date = sample(allcowlocs$Date, 10))
t2 <- filter(test, Date > 2)
# oh just kidding, just do by month since multiple years



### ### ### ### ### ### ### ##
#### FOR HOME RANGE STUFF ####
### ### ### ### ### ### ### ##


# POPN don't have at least 3 relocs per popn?! ####

test <- popnlocs %>%
  group_by(Herd) %>%
  summarise(nLocs = n_distinct(Latitude))
# wtf, should be plenty of relocations, at least 2K/pop

# plotted and visually verified lots of relocs do exist
kuds <- kernelUD(spdf.sp)
plot(kuds)
#ok, worsk with all locs but doesn't work split by popn
#wonder if bc retining info for excluded popns?

# turned out to be because filtered out population data were still retained
# just had to re factor() it to keep only the pops of interest



#### getting rid of ski hill elk ####

# can't do this because it also removes NAs
cap <- filter(rawcap, CaptureArea != "Ski Hill") 

# fixedcap without ski hill removed
  # 1368 obs
length(unique(fixedcap$AnimalID)) 
#1264 animal ids down to 1264 - correct


# seeing whether they're still included in popn hr estimation
# or whether kde just has that wide a buffer
length(unique(popnlocs$AnimalID))
#


# INDIV NAs in coords

test <- locs
any(is.na(test$Latitude))
# nas were successfully removed
length(unique(test$AnimalID))
# 356, as expected

test2 <- allcowlocs
any(is.na(test2$Latitude))
# nas were successfully removed
length(unique(test2$AnimalID))
# 512 individuals in full cow dataset
# (incls those who did not make it thru full yr)



#### trying to programmatically define column to use for hr est'n ####
    
    #what i want to define
    spdf.sp[,3] 
    ?which
