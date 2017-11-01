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
# eh fuck it for now; work on this if time later


#### testing removing indivs with <5 relocs (for hr estn) ####

length(unique(indivlocs$AnimalID))
length(unique(locs$AnimalID))
# we lose 54 individuals when we subset out winter locs only, wtf
# thought winter captures... oh maybe there are some in march, ugh

hmcap <- fixedcap %>%
  # only indivs in herds and yrs of interest
  mutate(Year = as.numeric(substr(CaptureDate, 0, 4))) %>%
  semi_join(popnyrs, by = c("Herd", "Year")) %>%
  filter(Sex == "F")

length(unique(hmcap$AnimalID))
length(unique(hmcap$Herd))

test <- indivlocs %>%
  group_by(AnimalID) %>%
  summarise(count = n())




#### creating one polygon covering all popns of interest

## create buffered area covering all population ranges
test <- unionSpatialPolygons(popnhrsall, popnhrsall@data$id)
length(popnhrsall@polygons)
par(mfrow=c(1,2))
plot(popnhrsall); plot(test)
# i think this just unions disparate polygons per each herd
# not all polygons together
# could i change the underlying data of the spdf to id all as same herd tho?
  # i don't think so; just changing data doesn't change length of polygons slot
    # which is exactly what you're trying to change, incidentally...

library(rgeos)
test <- gUnion(popnhrsall, byid = FALSE)
# newp, this requires 2 separate objects to union

require(sp)
test <- aggregate(popnhrsall, FUN = sum, dissolve = TRUE)
popnhrsall

plot(popnhrsall); plot(test)
par(mfrow=c(1,1))
plot(popnhrsall); plot(test, add=T)

## try kde instead of mcp for full popn area
# bc mcp incls a bunch of unoccupied space bt herds
# and kde may buffer better anyway

kde <- kernelUD(spdf.sp, grid = 500)
kdevert <- getverticeshr(kde, percent = 100)
# can't estimate this at 100% bc grid is too small
# says rerun w lgr extent but tried 1 and 100, no diff
# trying diff grid sizes instead
plot(kdevert)
par(mfrow=c(1,1))
plot(kdevert, col = "blue"); plot(test, add=T)
# nope, this loses a bunch of satellite type locations for 

## estimate mcp for all populations
popnhrsallmcp <- mcp(spdf.sp, percent = 100) #,3 = Herd


## ok lets try that again
# grid size 500 is unreasonably large, but 100 was a bit too small

## mcps for all populations
popnhrsallmcp <- mcp(spdf.sp, percent = 100) #,3 = Herd
popnhrsmcp <- mcp(spdf.sp[,3], percent = 100) #,3 = Herd

## trying diff grid sizes
popnhrsallkde <- kernelUD(spdf.sp, grid = 50)
popnhrsoutline <- getverticeshr(popnhrsallkde, percent = 100)
par(mfrow=c(1,1))
plot(popnhrsoutline, col = "red", main = "Grid50"); plot(popnhrsmcp, add = T, col = "blue")


popnhrsallkde <- kernelUD(spdf.sp, grid = 100)
popnhrsoutline <- getverticeshr(popnhrsallkde, percent = 100)
par(mfrow=c(1,1))
plot(popnhrsoutline, col = "red", main = "Grid100"); plot(popnhrsmcp, add = T, col = "blue")


popnhrsallkde <- kernelUD(spdf.sp, grid = 110)
popnhrsoutline <- getverticeshr(popnhrsallkde, percent = 100)
par(mfrow=c(1,1))
plot(popnhrsoutline, col = "red", main = "Grid110"); plot(popnhrsmcp, add = T, col = "blue")
plot(spdf.sp, add = T)

#closest so far?
popnhrsallkde <- kernelUD(spdf.sp, grid = 115)
popnhrsoutline <- getverticeshr(popnhrsallkde, percent = 100)
par(mfrow=c(1,1))
plot(popnhrsoutline, col = "red", main = "Grid115")#; plot(popnhrsmcp, add = T, col = "blue")
plot(spdf.sp, add = T)
# ok, looks like 110 is our cutoff grid size. need to buffer it a bit to incl those few weird locs.



### buffer 1km (jic, and to cover those few outlying locs)
popnbuff <- buffer(popnhrsoutline, dist = 1000)
#x is missing - need spatial pixels df
data("lynxjura")
str(lynxjura)
str(lynxjura$map)
# maybe just create a sppix from the sppoly?
test <- as(popnhrsoutline, "SpatialPixelsDataFrame")
# can't do that, gotta make a new spatial pixels df
pix <- SpatialPixelsDataFrame(xy, popnlocsall, proj4string = latlong)
# error, dimension 1: coordinate intervals are not constant
spix <- as(popnhrsoutline, "SpatialPixels")
# no method for coercing... make it from scratch
  # NEED: SpatialPoints coordinates, proj4string, maybe grid?)
str(xy) #df, need spatialpoints
pts <- spTransform(SpatialPoints(xy, proj4string = latlong), stateplane)
# cool, looks like that might have actually workes
pix <- SpatialPixels(pts, proj4string = stateplane)
# still withthe coordinate intervals
# it's trying to make a grid from the points rather than underlying data

library(rgeos)
wtf2 <- gBuffer(popnhrsoutline, width = 1)
plot(wtf2, col = "blue", main = "width1") 
plot(wtf, add = T, col ="red") #wtf was the original "buffer" that i think wasn't
# ok width1 is the same, must be default, not buffered.

test <- gBuffer(popnhrsoutline, width = 2)
plot(test, col = "blue", main = "width2") 
plot(wtf, add = T, col ="red") 
#mmk that did basically nothing

test <- gBuffer(popnhrsoutline, width = 1000) #i assume width is m
plot(test, col = "blue", main = "width2") 
plot(wtf, add = T, col ="red") # still going to miss those points
plot(spdf.sp, add = T)

test <- gBuffer(popnhrsoutline, byid = TRUE, width = 10000) #byid makes spdf
plot(test, col = "blue", main = "width2") 
plot(wtf, add = T, col ="red") 
plot(spdf.sp, add = T)
# oh my god finally, 3 hours later i have the kde i want
# now see whether to use kde or mcp

plot(popnhrsallmcp, add = T, col = "blue")
popnhrsallmcp@data$area # 8634958
test@polygons[1] #86630053796
plot(spdf.sp, add = T)
plot(popnhrsmcp, add = T, col = "red")

plot(test); plot(spdf.sp, add = T)
# kde ftw

# which sucks bc now we have to remove that pesky little hole in the middle...
test@polygons # goal: index Slot "hole" from here (heh)
test@polygons[[1,2]]
test@polygons[1]
test@polygons[1,]
test@polygons[1,1]
test@polygons[[1]]

# help me stackoverflow, youre my only hope
slotNames(test)
#huh, probably should;ve known i could do that...
slotNames(test@polygons)
#newp
slotNames(test@polygons[1])
#newp
slotNames(test@polygons[[1]])
#my god kristin, you really need to get this double bracket thing down
test@polygons[[1]]@Polygons
test@polygons[[1]]@Polygons[[1]]
#oooooh

kdeoutline <- test@polygons[[1]]@Polygons[[1]]
kdeoutline
plot(kdeoutline)
#fuckkkkk
#ok i think this fails to pull actual spatial data with it (?)

# indexing hole slot, finally :)
test@polygons[[1]]@Polygons[[1]]@hole
test@polygons[[1]]@Polygons[[2]]@hole

# maybe write my own function for once? 
# based on other people's work of course
diehole <- function(x) {
 if (x@hole == TRUE) print("nice work") else x 
}

diehole(test@polygons[[1]]@Polygons[[1]])
diehole(test@polygons[[1]]@Polygons[[2]])
#ok so this correctly identifies and extracts the polygon programmatically
#but you need to drop the hole and keep allll the other data

#ok i think youve sunk too much time into this sorry bro
#just replace the inital polygons slot with only the outline one?

plz <- test
test@polygons <- test@polygons[[1]]@Polygons[[1]]
#god dammit

diehole <- function(x) {
  if (x@hole == TRUE) NULL else x 
}

unholey <- function(x) {
  noHoles <- lapply(x@polygons, diehole)
  # Put the un holey Polygons list back into the @polygons slot 
  x@polygons <- noHoles
  #return the modified SpatialPolygonsDataFrame 
  x
}

eh <- unholey(test)
test@polygons

diehole <- function(x) {
  if (x@hole == TRUE) NULL else x 
}
noHoles <- lapply(test@polygons[[1]]@Polygons, diehole)
test@polygons <- noHoles
test
plot(test)
#KILL MEEEEEEEEEEEEEE

# ok. you have made progress. almost there.
# right now the hole is a NULL
# you want it to FUCKING DIE
# try going back to your original spolydf

test <- gBuffer(popnhrsoutline, byid = TRUE, width = 10000) #byid makes spdf
test[1]
test[1,]
test[[1]]
test[[1,]]
test[[1,2]]
test[1,2]
test@polygons[[1]]@Polygons
test@polygons[1]
test@polygons[2]
names(test)
  # these are only from @data

slotNames(test)
  # these are the slots

slotNames(test@polygons)
#NULL

names(test@polygons)
#also NULL

meh <- SpatialPolygons(list(Polygons(list(test@polygons[[1]]@Polygons[[1]]),ID=1)))
plot(meh)
# jesus h christ that was all i needed?
# granted this isn't a spoly dataframe, just a spoly...
# see if that'll work
writeOGR(z, 
         dsn = "../GIS/Shapefiles/Elk", 
         layer = "DidThisWork", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)
# motherfucker, nope, gotta be a spolydf
a <- list(Polygons(list(test@polygons[[1]]@Polygons[[1]]),ID=1))
test@data
b <- test@data
cmon <- SpatialPolygonsDataFrame(a, b)
# error with your a

# think kristin

howbout <- test[test@polygons <- meh]


# think harder

str(test@polygons)
test@polygons
test@polygons[1]
str(test@polygons[1])
str(test@polygons[1]@Polygons)
  # error here, can't pulla slot from a list
test@polygons[1]@Polygons
  # ditto above
str(test@polygons[[1]])
str(test@polygons[[1,1]]) # predict won't work bc now need to call slot
# woo
str(test@polygons[[1]]@Polygons)

a <- list(Polygons(list(test@polygons[[1]]@Polygons[[1]]),ID=1))

z <- test
z@polygons[[1]]@Polygons <- a   
plot(z)
plot(test)

b <- list(test@polygons[[1]]@Polygons[[1]])
z@polygons[[1]]@Polygons <- b
plot(z)
# hallefuckingluyah


## remove weird -attr data from coords of polygon

test <- buff
test@polygons[[1]]@Polygons[[1]]@coords
str(test@polygons[[1]]@Polygons[[1]]@coords)
test@polygons[[1]]@Polygons[[1]]@coords[1]
test@polygons[[1]]@Polygons[[1]]@coords[[1]]
test@polygons[[1]]@Polygons[[1]]@coords[1,2]

# check whether this one works

## export buffered area of all elk ranges all year
writeOGR(obj = popnhrsoutline, 
         verbose = TRUE,
         dsn = "../GIS/Shapefiles/Elk", 
         layer = "DoesThisWork", 
         driver = "ESRI Shapefile")
# yes,want str of buff to match this

test@polygons[[1]]@Polygons[[1]]@coords
str(test@polygons[[1]]@Polygons[[1]]@coords)
test@polygons[[1]]@Polygons[[1]]@coords[1,]

a <- matrix(as.numeric(test@polygons[[1]]@Polygons[[1]]@coords))
test@polygons[[1]]@Polygons[[1]]@coords <- a
a
str(a)
attr(test@polygons[[1]]@Polygons[[1]]@coords, "dimnames")
attr(test@polygons[[1]]@Polygons[[1]]@coords, "dim")

test@polygons[[1]]@Polygons[[1]]@coords

test <- buff
test@polygons[[1]]@Polygons[[1]]@coords
test@polygons[[1]]@Polygons[[1]]@coords



# attr() help
x <- 1:10
x
attr(x, "dim") <- c(2,5)
x
attr(x, "dimnames") <- list(c("g", "f"))
x
str(x)
# ok you now have an x with attributes including a null

test <- buff
attr(test@polygons[[1]]@Polygons[[1]]@coords, "dim")

# task1: try to remove attr from x
str(x)

z <- unname(x)
z
str(z)

test@polygons[[1]]@Polygons[[1]]@coords <- unname(test@polygons[[1]]@Polygons[[1]]@coords)

#### alright, i fucking give
# have to remove hole in arcmap like some kind of caveman

outline <- list(buff@polygons[[1]]@Polygons[[1]])
buff@polygons[[1]]@Polygons <- outline
plot(buff)
# remove NULL attribute data causing writeOGR to fail
buff@polygons[[1]]@Polygons[[1]]@coords <- unname(buff@polygons[[1]]@Polygons[[1]]@coords)
buff@polygons[[1]]@plotOrder <- as.integer(1)

## fuck that wasn't the fucking problem fucking kill me

# issue is with gBuffer because it fucks up when you have holes
# so either export unbuffered thing into arcmap, remove and buffer there
# or figure out here
#first step: use adehabitat's buffer() instead
# which requires a sppixdf (reason you didn't use before)

z <- as(popnhrsoutline, "SpatialPixelsDataFrame")
# can't convert spoly to spix

test <- popnhrsoutline
test@polygons[[1]]@Polygons[[2]]@hole

for (i in 1:3) {
 print(test@polygons[[1]]@Polygons[[i]]@hole)
}

for (i in 1:3) {
 if (test@polygons[[1]]@Polygons[[i]]@hole == FALSE) print("yes") else print("no")
}

for (i in 1:3) {
  if (test@polygons[[1]]@Polygons[[i]]@hole == TRUE) NULL else test@polygons[[1]]@Polygons[[i]]@hole
}

# lets try another tack
test <- popnhrsoutline
test <- slot(test, "polygons") <- lapply(slot(test, "polygons"),
                                   checkPolygonsHoles)

lapply(slot(test, "polygons"), comment)
# this makes a "comment" of 0 if outline, 1 if hole, big whoop

slot(test, "polygons") <- lapply(slot(test, "polygons"),
                                   "comment<-", NULL)
# removes comments

## ok so take-home is i need to either use createSPComment() or modified code above
# to create a comment about whether something is ahole or not
# before I buffer popnhrsoutline.
# great. perfect. can't wait to waste another few hours on this.



## back at it, now with food etc for optimum brain function

# Warning message:
#   In gBuffer(popnhrsoutline, byid = TRUE, width = 10000) :
#   Polygons object missing comment attribute ignoring hole(s). See function createSPComment.

# i think i need to add an actual attribute that's a comment abt the hole
?createSPComment
  # yes, a comment for the Polygons class identifying polygon to which hole belongs

# goal1: create a comment about holes for each polygon in original, non-buffered spolys
test <- popnhrsoutline

test@polygons[[1]]@Polygons # this is my list of Polygons to which I want to add comments
str(test@polygons[[1]]@Polygons)

# is there an easier way to get to this slot?
?slot
slot(test@polygons[[1]], "Polygons") #not super helpful
slotPoly <- slot(test@polygons[[1]], "Polygons")
  #ok this is a LIST of the 3 Polygons you want to add comments to
  #so NOW you can start trying to use lapply on slotPoly

# from bivand's rsig-geo(?) post
lapply(slot(test@polygons[[1]], "Polygons"), checkPolygonsHoles)
# why no work?
# error says it's not a Polygons object
str(slot(test@polygons[[1]], "Polygons"))
# well. it's a LIST of Polygons objectS...

pls1 <- lapply(slotPoly, function(p) {slot(p, "hole") <- FALSE; return(p)})
pls1
lapply(slot(createSPComment(usa1), "polygons"), comment)

?gIsValid

## never mind, youve sunk way too much time into this
## admitting defeat and using arcmap
## useless remaining code is:

## add 1km buffer just in case, and remove hole
buff <- gBuffer(popnhrsoutline, byid = TRUE, width = 10000) #byid makes spdf


## export buffered area of all elk ranges all year
writeOGR(buff,
         verbose = TRUE,
         dsn = "../GIS/Shapefiles/Elk", 
         layer = "AllElkYrHR", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)



#### KDE OF BORDER (CLARKS FORK) HERD (FOR WY DATA REQUEST) ####

locs <- read.csv("locs.csv") %>%
  filter(Herd == "Border")

library(adehabitatHR)
xy <- data.frame("x"=locs$Long,"y"=locs$Lat)
ll <- SpatialPointsDataFrame(xy, locs, proj4string = latlong)
# kde <- kernelUD(ll, h="href", grid = 10000)
# raster <- raster(kde)
# writeRaster(raster, 
#             paste0("../GIS/Shapefiles/Elk/PopnHRs/", "bor-HRyr"), 
#             format="GTiff", overwrite=TRUE)
# # never mind, that takes for damn ever

mcp <- mcp(ll, percent = 100)
## export population home ranges
writeOGR(mcp, 
         dsn = "../GIS/Shapefiles/Elk/PopnHRs", 
         layer = "bor-HRyr-mcp", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)



#### Discrepancy bt # indivs in indiv df and from popn df ####


# (at least one) issue is that all of HD314 gets dropped
# this happens when semi_join by herd and year
# but does not happen when semi_join by just herd

# winter locations (to create individual winter home ranges)
indivlocswin <- locs %>%
  mutate(Year = as.numeric(substr(Date, 0, 4)),
         Month = as.numeric(substr(Date, 6, 7))) %>%
  # only locns collected during winter (dec-feb, note captures didn't start until jan)
  filter(Month == 12 | Month == 1 | Month == 2) %>%
  # map december locs to following year's winter
  mutate(Year = ifelse(Month == 12, Year + 1, Year))

# quick summary info
length(unique(indivlocswin$Herd))
unique(indivlocswin$Herd)

# HD314 is gone
hd314 <- filter(locs, Herd == "HD314")
View(hd314)
unique(hd314$Year)
popnyrs[popnyrs$Herd == "HD314", 2]
str(indivlocswin) # yr is numeric
str(popnyrs) # yr is numeric
  # huh. no idear. pick it up in the morning


## take 2 ##

# data frame before join fucks it up
test <- locs %>%
  mutate(Year = as.numeric(substr(Date, 0, 4)),
         Month = as.numeric(substr(Date, 6, 7))) %>%
  # only locns collected during winter (dec-feb, note captures didn't start until jan)
  filter(Month == 12 | Month == 1 | Month == 2) %>%
  # map december locs to following year's winter
  mutate(Year = ifelse(Month == 12, Year + 1, Year))

unique(test[test$Herd == "HD314", 11])
popnyrs[popnyrs$Herd == "HD314", 2]

# ok issue is
# in the locations db, the only recorded year for HD314 is 2010
# but i identified 2009 as the year of interest
# so now to figure out if i have to admit to the biologists
# that i'm an idiot and need a popn estimate for a different year

# hd314 was captured in one session, mar 21-29, 2009
  # ok so when i pull out only winter locations, they by definition can't be 2009
  # that's the issue
  # how long did collars stay on?
    # the good news is, this is the only herd you should have to worry about this with
  # maxof dates from GPS_Herd_TimePeriod table in Access db shows most stay on thru
  # the following winter of 2010 and drop off in either april or july of 2010

# ugh, only have 1 full year of data for them starting in late march
# switching to wordvom doc to ponder whether we can use this population




#### verifying kept original (not last) capture info for indivs captured >1x ####
  which(duplicated(fixedcap$AnimalID))
  a <- fixedcap[213:216,]
  fixedcap[fixedcap$AnimalID == unique(a$AnimalID),]
  allcap[allcap$AnimalID == unique(a$AnimalID),]
  # check
  rm(a, allcap)

  
#### determining whether have enough data to actually estimate winter hrs for all pops ####
  
  testi <- indivcap %>%
    mutate(CapMo = substr(CaptureDate, 6, 7))
unique(test$CapMo)

testp <- testi %>%
  mutate(Date = as.Date(CaptureDate, format = "%Y-%m-%d")) %>%
  group_by(Herd) %>%
  summarise(FirstCap = min(Date), LastCap = max(Date))

fks <- filter(testi, Herd == "East Fork" | Herd == "West Fork")


#### just figuring out how to ask number of indivs in a specific herd ####

length(unique(popnlocsall$Herd))
length(which(popnlocsall$Herd == "Tobacco Roots"))
?n_distinct # same as length(unique()) just faster or whatever
popnlocsall %>% filter(Herd == "Tobacco Roots") %>% summarise(n_distinct(AnimalID))



#### determining whether can estimate winHR for tobacco roots (late capture) ####

tob <- filter(popnlocsall, Herd == "Tobacco Roots") %>%
  mutate(Season = ifelse(Month == 12 | Month == 1 | Month == 2, "Winter",
         ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",
         ifelse(Month == 6 | Month == 7 | Month == 8, "Summer", "Fall"))))
unique(tob$Year)
tobtime <- filter(tob, Month == 2 | Month == 3 | Month == 4 | Month == 5)
#### get xy points; write to dataframe, to spatial data frame, to stateplane ####
xyt <- data.frame("x"=tobtime$Long,"y"=tobtime$Lat)
spdf.llt <- SpatialPointsDataFrame(xyt, tobtime, proj4string = latlong)
spdf.spt <- spTransform(spdf.llt,stateplane)

writeOGR(spdf.spt, 
         dsn = "../GIS/Shapefiles/Elk/zOldMisc", 
         layer = "Tob-Winsprlocs", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)


#### ditto madison (late capture) ####

popnlocsall %>% filter(Herd == "Madison") %>% summarise(n_distinct(AnimalID))

mad <- filter(popnlocsall, Herd == "Madison") %>%
  mutate(Season = ifelse(Month == 12 | Month == 1 | Month == 2, "Winter",
                         ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",
                                ifelse(Month == 6 | Month == 7 | Month == 8, "Summer", "Fall"))))
unique(mad$Year)
madtime <- filter(mad, Month == 2 | Month == 3 | Month == 4 | Month == 5)
#### get xy points; write to dataframe, to spatial data frame, to stateplane ####
xym <- data.frame("x"=madtime$Long,"y"=madtime$Lat)
spdf.llm <- SpatialPointsDataFrame(xym, madtime, proj4string = latlong)
spdf.spm <- spTransform(spdf.llm,stateplane)

writeOGR(spdf.spm, 
         dsn = "../GIS/Shapefiles/Elk/zOldMisc", 
         layer = "Mad-Winsprlocs", 
         driver = "ESRI Shapefile",
         overwrite = TRUE)

madindiv <- madtime %>%
  group_by(AnimalID) %>%
  summarise(Indivs = unique(AnimalID)) %>%
  dplyr::select(AnimalID)
write.csv(madindiv, "../GIS/Data/zMisc/madisonindivs.csv", row.names=F)
