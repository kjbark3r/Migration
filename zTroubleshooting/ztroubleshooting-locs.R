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
