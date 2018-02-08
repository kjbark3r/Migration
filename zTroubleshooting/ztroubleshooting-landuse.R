### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###   ownership classification (for land use covariate)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

       
       colnames(cad08mt@data)
       colnames(cad09mt@data)
       colnames(cad10mt@data)
       colnames(cad11mt@data)
       colnames(cad12mt@data) # gendescrip dies here
       colnames(cad08mt@data)
       colnames(cad08mt@data)
       colnames(cad08mt@data)
       
       
       # identify all unique OwnerNames starting in 2012
       # ultimately reassign all other than matching what 08-10 GENDESCRIP has to pvt
       
       descrip <- list(unique(cad08mt@data$GENDESCRIP))
       descrip
       
       c12 <- data.frame(Nms = unique(cad12mt@data$OwnerName))
       c08dat <- cad08mt@data
       
       # check out property types for classification
       write.csv(unique(cad08mt@data$PROPTYPE), "./zOldAndMisc/proptypes1.csv", row.names=F)
       write.csv(unique(cad16mt@data$PropType), "./zOldAndMisc/proptypes2.csv", row.names=F)
       
       # create df of just GENDESCRIP, OWNR_NAM1
       descrips <- cad11mt@data %>%
         dplyr::select(GENDESCRIP, OWNR_NAM1) %>%
         rename(OwnerName = OWNR_NAM1) %>%
         distinct()

       
       # join to 2012 data and see what's still missing
       test12 <- left_join(cad12mt@data, descrips, by = "OwnerName")
       nas <- filter(test12, is.na(GENDESCRIP)) %>%
         dplyr::select(OwnerName, GENDESCRIP) %>%
         distinct()
       ## see data prep notes for list of things to grep and fix, others are pvt or NA
    
 rm(descrips, test12, nas)
 
 
 
 
 ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
 
 #### playing with how to id correct cadastral yr per herd ####
 
 cadlist
 sort(unique(hrByHerd@data$Year))
 # if 2006, use 08. If 2015, use 2014. Else use match.
 
 
 
 ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
 
 #### standardizing column names across cadastrals #### 
 
 # first make list of colnames of each
 # then do the lapply trick

 oldnamelist <- list(
   cad08 = colnames(cad08@data),
   cad10 = colnames(cad10@data),
   cad11 = colnames(cad11@data),
   cad12 = colnames(cad12@data),
   cad13 = colnames(cad13@data),
   cad14 = colnames(cad14@data),
   cad16 = colnames(cad16@data)
   )
 
 oldnames <- data.frame(lapply(oldnamelist, "length<-", max(lengths(oldnamelist))))
  # because i created that wy later using 2008 cadastral attribute table
  # as base, they do all have the same column NAMES now, but not all columns
  # are populated the same way
  # so what i'll need to do is say, if this is NA, use the other one, else use this
 
 testlista <- c("cad08", "cad10", "cad11")
 
 for (i in 1:length(testlista)) {
  cadi <- get(testlista[i]) #please try to remember get() this time
  dati <- data.frame(
    myClassn = NA,
    #yrCad = rep(substr(cadi, 4, 5), length.out = nrow(cadi@data)),
    owner = cadi@data$OWNR_NAM1,
    descr = cadi@data$GENDESCRIP,
    grazAcre = cadi@data$GRAZING_AC,
    irrigAcre = cadi@data$IRRIG_ACRE,
    hayAcre = cadi@data$WILD_HAY_A)
  cadi@data = dati
  assign(paste0(testlista[i], "upd"), cadi)
 }
 

  testlistb <- c("cad08", "cad10", "cad11")
 
 for (i in 1:length(testlista)) {
  cadi <- get(testlista[i]) #please try to remember get() this time
  dati <- data.frame(
    myClassn = NA,
    #yrCad = rep(substr(cadi, 4, 5), length.out = nrow(cadi@data)),
    owner = cadi@data$OWNR_NAM1,
    descr = cadi@data$GENDESCRIP,
    grazAcre = cadi@data$GRAZING_AC,
    irrigAcre = cadi@data$IRRIG_ACRE,
    hayAcre = cadi@data$WILD_HAY_A)
  cadi@data = dati
  assign(paste0(testlista[i], "upd"), cadi)
 }
  
 
  
  
  #### identify files that have standardized colnames
  
             
        # using files that have standardized column names  
        cadlist <- apropos(ls("upd"))
        apropos(ls())
        objects()
        
        cadlist <- grep("upd", objects(), value = TRUE)
        
        # fcn?
        
        testfcn <- function(x) { ifelse(x > 0, paste("AGR"), paste("NA")) }
        testfcn(1)
        testfcn(0)
        # coooool
        
        test <- testfcn(dati$grazAcre)
        unique(test)
        # holy shitsnacks
        
        dati$test <- agrfcn(dati$grazAcre)
        # eeeee
        
        
        ## ok so need to do this for each ##
        cadlist <- grep("upd", objects(), value = TRUE)
        
        agrfcn <- function(x) { ifelse(x > 0, paste("AGR"), paste("NA")) }
        # oh man i really wish id figured out how to use this
        
        for (i in 1:length(cadlist)) {
         cadi <- get(cadlist[i])
         yr <- substr(cadlist[i], 4, 5)
         cadi@data <- mutate(cadi@data,
           myClassn = ifelse(grazAcre > 0 | irrigAcre > 0 | hayAcre > 0, "AGR", "UNK"))
         assign(paste0("lu", yr), cadi)
         }
        
        lapply(get(cadlist), transform, myClassn = agrfcn(grazAcre))
        
        
        
    ## prop type name-fixin' ##
        for (i in 1:length(cadlist)) {
         cadi <- get(cadlist[i])
         cadi@data <- mutate(cadi@data,
           propType = ifelse(grepl("-"), substr(propType, nchar(propType)-5, nchar(propType))))
         assign(paste0(cadlist[i]), cadi)
        }
        
 test <- get(cadlist[1])
 test@data$propType = ifelse()
 ?grepl
 
 
        ## standardize names of property types ##
           
        # for all files that have standardized column names  
        cadlist <- grep("upd", objects(), value = TRUE)
        
        for (i in 1:length(cadlist)) {
         cadi <- get(cadlist[i])
         cadi@data$propType <- as.character(cadi@data$propType)
         cadi@data <- mutate(cadi@data,
           propType = ifelse(grepl("-", propType), 
             substr(propType, nchar(propType)-5, nchar(propType)),
             propType))
         assign(paste0(cadlist[i]), cadi)
        }

        
        ## decided i don't need to do this
        ## just search for instances of "mining" or "exempt" or =NA
        
        
        ## below is close but the second ifelse is wrong
                
        # if parcel has any grazing, irrigated, or wild hay acreage, class as AGR
        for (i in 1:length(cadlist)) {
         cadi <- get(cadlist[i])
         yr <- substr(cadlist[i], 4, 5)
         cadi@data <- mutate(cadi@data,
           myClassn = ifelse(grazAcre > 0 | irrigAcre > 0 | hayAcre > 0, "AGR", 
             ifelse(propType != "mining claim" & 
                 propType != "exempt property" & 
                 propType != "", "DEV", "UNK")))
         assign(paste0("lu", yr), cadi)
        }
        
        
        
        
    #### use older cadastrals to classify some of the UNKs in newer ####
        
        olddescr <- rbind(data.frame(lu08@data), data.frame(lu10@data), data.frame(lu11@data)) %>%
              dplyr::select(owner, descr, myClassn) %>%
              filter(myClassn == "UNK") %>%
              distinct()
        
        unique(olddescr$descr)
        
        known <- data.frame(descr = unique(olddescr$descr), myClassn = NA) %>%
          mutate(myClassn = ifelse(descr == "Public" | descr == "USFS" | 
                                   descr == "BLM" | descr == "NPS", "PUB", 
                            ifelse(descr == "Private" | descr == "Utl Ease" | 
                                   descr == "RgtOfWay", "DEV", 
                            ifelse(descr == "Water", "NA", "UNK"))))

              
        
        
      #### investigating whether newer cadastrals have some same owner names as older ####
        
 
        newdescr <- rbind(data.frame(lu12@data), data.frame(lu13@data), 
          data.frame(lu14@data), data.frame(lu16@data)) %>%
              dplyr::select(owner, descr, myClassn) %>%
              filter(myClassn == "UNK") %>%
              distinct()
        
        
        ## duplicate owner weirdness
        dupes <- olddescr %>%
          group_by(owner) %>%
          filter(n() >1) %>%
          ungroup()
        #ah ok
        # need to remove USAs and NAs bc have multiple possible descrs
        # and manually class USFS and MT State 
        
        
        # play with one new@data to figure out why join not working
        test <- lu16@data %>%
          mutate(owner = as.character(owner)) %>%
          dplyr::select(-descr) %>%
          left_join(ownrs, by = "owner")
        
        
    #### checking out remaining unknowns based on owner names ####
        
        unks <- rbind(lu08@data, lu10@data, lu11@data, lu12@data, lu13@data, lu14@data, lu16@data) %>%
          filter(myClassn == "UNK") %>%
          dplyr::select(myClassn, owner, descr, propType) %>%
          distinct()

        
        
    #### check out NAs for low-hanging fruit ####
        nas <- rbind(newlu08@data, newlu10@data, newlu11@data, newlu12@data, newlu13@data, newlu14@data, newlu16@data) %>%
          filter(is.na(myClassn)) %>%
          dplyr::select(myClassn, owner, descr, propType) %>%
          distinct() %>%
          filter(!is.na(owner))
        
        
        
        
        
  #### figuring out what the goddamn motherfucking shit is wrong with this loop
        ## that it would work for older files (pre-2012) but not the newer ones
        
        
       for (i in 1:length(lulist)) {
          
          # identify the file
          lui <- get(lulist[i])

          # update some unknown classifications
          lui@data <- lui@data %>%
            # if already classified as ag or dev, don't mess with it
            mutate(myClassn = ifelse(myClassn != "UNK", myClassn,
              # classify obvious public as such
              ifelse(descr == "Public" | descr == "USFS" | 
                     descr == "BLM" | owner == "BLM" | descr == "NPS", "PUB", 
              # and classify water as NA  
              ifelse(descr == "Water", "NA", "UNK"))))

            
          # store as r object
          assign(paste0("wtf", lulist[i]), lui)

       }   
        
        
        
        
        
        
           #### check out NAs for low-hanging fruit ####
        nas <- rbind(wtflu08@data, wtflu10@data, wtflu11@data, wtflu12@data, wtflu13@data, wtflu14@data, wtflu16@data) %>%
          filter(is.na(myClassn)) %>%
          dplyr::select(myClassn, owner, descr, propType) %>%
          distinct() %>%
          filter(!is.na(owner))
        
        
        
        ## take 2, now with sleep and food!
        
        for (i in 1:length(lulist)) {
          
          # identify the file
          lui <- get(lulist[i])

          # update some unknown classifications
          lui@data <- lui@data %>%
            # if already classified as ag or dev, don't mess with it
            mutate(myClassn = ifelse(myClassn != "UNK", myClassn,
              # classify obvious public as such
              ifelse(descr == "Public" | descr == "USFS" | 
                     descr == "BLM" | owner == "BLM" | descr == "NPS", "PUB", "UNK")))
          
          # classify private non-agricultural land as dev
          lui@data$myClassn <- ifelse(lui@data$myClassn != "UNK", lui@data$myClassn,
            ifelse(lui@data$descr == "Private" | lui@data$descr == "Utl Ease" | 
                     lui@data$descr == "RgtOfWay", "DEV", "UNK"))
          
          # classify mining land that has no ag acreage as dev
          lui@data$myClassn <- ifelse(lui@data$myClassn != "UNK", lui@data$myClassn,
            ifelse(lui@data$propType == "MC - Mining Claim", "MINE", "UNK"))

          
               



            
          # store as r object
          assign(paste0("wtf", lulist[i]), lui)

                }        
        
        testold <- wtflu10@data
        testnew <- wtflu14@data
        
        unique(testold$myClassn)
        unique(testnew$myClassn)
        
        unique(testold$descr)
        unique(testnew$descr)
        
        
        unique(testnew$propType)
        
        any(grepl("[Mm]ining", testnew$propType)) # TRUE
        any(testnew$propType == "MC - Mining Claim") # TRUE
        
        str(testold)
        
        
        test <- rbind(wtflu08@data, wtflu10@data, wtflu11@data, wtflu12@data, wtflu13@data, wtflu14@data, wtflu16@data) %>%
          filter(is.na(myClassn)) %>%
          dplyr::select(myClassn, owner, descr, propType) %>%
          distinct() %>%
          filter(!is.na(owner))
        
        
        
        
        
        
        
        #### checking ownership (prep for calculating ownership density ####
        
        ownrs <- rbind(lu08@data, lu10@data, lu11@data, lu12@data, lu13@data, lu14@data, lu16@data) %>%
          dplyr::select(myClassn, owner, descr, propType) 
        length(which(is.na(ownrs$owner))) # 394 NAs (out of 12773 parcels, actually not too bad)
        ownrs <- dplyr::select(ownrs, myClassn, owner) %>% distinct()
        
        
        
        #### conditional join to update some classifications based on landowners  ####
        
        test <- lu16
        unique(test@data$myClassn)

        test2 <- merge(lu16@data, descrips, by = "owner")
        View(test2)
        test@data <- test@data %>%
          merge(descrips, by = "owner") %>%
          merge(test@data, descrips, by = "owner")
        test2$Classn <- ifelse(test2$myClassn.x == "UNK", myClassn.y, myClassn.x)
        
## <3 stackoverflow                 
res1 <- merge(df, other.df, by = "a", all.x = TRUE)[-2]
names(res1) <- names(df)
res1[is.na(res1)] <- 0





head(lu08@data)
head(lu16@data)
length(which(is.na(lu16@data$owner)))
test <- lu16@data; View(test)

# newp


         if(lui@data$owner == "BLM"){lui@data$updClassn == "PLZWRK"}


## classns to update, implemented these rules piecemeal manually


    # update some unknown classifications
    lui@data <- lui@data %>%
      # if already classified as ag or dev, don't mess with it
      mutate(myClassn = ifelse(myClassn != "UNK", myClassn,
        # classify obvious public as such
        ifelse(descr == "Public" | descr == "USFS" | 
               descr == "BLM" | owner == "BLM" | descr == "NPS", "PUB", 
        # ditto obvious developed/developable lands   
        ifelse(descr == "Private" | descr == "Utl Ease" | 
               descr == "RgtOfWay" | propType == "MC - Mining Claim", "DEV", 
        # and classify water as NA  
        ifelse(descr == "Water", "NA", "UNK")))))
    
    
    # update some unknown classifications
    lui@data <- lui@data %>%
      # if already classified as ag or dev, don't mess with it
      mutate(myClassn = ifelse(myClassn != "UNK", myClassn,
                   ifelse(descr == "Water", "NA", "UNK")))
  
          
#### unique landowners (ensure no obvious dupes) ####
          
    ## make df of unique owners from each yrs' shp; may need to break down by popn if too many ##
    
    # add myClassns from updated descrips
    lulist <- grep("^newlu[[:digit:]]", objects(), value = TRUE)  
    
    
    # if myClassn is UNK or NA, update with new classns from descrips df
    for (i in 1:length(lulist)) {
    lui <- get(lulist[i])
    lui@data$owner <- trimws(lui@data$owner)
    dat <- lui@data %>%
      dplyr::select(owner) %>%
      distinct() %>%
      arrange(owner)
    assign(paste0("own", lulist[i]), dat)
    }
         
          
          
#### checking most efficient owner renaming option ####

    
    d <- newlu14@data$owner
    
    # BLM
    length(which(d == "BLM"))
    length(which(d == "BUREAU OF LAND MANAGEMENT"))
    length(which(d == "U S BUREAU OF LAND MANAGEMENT")) # FTW 82
    length(which(d == "US BEREAU OF LAND MANAGEMENT"))
    length(which(d == "US BUREAU OF LAND MANAGEMENT"))
    length(which(d == "USDI BUREAU OF LAND MANAGEMENT"))
    
    # FWP
    length(which(d == "MONTANA DEPT OF FISH WILDLIFE & PARKS"))
    length(which(d == "MONTANA DEPT OF FISH, WILDLIFE & PARKS"))
    length(which(d == "MONTANA FISH & GAME COMMISSION"))
    length(which(d == "MONTANA FISH & GAME DEPT")) # FTW 5
    
    # USFS
    length(which(d == "U S FOREST SERVICE"))
    length(which(d == "UNITED STATES FOREST SERVICE")) # ftw 22
    length(which(d == "US FOREST SERVICE"))
    length(which(d == "USA - FOREST SERVICE"))

    # MT State
    length(which(d == "MONTANA STATE OF"))
    length(which(d == "STATE OF MONTANA")) # FTW 83
    
    # USA
    length(which(d == "THE UNITED STATES OF AMERICA"))
    length(which(d == "UNITED STATES OF AMERICA")) # FTW (49)
    length(which(d == "USA"))
    
    #Dept of Ag, prob USFS?
    length(which(d == "UNITED STATES OF AMERICA - DEPT OF AG")) # ftw but just 2
    length(which(d == "USA DEPT OF AGRICULTURE"))
    
    
    #Sitz Angus Farms
    length(which(d == "SITZ ANGUS FARM LTD PARTNERSHIP")) 
    length(which(d == "SITZ ANGUS FARMS LIMITED PARTNERSHIP"))
    length(which(d == "SITZ ANGUS FARMS LTD")) # ftw
    length(which(d == "SITZ ANGUS FARMS LTD PTNRSP")) 
    
    # # Spanish Q
    # length(which(d == "SPANISH Q RANCHES LLC"))
    # length(which(d == "SPANISH Q"))
    # length(which(d == "SPANISH Q INC"))
    