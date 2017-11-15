### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###       CLASSIFYING MIGRATORY BEHAVIOR OF ELK         ###
###      USING MSD (BASE CODE FROM SINGH ET AL 2016)    ###
###              KRISTIN BARKER 2017-2018               ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###


  #### Packages ####
  
    library(adehabitatLT)
    library(adehabitatHR)
    library(plotrix)
    library(lattice)
    library(gdata)
    library(nlme)
    library(plyr) # estimate msd
    library(zoo) # estimate msd
    library(ggplot2) # nsd/msd plots
    library(scales) # nsd/msd plots


  #### Working directory ####
  
    wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration"
    wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration"
    wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Migration"
    if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
    } else {
      if(file.exists(wd_laptop)) {setwd(wd_laptop)
      } else {
        setwd(wd_worklaptop)
      }
    }
    rm(wd_workcomp, wd_laptop, wd_worklaptop)
  

  #### Data read-in and prep ####
    
    ## 11.13.17 trial run ##
      # using dataframe and ltraj obj already created while trying migrater
      # (the one of the 10 "known" sapph elk)
      # naming everything same as OG code names for ease of use
    
    # collar data
    DayPos5 <- droplevels(testlocs)
    DayPos5$nDaysYr <- as.POSIXlt(DayPos5$Date)$yday
    
    # make it ltraj...
    litr <- as.ltraj(xy = testlocs[,c("X", "Y")], 
      # note date must be POSIXct
      date = testlocs$Date, 
      # specify indiv (also serves as default burst)
      id = testlocs$AnimalID,
      # typeII trajectory incls times assoc'd with locs
      typeII = TRUE)
    
    # ...with assoc'd dataframe
    nsds <- do.call(rbind, litr)

    # then reorder nsds back to match order of collar data
    nsds <- nsds[order(as.numeric(rownames(nsds))),]
    
    # add NSD to each loc in collar data
    DayPos5$NSDm <- nsds$R2n/1000000

    

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### #
####  |MSD|  ####
### ### ### ### #    
        
    
 #### Estimate MSD using 30 time steps ####


    DayPos6 <- ddply(DayPos5, .(AnimalID), function(DayPos5) {
        z <- zoo(DayPos5$NSDm, DayPos5$nDaysYr)
        DayPos5$rollmean <- rollapply(z, width=30, align = "right", partial=TRUE, FUN=mean, na.rm=TRUE)
        DayPos5
    })
    
    DayPos5$MSD <- coredata(DayPos6$rollmean)
    
    head(DayPos5)

    
 #### Check the NSDs visually ####

    DayPos5$date <- as.Date(DayPos5$Date)
    
    plot2<-qplot(date, sqrt(NSDm), data = DayPos5, geom = "path", colour = AnimalID)
    
    Plot2 <- plot2 +  geom_path(size = 1) + 
    theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) + 
    theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) + 
    scale_x_date(labels = date_format("%b")) +
    theme(panel.border = element_rect(colour = "black", fill = "NA")) +
    theme(panel.background = element_rect(fill = "white")) + 
    theme(panel.grid.minor.x = element_line(colour="white")) + 
    theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +  
    theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) + 
    theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
    Plot2
    
    Plot2 + facet_wrap(~AnimalID, scales = "free")
    
    
  #### Check MSDs visually? ####

    plot3<-qplot(date, sqrt(MSD), data = DayPos5, geom = "path", colour = AnimalID)
    
    Plot3 <- plot3 +  geom_path(size = 1) + 
    theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) + 
    theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) + 
    scale_x_date(labels = date_format("%b")) +
    theme(panel.border = element_rect(colour = "black", fill = "NA")) +
    theme(panel.background = element_rect(fill = "white")) + 
    theme(panel.grid.minor.x = element_line(colour="white")) + 
    theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +  
    theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) + 
    theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
    Plot3
    
    Plot3 + facet_wrap(~AnimalID, scales = "free")
    
    # yassss. jfc i've died and gone to heaven
             

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      

### ### ### ### ### ##
####   |MODELS|   ####
### ### ### ### ### ##
    
    
    
    #### Null model ####
    
      null.HRmod <- nlme(MSD ~ A,
                    data = DayPos5,
                    fixed = A ~ 1,
                    random = A ~ 1,
                    groups = ~ AnimalID,
                    start = c(A = mean(DayPos5[,'MSD'])))

    
    
    #### Sedentary model ####

      asym.HRmod <- nlme(MSD ~ Asym*(1 - exp(lrc*nDaysYr)),
                           data     = DayPos5,
                           fixed     = Asym + lrc ~ 1,
                           random     = Asym ~ 1,
                           groups     = ~ AnimalID,
                           start     = c(Asym = summary(null.HRmod)$tTable[1],
                                                lrc = -0.002))
    
    
    #### Dispersal model  with distance (Asym) and timing (xmid) varying between indivs ####
    
        # Providing a start to improve model convergence, 
        # which can also be obtained by using parameters from previously estimated models

        ranef2.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
                      data = DayPos5,
                      fixed = Asym + xmid + scal ~ 1,
                      random = Asym + xmid ~ 1,
                      groups = ~ AnimalID,
                              na.action = na.exclude,
                              start = c(Asym = summary(asym.HRmod)$tTable[1], 
                                               xmid = 30, scal = 2),
                      verbose = T)
        summary(ranef2.Dispmod)


        # CONVERGENCE ERROR
            # Error in nlme.formula(MSD ~ Asym/(1 + exp((xmid - nDaysYr)/scal)), data = DayPos5,  : 
            # step halving factor reduced below minimum in PNLS step
        

      
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      
####  |OG CODE|  ####


###Sedentary HR model
######################################
asym.HRmod <- nlme(MSD ~ Asym*(1 - exp(lrc*nDaysYr)),
                     data     = DayPos5,
                     fixed     = Asym + lrc ~ 1,
                     random     = Asym ~ 1,
                     groups     = ~ yrMooseID,
                     start     = c(Asym = summary(null.HRmod)$tTable[1], 
lrc = -0.002))

##Dispersal model with distance (Asym) and timing (xmid) varying between individuals. 
##Providing a start improve model convergence, which can also be obtained by using parameters from previously estimated models
########################################################
ranef2.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
              data = DayPos5,
              fixed = Asym + xmid + scal ~ 1,
              random = Asym + xmid ~ 1,
              groups = ~ yrMooseID,
                      na.action = na.exclude,
                      start = c(Asym = summary(asym.HRmod)$tTable[1], xmid = 30, scal = 2))
summary(ranef2.Dispmod)

##Dispersal model with Distance, timing and duration (scal) varying between individuals. 
########################################################
full.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
         data = DayPos5,
        fixed = Asym + xmid + scal ~ 1,
        random = Asym + xmid + scal ~ 1,
        groups = ~ yrMooseID,
               start = c(Asym = summary(ranef2.Dispmod)$tTable[1],
	xmid = summary(ranef2.Dispmod)$tTable[2], scal = summary(ranef2.Dispmod)$tTable[3]))

summary(full.Dispmod)

##Simple Migration model with only distance varying between individuals
########################################################
asym.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym ~ 1,
            groups =~ yrMooseID,
           start = c(asym = 5515,xmidA = 90, xmidB = 242, scale1 = 4, scale2 = 4))
summary(asym.Migrmod)

##More complex migration model with distance, timing of Migration 1 (xmidA) and timing of migration 2 (xmidB)
########################################################
ranef2.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB ~ 1|yrMooseID,
            #groups =~ yrMooseID, control=contr,
           start = c(asym = 2200, xmidA = 100,
               xmidB = 295, scale1 = 10, scale2 = 15))
summary(ranef2.Migrmod)

##Complete migration model with all parameters varying between individuals. Normally this will struggle to converge in a dataset
##that contain a mix of migrants and non-migrants, and is more important in later stages when estimating timing of movements for 
##migratory individuals only using the NSD rather than the MSD 
ranef4.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = list(asym + xmidA + xmidB + scale1 + scale2 ~ 1),
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1|yrMooseID,
            #groups =~ yrMooseID, control=contr,
           start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
               xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 5, 
		scale2 = 10))
summary(ranef4.Migrmod)

##Mixed Migratory Model with distance and timing of Migration 1, and distance and timing of Migration 2, varying between individuals. 
mix.Migrmod <- nlme(MSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asymA + asymB +xmidA + xmidB~ 1,
            groups =~ yrMooseID,
           start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

##Nomadic model
#####################################################################################################
all.linear <- nlme(MSD ~ 4*D*nDaysYr,
              data = DayPos5,
              fixed = D ~ 1,
              random = D ~ 1,
              groups = ~ yrMooseID,
              start = c(D = 3))
########################################################
########################################################
### GOF indiv level
### change this list below to include the model that fitted (with highest number of random effects) for each movement type

HRmod <- asym.HRmod
NullMod <- null.HRmod
Dispmod <- full.Dispmod  ##here we managed to a fit a disp mod with distance, timing and duration varying between all individuals
MigrMod <- ranef2.Migrmod ##here we could only fit a model with distance and timing varying between individuals (in these initial classification stages)
MigrModC <-mix.Migrmod
nomadMod <- all.linear


###Now perform the calculations for the goodness of fit (i.e. concordance criterion). 
### CC1-IDlev
########################################################

# check first that yrMooseID levels are the same

all.equal(levels(DayPos5$yrMooseID),rownames(coef(HRmod)))

#[1] TRUE    # same for Dispmod etc.


###
#HR
###
CC1ID.HRmod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.HRmod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(HRmod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }
###
#NULL
##
CC1ID.NullMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.NullMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(NullMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Dispersal
##
CC1ID.Dispmod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.Dispmod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(Dispmod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Migration
###
CC1ID.MigrMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.MigrMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#MigrationC
###
CC1ID.MigrModC <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.MigrModC[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrModC)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Nomad
###
CC1ID.nomadMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.nomadMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(nomadMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###############################################
###Create a dataframe of the results
SpaceUseClass <- data.frame(CC1ID.HRmod = CC1ID.HRmod,CC1ID.NullMod = CC1ID.NullMod,CC1ID.Dispmod = CC1ID.Dispmod,
		CC1ID.MigrMod = CC1ID.MigrMod,CC1ID.MigrModC = CC1ID.MigrModC,CC1ID.nomadMod = CC1ID.nomadMod, yrMooseID = levels(DayPos5$yrMooseID))

### add column with space use classification according to highest CC1 value
maxCC1ID <- (apply(SpaceUseClass[,1:6], 1, which.max))
SpaceUseClass$bestMod.CC1ID <- factor(ifelse(maxCC1ID == 1, "HRmod", ifelse(maxCC1ID == 2, "NullMod", ifelse(maxCC1ID == 3, "Dispmod",
		ifelse(maxCC1ID == 4, "MigrMod", ifelse(maxCC1ID == 5, "MigrModC", ifelse(maxCC1ID == 6, "nomadMod", NA)))))))

###Save results of movement classifications and goodness of fits.  
coeffs<-coef(MigrModC)
params<-data.frame(coeffs,SpaceUseClass)
sink("BD MSD Output.txt")
summary(MigrModC)
sink()
write.csv(params,"BD MSD Parameters.csv")

###Check classifications. Things to note
##Check values of asymA and asymB for individuals that were Mixed Mig. If these are only 1 or 2km apart (for Moose), it is probably just migratory
##Check value of asym for migratory, it may be that Migrmod provided the best fit, but all models may provide a poor fit
##On this point, check the value of the concordance criterion. We have found that migratory models with a CC<0.7 are often incorrect
##Check values of xmidB and scal2. Since it is not possible to constrain nlme, the migration may return to zero after the last data point, 
##for e.g. xmidB = 400, but data only has 365 days. This is in fact a dispersal then

##Afterwards, continue by only selecting individuals identified as migratory to obtain more accurate estimates for distance, timing
###and duration using the NSD and including all movement parameters as random effects in the nlme. 

params <- read.csv("BD MSD Parameters 2.csv")

SpaceUseClassMigr <- params[params$bestMod.CC1ID %in% c("MigrMod", "MigrModC"),]
SpaceUseClassMigr <- droplevels(SpaceUseClassMigr)

migrMoose <- SpaceUseClassMigr$yrMooseID
DayPos6 <- DayPos5[DayPos5$yrMooseID %in% migrMoose,]
DayPos6 <- droplevels(DayPos6)
summary(DayPos6)

############Now reapply models using the NSD#######
##These final NSD models were also applied to the second part of our study that examines the effect of starting date and location, and how data resolution influence model performance. 

##First a simpler mode to obtain starting values for the sampled dataset (i.e. it now only contains migrators)
ranef2.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB ~ 1|yrMooseID,
            start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
            xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 8, scale2 = 10))
summary(ranef2.MigrmodLD)

##Then a complex model that hopefully converges. This step may still prove a challenge. Here we have two versions, the second uses results from the above
##model as starting values, whereas the first I manually manipulate to try improve model convergence. Viewing plots of the movement trajecotry may assist
##in identifying appropriate starting values for the model. If convergence continues to fail, it may be necessary
##to sub-sample the dataset, or remove individuals with abnormal NSD patterns. 
ranef4.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = list(asym+ xmidA + xmidB + scale1 + scale2 ~ 1),
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            groups =~ yrMooseID,
           start = c(asym = 1805, xmidA = 85,
               xmidB = 299, scale1 = 1.5, 
		scale2 = 6))
summary(ranef4.MigrmodLD)

ranef4.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            groups =~ yrMooseID,
           start = c(asym = summary(ranef2.MigrmodLD)$tTable[1], xmidA = summary(ranef2.MigrmodLD)$tTable[2],
               xmidB = summary(ranef2.MigrmodLD)$tTable[3], scale1 = 5, scale2 = 4))
summary(ranef4.MigrmodLD)

##Obtain GOF for new model

MigrModLD <-ranef4.MigrmodLD

CC1ID.MigrModLD <- numeric(length(levels(DayPos6$yrMooseID)))

for(k in 1:length(levels(DayPos6$yrMooseID))) {

CC1ID.MigrModLD[k] <- 1 - (sum((DayPos6[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k]),'MSD'] -
         fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])])^2)) / (
         (sum((DayPos6[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k]),'MSD'] -
         mean(DayPos6[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrModLD)[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k])] -
         mean(fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])]))^2)) +
         (length(DayPos6[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k]),'MSD'])*((mean(DayPos6[which(DayPos6$yrMooseID==levels(DayPos6$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])]))^2))
         )
         }

SpaceUseClassLD <- data.frame(CC1ID.MigrModLD = CC1ID.MigrModLD, yrMooseID = levels(DayPos6$yrMooseID))

coeffs<-coef(ranef4.MigrmodLD)
params<-data.frame(coeffs,SpaceUseClassLD)
write.csv(params,"BD NSD MigrMod results.csv")


