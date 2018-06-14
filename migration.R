### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  ASSESSING RELATIVE INFLUENCE OF FORAGE, HUMANS, AND    #
#    INTRINSIC FACTORS ON MIGRATORY BEHAVIOR OF ELK       #
#                   KRISTIN BARKER                        #
#               DEC 2017 - MAY 2018                      #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


## This code assesses the relationship 
## between hypothesized covariates and migratory behavior 
## of individual elk across SW Montana.



### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(foreign) # unimportant?
    library(ggplot2) # pretty plots
    library(gridExtra) # >1 plot per display
    library(reshape2) # jitter etc for data viz
    library(ordinal) # ordinal response w random effect
    library(AICcmodavg) # model comparison
    library(MuMIn) # model averaging
    library(ROCR) # ROC/AUC
    library(dplyr) # data manipulation
    
    
  
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
    

    

### ### ### ###  ### 
####  | DATA |  ####
### ### ### ###  ### 
    
    
    # read in data (from dataprep-migration.R) 
    dat <- read.csv("moddat-feb.csv") # indivdual data
    popdat <- read.csv("pop-summaries-feb.csv") # herd summaries
    
    # format behavior as ordinal (res < int < mig) and format dummy variables as factors
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    dat$irrig <- factor(dat$irrig)
    dat$Old <- factor(dat$Old)
    
    # order popdat herds by ppnMig (for later plotting)
    popdat$Herd = reorder(popdat$Herd, popdat$ppnMig) 
    
    
    # create df for indivs with no age estimation 
    olddat <- dplyr::filter(dat, !is.na(Old))
    



### ### ### ### ### ### ### ##
#### |DATA SUMMARIES ETC| ####
### ### ### ### ### ### ### ##    
    
    

    #### General looks at indiv covariates ####

    
      # number of elk >10yrs old
      length(which(olddat$Old == 1))
      length(which(olddat$Old == 1)) / nrow(olddat)
      
      # number of migrants
      length(which(dat$Behav == "migrant"))
      length(which(dat$Behav == "migrant"))/nrow(dat)
      
      # and residents
      length(which(dat$Behav == "resident"))/nrow(dat)      
      
      # summary tables of binary covariates
      lapply(dat[, c("Behav", "Old", "irrig")], table)
      
      # three-way contingency table
      ftable(xtabs(~ Old + Behav + irrig, data = dat))
      
      # distributions of continuous covariates
      par(mfrow = c(2,2))
      hist(dat$predFor, main = "predFor")
      hist(dat$deltaFor, main = "deltaFor")
      hist(dat$Dens, main = "Cons Dens")
      hist(dat$densOwn, main = "Human use")
      par(mfrow = c(1,1))
      
      # digging deeper into distributions
      summary(dat$Dens)
      summary(dat$densOwn)
      
      hist(log(dat$Dens), main = "log(cons dens)") # eh, passable?
      hist(log(dat$densOwn), main = "log(human)") # hello gorgeous
      hist(sqrt(dat$Dens), main = "sqrt(cons dens)") # worse
      hist((dat$Dens)^(0.3333)) # log still better
      hist(1/log(dat$Dens)) # yeesh
      
      
      # ag access, res and in herd
      with(dat, 
        length(which(Behav == "resident" & irrig == 0)) / 
          length(which(Behav == "resident"))) # 40% res no ag
      herdag <- dat %>%
        group_by(Herd) %>%
        summarize(yAg = sum(as.numeric(as.character(irrig))),
          nAg = n() - yAg,
          ppnNag = nAg/n())
      herdagy <- filter(herdag, yAg != 0)
      hist(herdagy$ppnNag)
      with(dat, length(which(Behav == "resident" & irrig == 0))) # n = 25
      

  
      
   #### General plots of univariate relationships ####
      
      # deltaFor, split by old & irrig
      ggplot(olddat, aes(x = Behav, y = deltaFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE)
      
      # predFor, split by old & irrig
      ggplot(olddat, aes(x = Behav, y = predFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 
      
      # ppnMig per herd by ppnOld
      ggplot(popdat, aes(x = ppnOld, y = ppnMig, color = Herd)) +
        labs(x = "ppnOld", y = "Proportion Migrant") +
        geom_smooth(color = "black") +
        geom_point()
        
      
      
    #### Summarizing covariates ####  
      
      
      ##### a) full-group summaries ####
      
      # summaries <- read.csv("summaries-covariates-feb.csv") # or run below
      
      
      summaries <- dat %>% summarise(
        # predFor's surprisingly normal
        predForAvg = mean(predFor),
        predForSd = sd(predFor),
        predFormin = min(predFor),
        predFormax = max(predFor),
        # delatFor not so much
        deltaForMed = median(deltaFor),
        deltaForMin = min(deltaFor),
        deltaForMax = max(deltaFor),
        deltaForIQR = IQR(deltaFor),
        # for age, don't count the 10 elk we have no age for
        oldN = length(which(Old == 1)),
        oldPpn = length(which(Old == 1)) / (n()-10),
        # density positively skewed
        densMed = median(Dens),
        densMin = min(Dens),
        densMax = max(Dens),
        densIQR = IQR(Dens),
        # irrig can count everybody
        irrigN = length(which(irrig == 1)),
        irrigPpn = length(which(irrig == 1)) / n(),
        # and human land use suuuuper skewed
        densOwnMed = median(densOwn),
        densOwnMin = min(densOwn),
        densOwnMax = max(densOwn),
        densOwnIQR = IQR(densOwn))
      write.csv(summaries, "summaries-covariates-feb.csv", row.names = F)
      

      
      #### b) herd summaries ####
      
      
        # herdsums <- read.csv("summaries-herds-feb.csv") # or run below

      
        herdsums <- dat %>%
    	    group_by(Herd) %>%
    	    summarise(nIndivs = n(),
    	              nMig = length(which(Behav == "migrant")),
    	              nRes = length(which(Behav == "resident")),
    	              nOth = length(which(Behav == "other")),
    	              ppnIrrig = length(which(irrig == "1"))/n()) %>%
    	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
    	           ppnRes = round(nRes/nIndivs, digits =2),
    	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
    	    dplyr::select(Herd, ppnMig, ppnRes, ppnOth, nIndivs, ppnIrrig)
        write.csv(herdsums, "summaries-herds-feb.csv", row.names=F)
        
        allherdsum <- herdsums %>%
          summarise(
            medMig = median(ppnMig),
            iqrMig = IQR(ppnMig),
            minMig = min(ppnMig),
            maxMig = max(ppnMig),
            medOth = median(ppnOth),
            iqrOth = IQR(ppnOth),
            minOth = min(ppnOth),
            maxOth = max(ppnOth),
            medRes = median(ppnRes),
            iqrRes = IQR(ppnRes),
            minRes = min(ppnRes),
            maxRes = max(ppnRes))
        
        
        # herdcovs <- read.csv("summaries-herdcovs-feb.csv") # or run below
        
        herdcovs <- dat %>%
          group_by(Herd) %>%
          summarise(
            nIndivs = n(),
            deltaForMed = median(deltaFor),
            deltaForMin = min(deltaFor),
            deltaForMax = max(deltaFor),
            deltaForIQR = IQR(deltaFor),
            # for age, don't count the 10 elk we have no age for
            oldN = length(which(Old == 1)),
            oldPpn = length(which(Old == 1)) / (n()-10),
            # irrig can count everybody
            irrigN = length(which(irrig == 1)),
            irrigPpn = length(which(irrig == 1)) / n(),
            # and human land use suuuuper skewed
            densOwnMed = median(densOwn),
            densOwnMin = min(densOwn),
            densOwnMax = max(densOwn),
            densOwnIQR = IQR(densOwn))    
        write.csv(herdcovs, file = "summaries-herdcovs-feb.csv", row.names=F)
        
        
        # variation within herds (maxNDVI and densOwn)
        
          # max NDVI inside indiv winHRs
          maxforwin <- read.csv("deltafor-prelim-feb.csv")
          hist(maxforwin$maxNDVIin) # super skewed, so report range not stdev
          
          herdvar <- dat %>%
            left_join(maxforwin, by = "AnimalID") %>%
            dplyr::select(-maxNDVIout) %>%
            group_by(Herd) %>%
            summarise(rangeNDVIwin = max(maxNDVIin) - min(maxNDVIin),
                      rangeDensOwn = max(densOwn) - min(densOwn))
          
          hist(herdvar$rangeNDVIwin)
          hist(herdvar$rangeDensOwn)
          
          herdvarsum <- herdvar %>%
            summarise(minNDVIrange = min(rangeNDVIwin),
                      maxNDVIrange = max(rangeNDVIwin),
                      minDensOWnRange = min(rangeDensOwn),
                      maxDensOWnRange = max(rangeDensOwn))
          write.csv(herdvarsum, file = "summaries-withinherdvarn-feb.csv", row.names=F)
            
        
        
        
      
      #### c) behavior type summaries ####
        
        # behavsums <- read.csv("summaries-behav-feb.csv") # or run below
        # behavsumslong <- t(behavsums)
      
        
        behavsums <- dat %>%
          group_by(behavO) %>%
          summarise(
            n = n(),
            ppn = n/nrow(dat),
            predForAvg = mean(predFor),
            predForSd = sd(predFor),
            predFormin = min(predFor),
            predFormax = max(predFor),
            # delatFor not so much
            deltaForMed = median(deltaFor),
            deltaForMin = min(deltaFor),
            deltaForMax = max(deltaFor),
            deltaForIQR = IQR(deltaFor),
            # for age, don't count the 10 elk we have no age for
            oldY = length(which(Old == 1)),
            oldPpn = length(which(Old == 1)) / (n()-10),
            # density positively skewed
            densMed = median(Dens),
            densMin = min(Dens),
            densMax = max(Dens),
            densIQR = IQR(Dens),
            # irrig can count everybody
            irrigY = length(which(irrig == 1)),
            irrigPpn = length(which(irrig == 1)) / n(),
            # and human land use suuuuper skewed
            densOwnMed = median(densOwn),
            densOwnMin = min(densOwn),
            densOwnMax = max(densOwn),
            densOwnIQR = IQR(densOwn)) 
        write.csv(behavsums, "summaries-behav-feb.csv", row.names=F)
        write.csv(t(behavsums), "summaries-behav-longways-feb.csv")

        # migration
       
          migparams <- read.csv("migrant-parameters.csv")
          
          # distance
          hist(migparams$delta)
          summary(sqrt(migparams$delta))
          
          # mid-movement date
          hist(migparams$theta)
          summary(sqrt(migparams$theta))
          
          # duration on summer
          hist(migparams$rho)
          summary(migparams$rho)
          IQR(migparams$rho)
        
          
        
       # deltafor
      
      
      # predfor
      
      
      # consdens
      summary(dat$Dens)
      table(dat$Dens, dat$Behav)
      

      # age
      summary(dat$Old)
      table(dat$Behav, dat$Old)
      chisq.test(dat$Old, dat$Behav)
      chisq.test(table(dat$Behav, dat$Old))
      # chi-sq = 10.898, df = 2, p-val = 0.004301

      
      #densown
      
      
      # ppnAg
      table(dat$Behav, dat$irrig)
      chisq.test(table(dat$Behav, dat$irrig))
      # chi-sq = 0.50484, df = 2, p-val = 0.7769
      hist(herdsums$ppnIrrig)
      summary(herdsums$ppnIrrig)
      #IQR: > 0.8361-0.3956 = 0.4405
      
        
        
  #### Prediction data ####
        
      ## i have no idea what this does tbh but predict.clm2 rdocumentation includes it so...
      options(contrasts = c("contr.treatment", "contr.poly"))
      

      ## duplicate data for each response category (make all possible combos of covs for ea behav)
      dupedat <- expand.grid(behavO = unique(dat$behavO), 
        predFor = unique(dat$predFor),
        deltaFor = unique(dat$deltaFor),
        irrig = as.factor(c(0, 1)),
        Dens = unique(dat$Dens),
        Old = as.factor(c(0, 1)),
        densOwn = unique(dat$densOwn))
    

    
  #### check covariates for collinearity & assess univariate relationships ####
      
      dat.cor <- dat %>%
        dplyr::select(behavO, predFor, deltaFor, Dens, Old, densOwn, irrig)
      source("pairs-panels.R")
      pairs.panels(dat.cor)
      # all < 0.60
      
      
      
      
### ### ### ### ##
#### |MODELS| ####
### ### ### ### ##
       
       

    #### a priori models ####
      
      m1 <- clmm(behavO ~ deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m2 <- clmm(behavO ~ predFor + deltaFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m3 <- clmm(behavO ~ predFor + deltaFor + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m4 <- clmm(behavO ~ predFor + deltaFor + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m5 <- clmm(behavO ~ predFor + deltaFor + Dens + deltaFor:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m7 <- clmm(behavO ~ predFor + deltaFor + Old + deltaFor:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m8 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m9 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m10 <- clmm(behavO ~ densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m12 <- clmm(behavO ~ predFor + deltaFor + densOwn + deltaFor:predFor + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m13 <- clmm(behavO ~ predFor + deltaFor + densOwn + deltaFor:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m14 <- clmm(behavO ~ densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + densOwn:Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m16 <- clmm(behavO ~ irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m19 <- clmm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m20 <- clmm(behavO ~ predFor + deltaFor + irrig + predFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m21 <- clmm(behavO ~ irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m22 <- clmm(behavO ~ predFor + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m23 <- clmm(behavO ~ predFor + irrig + Old + Dens + Old:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m24 <- clmm(behavO ~ densOwn + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m25 <- clmm(behavO ~ densOwn + irrig + Dens + densOwn:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      m26 <- clmm(behavO ~ predFor + irrig + densOwn + irrig:densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      
      
      # compete with AICc 
      mods <- list()
      modnms <- paste0("m", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab <- data.frame(aictab(cand.set = mods, modnames = modnms))
      write.csv(aictab, "aic-allbehavmods-feb.csv", row.names=F)
      
      # identify prelim top-supported models (still need to look at LL, K, etc) 
      ftw <- subset(aictab, Delta_AICc < 4.0); ftw <- droplevels(ftw)
      

      # and split out moderately-supported models from top model
      topmod <- m19
      okmods <- ftw[ftw$Modnames != "m19",]
      
      
      

  #### AMERICA'S NEXT TOP MODELS! #### 
      
    # add back in the indivs for whom we don't have age estimations (age isn't a covariate; might as well use everybody)
    modDelta <- clmm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = dat)  
    modDens <- clmm(behavO ~ predFor + irrig + Dens + irrig:Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = dat)  

    #### summary info ####
      
      summary(modDelta)
      summary(modDens)

  
    #### test proportional odds assumptions ####
      
      
      ## [not reported] nominal & scale effects (does predFor have same effect on ea response category?) ##
          
          ## predFor  - nominal effect
          mod2nompred <- clmm2(behavO ~ deltaFor + irrig + deltaFor:irrig, nominal = ~predFor, 
            random = Herd, Hess = TRUE, nAGQ = 10, dat = dat)
          anova(mod2, mod2nompred)
          # no evidence of difference in how behavs respond to predfor (p = 0.449)
          
          ## predFor - scale effect
          mod2scalepred <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, scale = ~predFor, 
            random = Herd, Hess = TRUE, nAGQ = 10, dat = dat) 
          anova(mod2, mod2scalepred) 
          # samesies p = 0.9698

      
      ## thresholds (does whole model describe diff effects for ea response category?) ##

          ## define topmodel using clmm2 so anova will work (using default flexible thresholds)
          mod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
            random = Herd, Hess = TRUE, nAGQ = 10, dat = dat, threshold = "flexible")  
          
          ## same model but with equidistant thresholds
          mod2equ <-  clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
            random = Herd, Hess = TRUE, nAGQ = 10, dat = dat, threshold = "equidistant")  
          
          ## test for diffs
          anova(mod2, mod2equ)
          # no diff in likelihood between the models (p = 1)      
      
        
    
     #### test whether random effect merits increased model complexity ####
        
  
        ## define model without random effect
        nore <- clm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, dat = dat) 

        ## compete top model with the one without random effect
        anova(mod2, nore)   # p = 9.63e-11, strong support for including random effect      
        summary(nore); summary(mod2)
        
        
        
    #### compare top model to one of just herd (for funzies) ####
        
        modHerd <- clm2(behavO ~ Herd, dat = dat)
        anova(mod2, modHerd) # p = 9.78e-05, herd alone explains behavior better :(
        summary(mod2); summary(modHerd) # oh but Hessian is freakin' huge for herd model
                                        # and so are z values for 3 herds
        mod2$ranef
        levels(dat$Herd)
        
        
    
    #### convert to estimates and CIs to odds ratios and store ####

      ciTop <- confint(modDelta)
      coefsTop <- coef(modDelta)
      orsTop <- exp(cbind(OR = coefsTop, ciTop))
      resTop <- data.frame(orsTop) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, CIlow = X2.5.., CIhigh = X97.5..)
      write.csv(resTop, "results-topmodel-feb-delta.csv", row.names=F)              
        
      ciTop2 <- confint(modDens)
      coefsTop2 <- coef(modDens)
      orsTop2 <- exp(cbind(OR = coefsTop2, ciTop2))
      resTop2 <- data.frame(orsTop2) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, CIlow = X2.5.., CIhigh = X97.5..)
      write.csv(resTop2, "results-topmodel-feb-dens.csv", row.names=F)          
      
      
      
    #### herd effects ####
      
      
        
        ## deltaFor model ##
      
            # herdeffectDelta <- read.csv("herdeffect-feb-delta.csv") # or run below
  
            # calc center +- spread of variance per herd, ordered low to high
            herdDelta <- modDelta$ranef + qnorm((1+0.95)/2) * sqrt(modDelta$condVar) %o% c(-1, 1) # %o% = genius
        
            # df of herd number, center, and spread
            herd2Delta <- data.frame(cbind(herdDelta, modDelta$ranef)) %>%
              tibble::rownames_to_column()  
            colnames(herd2Delta) <- c("herdNum", "CIlow", "CIhigh", "Est")
            herd2Delta$herdNum <- as.integer(herd2Delta$herdNum)
            
            # map herd number to correct herd
            herdeffectDelta <- data.frame(Herd = levels(modDelta$model$Herd), herdNum = 1:16) %>%
              left_join(herd2Delta, by = "herdNum")  %>%
              mutate(Herd = factor(Herd, levels = levels(popdat$Herd)),
                     CIoverlap = ifelse(CIlow < 0 & CIhigh > 0, 1, 0))
            write.csv(herdeffectDelta, file = "herdeffect-feb-delta.csv", row.names = F)
                  
            
            
                  
          ## dens model ##
            
              # herdeffectDens <- read.csv("herdeffect-feb-dens.csv") # or run below
  
            # calc center +- spread of variance per herd, ordered low to high
            herdDens <- modDens$ranef + qnorm(0.975) * sqrt(modDens$condVar) %o% c(-1, 1) # %o% = genius
        
            # df of herd number, center, and spread
            herd2Dens <- data.frame(cbind(herdDens, modDens$ranef)) %>%
              tibble::rownames_to_column()  
            colnames(herd2Dens) <- c("herdNum", "CIlow", "CIhigh", "Est")
            herd2Dens$herdNum <- as.integer(herd2Dens$herdNum)
            
            # map herd number to correct herd
            herdeffectDens <- data.frame(Herd = levels(modDens$model$Herd), herdNum = 1:16) %>%
              left_join(herd2Dens, by = "herdNum")  %>%
              mutate(Herd = factor(Herd, levels = levels(popdat$Herd)),
                     CIoverlap = ifelse(CIlow < 0 & CIhigh > 0, 1, 0))
            write.csv(herdeffectDelta, file = "herdeffect-feb-dens.csv", row.names = F) 
            
        


### ### ### ### ### #
#### |MODEL FIT| ####
### ### ### ### ### #
        
          
        
      #### pseudo r-sq ####
        
        # define null model to compare actual model to
            # if null includes random effects, results tell variance explained by fixed effects alons
            # if null doesn't include random effects, results tell variance explained by all effects
        nullmod.nore <- clm(behavO ~ 1, Hess = TRUE, nAGQ = 10, data = dat)
        nullmod.re <- clmm(behavO ~ 1 + (1|Herd), Hess = TRUE, nAGQ = 10,  data = dat)   

          
        # Nagelkerke #
        
          (nag.c <- r.squaredLR(modDelta, null = nullmod.nore)) # 0.31 explained by delta model
          (nag.c2 <- r.squaredLR(modDens, null = nullmod.nore)) # 0.30 explailed by dens model
          (nag.m <- r.squaredLR(modDelta, null = nullmod.re)) # 0.12 explained by fixed effects alone    
        

        
         
      #### ROC per behavior (just out of curiosity; probably not mathematically sound) ####
        
        
        #### predict probability of each behavior type ####
        # (can only do one at a time bc predict() predicts probability of one specific behavior given covs)
       
          # first predict probability of actual observed behaviors
          testdat1 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig)
          testpred1 <- predict(mod2, newdata = testdat1)
          pred1 <- cbind(testdat1, testpred1) %>%
            rename(behavior = behavO, prediction = testpred1) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          # predict using other instead of resident, resident instead of migrant, migrant instead of other
          testdat2 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "other",
              ifelse(behavO == "migrant", "resident", "migrant"))) %>%
            dplyr::select(-behavO)
          testdat2$behavO <- factor(testdat2$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred2 <- predict(mod2, newdata = testdat2)
          pred2 <- cbind(testdat2, testpred2) %>%
            rename(behavior = behavO, prediction = testpred2) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
          
          
          # predict final behav option
          testdat3 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig) %>%
            mutate(behav = ifelse(behavO == "resident", "migrant",
              ifelse(behavO == "other", "resident", "other"))) %>%
            dplyr::select(-behavO)
          testdat3$behavO <- factor(testdat3$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
          testpred3 <- predict(mod2, newdata = testdat3)
          pred3 <- cbind(testdat3, testpred3) %>%
            rename(behavior = behavO, prediction = testpred3) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
    
          
          # combine predictions and observations
          preds <- coalesce(pred1, pred2, pred3) %>%
            mutate(actualBehav = dat$behavO)

          # determine which behavior was predicted to be most likely
          preds$predBehav <- colnames(preds)[apply(preds,1,which.max)]
          preds <- preds %>% 
          mutate(didMig = as.numeric(ifelse(actualBehav == "migrant", 1, 0)),
            didRes = as.numeric(ifelse(actualBehav == "resident", 1, 0)),
            didInt = as.numeric(ifelse(actualBehav == "other", 1, 0)))
          

        # auc - migrant
        predClass <- prediction(preds$migrant, preds$didMig)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.72, "acceptable discrimination"
        
        # auc - intermediate
        predClass <- prediction(preds$other, preds$didInt)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.69, acceptable if you round up...
        
        
        # auc - resident
        predClass <- prediction(preds$resident, preds$didRes)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.68, acceptable if you round up...
        
        (0.72+0.69+0.68)/3 # 0.696 avg all
        
        
        # assess percentage of correct predictions
        preds$matchBehav = ifelse(preds$actualBehav == preds$predBehav, 1, 0)
        length(which(preds$matchBehav == 1)) / nrow(preds)
        # model predicts correctly 59% of the time
        # which is better than a random guess (33%) so i got that going for me which is nice
        # and that value aligns well with bayesian conceptions of acceptable model fit
        


        
### ### ### ### ###
#### |VISUALS| ####
### ### ### ### ###
 
        
        # oikos width options (PS - panel letters (lowercase) eg (a)) (pps - <50MB) (ppps = col = :))
        wcol1.0 = 8 # units = cm
        wcol1.5 = 12.5 # 1.5 column
        wcol2 = 16.6
    
        
    #### Proportion behavior by herd ####  
      
      # make it longform
      poppns <- popdat %>%
          dplyr::select(Herd, ppnMig, ppnOth, ppnRes) %>%
          tidyr::gather(key = behav, value = ppn, -Herd) %>%
          mutate(behav = ifelse(behav == "ppnMig", "Migrant",
            ifelse(behav == "ppnOth", "Intermediate", "Resident")) ) %>%
          mutate(behav = factor(behav, levels = c("Resident", "Intermediate", "Migrant"), ordered = TRUE)) %>%
          mutate(Herd = ifelse(Herd == "Dome", "N. Yellowstone", 
            ifelse(Herd == "Sapphire", "N. Sapphires", paste(Herd)))) %>%
          mutate(Herd = factor(Herd, levels = Herd[order(popdat$ppnMig)]))

        
      # bar graph - color
      pppp <- ggplot(poppns, aes(x = Herd, ppn, fill = behav)) +
        geom_bar(stat = "identity", position = 'fill') +
        labs(x = "", y = "Proportion") +
        theme(text = element_text(size=16),
          axis.text.x = element_text(angle = 55, hjust = 1),
          legend.title=element_blank())
          
          ggsave("./Plots/herd-behavppns.jpg",
            plot = pppp,
            device = "jpeg",
            dpi = 600,
            units = "cm",
            width = wcol2)

          
      # bar graph - b&w
      ppbw <- ggplot(poppns, aes(x = Herd, ppn, fill = behav)) +
        geom_bar(stat = "identity", position = 'fill') +
        labs(x = "", y = "Proportion") +
        theme_bw() +
        theme(text = element_text(size=16),
          # rotate x axis text & center below bars
          axis.text.x = element_text(angle = 55, hjust = 1),
          legend.title=element_blank(),
          # remove background grid
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_fill_grey(start = 0.9, end = 0)
          
          ggsave("./Plots/herd-behavppns-bw.jpg",
            plot = ppbw,
            device = "jpeg",
            dpi = 600,
            units = "cm",
            width = wcol2,
            height = wcol2)          
      
          
    #### Covariate summaries per herd ####
      
      par(mfrow = c(3, 1))
      hist(herdcovs$irrigPpn)
      hist(herdcovs$deltaForIQR)
      hist(herdcovs$densOwnIQR)
      

        
    #### Effects per herd ####

      
         # transparent plot to paste over bar graph (use delta model bc best supported)
         p <- ggplot(herdeffect, 
           aes(y = Est, x = Herd, ymin = CIlow, ymax = CIhigh)) +
           geom_point() +
           geom_errorbar(width = 0.1) +
           geom_hline(yintercept = 0) +
           scale_y_continuous(position = "right") +
           theme(
             axis.text.x=element_text(angle=90, hjust=1),
             panel.background = element_rect(fill = "transparent", colour = NA),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             plot.background = element_rect(fil = "transparent", colour = NA)
             )
         ggsave(p, filename = "./Plots/herdeffect-feb.png", bg = "transparent")


         # thick white version of above (for ms)
         pbw <- ggplot(herdeffect, 
           aes(y = Est, x = Herd, ymin = CIlow, ymax = CIhigh)) +
           geom_point(color = "white", size = 3) +
           geom_errorbar(width = 0.4, color = "white", size = 1) +
           geom_hline(yintercept = 0, color = "white") +
           scale_y_continuous(position = "right") +
           theme(
             axis.text.x=element_text(angle=90, hjust=1),
             panel.background = element_rect(fill = "transparent", colour = NA),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             plot.background = element_rect(fil = "transparent", colour = NA)
             )
  
         ggsave(pbw, filename = "./Plots/herdBw.jpg",
           bg = "transparent",
           device = "png",
           dpi = 600,
           units = "cm",
           width = wcol2,
          height = wcol2)        
         
         
         
        
    #### Prediction plots - Top models ####
      
      
        ## make predictions ##
         
         # specify models with clmm2 to allow prediction
  
           mod2Delta <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
              random = Herd, Hess = TRUE, nAGQ = 10, dat = dat, threshold = "flexible")  
            summary(mod2Delta) # Hess = 18794.45 => model not ill-defined (see clmm2 tutorial)
            
              
           mod2Dens <- clmm2(behavO ~ predFor + Dens + irrig + Dens:irrig, 
              random = Herd, Hess = TRUE, nAGQ = 10, dat = dat, threshold = "flexible")  
          summary(mod2Dens) # Hess = 29786.17 => model not ill-defined (see clmm2 tutorial)

    
            ## predict probability of falling into each response category given those same data
            predprobDelta <- predict(mod2Delta, newdata = dupedat)
            predprobDens <- predict(mod2Dens, newdata = dupedat)
                        
                ## sanity check, both should = 1
                sum(predprobDelta[1]+predprobDelta[2]+predprobDelta[3])
                sum(predprobDelta[4]+predprobDelta[5]+predprobDelta[6])
                ## sanity check, both should = 1
                sum(predprobDens[1]+predprobDens[2]+predprobDens[3])
                sum(predprobDens[4]+predprobDens[5]+predprobDens[6])
    
            ## combine predictions with data used to predict
            newdatDelta <- cbind(dupedat, predprobDelta)
            head(newdatDelta)
            
            ## combine predictions with data used to predict
            newdatDens <- cbind(dupedat, predprobDens)
            head(newdatDens)
            
            ## store
            write.csv(newdatDelta, file = "predictions-topmod-feb-delta.csv", row.names = F)
            write.csv(newdatDens, file = "predictions-topmod-feb-dens.csv", row.names = F)    
            
            
        ## plot predictions ##
            
            ## read in stord data from above
            # newdatDelta <- read.csv("predictions-topmod-feb-delta.csv")
            # newdatDens <- read.csv("predictions-topmod-feb-dens.csv")
            
            ## pull random subsample of 10000 predictions (takes forEVer otherwise)
            subdatDelta <- newdatDelta[sample(nrow(newdatDelta), 10000),] 
            subdatDelta$behavO <-  factor(subdatDelta$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
            subdatDens <- newdatDens[sample(nrow(newdatDens), 10000),]
            subdatDens$behavO <-  factor(subdatDens$behav, levels = c("resident", "other", "migrant"), ordered = TRUE)

            
            ## predFor - color
            pp.pf <- ggplot(subdatDelta, aes(x = predFor, y = predprobDelta, colour = behavO)) +
              geom_smooth(se = FALSE) +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(
                x = "Forage predictabilty \n (- 6-yr stdev of NDVI amplitude)", 
                y = "Predicted probability") +
              theme_minimal() +
              theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5))  

            ggsave("./Plots/predFor-col.jpg",
              plot = pp.pf,
              device = "jpeg",
              dpi = 600)   
            
            
            
            ## predFor - b&w for thesis
            pp.pf.bw <- ggplot(subdatDelta, aes(x = predFor, y = predprobDelta, linetype = behavO)) +
              geom_smooth(se = FALSE, color = "black", size = 0.5) +
              scale_linetype_manual(
                values=c("solid", "dotdash", "dotted"), 
                name = "", labels = c("Migrant", "Intermediate", "Resident")) +
              labs(
                x = "Forage predictabilty \n (- 6-yr stdev of NDVI amplitude)", 
                y = "Predicted probability") +
              theme_minimal() +
              theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5))  
            

            ggsave("./Plots/predFor-bw.jpg",
              plot = pp.pf.bw,
              device = "jpeg",
              dpi = 600,
              units = "cm",
              width = wcol2,
              height = wcol2/2)   
            


            
            ## deltaFor on Ag - color for presentations
            pp.df <- ggplot(
              subdatDelta[subdatDelta$irrig == 1,], 
              aes(
                x = deltaFor, 
                y = predprobDelta, 
                colour = behavO#,
                #linetype = behavO # need to play with this for legend to work
                )
              ) +
              geom_smooth(se = FALSE) +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(
                x = "Forage difference \n (maxNDVI outside - inside winter range)", 
                y = "Predicted probability",
                title = "Agriculture on winter range") +
              theme_minimal() +
              theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) 
            
            

            ggsave("./Plots/deltaFor-col.jpg",
              plot = pp.df,
              device = "jpeg",
              dpi = 600)             
            
            
            
            ## deltaFor on Ag - b&w for thesis
            pp.df.bw <- ggplot(
              subdatDelta[subdatDelta$irrig == 1,], 
              aes(
                x = deltaFor, 
                y = predprobDelta, 
                linetype = behavO)) +
              geom_smooth(se = FALSE, color = "black", size = 0.5) +
              scale_linetype_manual(
                values = c("solid", "dotdash", "dotted"),
                name = "", labels = c("Migrant", "Intermediate", "Resident")) +
              labs(
                x = "Summer forage difference \n (maxNDVI outside - inside winter range)", 
                y = "Predicted probability",
                title = "Agriculture on winter range") +
              theme_minimal() +
              theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5)) 
            
  
            ggsave("./Plots/deltaFor-bw.jpg",
              plot = pp.df.bw,
              device = "jpeg",
              dpi = 600,
              units = "cm",
              width = wcol2,
              height = wcol2/2)               

            
            
            
            ## dens on ag
            pp.dn <- ggplot(subdatDens[subdatDens$irrig == 1,], aes(x = Dens, y = predprobDens, colour = behavO)) +
              geom_smooth(se = FALSE) +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(
                x = "Index of conspecific density", 
                y = "Predicted probability",
                title = "Agriculture on winter range") +
              theme_minimal() +
              theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) 
 

            
            ## export relevant plots from above with journal-specific standards
            ggsave("./Plots/predFor-nogray-feb.jpg", 
              plot = pp.pf, 
              device = "jpeg",
              dpi = 600,
              units = "cm",
              width = wcol2)
            ggsave("./Plots/deltaFor-nogray-feb.jpg", 
              plot = pp.df, 
              device = "jpeg",
              dpi = 600,
              units = "cm",
              width = wcol2)            
            ggsave("./Plots/dens-nogray-feb.jpg",
              plot = pp.dn,
              device = "jpeg",
              dpi = 600,
              units = "cm",
              width = wcol2)

   
                 
    #### Coefficient estimates - Averaged model ####     

        # remove threshold info 
        avgors <- avgdat %>%
          filter(!grepl("other", cov))
              
    
        ## plot all
            
        ggplot(avgors, 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 1, linetype = "dotted") +
          labs(y = "OR", x = "", title = "85% CI")       
          
        
        ## plot without densOwn (huge CI messes up scale)
        ggplot(avgors[avgors$cov != "densOwnCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 1, linetype = "dotted") +
          labs(y = "OR", x = "", title = "85% CI") +
          ylim(low = 0, high = 5)
        
        
        ## separate plots of just densOwn, Age, dens:irrig (for MT TWS presn)
        p.do <- ggplot(avgors[avgors$cov == "densOwnCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          labs(y = "", x = "", title = "Intensity of human use") +
          scale_x_discrete(labels="") +
          theme_light() +
          theme(text = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5))
        p.ol <- ggplot(avgors[avgors$cov == "oldCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          labs(y = "", x = "", title = "Age (Old)") +
          scale_x_discrete(labels="") +
          theme_light() +
          theme(text = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5))
        p.da <- ggplot(avgors[avgors$cov == "densCtr:irrigCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          labs(y = "Odds ratio", x = "", title = "Density with Irrigated ag") +
          scale_x_discrete(labels="") +
          theme_light() +
          theme(text = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5))
        covplots <- grid.arrange(p.da, p.ol, p.do, ncol = 3)

        
        ggsave("./Plots/avgmod-nogray.jpg", 
          plot = covplots, 
          device = "jpeg",
          dpi = 300)                
        
        
        ## pull out just ag interactions
        avgsub <- avgors %>%
          filter(cov == "densCtr" | cov == "densCtr:irrigCtr" |
              cov == "deltaForCtr" | cov == "deltaForCtr:irrigCtr") 
        # dplyr hates me so i'm doing this an awful way
        avgsub[1,1] <- "Density"
        avgsub[2,1] <- "Density with Ag"
        avgsub[3,1] <- "Forage Difference"
        avgsub[4,1] <- "Forage Diff with Ag"
        lvs <- c("Density", "Density with Ag", "Forage Difference", "Forage Diff with Ag")
        avgsub$cov <- factor(avgsub$cov, levels = lvs)
        
        ## plot just ag interactions
        densag <- ggplot(avgsub[grepl("Dens", avgsub$cov),], 
          aes(y = coefs, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0) +
          labs(title = "Conspecific density", y = "Coefficient", x = "") +
          theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5)) +
          ylim(-0.07, 0.275) +
          scale_x_discrete(labels=c("Density" = "Influence alone", 
            "Density with Ag" = "Influence with Ag"))
        forag <- ggplot(avgsub[grepl("For", avgsub$cov),], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0) +
          labs(title = "Difference in forage", y = "", x = "") +
          theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5)) +
          ylim(-0.07, 0.275) +
          scale_x_discrete(labels=c("Forage Difference" = "Influence alone", 
            "Forage Diff with Ag" = "Influence with Ag"))
        ags <- grid.arrange(densag, forag, ncol = 2) 
        
        
        ## export ##
        ggsave("./Plots/ag-intrxns.jpg", 
             plot = ags, 
             device = "jpeg",
             dpi = 300)        
              
        
        
        
        #### profile likelihood ####
        
        pr1 <- profile(topmod2, alpha=1e-4)
        pr1 # this is asymmetric, therefore asymmetric confidence intervals are most appropriate

