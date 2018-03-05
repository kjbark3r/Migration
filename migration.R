### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#  ASSESSING RELATIVE INFLUENCE OF FORAGE, HUMANS, AND    #
#    INTRINSIC FACTORS ON MIGRATORY BEHAVIOR OF ELK       #
#                   KRISTIN BARKER                        #
#                 DEC 2017 - FOREVER                      #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




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
    dat <- read.csv("moddat.csv") # indivdual data
    popdat <- read.csv("pop-summaries.csv") # herd summaries
    
    # format behavior as ordinal (res < int < mig) and format dummy variables as factors
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    dat$irrig <- factor(dat$irrig)
    dat$Old <- factor(dat$Old)
    
    
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
      
      summaries <- read.csv("summaries-covariates.csv")
      
      
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
      write.csv(summaries, "summaries-covariates.csv", row.names = F)
      

      
      #### b) population summaries ####
      
      
        herdsums <- read.csv("summaries-herds.csv")

      
        herdsums <- dat %>%
    	    group_by(Herd) %>%
    	    summarise(nIndivs = n(),
    	              nMig = length(which(Behav == "migrant")),
    	              nRes = length(which(Behav == "resident")),
    	              nOth = length(which(Behav == "other"))) %>%
    	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
    	           ppnRes = round(nRes/nIndivs, digits =2),
    	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
    	    dplyr::select(Herd, ppnMig, ppnRes, ppnOth, nIndivs)
        write.csv(herdsums, "summaries-herds.csv", row.names=F)
        
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
      
      
      
      #### c) behavior type summaries ####
        
        behavsums <- read.csv("summaries-behav.csv")
        behavsumslong <- t(behavsums)
      
        
        behavsums <- dat %>%
          group_by(behavO) %>%
          summarise(
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
        write.csv(behavsums, "summaries-behav.csv", row.names=F)
        write.csv(t(behavsums), "summaries-behav-longways.csv")

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
      


      
      # age
      
      
      #densown
      
      
      # ppnAg
      
      
 
        
        
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
       
       

    #### define a priori models ####
      
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
      
      
      # compete with AICc #
      mods <- list()
      modnms <- paste0("m", rep(1:26))
      for (i in 1:length(modnms)) { mods[[i]] <- get(modnms[[i]]) }
      aictab(cand.set = mods, modnames = modnms)
      aictab <- data.frame(aictab(cand.set = mods, modnames = modnms))
      write.csv(aictab, "aic-allbehavmods.csv", row.names=F)
      
      # identify prelim top-supported models (still need to look at LL, K, etc) 
      ftw <- subset(aictab, Delta_AICc < 4.0); ftw <- droplevels(ftw)
      
      # remove m15 from list of supported (only w/i 4 deltaAIC bc of extra parameter; LL not appreciably better)
      ftw <- ftw[ftw$Modnames != "m15",]
      
      # and split out moderately-supported models from top model
      topmod <- m19
      okmods <- ftw[ftw$Modnames != "m19",]
      
      
      

  #### AMERICA'S NEXT TOP MODEL! #### 
      
    # add back in the indivs for whom we don't have age estimations (age isn't a covariate; might as well use everybody)
    mod <- clmm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = dat)  
      
    #### summary info ####
      
      summary(mod)
      coef(mod)
      confint(mod, level = 0.85)
      confint(mod, type = "Wald", level = 0.85) # these are the same, good
      
    

        
  
    #### test proportional odds assumption ####
        
      # Agresti 2002 says this assumption should be based more on the inference you want to draw than
      # methematical considerations, but out of curiosity... (using models without random herd effect) 
      poddstest <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, Hess = TRUE, nAGQ = 10, dat = dat) 
      nominal_test(poddstest) # nothing significant, suggests meet assumption
      scale_test(poddstest) # predfor is significant; try scaling by it
      
      test <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, scale = ~predFor, dat = dat)
      summary(test) # worse LL and AIC than top model; not changing it.
      
        
     #### test whether random effect merits increased model complexity ####
        
  
        ## define model without random effect
        nore <- clm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, dat = dat) 
        
        ## define top model using clmm2 (to allow comparison with above model and for predict() to work) ##
        topmod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
                         random = Herd, Hess = TRUE, nAGQ = 10, dat = dat) 
        
        ## compete with model that includes random effect
        anova(topmod2, nore)   # p = 9.63e-11      
        summary(nore); summary(topmod2)
        
        
    #### play with standard errors adjusting for possible overdispersion ####
        
        ## SE * sqrt(Chi-sq/df)
        
        # try with deltafor:irrig cov
        se.di <- 0.05587
        est.di <- 0.15671
        se.adj <- se.di * (sqrt(chi.pr/dof.pr))
        se.new <- se.di*se.adj
        0.1567 - se.new
        0.1567 + 2*se.low
        confint(mod)

        
        
    
    #### convert to estimates and CIs to odds ratios and store ####
        
      resTop <- read.csv("results-topmodel.csv")
      
      ciTop <- confint(mod)
      coefsTop <- coef(mod)
      orsTop <- exp(cbind(OR = coefsTop, ciTop))
      resTop <- data.frame(orsTop) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, CIlow = X2.5.., CIhigh = X97.5..)
      write.csv(resTop, "results-topmodel.csv", row.names=F)              
        
  #### MODERATELY-SUPPORTED MODELS #### 
      
      #### looking at each ####
        
        ftw$Modnames
        # remove unsupported model (m19) and top model (m15)
        bottommods <- ftw[ftw$Modnames != "m19" & ftw$Modnames != "m15",]
        
        # Arnold 2010 recommends 85% CI | #CI!=0 other than predfor
        confint(m2, level = 0.85) 
        confint(m6, level = 0.85) 
        confint(m3, level = 0.85) 
        confint(m22, level = 0.85) 
        confint(m11, level = 0.85) #irrig & irrig:dens
        confint(m4, level = 0.85) 
      
      
      
  #### MODEL AVERAGING #### 
      
      
      #### center all covariates (because interaction terms) ####
      
      ctrdat <- olddat %>%
        mutate(predForCtr = predFor - mean(predFor),
               deltaForCtr = deltaFor - mean(deltaFor),
               irrigCtr = as.numeric(irrig) - mean(as.numeric(irrig)),
               densCtr = Dens - mean(Dens),
               oldCtr = as.numeric(Old) - mean(as.numeric(Old)),
               densOwnCtr = densOwn - mean(densOwn))
        
        
      #### define models using centered covariates ####
        
        m19c <- clmm(behavO ~ predForCtr + deltaForCtr + irrigCtr + deltaForCtr:irrigCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)  
        m2c <- clmm(behavO ~ predForCtr + deltaForCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m6c <- clmm(behavO ~ predForCtr + deltaForCtr + oldCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m3c <- clmm(behavO ~ predForCtr + deltaForCtr + deltaForCtr:predForCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m22c <- clmm(behavO ~ predForCtr + irrigCtr + densCtr + irrigCtr:densCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)  
        m11c <- clmm(behavO ~ predForCtr + deltaForCtr + densOwnCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m4c <- clmm(behavO ~ predForCtr + deltaForCtr + densCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
   
             
      #### average the models ####
        
        avgmod <- model.avg(m19c, m2c, m6c, m3c, m22c, m11c, m4c) 
        summary(avgmod)
        round(confint(avgmod, level = 0.85), 2)
        round(confint(avgmod, level = 0.85, full = T), 2)

        
      #### exponentiate relevant info from model ####
        
        avgdat <- read.csv("results-avgmodel.csv")
        
        
        orAvg <- round(exp(coef(avgmod)), 2)
        CI85 <- round(exp(confint(avgmod, level = 0.85)), 2)
        
        coefAvgUncond <- data.frame(
          OR = round(exp(coef(avgmod)), 2),
          ciLow85 = round(exp(confint(avgmod, level = 0.85, full = T)), 2)[,1],
          ciHigh85 = round(exp(confint(avgmod, level = 0.85, full = T)), 2)[,2]
        )


        avgdat <- data.frame(
          OR = orAvg,
          ciLow85 = CI85[,1],
          ciHigh85 = CI85[,2]) %>%
          tibble::rownames_to_column() %>%
          rename(cov = rowname)
        write.csv(avgdat, "results-avgmodel.csv", row.names = F)
        
        
        
        
            
   
        
### ### ### ### ###
#### |VISUALS| ####
### ### ### ### ###
      
        
        
    #### Proportion behavior by herd ####  
      
      # make it longform
      poppns <- popdat %>%
          dplyr::select(Herd, ppnMig, ppnOth, ppnRes) %>%
          tidyr::gather(key = behav, value = ppn, -Herd) %>%
          mutate(behav = ifelse(behav == "ppnMig", "Migrant",
            ifelse(behav == "ppnOth", "Intermediate", "Resident")) ) %>%
          mutate(behav = factor(behav, levels = c("Resident", "Intermediate", "Migrant"), ordered = TRUE)) %>%
          mutate(Herd = factor(Herd, levels = Herd[order(popdat$ppnMig)]))
        
      # oh my what a delightful plot
      ggplot(poppns, aes(x = Herd, ppn, fill = behav)) +
        geom_bar(stat="identity",position='fill') +
        labs(x = "", y = "Proportion") +
        theme(text = element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title=element_blank())
 
        
        
    #### Effects per herd ####

        
      # ordered low to high, not labeled with names
          
          # calc center +- spread of variance per herd, ordered low to high
          herd <- mod$ranef + qnorm(0.975) * sqrt(mod$condVar) %o% c(-1, 1) # %o% = genius
          herd2 <- cbind(herd, m19$ranef)
          herddat <- data.frame(herd2[order(KRISTINYOUEFFEDTHISUP),]) %>%
            rename(CIlow = X1, CIhigh = X2, Est = X3) %>%
            tibble::rownames_to_column() 
          
          # plot
          ggplot(herddat, aes(y = Est, x = rowname,
            ymin = CIlow, ymax = CIhigh)) +
            geom_point() +
              geom_errorbar(width = 0.1) +
              geom_hline(yintercept = 0) 
            
    
     # ordered same as ppn(mig) plot, labeled
          
          herd <- mod$ranef + qnorm(0.975) * sqrt(mod$condVar) %o% c(-1, 1) # %o% = genius
          herd3 <- cbind(herd, m19$ranef)
          herdnums <- data.frame(Herd = unique(dat$Herd), herdNum = 1:16)

          herddat2 <- data.frame(herd3) %>%
            rename(CIlow = X1, CIhigh = X2, Est = X3) %>%
            tibble::rownames_to_column()  %>%
            mutate(herdNum = as.integer(rowname)) %>%
            left_join(herdnums, by = "herdNum")

              
          # plot
          ggplot(herddat2, aes(y = Est, x = Herd,
            ymin = CIlow, ymax = CIhigh)) +
            geom_point() +
              geom_errorbar(width = 0.1) +
              geom_hline(yintercept = 0) +
            theme(axis.text.x=element_text(angle=90, hjust=1))
                          

      
        
    #### Prediction plots - Top model ####
      
      
        ## make predictions ##

        

           
            ## check it out
            summary(topmod2) # Hess = 18295.46 => model not ill-defined (see clmm2 tutorial)

    
            ## predict probability of falling into each response category given those same data
            predprob <- predict(topmod2, newdata = dupedat)
            
                ## sanity check, both should = 1
                sum(predprob[1]+predprob[2]+predprob[3])
                sum(predprob[4]+predprob[5]+predprob[6])
    
    
            ## combine predictions with data used to predict
            newdat <- cbind(dupedat, predprob)
            head(newdat)
            
            ## store
            write.csv(newdat, file = "predictions-topmod.csv", row.names = F)
    
            
            
        ## plot predictions ##
            
            
            ## pull random subsample of 10000 predictions (takes forEVer otherwise)
            subdat <- newdat[sample(nrow(newdat), 10000),] 

            
            ## predFor
            pp.pf <- ggplot(subdat, aes(x = predFor, y = predprob, colour = behavO)) +
              geom_smooth(se = FALSE) +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(x = "Forage predictabilty \n (- 6-yr stdev of NDVI amplitude)", y = "Probability") +
              theme_minimal() +
              theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))  


            ## deltaFor on Ag
            pp.df <- ggplot(subdat[subdat$irrig == 1,], aes(x = deltaFor, y = predprob, colour = behavO)) +
              geom_smooth(se = FALSE) +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(x = "Forage difference \n (maxNDVI outside - inside winter range)", y = "Probability",
              title = "Agriculture on winter range") +
              theme_minimal() +
              theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5)) 
 
            ## export each
            ggsave("./Plots/predFor-nogray.jpg", 
              plot = pp.pf, 
              device = "jpeg",
              dpi = 300)
            ggsave("./Plots/deltaFor-nogray.jpg", 
              plot = pp.df, 
              device = "jpeg",
              dpi = 300)            
         

   
                 
    #### Coefficient estimates - Averaged model ####     

    
        ## plot all
        ggplot(avgdat, 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          labs(y = "OR", x = "", title = "85% CI")       
          
        
        ## plot without densOwn (huge CI messes up scale)
        ggplot(avgdat[avgdat$cov != "densOwnCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          labs(y = "OR", x = "", title = "85% CI")
        
        
        ## separate plots of just densOwn, Age, dens:irrig (MT TWS)
        p.do <- ggplot(avgdat[avgdat$cov == "densOwnCtr",], 
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
        p.ol <- ggplot(avgdat[avgdat$cov == "oldCtr",], 
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
        p.da <- ggplot(avgdat[avgdat$cov == "densCtr:irrigCtr",], 
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
        avgsub <- avgdat %>%
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
        
        # whatever that means
        pr1 <- profile(topmod2, alpha=1e-4)
        pr1 # this is asymmetric, therefore asymmetric confidence intervals are most appropriate
        

        
        
### ### ### ### ### #
#### |MODEL FIT| ####
### ### ### ### ### #
        
          
      #### MY TEST : Proportion of correct predictions ####
        
        
        #### predict probability of each behavior type ####
        
        
          # first predict with original behaviors
          testdat1 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig)
          testpred1 <- predict(topmod2, newdata = testdat1)
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
          testpred2 <- predict(topmod2, newdata = testdat2)
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
          testpred3 <- predict(topmod2, newdata = testdat3)
          pred3 <- cbind(testdat3, testpred3) %>%
            rename(behavior = behavO, prediction = testpred3) %>%
            dplyr::select(behavior, prediction) %>%
            mutate(resident = ifelse(behavior == "resident", prediction, NA),
              other = ifelse(behavior == "other", prediction, NA),
              migrant = ifelse(behavior == "migrant", prediction, NA)) %>%
            dplyr::select(-c(behavior, prediction))
    
          # combine 
          preds <- coalesce(pred1, pred2, pred3)
          
          # and make it a numeric matrix
          predmat <- matrix(nrow = nrow(dat), ncol = length(unique(dat$behavO)))
          attributes(predmat)$dimnames[[1]] <- (1:308)
          attributes(predmat)$dimnames[[2]] <- c("resident", "other", "migrant")
          
          # pathetically combine predictions in numeric matrix 
          predmat[1 : nrow(preds)] <- preds$resident
          predmat[(nrow(preds)+1) : (nrow(preds)*2)] <- preds$other
          predmat[((nrow(preds)*2)+1) : (nrow(preds)*3)] <- preds$migrant
          
        
        hm <- preds
        hm$actualBehav <- dat$behavO
        hm$predBehav <- colnames(hm)[apply(hm,1,which.max)]
        warnings()
        summary(preds)
        # well... shit... this thing never predicts "other" as most likely behavior
        
        hm$matchBehav = ifelse(hm$actualBehav == hm$predBehav, 1, 0)
        length(which(hm$matchBehav == 1)) / nrow(hm)
        # it does predict correctly 59% of the time
        # which is marginally better than a random guess so i got that going for me which is nice 
        
        boxplot(preds)
        
        
        
        
      #### CHI-SQUARE ####        
            

        # FOLLOWING FAGERLAND HOSMER 2013
        
        
        # using df with predicted prob of each behav for each obs,
        faghos <- cbind(dat, preds) %>%
          # only keep columns of interest, and rename some (just for clarity)
          rename(prRes = resident, prInt = other, prMig = migrant) %>%
          dplyr::select(AnimalID, Herd, deltaFor, irrig, predFor, behavO, prRes, prInt, prMig) %>%
          # calculate ordinal score per observation
          mutate(score = prRes + 2*prInt + 3*prMig) %>%
          # partition into 10 groups based on ordinal score
          mutate(g = factor(findInterval(score, 
            quantile(score, probs = c((0:9)/10))))) %>%
          # calculate observed and expected obs per group
          group_by(g) %>%
          summarise(
            obsRes = length(which(behavO == "resident")) / n(),
            expRes = mean(prRes),
            obsInt = length(which(behavO == "other")) / n(), 
            expInt = mean(prInt),
            obsMig = length(which(behavO == "migrant")) / n(),
            expMig = mean(prMig)) %>%
          mutate(
            oeRes = (obsRes - expRes)^2 / expRes,
            oeInt = (obsInt - expInt)^2 / expInt,
            oeMig = (obsMig - expMig)^2 / expMig)
        
        chi.fh <- sum(faghos$oeRes + faghos$oeInt + faghos$oeMig)
        g.fh <- length(unique(faghos$g))
        c.fh <- 3 # bad kristin, fix this
        dof.fh <- (g.fh - 2)*(c.fh - 1) + (c.fh - 2)
        
        pchisq(chi.fh, dof.fh) # [1] 1.365561e-05 
        


        # FOLLOWING PULKSTENIS ROBINSON 2004
        
        # using df with predicted prob of each behav for each obs,
        pulrob <- cbind(dat, preds) %>%
          # only keep columns of interest, and rename some (just for clarity)
          rename(prRes = resident, prInt = other, prMig = migrant) %>%
          dplyr::select(AnimalID, Herd, deltaFor, irrig, predFor, behavO, prRes, prInt, prMig) %>%
          # calculate ordinal score per observation
          mutate(score = prRes + 2*prInt + 3*prMig) %>%
          # identify covariate patterns (levels of ea categorical cov, here just 2)
          mutate(covPat = as.factor(irrig)) %>%
          # split each covariate pattern into 2 groups using median ordinal score of that pattern
          group_by(covPat) %>%
          mutate(medScore = median(score)) %>%
          ungroup() %>%
          mutate(split = ifelse(score >= medScore, 1, 0)) %>%
          mutate(g = as.factor(paste0(covPat, split))) %>%
          # calculate observed and expected frequencies for each response in each group
          group_by(g) %>%
          summarise(
            obsRes = length(which(behavO == "resident")) / n(),
            expRes = mean(prRes),
            obsInt = length(which(behavO == "other")) / n(), 
            expInt = mean(prInt),
            obsMig = length(which(behavO == "migrant")) / n(),
            expMig = mean(prMig)) %>%
          # calculate [(obs-exp)^2 / exp] for each response in each group (for modified chi-sq)
          # and [obs*log(obs/exp)] for each response in each group (for modified deviance)
          mutate(
            oeRes = (obsRes - expRes)^2 / expRes,
            oeInt = (obsInt - expInt)^2 / expInt,
            oeMig = (obsMig - expMig)^2 / expMig,
            dRes = obsRes * log(obsRes/expRes),
            dInt = obsInt * log(obsInt/expInt),
            dMig = obsMig * log(obsMig/expMig)) 
        
        # sum [(o-e)^2/e] vals to get modified pearson chi-sq statistic
        chi.pr <- sum(pulrob$oeRes + pulrob$oeInt + pulrob$oeMig)
        dev.pr <- 2*(sum(pulrob$dRes + pulrob$dInt + pulrob$dMig))
        
        # calculate degrees of freedom
        i.pr <- 2 # I = number of covariate patterns
        j.pr <- 3 # J = number of response categories
        k.pr <- 1 # k = number of categorical terms
        dof.pr <- ((2*i.pr) - 1) * (j.pr - 1) - k.pr - 1
        
        pchisq(chi.pr, dof.pr) # [1] 0.003849673
        pchisq(dev.pr, dof.pr) # [1] 0.002411401

        

        
      #### ROC? ####
        
        # oh boy a new library
        library(ROCR)
        
        rocdat <- hm %>%
          mutate(didMig = as.numeric(ifelse(actualBehav == "migrant", 1, 0)),
            didRes = as.numeric(ifelse(actualBehav == "resident", 1, 0)),
            didInt = as.numeric(ifelse(actualBehav == "other", 1, 0)))

        # auc - migrant
        predClass <- prediction(rocdat$migrant, rocdat$didMig)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.72, "acceptable discrimination"
        
        # auc - intermediate
        predClass <- prediction(rocdat$other, rocdat$didInt)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.69, acceptable only if you round up...
        
        
        # auc - resident
        predClass <- prediction(rocdat$resident, rocdat$didRes)
        prefClass <- performance(predClass, "tpr", "fpr")
        BMauc <- performance(predClass, measure = "auc")
        auc <- as.numeric(BMauc@y.values)
        auc # 0.68, acceptable only if you round up...
        
        (0.72+0.69+0.68)/3 # 0.696 avg all, prob totally invalid math
        
        
        
      #### pseudo r-sq ####
        
        
        nullmod <- clmm(behavO ~ 1 + (1|Herd), data = dat)
        
        # McFadden's pseudo r-squared
        mcF <- 1 - topmod$logLik/nullmod$logLik
        
        # Nagelkerke's ("adj.r.squared" output here)
        r.squaredLR(topmod, null = nullmod) # do not include random effects of original model
        
        # Cox-Snell
        
        # you can calculate r2 for both both marginal (fixed only) and conditional (fixed+random)
        
        # marginal (pseudo) r-squared
        
        # if null includes random effects, results tell variance explained by fixed effects alons
        # if null doesn't include random, results tell conditional
         # use mod not topmod

        nullmod.nore <- clm(behavO ~ 1, Hess = TRUE, nAGQ = 10, data = dat)
        nullmod.re <- clmm(behavO ~ 1 + (1|Herd), Hess = TRUE, nAGQ = 10,  data = dat)   
        
        # McFadden #
        
          (mcF.c <- 1 - (mod$logLik/nullmod.nore$logLik)) # 0.16 varn explained by model
          (mcF.m <- 1 - (mod$logLik/nullmod.re$logLik)) # 0.04 varn explained by fixed effects alone
          
        # Nagelkerke #
        
          (nag.c <- r.squaredLR(mod, null = nullmod.nore)) # 0.30 varn explained by model
          (nag.m <- r.squaredLR(mod, null = nullmod.re)) # 0.07 varn explained by fixed effects alone        
        
     
                
                


        
        
#### [save all the things] ####         
           
  save.image("mig.Rdata")   
      

