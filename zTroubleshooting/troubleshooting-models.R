# figuring out actual modeling

# kristinbarker spr 2018




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Packages ####
  
    library(foreign)
    library(nnet) # multinom
    library(ggplot2) # pretty plots
    library(gridExtra) # >1 plot per display
    library(reshape2) # no idea, from idre stats ex
    library(ordinal) # ordinal response
    library(VGAM) # non-proportional odds
    library(AICcmodavg) # model comparison
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
    

    
  #### "Raw" data  ####
    
    deltafor <- read.csv("deltafor.csv") # from forage.R
    predfor <- read.csv("predfor.csv") # from forage.R
    dens <- read.csv("dens-indiv.csv") # from consdens.R
    age <- read.csv("indiv-dat.csv") # from dataprep-elkdb.R
    behav <- read.csv("mig-behav.csv") # from migbehav-migrateR.R
    behavpop <- read.csv("behav-per-popn.csv") # from migbehav-migrateR.R
    popndens <- read.csv("dens-group.csv") # from consdens.R
    
  #### Model data  ####
    
    
    dat <- age %>%
      dplyr::select(AnimalID, Old) %>%
      inner_join(behav, by = "AnimalID") %>%
      dplyr::select(AnimalID, Behav, Old) %>%
      left_join(dens, by = "AnimalID") %>%
      left_join(predfor, by = "Herd") %>%
      dplyr::select(AnimalID, Behav, Old, Dens, Herd, StDevAmp, StDevTi) %>%
      rename(PredForAmp = StDevAmp, PredForTi = StDevTi) %>%
      left_join(deltafor, by = "AnimalID") %>%
      dplyr::select(-c(maxNDVIin, maxNDVIout)) %>%
      rename(deltaFor = deltaNDVI) %>%
      filter(!is.na(Herd)) # rm 2 indivs with locs too late to estimate winHR


    # rescale ndvi data to make them comparable
    dat$rsPredForAmp <- scale(dat$PredForAmp, center = TRUE, scale = TRUE)
    dat$rsPredForTi <- scale(dat$PredForTi, center = TRUE, scale = TRUE)
    dat$rsDeltaFor <- scale(dat$deltaFor, center = TRUE, scale = TRUE)
    
    
    # set behav as either an ordered or an unordered factor (resident is baseline for both)
    dat$behavN <- relevel(dat$Behav, ref = "resident")
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)

    
    
### ### ### ### ### ### ### ### ### ### #
#### | STATS.IDRE NOMINAL LOGIT EX | ####      
### ### ### ### ### ### ### ### ### ### #      
    
    
        # find-replace "testdat" to "dat" first, then "test" to "mod" or whatever
    
    testdat <- dat
    unique(testdat$Behav)
    
    # rescale ndvi data to make them comparable
    testdat$rsPredForAmp <- scale(testdat$PredForAmp, center = TRUE, scale = TRUE)
    testdat$rsPredForTi <- scale(testdat$PredForTi, center = TRUE, scale = TRUE)
    testdat$rsDeltaFor <- scale(testdat$deltaFor, center = TRUE, scale = TRUE)
    
    
    # summary tables of relationships between each covariate and the response
    with(testdat, table(Old, Behav))
    with(testdat, table(Herd, Behav))
    with(testdat, do.call(rbind, tapply(deltaFor, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
    with(testdat, do.call(rbind, tapply(PredForAmp, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))
    with(testdat, do.call(rbind, tapply(PredForTi, Behav, function(x) c(M = mean(x, na.rm = T), SD = sd(x, na.rm = T)))))    
    
    # set resident as reference level
    testdat$Behav <- relevel(testdat$Behav, ref = "resident")
    
    # m1: predfor (higher numbers mean more variable and therefore less predictable)
    test <- multinom(Behav ~ PredForAmp, data = testdat)
    summary(test)
        # a one-unit increase in PredForAmp (i.e., in unpredictable forage variation)
        # is assocated with a dec in log-odds of being resident
        # in the amount of 1.32+-0.25SE beta(resmig) [need to better understand that beta]
    
    # calculate z-scores 
    testz <- summary(test)$coefficients/summary(test)$standard.errors
    testz
    
    # assess covariate significance using 2-tailed z test (a type of Wald test I think?)
    testp <- (1 - pnorm(abs(testz), 0, 1)) * 2
    testp
    
    # exponentiate coeffs to assess relative risk (e.g. Pr(mig)/Pr(res))
    exp(coef(test))
      # the relative risk ratio for a one-unit increase in standard deviation of forage
      # is 0.27 for migrating rather than remaining resident (and 1.01 for being intermediate)
    
    # calculate predicted probabilities for each outcome level
    head(testpp <- fitted(test))
      
    
    
  #### INITIAL PRELIM FORAGE MODEL COMPARISON ####      
    
    
          
    # make resident the baseline level
    testdat$Behav <- relevel(testdat$Behav, ref = "resident")
    
    
    # looking at all possible forage models for funzies
    mods.for <- list()
    modnms <- c("PredAmp", "DeltaFor", "AllForAmp", "PredTi", "AllForTi")
    mods.for[[1]] <- multinom(Behav ~ rsPredForAmp, data = testdat)
    mods.for[[2]] <- multinom(Behav ~ rsDeltaFor, data = testdat)
    mods.for[[3]] <- multinom(Behav ~ rsPredForAmp + rsDeltaFor, data = testdat)
    mods.for[[4]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for[[5]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for, modnames = modnms)
    aicres <- data.frame(aictab(cand.set = mods.for, modnames = modnms))
    write.csv(aicres, file = "prelim-forage-aic.csv", row.names = F)
    
    
    # for actual "competition" only consider full models
    mods.for2 <- list()
    modnms2 <- c("Amp", "Ti")
    mods.for2[[1]] <- multinom(Behav ~ rsPredForAmp + rsDeltaFor, data = testdat)
    mods.for2[[2]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for2, modnames = modnms2)
    aicres2 <- data.frame(aictab(cand.set = mods.for2, modnames = modnms2))
    write.csv(aicres2, file = "amp-vs-ti.csv", row.names = F) # time-integrated ftw
    
    # look at univariate and multivariate ti models
    mods.for3 <- list()
    modnms3 <- c("PredTi", "DeltaFor", "AllForTi")
    mods.for3[[1]] <- multinom(Behav ~ rsPredForTi, data = testdat)
    mods.for3[[2]] <- multinom(Behav ~ rsDeltaFor, data = testdat)
    mods.for3[[3]] <- multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
    aictab(cand.set = mods.for3, modnames = modnms3)
    aicres3 <- data.frame(aictab(cand.set = mods.for3, modnames = modnms3))
    write.csv(aicres3, file = "tis-aic.csv", row.names = F)
    
    # results of full ti model
    modfor <- multinom(Behav ~ PredForTi + deltaFor, data = testdat)
    summary(modfor)
    modfor
    modforz <- summary(modfor)$coefficients/summary(modfor)$standard.errors
    modforp <- (1 - pnorm(abs(modforz), 0, 1)) * 2
    modforp
    
    # predicted probabilities
    pp <- fitted(modfor)
    head(pp)
    
    
    
### ### ### ### ### ### ### ### ### ### ### ###
#### | MESSING AROUND WITH PACKAGES | ####      
### ### ### ### ### ### ### ### ### ### ### ###       
    
    
        
    
  #### TRYING MCMCGLMM ####
    
    library(MCMCglmm)
    
    MCMCglmm(Behav ~ PredForTi, random=~Herd, family = "multinomial", data=dat,
      nitt = 1000, burnin = 10, thin = 1, verbose = T)
    # Error in if (nJ ? 1) { missing value where TRUE/FALSE needed
    
    multinom(Behav ~ rsPredForTi + rsDeltaFor, data = testdat)
      
      
      data(PlodiaPO)
      View(PlodiaPO)
      
  
      
  #### TRYING MCLOGIT ####
    
    library(mclogit)
      
      # check out help example
      data(Transport)
      
      # make resident the baseline level
      dat$Behav <- relevel(dat$Behav, ref = "resident")
          
      # first just try the same base model with no random effect
      summary(mblogit(Behav ~ PredForTi + deltaFor, data = dat))
      
      # compare to multinom output
      summary(multinom(Behav ~ PredForTi + deltaFor, data = dat))
      
      # cool, numbers are the same
      
      # now add random effect of herd
      summary(mblogit(Behav ~ PredForTi + deltaFor, data = dat, random = ~1|Herd))
      #didn't converge but did give output
      
      
      ### KRISTIN YOU LEFT OFF MODELING HERE ####
      ## mclogit could be promising for a quick solution
      ## although it has convergence issues you need to figure out
      ## specifically it "cannot find an appropriate step size"
      ## but it's just a warning not an error so not sure how big an issue
      
      
      # below this not fixed to use delta max NDVI
      
      
      # # use mblogit to look at forage with random effect of herd included
      # 
      #   testdat$Behav <- relevel(testdat$Behav, ref = "resident")
      #   testdat$Old <- as.factor(testdat$Old)
      # 
      #   # looking at all possible forage models for funzies
      #   mods.for <- list()
      #   modnms <- c("PredAmp", "DeltaAmp", "AllForAmp", "PredTi", "DeltaTi", "AllForTi")
      #   mods.for[[1]] <- mblogit(Behav ~ rsPredForAmp, data = testdat, random = ~1|Herd)
      #   mods.for[[2]] <- mblogit(Behav ~ rsDeltaAmp, data = testdat, random = ~1|Herd)
      #   mods.for[[3]] <- mblogit(Behav ~ rsPredForAmp + rsDeltaAmp, data = testdat, random = ~1|Herd)
      #   mods.for[[4]] <- mblogit(Behav ~ rsPredForTi, data = testdat, random = ~1|Herd)
      #   mods.for[[5]] <- mblogit(Behav ~ rsDeltaTi, data = testdat, random = ~1|Herd)
      #   mods.for[[6]] <- mblogit(Behav ~ rsPredForTi + rsDeltaTi, data = testdat, random = ~1|Herd)
      #   
      #   mods.for[[1]]$deviance
      #   mods.for[[2]]$deviance
      #   mods.for[[3]]$deviance
      #   mods.for[[4]]$deviance
      #   mods.for[[5]]$deviance
      #   mods.for[[6]]$deviance

   ## mclogit help says it can only support random intercept, not random slope

    
### ### ### ### ### ### ### ### ### ### ### ###
#### | TRYING DIFFERENT MODELING OPTIONS | ####      
### ### ### ### ### ### ### ### ### ### ### ###   
  
    
  #### Nominal ####
    
    
    ## a) multinom from nlm package ####
    

      ## with an interaction ##
      mint <- multinom(Behav ~ rsPredForTi + rsDeltaFor + Old + rsDeltaFor*Old, data = dat)
  

  #### Ordinal ####   
    
    ## a) clm from ordinal package ####

        # goal 1: make one simple model work
        ord <- clm(behavO ~ rsPredForTi + rsDeltaFor, data = dat)
        ord
        summary(ord)
            
        # compare to nominal model    
        nom <- multinom(behavN ~ rsPredForTi + rsDeltaFor, data = dat)
        summary(nom)
    
    ## b) vglm with acat from VGAM package (adjacent-category) ####
        
        # i think this allows betas to differ bt groups
        adj <- vglm(behavO ~ rsPredForTi + rsDeltaFor, family = acat, data = dat)
        adj
        summary(adj)
        # yes it does, but i don't think it supports random effects
        
        #adyr <- vglm(behavO ~ rsPredForTi + rsDeltaFor + (1|Herd), family = acat, data = dat)
        # yeah nope
        
        # fixed effect of herd
        adjherd <- vglm(behavO ~ Herd, acat, data = dat)
        summary(adjherd)
        
    ## c) clmm from ordinal package, allows mixed effects ####    
    
        # try adding mixed effect to cumulative link
        me <- clmm(behavO ~ rsPredForTi + rsDeltaFor + (1|Herd), data = dat, Hess = TRUE)
        summary(me)
        me
        str(me)
        me$dims$df.residual # residual degrees of freedom
        

 
       
    
## ### ### ### ### ### ### ### ### ### ### ### ### ##
#### | IDENTIFYING MOST APPROPRIATE MODEL TYPE | ####      
## ### ### ### ### ### ### ### ### ### ### ### ### ##
    
    
    
    #### Proportional odds assumption met? ####
    
        ## ordinal model with proportional odds
        po <- vglm(behavO ~ rsPredForTi, 
          family = cumulative(parallel = TRUE), 
          data = dat)
        
    
        ## ordinal model without proportional odds
        npo <- vglm(behavO ~ rsPredForTi, 
          family = cumulative, 
          data = dat)
        
    
        ## compare models
        c1 <- pchisq(deviance(po) - deviance(npo),
          df = df.residual(po) - df.residual(npo),
          lower.tail= FALSE)
        c1 # 0.200
            # this result indicates that NOT using proportional odds 
            # does not significantly improve model fit. 
            # i interpret this to mean we meet the proportional odds assumption
            # (at least for the predFor covariate, but when I added rsDeltaFor
            #  the number actually increased further)
        
        
        ## oh i just learned the ordinal package has actual function to test this
        nominal_test(fe) # awesome, exact same number 
        scale_test(fe) # oh hell, this is significant. and i have no idea what that means.
        help(package = "ordinal")    
          # after going through help file and agresti 2002, learned that this is ok
          # the most important factor in deciding whether to use proportional odds vs
          # other kinds of models is how you want to be able to draw inference
          # and we passed the nominal test at least, so i'm not concerned abt the scale test
        
        
    
    #### Random effect of herd merit increased model complexity? ####
    
        ## model with random effect
        re <- clmm(behavO ~ rsPredForTi + (1|Herd), data = dat)
    
        ## model without random effect
        fe <- clm(behavO ~ rsPredForTi, data = dat)
    
        ## compare models
        anova(re, fe)
          # Pr(>Chisq) = 1.1e-08*** | LR.stat = 32.654
          # this result indicates that the random effect fits the data
          # "significantly" better than not including it
        
    ## takeaway from this section: 
    ## use clmm() to estimate proportional odds ordinal regressions with random effect of herd
    ## but also specify each model using clm() and test proportional odds assumption is met

        
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#### TRANSFORMING SKEWED COVARIATES PRIOR TO STANDARDIZATION ####        
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
        
      # histograms of covariate distributions
      par(mfrow=c(3,2))
      hist(dat$PredForAmp, main = "Variation in Forage (NDVI amp)")
      hist(dat$PredForTi, main = "Variation in Forage (time-int NDVI)")
      hist(dat$deltaFor, main = "Delta Forage")
      hist(dat$Dens, main = "Conspecific Density")
      hist(dat$ppnAg, main = "Proportion IrrigAg")
      hist(dat$densOwn, main = "Human Use Intensity")
      # most things (exception of varFor) are right-skewed; try transforming b4 standardize
      
      # log-transformed covariates
      par(mfrow=c(2,2))
      hist(log(dat$deltaFor), main = "LOG Delta Forage")
      hist(log(dat$Dens), main = "LOG Conspecific Density")
      hist(log(dat$ppnAg), main = "LOG Proportion IrrigAg")
      hist(log(dat$densOwn), main = "LOG Human Use Intensity")
      # better, rolling with it    
    # Dens and ppnAg are ok but not great after log transformation
    # inverse better tha log?
    par(mfrow=c(2,2))
    hist(log(dat$Dens), main = "logDens")
    hist(log(dat$ppnAg), main = "logAg")
    hist(dat$Dens^-1, main = "invDens")
    hist(dat$Dens^-1, main = "invAg")
    # no, log is better
    
    summary(log(dat$Dens))
    summary(log(dat$ppnAg)) # oh right, has 0s
    summary(dat$ppnAg)
    
    hist(log(dat$ppnAg+0.001))
    hist(dat$ppnAg+0.001)
    hist(log(dat$ppnAg+0.01))
    hist(log(dat$ppnAg+0.00000001))
    
    test <- filter(dat, ppnAg > 0)
    hist(test$ppnAg)
    hist(log(test$ppnAg))
    hist(log(dat$ppnAg))
    
    # dur, never mind, don't need to transform to compare models
    
    
    
### ### ### ### ### ##
#### PRELIM PLOTS ####        
### ### ### ### ### ##
    
        
    # plots of popn data and ppn mig
    m1 <- ggplot(popdat, aes(x = ppnOld, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "Proportion Migrant") +
      geom_smooth(color = "black") +
      geom_point()
    m2 <- ggplot(popdat, aes(x = Dens, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    m3 <- ggplot(popdat, aes(x = PredForTi, y = ppnMig, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i1 <- ggplot(popdat, aes(x = ppnOld, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "Proportion Intermediate") +
      geom_smooth(color = "black") +
      geom_point()
    i2 <- ggplot(popdat, aes(x = Dens, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    i3 <- ggplot(popdat, aes(x = PredForTi, y = ppnOth, colour = Herd)) +
      labs(x = "", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r1 <- ggplot(popdat, aes(x = ppnOld, y = ppnRes, colour = Herd)) +
      labs(x = "Proportion >10 yrs old", y = "Proportion Resident") +
      geom_smooth(color = "black") +
      geom_point()
    r2 <- ggplot(popdat, aes(x = Dens, y = ppnRes, colour = Herd)) +
      labs(x = "Conspecific density (elk/km^2)", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    r3 <- ggplot(popdat, aes(x = PredForTi, y = ppnRes, colour = Herd)) +
      labs(x = "Forage Unpredictability (5-yr sd)", y = "") +
      geom_smooth(color = "black") +
      geom_point()
    grid.arrange(m1, m2, m3, i1, i2, i3, r1, r2, r3, ncol = 3, nrow = 3)
    
    
    # actually forage is the only interesting one; just do that
    mf <- ggplot(popdat, aes(x = PredForTi, y = ppnMig)) +
      labs(x = "", y = "Proportion Migrant") +
      geom_smooth(color = "black") +
      geom_point() +
      theme(axis.title = element_text(size = 16))
    ifor <- ggplot(popdat, aes(x = PredForTi, y = ppnOth)) +
      labs(x = "", y = "Proportion Intermediate") +
      geom_smooth(color = "black") +
      geom_point()+
      theme(axis.title = element_text(size = 16))
    rf <- ggplot(popdat, aes(x = PredForTi, y = ppnRes)) +
      labs(x = "Forage Unpredictability (5-yr sd)", y = "Proportion Resident") +
      geom_smooth(color = "black") +
      geom_point()+
      theme(axis.title = element_text(size = 16))
    grid.arrange(mf, ifor, rf, ncol = 1)

    
    
    
    
### ### ### ### ### ### ### ### ### ### ### 
#### PRELIM SUMMARIES (FOR DRAFT RSLTS) ####        
### ### ### ### ### ### ### ### ### ### ###     
    
    
        
        # summary df of means and sd's (silly, some of these are skewed, re-report)  
      
          sumbehav <- dat %>%
            group_by(Behav) %>%
            summarise(densMean = mean(Dens, na.rm=T), densSD = sd(Dens, na.rm=T),
              dfMean = mean(deltaFor, na.rm=T), dfSD = sd(deltaFor, na.rm=T),
              pfMean = mean(PredForTi, na.rm=T), pfSD = sd(PredForTi, na.rm=T),
              nOld = sum(Old, na.rm=T), nYoung = n()-nOld, nTot = n(),
              ppnOld = nOld/nTot, ppnYoung = nYoung/nTot)
          write.csv(sumbehav, "summarytable-bybehav.csv", row.names=F)

          
          
          
      
### ### ### ### ### ### 
#### MISC/NO IDEAR ####        
### ### ### ### ### ###          
         
    
    # mod.dens <- glm(Dens ~ Behav, data = dat)
    # mod.df <- glm(deltaTi ~ Behav, data = dat)
    # mod.pf <- glm(PredForTi ~ Behav, data = dat)
    # 
    # summary(mod.dens)
    # summary(mod.df)
    # summary(mod.pf)
          
          
        

    #### plotting forage per indiv ####
         
         hist(deltafor$MaxTiIn)
         hist(deltafor$deltaFor)
        

     
    
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
  # prelim nominal models, just checking stuff out #  
    
    # m1: predictability of variation in forage
    testm1 <- multinom(Behav ~ PredForAmp, data = testdat)
    summary(testm1)
    testzm1 <- summary(testm1)$coefficients/summary(testm1)$standard.errors
    testpm1 <- (1 - pnorm(abs(testzm1), 0, 1)) * 2
    testpm1
    AIC(testm1)
    
    # m2: diff between forage outside and inside winter range
    testm2 <- multinom(Behav ~ deltaFor, data = testdat)
    summary(testm2)
    testzm2 <- summary(testm2)$coefficients/summary(testm2)$standard.errors
    testpm2 <- (1 - pnorm(abs(testzm2), 0, 1)) * 2
    testpm2
    
    # m3: full forage (both variation and difference)
    testm3 <- multinom(Behav ~ PredForAmp + deltaFor, data = testdat)
    summary(testm3)
    testzm3 <- summary(testm3)$coefficients/summary(testm3)$standard.errors
    testpm3 <- (1 - pnorm(abs(testzm3), 0, 1)) * 2
    testpm3
    
    
    # m4: conspecific density
    testm4 <- multinom(Behav ~ Dens, data = testdat)
    summary(testm4)
    testzm4 <- summary(testm4)$coefficients/summary(testm4)$standard.errors
    testpm4 <- (1 - pnorm(abs(testzm4), 0, 1)) * 2
    testpm4
    
    # m5: animal age
    testm5 <- multinom(Behav ~ Age, data = testdat)
          
    
    
    
    
#### playing with modely thoughts ####
    
    # seeing whether deltaFor coeff overlaps 0 if alone in model
    confint(clmm(behavO ~ deltaFor + (1|Herd), data = dat))
    # it does. huh.
    
    
    # this may kill my soul, but... reducing to mig/res (int = res)
    
    library(lme4)
    
      # new df so i don't taint the old one with such heresy
      testdat <- moddat %>%
        mutate(Mig = as.factor(ifelse(Behav == "migrant", 1, 0)),
          irrig = as.factor(irrig))
    
      # define models #
      tfmh <- glmer(Mig ~ predFor + deltaFor + (1 | Herd), 
        family = binomial(logit), data = testdat)
      tconsdens <- glmer(Mig ~ predFor + deltaFor + Dens + deltaFor*Dens + (1|Herd), 
        family = binomial(logit), data = testdat) # convergence failure
      told <- glmer(Mig ~ predFor + deltaFor + Old + deltaFor*Old + (1|Herd), 
        family = binomial, data = testdat) # convergence failure
      thumshld <- glmer(Mig ~ densOwn + (1|Herd), 
        family = binomial(link = "logit"), data = testdat)
      tagrsub <- glmer(Mig ~ ppnAg + (1|Herd), 
        family = binomial, data = testdat)
      
      # look at CIs of coefficients to assess hypothesis support within topics #
      confint(tfmh) # confusing error in zeta(shiftpar, start = opt[seqpar1][-w]) : profiling detected new, lower deviance
      confint(tconsdens) # similar result to ordinal; overlappy except predfor
      confint(told) # same confusing error
      confint(thumshld) # overlappy
      confint(tagrsub) # all jacked up
      
      # prelim aic and bic [don't forget you had convergence failures]
            # compete with AICc #
      mods <- list()
      modnms <- c("Forage", "Density", "Old", "HumanShld", "AgrSub")
      mods[[1]] <- tfmh
      mods[[2]] <- tconsdens
      mods[[3]] <- told
      mods[[4]] <- thumshld
      mods[[5]] <- tagrsub
      aictab(cand.set = mods, modnames = modnms)

      
      
      # compete with BIC #
      tbic <- data.frame(BIC(tfmh, tconsdens, told, thumshld, tagrsub))
      
      
      # the good news: similar results here give me no reason to switch to binary behavior :)
      # the bad news: similar results mean i still have, like, no support for anything :(
      