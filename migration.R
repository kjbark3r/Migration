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
      length(which(moddat$Old == 1))
      
      # number of migrants
      length(which(dat$Behav == "migrant"))
      length(which(dat$Behav == "migrant"))/nrow(dat)
      
      # and residents
      length(which(dat$Behav == "resident"))/nrow(dat)      
      
      # summary tables of binary covariates
      lapply(moddat[, c("Behav", "Old", "irrig")], table)
      
      # three-way contingency table
      ftable(xtabs(~ Old + Behav + irrig, data = moddat))
      
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
      ggplot(moddat, aes(x = Behav, y = deltaFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 
      
      # predFor, split by old & irrig
      ggplot(moddat, aes(x = Behav, y = predFor)) +
        geom_boxplot(size = 0.75) +
        geom_jitter(alpha = 0.5) +
        facet_grid(Old ~ irrig, margins = TRUE) 
      
      #
      ggplot(popdat, aes(x = ppnOld, y = ppnMig, color = Herd)) +
        labs(x = "", y = "Proportion Migrant") +
        geom_smooth(color = "black") +
        geom_point()
        
      
      
    #### Summarizing covariates ####  
      
      
      # deltafor
      
      
      # predfor
      
      
      # consdens
      
      
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
        dplyr::select(behavO, predFor, deltaFor, Dens, Old, densOwn, ppnAg, irrig)
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
      
    
    
    #### convert to estimates and CIs to odds ratios and store ####
      
      ciTop <- confint(mod)
      coefsTop <- coef(mod)
      orsTop <- exp(cbind(OR = coefsTop, ciTop))
      resTop <- data.frame(orsTop) %>%
        tibble::rownames_to_column() %>%
        rename(coeff = rowname, CIlow = X2.5.., CIhigh = X97.5..)
      write.csv(resTop, "results-topmodel.csv", row.names=F)
        
  
    #### proportional odds assumption ####
        
      # Agresti 2002 says this assumption should be based more on 
      # the inference you want to draw from your model than on
      # methematical considerations, but out of curiosity...
      # prelim quick test (using models without random effect of herd) 
      poddstest <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, Hess = TRUE, nAGQ = 10, dat = dat) 
      nominal_test(poddstest) # nothing significant, suggests meet assumption
      scale_test(poddstest) # predfor is significant here, cool, check this out later (no scale in model currently)
      

      
        
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

        
      #### exponentiate relevant info from model ####
        
        coefAvg <- round(coef(avgmod), 2)
        CI95 <- round(confint(avgmod, level = 0.95), 2)
        CI85 <- round(confint(avgmod, level = 0.85), 2)

        
      #### create and store dataframe of exponentiated info ####

        
        ## make dataframe of relevant info
        avgdat <- data.frame(
          coef = coefAvg,
          ciLow95 = CI95[,1],
          ciHigh95 = CI95[,2],
          ciLow85 = CI85[,1],
          ciHigh85 = CI85[,2]) %>%
          tibble::rownames_to_column() %>%
          rename(cov = rowname)
        write.csv(avgdat, "results-avgmodel.csv", row.names = F)
        
        
        
        
            
### ### ### ### ### #
#### |MODEL FIT| ####
### ### ### ### ### #
        
        
    library(generalhoslem)
        
    # define model without random effect of herd (tests can't handle mixed effects)
    # note: model without the random effect fits data worse than the one with it, so...
    mod.clm <- clm(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, data = dat)  
    
    
    
    
    #### TEST 1 : Lipsitz ####
    
    lipsitz.test(mod.clm) # p-val = 1.263e-5, ROUGH. does have some warnings...
    
    
    
    #### TEST 2 : Modified Hosmer-Lemeshow ####
        
    
        ## with the clm version ##
        
            predprobclm <- data.frame(
              predFor = dat$predFor, 
              deltaFor = dat$deltaFor,
              irrig = dat$irrig)
            fv <- predict(mod.clm, newdata = predprobclm)$fit
            logitgof(dat$behavO, fv, g = 10, ord = TRUE) # p = 0.0005, STILL ROUGH
 
            
        ## trick it into looking at the clmm instead of clm? ##
        
          # predict with original behaviors
          testdat1 <- dat %>%
            dplyr::select(behavO, deltaFor, predFor, irrig)
          testpred1 <- predict(mod2, newdata = testdat1)
          pred1 <- cbind(testdat1, testpred1) %>%
            rename(c("behavO" = "behavior", "testpred1" = "prediction")) %>%
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
            rename(c("behavO" = "behavior", "testpred2" = "prediction")) %>%
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
            rename(c("behavO" = "behavior", "testpred3" = "prediction")) %>%
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
          
          # gof test
          logitgof(dat$behavO, predmat, g = 10, ord = TRUE) # even more significant, which i don't understand
              # BUT warning: at least 1 cell in expected frequencies table is <1. Chi-sq approximation may be wrong
          
          
          
      #### TEST 3 : Pulkstenic-Robinson ####
        
        pulkrob.chisq(mod.clm, catvars = c("irrig")) # p = 0.6491
        pulkrob.deviance(mod.clm, c("irrig")) # p = 0.6125
        
        
          
      #### MY TEST : Proportion of correct predictions ####
        
        hm <- preds; hm$actualBehav <- dat$behavO
        test <- colnames(hm)[apply(hm,1,which.max)]
        hm$predBehav <- test
        hm$matchBehav = ifelse(hm$actualBehav == hm$predBehav, 1, 0)
        length(which(hm$matchBehav == 1)) / nrow(hm)
        # predicts correctly 59% of the time
        # which is marginally better than a random guess so i got that going for me which is nice   
        
        
        
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

        
      # below plot is super ugly/unfinished because it's
      # not ordered low to high or labeled with herd names
      
      # calc center +- spread of variance per herd, ordered low to high
      herd <- topmod$ranef + qnorm(0.975) * sqrt(topmod$condVar) %o% c(-1, 1) # %o% = genius
      herd2 <- cbind(herd, m19$ranef)
      herddat <- data.frame(herd2[order(topmod$ranef),]) %>%
        rename(CIlow = X1, CIhigh = X2, Est = X3) %>%
        tibble::rownames_to_column() 
      
      # plot
      ggplot(herddat, aes(y = Est, x = rowname,
        ymin = CIlow, ymax = CIhigh)) +
        geom_point() +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0) 
        

      
        
    #### Prediction plots - Top model ####
      
      
        ## make predictions ##

        
            ## define top model using clmm2 (predict doesn't work with clmm) ##
            topmod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + deltaFor:irrig, 
                             random = Herd, Hess = TRUE, nAGQ = 10, dat = dat) 
           
            ## check it out
            summary(topmod2) # Hess = 18295.46 => model not ill-defined (see clmm2 tutorial)
            confint(topmod2, level = 0.85) # this only gives standard error
             
    
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
          aes(y = coef, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          labs(y = "coef", x = "", title = "85% CI")       
          
        
        ## plot without densOwn (huge CI messes up scale)
        ggplot(avgdat[avgdat$cov != "densOwnCtr",], 
          aes(y = coef, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          labs(y = "coef", x = "", title = "85% CI")
        
        
        ## separate plots of just densOwn, Age, dens:irrig (MT TWS)
        p.do <- ggplot(avgdat[avgdat$cov == "densOwnCtr",], 
          aes(y = coef, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(y = "", x = "", title = "Intensity of human use") +
          scale_x_discrete(labels="") +
          theme_light() +
          theme(text = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5))
        p.ol <- ggplot(avgdat[avgdat$cov == "oldCtr",], 
          aes(y = coef, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(y = "", x = "", title = "Age (Old)") +
          scale_x_discrete(labels="") +
          theme_light() +
          theme(text = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5))
        p.da <- ggplot(avgdat[avgdat$cov == "densCtr:irrigCtr",], 
          aes(y = coef, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.05) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(y = "Coefficient", x = "", title = "Density with Irrigated ag") +
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
              
        

        
        
        
        
        
        
#### [save all the things] ####         
           
  save.image("mig.Rdata")   
      

