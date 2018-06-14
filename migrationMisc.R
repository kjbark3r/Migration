## ### ### ### ### ### ### ## ### ### ### ### 
#       CODE THAT WORKS JUST FINE           #
# BUT ISN'T NECESSARY FOR OFFICIAL VERSION  #
#   OF THE STATEWIDE MIG ANALYSIS CODE      #
#         Kristin Barker Mar 2018           #
## ### ### ### ### ### ### ## ### ### ### ### 


  #### elev across study area ####
  
  # (you did this before finding you could do it
  #  programmatically for temp/precip too)
  
    library(beepr)
    memory.limit(size = 7500000)
    #### read in that raster i already conveniently created
    ras <- raster("alldems.tif")
    #plot(ras)
    aoi <- shapefile("../GIS/Shapefiles/Elk/AreaOfInterest.shp")
    aoi <- spTransform(aoi, ras@crs)
    #plot(aoi, add = TRUE)
    aoielevs <- raster::intersect(ras, aoi)
    writeRaster(aoielevs, "elevsAoi", format = "GTiff")
    min(aoielevs) # this isn't the right fcn probably but it works to display min/max

  #### MODEL AVERAGING #### 
      
      
      #### center all covariates (because interaction terms) ####
      
      ctrdat <- olddat %>%
        mutate(predForCtr = predFor - mean(predFor),
               deltaForCtr = deltaFor - mean(deltaFor),
               irrigCtr = as.numeric(irrig) - mean(as.numeric(irrig)),
               densCtr = Dens - mean(Dens),
               oldCtr = as.numeric(Old) - mean(as.numeric(Old)),
               densOwnCtr = densOwn - mean(densOwn))
      
 
             
    #### play with standard errors adjusting for possible overdispersion ####
      
      # never did figure out how to make this work
      # because SEs should be asymmetric 
      # and i didn't go far enough down the rabbit hole
      # to figure out how to calculate them that way
        
        ## SE * sqrt(Chi-sq/df)
        
        # try with deltafor:irrig cov
        se.di <- 0.05587
        est.di <- 0.15671
        se.adj <- se.di * (sqrt(chi.pr/dof.pr))
        se.new <- se.di*se.adj
        0.1567 - se.new
        0.1567 + 2*se.low
        confint(mod)      
        
        
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
        
