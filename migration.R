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
    dat <- read.csv("moddat.csv")
    
    # format behavior as ordinal (res < int < mig) and dummy variables as factors
    dat$behavO <- factor(dat$Behav, levels = c("resident", "other", "migrant"), ordered = TRUE)
    dat$irrig <- factor(dat$irrig)
    dat$Old <- factor(dat$Old)
    
    
    # create df for indivs with no age estimation 
    olddat <- filter(dat, !is.na(Old))
    


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
      m3 <- clmm(behavO ~ predFor + deltaFor + (deltaFor*predFor) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m4 <- clmm(behavO ~ predFor + deltaFor + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m5 <- clmm(behavO ~ predFor + deltaFor + Dens + (deltaFor*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m6 <- clmm(behavO ~ predFor + deltaFor + Old + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m7 <- clmm(behavO ~ predFor + deltaFor + Old + (deltaFor*Old) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m8 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m9 <- clmm(behavO ~ predFor + deltaFor + Old + Dens + (Old*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m10 <- clmm(behavO ~ densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m11 <- clmm(behavO ~ predFor + deltaFor + densOwn + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m12 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*predFor) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m13 <- clmm(behavO ~ predFor + deltaFor + densOwn + (deltaFor*densOwn) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m14 <- clmm(behavO ~ densOwn + Old + (densOwn*Old) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m15 <- clmm(behavO ~ predFor + deltaFor + densOwn + Old + (densOwn*Old) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m16 <- clmm(behavO ~ irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m17 <- clmm(behavO ~ densOwn + irrig + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m18 <- clmm(behavO ~ predFor + deltaFor + irrig   + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m19 <- clmm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m20 <- clmm(behavO ~ predFor + deltaFor + irrig + (predFor*irrig) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m21 <- clmm(behavO ~ irrig + Dens + (irrig*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m22 <- clmm(behavO ~ predFor + irrig + Dens + (irrig*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m23 <- clmm(behavO ~ predFor + irrig + Old + Dens + (Old*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)  
      m24 <- clmm(behavO ~ densOwn + irrig + Dens + (irrig*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat)
      m25 <- clmm(behavO ~ densOwn + irrig + Dens + (densOwn*Dens) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      m26 <- clmm(behavO ~ predFor + irrig + densOwn + (irrig*densOwn) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = olddat) 
      
      
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
    mod <- clmm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = dat)  
      
    #### summary info ####
      
      summary(mod)
      coef(mod)
      confint(mod, level = 0.85)
      
    
    
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
      m19test <- clm(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig), Hess = TRUE, nAGQ = 10, dat = dat) 
      nominal_test(m19test) # nothing significant, good enough for me
      scale_test(m19test) # predfor is significant here, cool, check this out later
      

      
        
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
        
        m19c <- clmm(behavO ~ predForCtr + deltaForCtr + irrigCtr + (deltaForCtr*irrigCtr) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)  
        m2c <- clmm(behavO ~ predForCtr + deltaForCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m6c <- clmm(behavO ~ predForCtr + deltaForCtr + oldCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m3c <- clmm(behavO ~ predForCtr + deltaForCtr + (deltaForCtr*predForCtr) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m22c <- clmm(behavO ~ predForCtr + irrigCtr + densCtr + (irrigCtr*densCtr) + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)  
        m11c <- clmm(behavO ~ predForCtr + deltaForCtr + densOwnCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
        m4c <- clmm(behavO ~ predForCtr + deltaForCtr + densCtr + (1|Herd), Hess = TRUE, nAGQ = 10, dat = ctrdat)
   
             
      #### average the models ####
        
        avgmod <- model.avg(m19c, m2c, m6c, m3c, m22c, m11c, m4c) 
        summary(avgmod)
        round(confint(avgmod, level = 0.85), 2)

        
      #### exponentiate relevant info from model ####
        
        orsAvg <- round(exp(coef(avgmod)), 2)
        ciAvg95 <- round(exp(confint(avgmod, level = 0.95)), 2)
        ciAvg85 <- round(exp(confint(avgmod, level = 0.85)), 2)

        
      #### create and store dataframe of exponentiated info ####
        
        avgdat <- data.frame(
          OR = orsAvg,
          ciLow95 = ciAvg95[,1],
          ciHigh95 = ciAvg95[,2],
          ciLow85 = ciAvg85[,1],
          ciHigh85 = ciAvg85[,2]) %>%
          tibble::rownames_to_column() %>%
          rename(cov = rowname)
        write.csv(avgdat, "avgmod-estiamtes.csv", row.names = F)
            
          
### ### ### ### ###
#### |VISUALS| ####
### ### ### ### ###
      
      
 
    #### Effects per herd ####
        
        
      ## *KRISTIN MAKE THIS NOT UGLY* ####
        
      # this is basically doing center +- spread for the variance per herd
      ci <- m19$ranef + qnorm(0.975) * sqrt(m19$condVar) %o% c(-1, 1) # %o% = genius
      # order effect from low to high
      ord.re <- order(m19$ranef)
      ci <- ci[order(m19$ranef),]
      # and order herds to match their index numbers
      herdnums <- data.frame(Herd = unique(olddat$Herd), herdNum = 1:16)
      herdnums <- herdnums[match(ord.re, herdnums$herdNum),]
      #plot estimates
      plot(1:16, m19$ranef[ord.re], axes = FALSE, ylim = range(ci),
        xlab = "", ylab = "Herd effect")
      axis(1, at = 1:16, labels = herdnums$Herd, las=2) #las=2 rotates vert
      axis(2)
      # add 95% CIs
      for(i in 1:16) segments(i, ci[i, 1], i, ci[i, 2])
      abline(h = 0, lty = 2)
      # 4 don't overlap 0; guess those would be considered the "extreme" herds
         # pio, sage ck, blacktail (neg) and silver run (pos)   
          # silver run only has 5 indivs; could cut them
            # [later...] tried; doesn't change inference so left them in

 
      
        
    #### Prediction plots - Top model ####
      
      
        ## make predictions ##

        
            ## define top model using clmm2 (predict doesn't work with clmm) ##
            topmod2 <- clmm2(behavO ~ predFor + deltaFor + irrig + (deltaFor*irrig), 
                             random = Herd, Hess = TRUE, nAGQ = 10, dat = dat) 
           
            ## check it out
            summary(topmod2) # Hess = 18295.46 => model not ill-defined (see clmm2 tutorial)
            confint(topmod2, level = 0.85) # this only gives standard error, prob bc Hessian = T
             
    
            ## predict probability of falling into each response category given those same data
            predprob <- predict(topmod2, newdata = dupedat)
            
                ## sanity check, both should = 1
                sum(predprob[1]+predprob[2]+predprob[3])
                sum(predprob[4]+predprob[5]+predprob[6])
    
    
            ## combine predictions with data used to predict
            newdat <- cbind(dupedat, predprob)
            head(newdat)
    
            
            
        ## plot predictions ##
            
            
            ## pull random subsample of 10000 predictions (takes forEVer otherwise)
            subdat <- newdat[sample(nrow(newdat), 10000),] 

            
            ## predFor
            pp.pf <- ggplot(subdat, aes(x = predFor, y = predprob, colour = behavO)) +
              geom_smooth() +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(x = "Forage predictabilty \n (- 6-yr stdev of NDVI amplitude)", y = "Probability") +
              theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

            ## deltaFor on Ag
            pp.df <- ggplot(subdat[subdat$irrig == 1,], aes(x = deltaFor, y = predprob, colour = behavO)) +
              geom_smooth() +
              scale_color_hue(name = "", labels = c("Resident", "Intermediate", "Migrant")) +
              labs(x = "Forage difference \n (maxNDVI outside - inside winter range)", y = "Probability",
                title = "Agriculture on winter range") +
              theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))
         
            
            
        ## export ##
            
        ggsave("./Plots/predFor-prdxns.jpg", 
             plot = pp.pf, 
             device = "jpeg",
             dpi = 300)   
        
        
        ggsave("./Plots/deltaFor-prdxns.jpg", 
             plot = pp.df, 
             device = "jpeg",
             dpi = 300)         
        
            
    #### Coefficient estimates - Averaged model ####     
        

        ## extract relevant model info
        coefAvg <- round(coef(avgmod), 2)
        ciAvg95 <- round(confint(avgmod, level = 0.95), 2)
        ciAvg85 <- round(confint(avgmod, level = 0.85), 2)    
        
        ## make dataframe of relevant info
        avgdf <- data.frame(
          OR = coefAvg,
          ciLow95 = ciAvg95[,1],
          ciHigh95 = ciAvg95[,2],
          ciLow85 = ciAvg85[,1],
          ciHigh85 = ciAvg85[,2]) %>%
          tibble::rownames_to_column() %>%
          rename(cov = rowname) %>%
          filter(!grepl("other", cov)) 
          
        
        ## plot without densOwn (huge CI messes up scale)
        ggplot(avgdf[avgdf$cov != "densOwnCtr",], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
          geom_point(size = 3) +
          geom_errorbar(width = 0.1) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          labs(y = "coef", x = "")
        
        
        ## pull out just the interesting things
        avgsub <- avgdf %>%
          filter(cov == "densCtr" | cov == "densCtr:irrigCtr" |
              cov == "deltaForCtr" | cov == "deltaForCtr:irrigCtr") 
        # dplyr hates me so i'm doing this an awful way
        avgsub[1,1] <- "Density"
        avgsub[2,1] <- "Density with Ag"
        avgsub[3,1] <- "Forage Difference"
        avgsub[4,1] <- "Forage Diff with Ag"
        lvs <- c("Density", "Density with Ag", "Forage Difference", "Forage Diff with Ag")
        avgsub$cov <- factor(avgsub$cov, levels = lvs)
        
        ## plot the interesting things
        densag <- ggplot(avgsub[grepl("Dens", avgsub$cov),], 
          aes(y = OR, x = cov, ymin = ciLow85, ymax = ciHigh85)) +
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
      

