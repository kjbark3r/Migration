## troubleshooting Singh et al 2016 code
## nsd and msd analyses

####  CONVERGENCE ISSUES ####


    #### Dispersal model w dist & timing varying between indivs ####




##Simple Migration model with only distance varying between individuals
########################################################
asym.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym ~ 1,
            groups =~ AnimalID,
           start = c(asym = 5515,xmidA = 90, xmidB = 242, scale1 = 4, scale2 = 4))
summary(asym.Migrmod)