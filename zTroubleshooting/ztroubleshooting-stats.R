### ### ### ### ### ### ### ### ### ### ### ### 
## playing with functions, probabilities, etc #
## kjb 2017
### ### ### ### ### ### ### ### ### ### ### ### 


##### BINOMIAL LIKELIHOOD ####

  ?dbinom
  dbinom(x = 0, size = 5, prob = 0.5)
  dbinom(0, 5, 0.5)
  #x is X, NUMBER OF SUCCESSES (not a single obs)
  #dbinom output here is the likelihood of having 0 successes 
  #5 times with 0.5 probability of failure each time
  (0.5)^5 #see, told ya
  
  
  
  # plotting the function over 3 different numbers of successes in 5 trials (x = 0,1,2)
  
  plot(0:2, dbinom(0:2, 5, 0.5))
    # this has x on x axis (go fig) and probability on y
    # i.e. it is NOT the likelihood plot
  
  
  # here are 3 separate likelihood function plots
  # one for each of those values of x (0, 1, 2)
  par(mfrow = c(3,1))
  curve(dbinom(0, 5, x), xlim = c(0,1), main = "x=0")
  curve(dbinom(1, 5, x), xlim = c(0,1), main = "x=1")
  curve(dbinom(2, 5, x), xlim = c(0,1), main = "x=2")
  # the maximum likelihood estimate is where 
  # a likelihood function is maximized, obvs
  
  
  # and here are the log-likelihoods
  # which are basically the same exact thing as above
  # but algebraically easier to work with
  curve(log(dbinom(0, 5, x)), xlim = c(0,1), main = "x=0, log-L")
  curve(log(dbinom(1, 5, x)), xlim = c(0,1), main = "x=1, log-L")
  curve(log(dbinom(2, 5, x)), xlim = c(0,1), main = "x=2, log-L")
  
  #then of course you find maxima by setting deriv = 0
  # and solving for the parameter of interest
  # and log-likelihood is easier to differentiate
  


#### CHI SQUARE DISTRIBUTION ####
##   from onlinecourses.science.psu.edu 

  #### Function for plotting a chi-squared distribution with various DFs
  
  #### Create a vector of values from 1 to 50
  
  x=seq(1:50)
  
  #### plot the first density chi-square function with df=1, 
  #### using dchisq() function, 
  #### type="l" denotes a line with default color black
  
  par(mfrow = c(1,1))
  plot(dchisq(x,1), type="l", xlab="Value of X^2", ylab="Probability Density", main="Examples of chi-squared distributions")
  
  
  #### To the same plot, add more density lines for various DFs 
  
  lines(dchisq(x,5), col="red")
  lines(dchisq(x,10), col="blue")
  lines(dchisq(x,20), col="green")
  lines(dchisq(x,40), col="purple")
  
  #### adding a legend
  
  legend(40,0.2, c("df=1", "df=5", "df=10", "df=20", "df=40"), text.col=c("black", "red","blue", "green","purple"))
  
  
  
  
#### ASSESSING GOODNESS OF FIT GOF ####  
  
  
  ##   from ex 8.2.2 on onlinecourses.science.psu.edu/STAT504
  
  ?pchisq # pchisq(q, df)
  # stackexchange says pval = 1 - pchisq(deviance, degrees of freedon)
  1 - pchisq(35.40, 32)
  1 - pchisq(0.5415^2, 1)
  
