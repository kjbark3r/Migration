### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###   learning bayesian methods etc
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## working thru kruschke's book

# figuring out if i can make afunction with 2 unspecified variables
    
    f1 <- function(a, b=1) {
      c = a^2 + b
      return(c)
    }
    
    f1(a=3,b=2)
    
    
    f2 <- function(a, b) {
      c = a^2 + b
      return(c)
    }
    
    f1(a=3,b=2)
    
    # yeah ok no idea why he specified b in that example
    # ohhhh yep now he tells me... that sets the default


    
# info about loops and conditions (eg while, break)
    ?"Control"
    
    
    
# timing
    
    proc.time() #records system times; i think "user" is the one youd want
    # specify startTime and endTime this way before and after running something
    # and calc diff bt them to see how long it took

    
#### matrix algebra ####
    
    t <- matrix(c(1, 2, 3, 4), nrow = 2)
    w <- matrix(c(1, 2), nrow = 1)
    t*w
    
    