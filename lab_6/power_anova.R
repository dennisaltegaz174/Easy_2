# This script performs a power analysis for a one-way ANOVA with three groups

####################################################################
## Constants that you can change to examine the power of your test##
### to run in script directly remove the # in front of lines 7-15 ##
### then highlight and run everything all at once ##################
####################################################################
# PICKLEMEAN = 74 # in minutes
# NOTHINGMEAN = 180 # in minutes
# STD = 90 # in minutes
# 
# max.students = 50 # Max number of students in each group (balanced design)
# students = c(2:max.students)
# alpha = 0.05 # Type I Error rate
# 
# nsim = 100 # Number of simulations

########################################################################
## DON"T CHANGE ANYTHING BELOW THIS LINE ##

# Making arrays to hold my simulation data
pvals = array(NA, dim=nsim)
power.vals = array(NA, dim=max.students - 1)

# Run the simulation
for (N in students){
  for (i in 1:nsim){
    
    # Randomly sample my three treatments
    pickle = rnorm(N, PICKLEMEAN, STD)
    nothing = rnorm(N, NOTHINGMEAN, STD)
    water = rnorm(N, mean(PICKLEMEAN, NOTHINGMEAN), STD)
    
    # Reformat the data to run an ANOVA
    cure.time = c(pickle, nothing, water)
    home.remedy = c(rep("pickle", N), rep("nothing", N), rep("water", N))
    
    # Run an ANOVA and save p-values
    fit = aov(cure.time ~ home.remedy)
    pvals[i] = summary(fit)[[1]][["Pr(>F)"]][1]
    
  }
  
  power.vals[N - 1] = (sum(pvals < alpha)) / nsim
}

# Plot the simulation data
plot(students, power.vals, xlab="# students in each group", #scatter plot
     ylab="power")
lo = loess(power.vals ~ students) #find smooth curve
lines(predict(lo), col='red', lwd=2) # add it to the plot
grid(lty=2, col="gray",lwd=2) #add grid lines so easier to see values

## remove all the object created above
rm(pvals, power.vals, pickle, nothing, water, cure.time, home.remedy, fit, lo)






