##########
#From March 2017 Google+ post (https://plus.google.com/+AppliedDemographyToolbox/posts/TPFmUnch3tQ) 
#Eddie Hunsinger, December 2018
#http://www.demog.berkeley.edu/~eddieh/
##########

#Demographers are sometimes asked what-to-expect for population next year, and total population (rather than births or deaths, 
#or general migration or aging trends, etc) is often the most difficult part to provide helpful information on, 
#so: To view a distribution of possibilities for next year's total population based on best guesses you have for high/lows 
#on components of change (easy to forget how Very easy that is to do/see in R, say w uniform distributions, and no correlation between components):

#####
#####
# Number of iterations
iter<-10000

# Starting population
Pop2016<-739828

# Components of change for the period (year) 
# Uniform distribution guesses for components (iter, low bound, highbound)
Deaths<-runif(iter,4000,5000)
Births<-runif(iter,9000,14000)
InMigration<-runif(iter,30000,50000)
OutMigration<-runif(iter,40000,70000)

# Sampling
Pop2017<-array(0,iter)
for(i in 1:iter){Pop2017[i]<-(Pop2016 - Deaths[i] 
+ Births[i] - OutMigration[i] + InMigration[i])}

# Output
quantile(Pop2017-Pop2016,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))
quantile(Pop2017,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))
hist(Pop2017-Pop2016,50)
#####
#####

Related interface posted at: http://shiny.demog.berkeley.edu/eddieh/NextYearPop/

Also for above (repost of https://plus.google.com/+AppliedDemographyToolbox/posts/Zu4yQphpF8c): ~fun to combine and see: if unsure of the high/low bounds for components, can very easily set those as distributions as well (I just use uniform here, but could use anything, incl sample (sample()) of experts, etc):

#####
#####
# Number of iterations
iter<-10000

# Starting population
Pop2016<-739828

# Components of change for the period (year) 
# Uniform distribution guesses for components (iter, low bound, highbound)
Deaths<-runif(iter,4000,5000)
Births<-runif(iter,runif(iter,9000,10000),runif(iter,13000,14000))
InMigration<-runif(iter,runif(iter,25000,30000),runif(iter,45000,55000))
OutMigration<-runif(iter,40000,runif(iter,60000,80000))

# Sampling
Pop2017<-array(0,iter)
for(i in 1:iter){Pop2017[i]<-(Pop2016 - Deaths[i] 
+ Births[i] - OutMigration[i] + InMigration[i])}

# Output
quantile(Pop2017-Pop2016,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))
quantile(Pop2017,c(.005,.05,.1,.25,.5,.75,.90,.95,.995))
hist(Pop2017-Pop2016,50)
hist(Pop2017,50)
#####
#####
