#Matt Kustra 4/20/21
#tutorial on how to use foreach (and other packages) to parallel compute
rm(list=ls())

# 1. Load up Libraries ------------------------------------------------------------
library(foreach)
library(doParallel)
library(tidyverse)

# 2. For loop to calculate if a number is prime ---------------------------
# * 2.a prime calculation function ----------------------------------------------------
#from https://en.wikipedia.org/wiki/Primality_test#:~:text=A%20primality%20test%20is%20an,an%20input%20number%20is%20prime.&text=Some%20primality%20tests%20prove%20that,tests%20instead%20of%20primality%20tests.
#function that tests for primality
is_prime<-function(n){
  if (n <= 3){
    return (n > 1)
  }
  if (n %% 2 == 0 | n %% 3 == 0){
    return (FALSE)}
  i <- 5
  while (i ** 2 <= n){
    if (((n %% i) == 0) | ((n %% (i + 2)) == 0)){
      return(FALSE)}
    i<-i+6}
  return(TRUE)
}

# * 2.b generate a vector of numbers-------------------------------------------------------------------
#Long array of large prime values
numbers<-read.csv("data/Large_primeNumbers.csv")[,1]

# * 2.c For loop ----------------------------------------------------------
#initilize and empty vector
forresult<-rep(NA,length(numbers))
system.time(
for(i in 1:length(numbers)){
  #store the result
  forresult[i]<-is_prime(numbers[i])
})
#but what about apply famly isn't that good?
system.time(applyresult<-lapply(numbers,is_prime))

# 3. foreach --------------------------------------------------------------
?foreach
#foreach is a function that acts like a for loop but returns a value i.e. 
#the structure is foreach(iterator=xxx) %operator% {expression}
#different operators, mainly %do% and %dopar%
#%do% is not parallel and similar to apply family but looks more like a typical for loop 
#%dopar% is parallel
#think of the expression as what you would put in the body of the for loop

#lets make the above for loop as a foreach version
system.time(foreachresult<-foreach(i=1:length(numbers)) %do% {
  is_prime(numbers[i])
})
#similar time as above examples because we are using %do%
foreachresult
#and results look very similar to lapply.
#but now lets try doing parallel.
#but first we need to let R know we are parallel computing by setting up the cluster.

# 4. Registering Parallel backend --------------------------------------------------
#First see how many cores you have on your computer!
detectCores()
#Want to register a certain number of cores
#I usually use less than max, so I can do other things on my computer.

#One way is to make a cluster
#registerDoParalle(cores=number of coores you want to use)
registerDoParallel(cores=5)
system.time(foreach(i=1:length(numbers)) %dopar% {
  is_prime(numbers[i])
})
#generally good practice to end cluster, prevents system from still taking up cpu's
stopImplicitCluster()

#compare times 
registerDoParallel(cores=5)
system.time(foreach(i=1:length(numbers)) %dopar% {
  is_prime(numbers[i])
})
system.time(foreach(i=1:length(numbers)) %do% {
  is_prime(numbers[i])
})

# 5. Changing output ---------------------------------------------------
foreachresultdefault<-foreach(i=1:length(numbers)) %dopar% {
  is_prime(numbers[i])
}
#by default it's a list of results, which isn't that great
head(foreachresultdefault)

#we can change how information is compiled wiith the .combine function
#this says how we want to combine the various tasks back together

#c makes it a vector
foreachresultC<-foreach(i=1:length(numbers),.combine="c") %dopar% {
  is_prime(numbers[i])
}
foreachresultC
#rbind makes it a matrix and or dataframe
foreachresultR<-foreach(i=1:length(numbers),.combine="rbind")%dopar%{
  is_prime(numbers[i])
}
foreachresultR

#not that informative.
#let's do it as a dataframe
foreachresultdf<-foreach(i=1:length(numbers),.combine="rbind")%dopar%{
  col<-data.frame(Number=numbers[i],IsPrime=is_prime(numbers[i]))
  return(col)
}
head(foreachresultdf)
#although simple, like a for loop inside the expression {} we can do any number of tasks

# 6. More practical example -----------------------------------------------
# * 6.a load up dataset ----------------------------------------------
#birthrate dataset
#going to only use the year 2000
birth_ratesD<-read.csv("http://stewartecology.org/docs/Misc/children-per-woman-UN.csv")%>%filter(Year==2000)
head(birth_ratesD)
str(birth_ratesD)
#lets just calculate the difference in birth rates between all possible combinations of entities 
#save the unique entities
Entities<-unique(birth_ratesD$Entity)
#create empty data frame to store data
Resultsd<-expand.grid(Entities,Entities)%>%
  mutate(diff=0)
#now let's time it
system.time(for(i in Entities){
  for(j in Entities){
      curdif<-birth_ratesD$births_per_woman[birth_ratesD$Entity==j]-birth_ratesD$births_per_woman[birth_ratesD$Entity==i]
      Resultsd$diff[Resultsd$Var1==i&Resultsd$Var2==j]<-curdif
    }
  })
Resultsd

# 7. Nesting foreach ------------------------------------------------------
#we can nest foreach with the %:% operator
#first register cores
registerDoParallel(9)
system.time(ResultsPar<-
      #two different foreachs to nest using %:% after first one
      foreach(i =Entities,.combine=rbind)%:%
        #only have %dopar% for last one
        foreach(j=Entities,.combine=rbind)%dopar%{
     return(data.frame(Entity1=i,Entity2=j,diff=birth_ratesD$births_per_woman[birth_ratesD$Entity==j]-birth_ratesD$births_per_woman[birth_ratesD$Entity==i]))
})
ResultsPar
stopImplicitCluster()

# 8. Alternative parallelization methods ------------------------------------------------
# Tanya Rogers

# * 8.a  parallel::parLapply ----------------------------------------------
# Remember lapply? 
system.time(applyresult<-lapply(numbers,is_prime))
# There is a parallel version of that too.
library(parallel)
# set up the cluster first
num_cores <- 5
clust <- makeCluster(num_cores)
#The parallel version of lapply() is parLapply() and needs an additional cluster argument
system.time(parLapply_result<-parLapply(clust,numbers,is_prime))
#To get output as a vector rather than a list, use parSapply
system.time(parSapply_result<-parSapply(clust,numbers,is_prime))
#remember to stop cluster
stopCluster(clust) 

# * 8.b  furrr::future_map ----------------------------------------------
# The purrr package in tivyverse contains the map function which is very much like lapply
system.time(mapresult<-map(numbers,is_prime))
# To return as a vector rather than a list, use map_dbl(), map_lgl(), or map_chr() depending
#   on the type of output. Here is would be map_lgl because the outputs are logical.
# The furrr package contains a parallel version of map. It uses the future package in the background.
library(furrr)
#set up how many cores to use
num_cores <- 5
plan(multiprocess, workers = num_cores) #see ?plan for more detail
# arguments are identical to map, just change the function to future_map
system.time(fmapresult<-future_map(numbers,is_prime))
#if you want future_map to return to using only one core
plan(sequential)
