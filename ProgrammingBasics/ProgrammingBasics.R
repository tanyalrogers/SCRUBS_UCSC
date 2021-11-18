# Programming Basics
# Tanya Rogers
# adapted from Umi Hoshijima, adapted from software carpentry
# Nov 2021

# Functions ---------------------------------------------------------------

# defining a function
# name_of_function <- function(inputs) {
#   output <- # calculations involving inputs
#   return(output)
# }
# The function called "function" creates a function. You assign the function to
# a name. The function can have one or more inputs. Inside the {}, insert all
# the calculations you want to do with the inputs. There can be multiple
# intermediate steps. return() indicates which value the function should return
# as its output. If you don't include return() the function will just return
# whatever the last calculated value was, which is usually what you want, but
# not always, so it's good practice to include return()

# using a function
# myoutput <- name_of_function(inputs = myinputs)

# A function to compute standard error 
se <- function(x) { 
  # It is often useful here to write a comment about what the function does.
  # Outputs the std. error of the mean. 
  # x should be a vector of numbers.
  sd_x <- sd(x) #get the sd
  n <- length(x) #get n
  se_x <- sd_x/sqrt(n)
  return(se_x)
}

dat <- c(1,2,3,4,5)
se(dat)
sd_x
# Note that even though we used sd_x within the function se, it doesn't exist
# outside of the function. This is known as a NAMESPACE - the sd_x only exists
# within the namespace of the function se. If you really want to change a
# variable in your global environment from within a function, you can use "<<-"
# but this is pretty unusual.

# A function that recenters the mean of a set of data.
# Subtracting the mean from a set a data will make the mean 0.
# Adding a number to the zero-mean data, will make the mean that number.
mean(dat)
dat_zero <- dat - mean(dat)
mean(dat_zero)

# This function has TWO inputs: 
center_mean <- function(x, center){
  # centers the mean
  # x should vector of numbers
  # center should be a single number
  zeroed_dat <- x - mean(x)
  return(zeroed_dat + center)
}
center_mean(dat, 0)
center_mean(dat, 100)
center_mean(dat)
# This last one doesn't work because center_mean() expects two inputs. 

# You can set up center_mean so the second input is optional.
# If you don't put a second input, have it default to 0: 

center_mean <- function(x, center = 0) {
  # centers the mean.
  # x should be a number, or vector of numbers
  # center should be a single number
  zeroed_dat <- x - mean(x)
  return(zeroed_dat + center)
}

center_mean(dat)

# If the arguments (inputs) are supplied in order, they do not need to be named,
# but you can name them if you want. If there a lot of inputs, it's hard to
# remember what order they're in, so this is often a good idea. If you name
# them, they don't have to be in order.

center_mean(x=dat, center=100)
center_mean(center=100, x=dat)

# Beware: we haven't made this function completely fool-proof: 
center_mean('text')
# This will break it. 

# You can look at a functions' definition if you type it without parentheses: 

center_mean

# This doesn't work for all functions that R comes with.

# Conditionals ------------------------------------------------------------

# The setup of a conditional (if/else) statement

# if(logicalcondition) {
#   code that will run if condition is T
# } 
# else {
#   code that will run if condition is F
# }

# 'logicalcondition' must return a single T/F value (not a vector). Including
# the 'else' statement is not required. If the condition is F, then R will just
# do nothing.

# Including logical arguments or arguments linked to logical conditions is a
# useful way to control what your function does. e.g. na.rm=T in the 'mean'
# function

# Adding a scaling option to the center_mean function. We will add another
# argument indicating whether or not to divide by the sd after centering, so as
# to make the variance equal to 1.

center_scale <- function(x, center = 0, scale = TRUE) {
  # centers the mean, optionally scales
  # x should be a number, or vector of numbers
  # center should be a single number
  zeroed_dat <- x - mean(x)
  if(scale==TRUE) {
    zeroed_dat <- zeroed_dat/sd(zeroed_dat)
  }
  return(zeroed_dat + center)
}

test <- center_scale(dat)
mean(test)
sd(test)

test <- center_scale(dat, scale = FALSE)
mean(test)
sd(test)

test <- center_scale(dat, center = 10, scale = TRUE)
mean(test)
sd(test)

# Loops -------------------------------------------------------------------

# additional resources
# http://swcarpentry.github.io/r-novice-inflammation/03-loops-R/index.html
# http://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/

# If you find yourself copying and pasting the same code over and over again and
# just changing one or a few variables, this is a good indicator that you could
# use a loop. This could be in the context of making plots, performing analyses,
# or anywhere you want to repeat the same set of operations.

# The general structure of a 'for' loop:

# for(variable in sequence) {
#   #code that involves variable
# }

# The code inside the loop gets run multiple times. The number of times it is
# run (iterated) is length(sequence). In each run, 'variable' takes on a
# different value. The values it takes on are the values in 'sequence'.
# 'variable' is often given the a single letter name like 'i', but it can be
# called anything.

# Here's a simple example

sequence <- 1:5
for(i in sequence) {
  print(i*2)
}

# This examples just prints the result. If we want to store the result, we will
# have to assign it to something.

for(i in sequence) {
  A <- i*2
}

# But notice how this doesn't work! 'A' gets overwritten during each iteration,
# so it's value is just 10 (the value after the last iteration). The solution is
# that A needs to be a vector, and the result is stored as the ith value.

for(i in sequence) {
  Avec[i] <- i*2
}

# But this doesn't work either! 'A' gets generated anew each cycle, but here, R
# is looking for the ith value fo 'Avec', assuming that 'Avec' already exists.
# The solution is that we need to initialize Avec prior to starting the loop.

Avec <- vector(length = length(sequence))
for(i in sequence) {
  Avec[i] <- i*2
}
Avec

# Now it works. Loops usually require some sort of initialization of the object
# where the results of the loop are to be stored.

# This example is not a case where you would actually want to use a loop because
# most mathematical operations are vectorized. In other words, you could just to
# this and get the same answer with a lot less code:

Avec2 <- sequence*2

# However, loops are useful when you are doing something more complicated, e.g.
# applying the same analysis to multiple datasets, bootstrapping, simulating
# time series data
