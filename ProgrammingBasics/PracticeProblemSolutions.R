# Practice Problem Solutions

# 1.  ---------------------------------------------------------------------

ZerotoNA <- function(x) {
  x[which(x==0)] <- NA
  return(x)
}

test <- c(1,2,3,0,0,4,5,NA)
ZerotoNA(test)

# 2.  ---------------------------------------------------------------------

NAtoZero <- function(x) {
  x[which(is.na(x))] <- 0
  return(x)
}

NAtoZero(test)

# 3.  ---------------------------------------------------------------------

times2 <- function(x) {
  if(!is.numeric(x)) {
    return("Not a number")
  } else {
    x2 <- x*2
    return(x2)
  }
}

times2(5)
times2("A")

# 4.  ---------------------------------------------------------------------

lengthdata <- data.frame(Length=c(10,11,50,12,NA,0.5,100,0.2),
                         Units=c("cm","cm","mm","cm",NA,"not recorded","mm","m"))

convertlengths <- function(len, units) {
  if(!is.na(units) & !is.na(len)) {
    if(units=="cm") {
      y=len
    } 
    else if(units=="mm") {
      y=len/10
    } 
    else if(units=="m") {
      y=len*100
    } 
    else { #if units do not match "m", "cm", or "mm"
      y=NA
    }
  } 
  else { #if the units are NA
    y=NA
  }
  return(y)
}

lengthdata$Lengthcm=NA
for(i in 1:nrow(lengthdata)) {
  lengthdata$Lengthcm[i] <- convertlengths(len=lengthdata$Length[i],units=lengthdata$Units[i])
}
lengthdata

# 5.  ---------------------------------------------------------------------

convertlengths2 <- function(len, units, outunits="cm") {
  if(!is.na(units) & !is.na(len)) {
    if(units=="cm") {
      y=len
    } 
    else if(units=="mm") {
      y=len/10
    } 
    else if(units=="m") {
      y=len*100
    } 
    else { #if units do not match "m", "cm", or "mm"
      y=NA
    }
  } 
  else { #if the units are NA
    y=NA
  }
  
  if(outunits=="cm") {
    return(y)
  }
  if(outunits=="mm") {
    return(y*10)
  }

}

lengthdata$Lengthmm=NA
for(i in 1:nrow(lengthdata)) {
  lengthdata$Lengthmm[i] <- convertlengths2(len=lengthdata$Length[i],units=lengthdata$Units[i],outunits = "mm")
}
lengthdata

# 6.  ---------------------------------------------------------------------

randwalk <- numeric(length = 50)
randwalk[1] <- 0

for(i in 2:length(randwalk)) {
  randwalk[i] <- randwalk[i-1] + rnorm(n = 1, mean = 0, sd = 1)
}

plot(randwalk, type="l")

# 7.  ---------------------------------------------------------------------

#rows are timesteps, cols are simulations
randwalk2 <- matrix(ncol = 10, nrow = 50)
randwalk2[1,] <- 0

for(j in 1:ncol(randwalk2)) { #simulations
  for(i in 2:nrow(randwalk2)) { #timesteps
    randwalk2[i,j] <- randwalk2[i-1,j] + rnorm(n = 1, mean = 0, sd = 1)
  }
}

plot(randwalk2[,1], type="n",ylim=range(randwalk2),ylab="Value")
for(j in 1:ncol(randwalk2)) {
  lines(randwalk2[,j], col=j)
}

# 8.  ---------------------------------------------------------------------

fish <- read.csv("data/groundfish.csv")

gbfish <- fish[fish$Region=="Georges Bank",-c(1,2)]

cor(gbfish)

nspecies=ncol(gbfish)
cormat=matrix(nrow = nspecies, ncol = nspecies)
for(i in 1:nspecies) {
  for(j in 1:nspecies) {
    cormat[i,j]=cor(gbfish[,i],gbfish[,j])
  }
}
rownames(cormat) <- colnames(cormat) <- colnames(gbfish)
cormat

# 9.  ---------------------------------------------------------------------

pmat=matrix(nrow = nspecies, ncol = nspecies)
for(i in 1:nspecies) {
  for(j in 1:nspecies) {
    pmat[i,j]=cor.test(gbfish[,i],gbfish[,j])$p.value
  }
}
rownames(pmat) <- colnames(pmat) <- colnames(gbfish)
pmat
