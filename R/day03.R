# Advent of Code Day 3
# Part 1

## Read in the file
fileName<-"Data/day03.txt"
no_characters_in_line<-100
data<-read.fwf(fileName,rep(1,no_characters_in_line))

## Stop R using scientific notation
options(scipen = 999)

## Initialise result
result<-0
## Loop over each bank
for (i in 1:nrow(data)){
  bank<-data[i,]
  maxJolt<-0
  ## Loop over each combination and store the biggest value
  for(x in 1:(ncol(bank)-1)){
    for(y in (x+1):ncol(bank)){
      testValue<-as.numeric(paste0(bank[1,x],bank[1,y]))
      if(testValue>maxJolt){
        maxJolt<-testValue
      }
    }
  }
  ## Add the biggest to the result
  result<-result+maxJolt
}

## Read out the result
result


#########################################################################

# Part 2

## Initialise result
result<-0

## Loop over each bank
for (i in 1:nrow(data)){
  bank<-data[i,]
  maxJolt<-""
  currentMinLoc <-1
  ## Find the largest number in the section of possible batteries
  for(d in 11:0){
    maxJolt<-paste0(maxJolt,max(bank[currentMinLoc:(ncol(bank)-d)]))
    currentMinLoc<-currentMinLoc+max.col(bank[currentMinLoc:(ncol(bank)-d)],ties.method = "first")
  }
  ## Add the biggest to the result
  result<-result+as.numeric(maxJolt)
}

## Read out the result
result
