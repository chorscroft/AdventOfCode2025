# Advent of Code Day 1
# Part 1

## Read in the file
fileName<-"Data/day01.txt"
data<-read.table(fileName,col.names=c("rotations"))

## Initialise zero count
countZeros<-0
## Initialise current location
currentLocation<-50

## loop through each line of instructions
for(i in 1:nrow(data)){
  ## Extract the direction
  direction<-substr(data$rotations[i],1,1)
  ## Extract the number of clicks
  clicks<-as.numeric(substr(data$rotations[i],2,nchar(data$rotations[i])))
  
  ## Turn the dial, take into account numbers > 100
  if(direction=="L"){
    currentLocation<-currentLocation-clicks
    while(currentLocation<0){
      currentLocation<-100+currentLocation
    }
  } else {
    currentLocation<-currentLocation+clicks
    while(currentLocation>99){
      currentLocation<-currentLocation-100
    }
  }
  
  ## Count if the new location is zero
  if(currentLocation==0){
    countZeros<-countZeros+1
  }
}
result<-countZeros

## Read out the result
result

#########################################################################

# Part 2

## Initialise zero count
countZeros<-0
## Initialise current location
currentLocation<-50

## loop through each line of instructions
for(i in 1:nrow(data)){
  ## Extract the direction
  direction<-substr(data$rotations[i],1,1)
  ## Extract the number of clicks
  clicks<-as.numeric(substr(data$rotations[i],2,nchar(data$rotations[i])))
  
  if(direction=="L"){
    ## Stops overcounting if you start at 0
    if(currentLocation==0){
      countZeros<-countZeros-1
    }
    ## take into account numbers over 100
    while(clicks>99){
      clicks<-clicks-100
      countZeros<-countZeros+1
    }
    ## update location
    currentLocation<-currentLocation-clicks
    ## count if end on zero
    if(currentLocation==0){
      countZeros<-countZeros+1
    }
    ## if negative, add 100 and count
    if(currentLocation<0){
      currentLocation<-100+currentLocation
      countZeros<-countZeros+1
    }
  } else { ##Turn right
    currentLocation<-currentLocation+clicks
    while(currentLocation>99){
      currentLocation<-currentLocation-100
      countZeros<-countZeros+1
    }
  }
}
result<-countZeros

## Read out the result
result
