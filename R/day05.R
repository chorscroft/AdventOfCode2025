# Advent of Code Day 5
# Part 1

## Read in the file
fileName<-"Data/day05.txt"

## Initialise ranges data frame and ingredients list
ranges<-data.frame(x=integer(),y=integer())
ingredients<-NULL

## Loop through each line of the file
onRanges <- TRUE
mydata<-file(fileName,"r")
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    onRanges<-FALSE
  } else {
    if(onRanges==TRUE){
      range<-as.numeric(unlist(strsplit(line,"-")))
      ranges<-rbind(ranges,data.frame(x=range[1],y=range[2]))
    } else {
      ingredients<-c(ingredients,as.numeric(line))
    }
  }
}
close(mydata)

## Initialise result
result<-0
## Loop through each ingredient
for(i in 1:length(ingredients)){
  fresh<-FALSE
  ## See if it falls in any of the ranges, if so stop
  for(j in 1:nrow(ranges)){
    if(ingredients[i]>=ranges$x[j] && ingredients[i]<=ranges$y[j]){
      fresh <-TRUE
      break
    }
  }
  if(fresh==TRUE){
    ## iterate the result
    result<-result+1
  }
}

## Read out the result
result

#########################################################################

# Part 2
## Stop R using scientific notation
options(scipen = 999)

## Loop until no changes are made to the ranges
change<-TRUE
while(change==TRUE){
  i<-1
  change<-FALSE
  ## Check each pair of ranges
  while(i<nrow(ranges)){
    j<-i+1
    while(j<=nrow(ranges)){
      ## i Completely inside j
      if(ranges$x[i]>=ranges$x[j] && ranges$x[i]<=ranges$y[j] && ranges$y[i]>=ranges$x[j] && ranges$y[i]<=ranges$y[j]){
        ranges<-ranges[-i,]
        i<-i-1
        change<-T
        ## j Completely inside i 
      } else if(ranges$x[j]>=ranges$x[i] && ranges$x[j]<=ranges$y[i] && ranges$y[j]>=ranges$x[i] && ranges$y[j]<=ranges$y[i]){
        ranges<-ranges[-j,]
        j<-j-1
        change<-T
        ## j overlaps i's max
      } else if(ranges$y[i]>=ranges$x[j] && ranges$y[i]<=ranges$y[j]){
        ranges$y[i]<-ranges$y[j]
        ranges<-ranges[-j,]
        j<-j-1
        change<-T
        ## i overlaps j's max
      } else if(ranges$y[j]>=ranges$x[i] && ranges$y[j]<=ranges$y[i]){
        ranges$y[j]<-ranges$y[i]
        ranges<-ranges[-i,]
        i<-i-1
        change<-T
      }
      j<-j+1
    }
    i<-i+1
  }
}

## Initialise the result
result<-0
## Count all the IDs in the ranges
for(i in 1:nrow(ranges)){
  result<-result+ranges$y[i]-ranges$x[i]+1
}

## Read out the result
result

