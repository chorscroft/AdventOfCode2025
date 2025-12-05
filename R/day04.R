# Advent of Code Day 4
# Part 1

## Read in the file
fileName<-"Data/day04.txt"
no_characters_in_line<-139
data<-read.fwf(fileName,rep(1,no_characters_in_line))

## Initalise map that shows the number of rolls adjacent to a roll
countMap<-matrix(0,nrow=nrow(data),ncol=ncol(data))
## Loop over each location
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    ## If it isn't a roll then count 9
    if(data[i,j]=="."){
      countMap[i,j]<-9
    } else {
      ## Check each location around the roll and count other rolls
      if(i>1 && j>1 && data[i-1,j-1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(i>1 && data[i-1,j]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(i>1 && j<ncol(data) && data[i-1,j+1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(j>1 && data[i,j-1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(j<ncol(data) && data[i,j+1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(i<nrow(data) && j>1 && data[i+1,j-1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(i<nrow(data) && data[i+1,j]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
      if(i<nrow(data) && j<ncol(data) && data[i+1,j+1]=="@"){
        countMap[i,j]<-countMap[i,j]+1
      }
    }
  }
}

## Count the number of rolls with less than 4 other rolls around it
result<-sum(countMap<4)

## Read out the result
result

#########################################################################

# Part 2
fileName<-"Data/day04.txt"
no_characters_in_line<-139
data<-read.fwf(fileName,rep(1,no_characters_in_line))
result<-0

while(TRUE){
  ## Initalise map that shows the number of rolls adjacent to a roll
  countMap<-matrix(0,nrow=nrow(data),ncol=ncol(data))
  ## Loop over each location
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      ## If it isn't a roll then count 9
      if(data[i,j]=="."){
        countMap[i,j]<-9
      } else {
        ## Check each location around the roll and count other rolls
        if(i>1 && j>1 && data[i-1,j-1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(i>1 && data[i-1,j]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(i>1 && j<ncol(data) && data[i-1,j+1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(j>1 && data[i,j-1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(j<ncol(data) && data[i,j+1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(i<nrow(data) && j>1 && data[i+1,j-1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(i<nrow(data) && data[i+1,j]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
        if(i<nrow(data) && j<ncol(data) && data[i+1,j+1]=="@"){
          countMap[i,j]<-countMap[i,j]+1
        }
      }
    }
  }
  if(sum(countMap<4)>0){
    ## Count number of rolls to be removed
    result<-result+sum(countMap<4)
    ## remove rolls that have less than 4 other rolls around it
    data[countMap<4]<-"."
  } else {
    ## stop if nothing was removed
    break
  }
}

## Read out the result
result

