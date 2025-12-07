# Advent of Code Day 7
# Part 1

## Read in the file
fileName<-"Data/day07.txt"
no_characters_in_line<-141
data<-read.fwf(fileName,rep(1,no_characters_in_line))

## Locate start position
start<-which(data[1,]=="S")
## Turn it into a beam
data[1,start]<-"|"

## Initialise result
result<-0

## Loop through each location
for(i in 2:nrow(data)){
  for(j in 1:ncol(data)){
    ## Find where the location above was a beam
    if(data[i-1,j]=="|"){
      ## If a dot, then become a beam
      if(data[i,j]=="."){
        data[i,j]<-"|"
      } else if(data[i,j]=="^"){
        ## If a splitter, then count the split and create beams either side
        result<-result+1
        if(data[i,j-1]=="."){
          data[i,j-1]<-"|"
        }
        if(data[i,j+1]=="."){
          data[i,j+1]<-"|"
        }
      }
    }
  }
}


## Read out the result
result

#########################################################################

# Part 2

## Initialise the count of times a location has been visited
map<-matrix(0,nrow=nrow(data),ncol=ncol(data))
## First beam visited once
map[1,start]<-1

## Loop over each location
for(i in 2:nrow(map)){
  for(j in 1:ncol(map)){
    ## if the data is a beam count how many times it is visited
    if(data[i,j]=="|"){
      ## Add visits from above
      if(data[i-1,j]=="|"){
        map[i,j]<-map[i,j]+map[i-1,j]
      }
      ## Add visits from the left
      if(j>1 && data[i,j-1]=="^"){
        map[i,j]<-map[i,j]+map[i-1,j-1]
      }
      ## Add visits from the right
      if(j<ncol(data) && data[i,j+1]=="^"){
        map[i,j]<-map[i,j]+map[i-1,j+1]
      }
    }
  }
}

## Add visits on the lowest line
result<-sum(map[nrow(map),])

## Read out the result
result

