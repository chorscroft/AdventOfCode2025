# Advent of Code Day 8
# Part 1

## Read in the file
fileName<-"Data/day08.txt"
boxes<-read.table(fileName,col.names=c("x","y","z"),sep=",")

## Set the number of loops
loops<-1000

## Initialise the list of pairs of boxes
pairs<-matrix(0,nrow=loops,ncol=2)
## Initialise the vector of shortest distances
shortest<-rep(Inf,loops)

## Loop over each pair of boxes
for(i in 1:(nrow(boxes)-1)){
  for(j in (i+1):nrow(boxes)){
    ## Get the distance
    distance<-sqrt((boxes$x[i]-boxes$x[j])^2+(boxes$y[i]-boxes$y[j])^2+(boxes$z[i]-boxes$z[j])^2)
    
    ## store the distance if it is in the current top 1000
    if(distance<shortest[loops]){
      for(l in 1:loops){
        if(distance<shortest[l]){
          if(l<loops){
            ##shuffle
            shortest[(l+1):loops]<-shortest[l:(loops-1)]
            pairs[(l+1):loops,]<-pairs[l:(loops-1),]
          }
          shortest[l]<-distance
          pairs[l,1]<-i
          pairs[l,2]<-j
          break
        }
      }
    }
  }
}

## put in groups

## Initialise groups
groups<-list()
## Add a new column to record the group
pairs<-cbind(pairs,rep(0,loops))
## Initialise count of groups
g<-0

## loop until every pair is in a group
while(sum(pairs[,3]==0)>0){
  ## find first pair not already in a group
  for(i in 1:nrow(pairs)){
    if(pairs[i,3]==0){
      ## Create new group with this pair
      g<-g+1
      groups[[g]]<-pairs[i,1:2]
      pairs[i,3]<-g
      break
    }
  }
  
  ## Find every other pair that can join in this group
  change<-T
  while(change==T){
    change<-F
    for(i in 1:nrow(pairs)){
      ## first element in group
      if(pairs[i,1] %in% groups[[g]] && !(pairs[i,2] %in% groups[[g]])){
        change<-T
        groups[[g]]<-c(groups[[g]],pairs[i,2])
        pairs[i,3]<-g
      ## second element in group
      } else if(pairs[i,2] %in% groups[[g]] && !(pairs[i,1] %in% groups[[g]])){
        change<-T
        groups[[g]]<-c(groups[[g]],pairs[i,1])
        pairs[i,3]<-g
      ## both elements already in group
      } else if(pairs[i,3]==0 && pairs[i,2] %in% groups[[g]] && pairs[i,1] %in% groups[[g]]){
        change<-T
        pairs[i,3]<-g
      }
    }
  }
}

## get the sizes of the groups
size<-sort(sapply(groups,length),decreasing = T)

## calculate the result
result<-size[1]*size[2]*size[3]

## Read out the result
result

#########################################################################

# Part 2

## Create a vector of distances
distanceVec<-matrix(0,ncol=3,nrow=choose(nrow(boxes),2))
row<-1
for(i in 1:(nrow(boxes)-1)){
  for(j in (i+1):nrow(boxes)){
    distanceVec[row,]<-c(i,j,sqrt((boxes$x[i]-boxes$x[j])^2+(boxes$y[i]-boxes$y[j])^2+(boxes$z[i]-boxes$z[j])^2))
    row<-row+1
  }
}

## Record if a pair has been chosen
chosen<-rep(F,nrow(distanceVec))
## Initlaise if the pair is in the main group
in_group<-NULL
## Initialise the boxes in the main group
groups<-NULL
## Initlaise the list of pairs that have the shorest distances
pairs<-NULL

while(T){
  ## Find the pair with the smallest distance
  mindist<-min(distanceVec[chosen==F,3])
  chosenloc<-which(distanceVec[,3]==mindist)
  shortestpair<-distanceVec[chosenloc,1:2]
  chosen[chosenloc]<-T
  pairs<-rbind(pairs,shortestpair)
  in_group<-c(in_group,F)
  
  ## If first pair then start the main group
  if(is.null(groups)){
    groups<-pairs[1,]
  }
  ## Find every other pair that can join in this group
  change<-T
  while(change==T){
    change<-F
    for(i in 1:nrow(pairs)){
      if(in_group[i]==F){
        ## first element in group
        if(pairs[i,1] %in% groups && !(pairs[i,2] %in% groups)){
          change<-T
          groups<-c(groups,pairs[i,2])
          in_group[i]<-T
        ## second element in group
        } else if(pairs[i,2] %in% groups && !(pairs[i,1] %in% groups)){
          change<-T
          groups<-c(groups,pairs[i,1])
          in_group[i]<-T
        ## both elements already in group
        } else if(in_group[i]==F && pairs[i,2] %in% groups && pairs[i,1] %in% groups){
          change<-T
          in_group[i]<-T
        }
      }
    }
  }
  ## If all boxes are in the group, stop
  if(length(groups)==nrow(boxes)){
    break
  }
}
  
## Calculate the result
result<-boxes$x[pairs[nrow(pairs),1]]*boxes$x[pairs[nrow(pairs),2]]

## Read out the result
result

  