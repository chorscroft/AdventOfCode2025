# Advent of Code Day 2
# Part 1

## Read in the file
fileName<-"Data/day02.txt"
data<-read.table(fileName,col.names=c("ids"))

## Get the IDs
ids<-unlist(strsplit(data$ids,","))

## Initialise sum of invalid IDs
sumInvalid <- 0

## Create a function to test if the ID is invalid
failtest<-function(test){
  str_test<-as.character(test)
  if(nchar(str_test) %% 2 ==0){
    ## If ID is an even number of digits long, split in half
    half1<-substr(str_test,1,nchar(str_test)/2)
    half2<-substr(str_test,nchar(str_test)/2+1,nchar(str_test))
    ## If the halves are the same, then it is valid
    if(half1==half2){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

for(i in 1:length(ids)){
  ## get the first and last id in the range
  first<-as.numeric(substr(ids[i],1,regexpr("-",ids[i])-1))
  last<-as.numeric(substr(ids[i],regexpr("-",ids[i])+1,nchar(ids[i])))
  ## loop over each ID
  for(test in first:last){
    if (failtest(test)==TRUE){
      sumInvalid<-sumInvalid+test
    }
  }
}

result<-sumInvalid

## Read out the result
result

#########################################################################

# Part 2

## Initialise sum of invalid IDs
sumInvalid <- 0

## Create a function to test if the ID is invalid
failtest<-function(test){
  str_test<-as.character(test)
  ## make ID into an array
  str_test_split<-unlist(strsplit(str_test,""))
  ## only test for IDs longer than one digit
  if(nchar(str_test)>1){
    ## try to split
    for(splits in 2:nchar(str_test)){
      pass<-FALSE
      ## if the ID splits equally into that many parts then try it
      if(nchar(str_test) %% splits ==0){
        addInd<-nchar(str_test)/splits
        fail<-FALSE
        ## See if each part is the same
        for(i in 1:addInd){
          for(j in 2:splits){
            if(str_test_split[i]!=str_test_split[i+addInd*(j-1)]){
              fail<-TRUE
              break
            }
          }
          if(fail==TRUE){
            break
          }
        }
        if(fail==FALSE){
          pass<-TRUE
        }
      }
      splits<-splits+1
      if(pass==TRUE){
        break
      }
    }
    if(pass==TRUE){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


for(i in 1:length(ids)){
  ## get the first and last id in the range
  first<-as.numeric(substr(ids[i],1,regexpr("-",ids[i])-1))
  last<-as.numeric(substr(ids[i],regexpr("-",ids[i])+1,nchar(ids[i])))
  ## loop over each ID
  for(test in first:last){
    if (failtest(test)==TRUE){
      sumInvalid<-sumInvalid+test
    }
  }
}

result<-sumInvalid

## Read out the result

result