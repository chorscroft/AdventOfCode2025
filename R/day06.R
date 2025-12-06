# Advent of Code Day 6
# Part 1

## Read in the file
fileName<-"Data/day06.txt"
data<-read.table(fileName)

## Initialise the result
result<-0
## Loop over each column
for(j in 1:ncol(data)){
  ## Initialise the value for the column with the first number
  colTotal<-as.numeric(data[1,j])
  if(data[nrow(data),j]=="+"){
    ## If +, then add the rest of the column together
    for(i in 2:(nrow(data)-1)){
      colTotal<-colTotal+as.numeric(data[i,j])
    }
  } else {
    ## If *, then multiply the rest of the column together
    for(i in 2:(nrow(data)-1)){
      colTotal<-colTotal*as.numeric(data[i,j])
    }    
  }
  ## Add the column total to the final result
  result<-result+colTotal
}

## Read out the result
result

#########################################################################

# Part 2

## Initialise the result
result<-0

## Read in the file
no_characters_in_line<-3770
data<-read.fwf(fileName,rep(1,no_characters_in_line))

## Loop over each column in the data (each character is a column)
for(j in 1:ncol(data)){
  ## If the column is blank, add the column total to the final result
  if(sum(is.na(data[,j]))==nrow(data)){
    result<-result+colTotal
  } else {
    ## Initialse the number for the column of digits
    number<-""
    ## Create the full number from the column of digits
    for(i in 1:(nrow(data)-1)){
      if(!is.na(data[i,j]) & data[i,j]!=" "){
        number<-paste0(number,data[i,j])
      }
    }
    if(is.na(data[nrow(data),j])){
      ## Add or multiply the number to the column total
      if(currentOp=="+"){
        colTotal<-colTotal+as.numeric(number)
      } else {
        colTotal<-colTotal*as.numeric(number)
      }
    ## Initialise the column total and set the operator
    } else if(data[nrow(data),j]=="+"){
      currentOp<-"+"
      colTotal<-as.numeric(number)
    } else if(data[nrow(data),j]=="*"){
      currentOp<-"*"
      colTotal<-as.numeric(number)
    }
  }
}

## Get final colTotal
result<-result+colTotal

## Read out the result
result

