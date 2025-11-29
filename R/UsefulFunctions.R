nrows, skip, sep, colClasses, ,comment.char="", header
## Read in text file line by line
read.table(fileName,col.names=c("my","column","names"))

## Read in text file by character by line (fixed length)
read.fwf(fileName,rep(1,no_characters_in_line))

## Convert single character into a vector of individual characters
unlist(strsplit(mydata$V1,""))

## Stop R using scientific notation
options(scipen = 999)

## Read a file in line by line
## Read data into a vector - one entry for each passport
## Each passport is concatenated into a single string
mydata<-file(fileName,"r")
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
  } else {
  }
}
close(mydata)

###################################


## Convert binary vector to numeric
convertBinaryToNumeric<-function(x){
  len<-length(x)
  dec<-0
  for (i in 1:len){
    dec<-dec+x[len-i+1]*2^(i-1)
  }
  return(dec)
}

## Convert numeric to binary vector
convertNumericToBinary<-function(number,len){
  bin<-rep(0,len)
  i<-1
  while(number>0){
    if (number>=2^(len-i)){
      bin[i]=1
      number<-number-2^(len-i)
    }
    i<-i+1
  }
  return(bin)
}

## Convert a single hex number to binary
convertHexToBinary<-function(x){
  if(x==0){
    return(c(0,0,0,0))
  } else if(x==1){
    return(c(0,0,0,1))
  } else if(x==2){
    return(c(0,0,1,0))
  } else if(x==3){
    return(c(0,0,1,1))
  } else if(x==4){
    return(c(0,1,0,0))
  } else if(x==5){
    return(c(0,1,0,1))
  } else if(x==6){
    return(c(0,1,1,0))
  } else if(x==7){
    return(c(0,1,1,1))
  } else if(x==8){
    return(c(1,0,0,0))
  } else if(x==9){
    return(c(1,0,0,1))
  } else if(x=="A"){
    return(c(1,0,1,0))
  } else if(x=="B"){
    return(c(1,0,1,1))
  } else if(x=="C"){
    return(c(1,1,0,0))
  } else if(x=="D"){
    return(c(1,1,0,1))
  } else if(x=="E"){
    return(c(1,1,1,0))
  } else if(x=="F"){
    return(c(1,1,1,1))
  }
}

combineMultipleDigits<-function(x){
  ## initialise the index along the vector
  j<-1
  ## loop along the vector
  while(j < length(x)){
    ## if both this element and the next are numbers then join them
    if(!is.na(as.numeric(x[j])) & !is.na(as.numeric(x[j+1]))){
      ## join numbers together
      x[j]<-paste0(x[j],x[j+1])
      ## remove second number
      x<-x[-(j+1)]     
    } else {
      ## iterate along the vector
      j<-j+1
    }
  }
  ## return the final vector
  return(x)
}

## Initialise blank data frame
bob<-data.frame(name=character(),x=integer(),y=integer())

