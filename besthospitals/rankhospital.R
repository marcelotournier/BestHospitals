rankhospital <- function(x,y,num="best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  lista = c('heart attack','heart failure','pneumonia')
## Check that state and outcome are valid
  if(x %in% outcome[,7] == FALSE){
    stop('invalid state')
    }
  if(y %in% lista == FALSE){
    stop('invalid outcome')
    }
  if(y == 'heart attack'){
    r=11
    # heart attack = [,11]
    }
  if(y == 'heart failure'){
    r=17
    # heart failure = [,17]
    }
  if(y == 'pneumonia'){
    r=23
    # pneumonia = [,23]
    }
  rank=outcome[outcome$State == x,]
  rank[,r]=gsub("Not Available",NA, rank[,r])
  rank[,r]=as.numeric(as.character(rank[,r]))
  ord=rank[ order(rank[,r], rank[,2], na.last = NA), ]
  if(num == "best"){
    head(ord[,2],1)
    } else if(num == "worst"){
    tail(ord[,2],1)
    } else if(is.numeric(num)) {
    ord[num,2]
  }
  }
## Return hospital name in that state with the given rank
## 30-day death rate
#Selection: 4
#Running test:
 # rankhospital("NC", "heart attack", "worst")
# Result:  Incorrect!