rankall <- function(y, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  lista = c('heart attack','heart failure','pneumonia')
  states=unique(rank$State)
  ## Check that state and outcome are valid
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
  outcome[,r]=gsub("Not Available",NA, outcome[,r])
  outcome[,r]=as.numeric(as.character(outcome[,r]))
  rank=outcome[ order(outcome[,r], outcome[,2]), ]
  hospital=c("hospital","state")
  state=c(0,0)
  dat=data.frame(hospital,state)
  dat[1]=as.character(dat[1])
  dat=dat[0,]
  if(num == "best"){
    for(i in 1:54){
      rank2=rank[rank$State == states[i],]
      dat[i,1]=head(rank2,1)[2]
      dat[i,2]=states[i]
    }
    row.names(dat)=dat$state
    dat
  } else if(num == "worst"){
    for(i in 1:54){
      rank2=rank[rank$State == states[i],]
      dat[i,1]=tail(rank2,1)[2]
      dat[i,2]=states[i]
    }
    row.names(dat)=dat$state
    dat
  } else if(is.numeric(num)) {
    for(i in 1:54){
      rank2=rank[rank$State == states[i],]
      dat[i,1]=rank2[num,2]
      dat[i,2]=states[i]
    }
    row.names(dat)=dat$state
    dat
  }
}
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
