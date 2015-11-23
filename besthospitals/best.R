best <- function(x,y) {
  # best <- function(x = state, y = outcome) {
  ## Read outcome data
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  rank=outcome[outcome$State == x,]
  rank[,r]=gsub("Not Available",NA, rank[,r])
  rank[,r]=as.numeric(as.character(rank[,r]))
  p=which.min(rank[,r])
  rank[p,2]
}