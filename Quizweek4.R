best <-function(state, outcome_name){
  outcome <- read.csv("outcome-of-care-measures.csv")
  outcome_types <- c('heart attack', 'heart failure', 'pneumonia')
  
  if(!state %in% state.abb){
    stop("invalid state")
  }
  if(!outcome_name %in% outcome_types){
    stop("invalid outcome")
  }
  
  foutcome_name <- strsplit(outcome_name, " ")[[1]]
  if(length(strsplit(foutcome_name, " ")) > 1){
    temp_name <- paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
    temp_name <- paste(temp_name, paste(toupper(substr(foutcome_name[[2]],1,1)), substr(foutcome_name[[2]],2,nchar(foutcome_name[[2]])), sep = ''), sep = ".")
    outcome_name <- temp_name  
  }else if (length(strsplit(foutcome_name, " ")) == 1){
    outcome_name = paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
  }
   
  x <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome_name, sep = '')
  sub_outcome <- outcome[outcome$State == state,]
  sub_outcome[[x]] <- as.numeric(sub_outcome[[x]])
  #sub_outcome <-sub_outcome[!is.na(sub_outcome[[x]]),]
  #sub_outcome<- sub_outcome[na.omit(sub_outcome[[x]]),]
  min_death <- min(sub_outcome[[x]])
  sub_outcome <-sub_outcome[sub_outcome[[x]] == min_death,]
  sub_outcome <- unique.data.frame(sub_outcome)
  if(nrow(sub_outcome) == 1){
    as.character(sub_outcome$Hospital.Name[[1]])
  }else if(nrow(sub_outcome) > 1){
    sub_outcome <- sub_outcome[order(sub_outcome$Hospital.Name),]
    as.character(sub_outcome$Hospital.Name[[1]])
  }
}

worst <-function(state, outcome_name){
  outcome <- read.csv("outcome-of-care-measures.csv")
  outcome_types <- c('heart attack', 'heart failure', 'pneumonia')
  
  if(!state %in% state.abb){
    stop("invalid state")
  }
  if(!outcome_name %in% outcome_types){
    stop("invalid outcome")
  }
  
  foutcome_name <- strsplit(outcome_name, " ")[[1]]
  if(length(strsplit(foutcome_name, " ")) > 1){
    temp_name <- paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
    temp_name <- paste(temp_name, paste(toupper(substr(foutcome_name[[2]],1,1)), substr(foutcome_name[[2]],2,nchar(foutcome_name[[2]])), sep = ''), sep = ".")
    outcome_name <- temp_name  
  }else if (length(strsplit(foutcome_name, " ")) == 1){
    outcome_name = paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
  }
  
  x <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome_name, sep = '')
  sub_outcome <- outcome[outcome$State == state,]
  sub_outcome[[x]] <- as.numeric(sub_outcome[[x]])
  #sub_outcome <-sub_outcome[!is.na(sub_outcome[[x]]),]
  #sub_outcome<- sub_outcome[na.omit(sub_outcome[[x]]),]
  max_death <- max(sub_outcome[[x]])
  sub_outcome <-sub_outcome[sub_outcome[[x]] == max_death,]
  sub_outcome <- unique.data.frame(sub_outcome)
  if(nrow(sub_outcome) == 1){
    as.character(sub_outcome$Hospital.Name[[1]])
  }else if(nrow(sub_outcome) > 1){
    sub_outcome <- sub_outcome[order(sub_outcome$Hospital.Name),]
    as.character(sub_outcome$Hospital.Name[[1]])
  }
}

rankhospital <- function(state, outcome_name, num = "best"){
  outcome <- read.csv("outcome-of-care-measures.csv")
  outcome_types <- c('heart attack', 'heart failure', 'pneumonia')
  
  if(!state %in% state.abb){
    stop("invalid state")
  }
  if(!outcome_name %in% outcome_types){
    stop("invalid outcome")
  }
  
  if(num == "best"){
    best(state, outcome_name)
  }else if (num =="worst"){
    worst(state, outcome_name)
  }else{
    foutcome_name <- strsplit(outcome_name, " ")[[1]]
    if(length(strsplit(foutcome_name, " ")) > 1){
      temp_name <- paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
      temp_name <- paste(temp_name, paste(toupper(substr(foutcome_name[[2]],1,1)), substr(foutcome_name[[2]],2,nchar(foutcome_name[[2]])), sep = ''), sep = ".")
      outcome_name <- temp_name  
    }else if (length(strsplit(foutcome_name, " ")) == 1){
      outcome_name = paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
    }
    x <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome_name, sep = '')
    sub_outcome <- outcome[outcome$State == state,]
    sub_outcome[[x]] <- as.numeric(sub_outcome[[x]])
    max_death <- max(sub_outcome[[x]])
    sub_outcome <- unique.data.frame(sub_outcome)
    sub_outcome <- sub_outcome[order(sub_outcome[[x]]),]
    as.character(sub_outcome$Hospital.Name[num])
  }
}

rankall <- function(outcome_name, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv")
  outcome_types <- c('heart attack', 'heart failure', 'pneumonia')
  if(!outcome_name %in% outcome_types){
    stop("invalid outcome")
  }
  foutcome_name <- strsplit(outcome_name, " ")[[1]]
  if(length(strsplit(foutcome_name, " ")) > 1){
    temp_name <- paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
    temp_name <- paste(temp_name, paste(toupper(substr(foutcome_name[[2]],1,1)), substr(foutcome_name[[2]],2,nchar(foutcome_name[[2]])), sep = ''), sep = ".")
    outcome_name <- temp_name  
  }else if (length(strsplit(foutcome_name, " ")) == 1){
    outcome_name = paste(toupper(substr(foutcome_name[1],1,1)), substr(foutcome_name[[1]],2, nchar(foutcome_name[[1]])), sep ='')
  }
  x <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome_name, sep = '')
  if(num == 'best'){num <- 1}
  if(num == "worst"){
    sub_outcome[[x]] <- as.numeric(sub_outcome[[x]])
    max_death <- max(sub_outcome[[x]])
    sub_outcome <-sub_outcome[sub_outcome[[x]] == max_death,]
    num = nrow(sub_outcome)
  }
  sub_outcome <- outcome
  sub_outcome[[x]] <- as.numeric(sub_outcome[[x]])
  sub_outcome <- unique.data.frame(sub_outcome)
  sub_outcome <- sub_outcome[order(sub_outcome[[x]]),]
  as.character(sub_outcome$Hospital.Name[num])
}




