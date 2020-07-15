## Write a function called best that take two arguments: 
## the 2-character abbreviated name of a state and an outcome name. 
##
## The function  returns a character vector with the name of the hospital 
## that has the best (i.e. lowest) 30-day mortality for the specified
## outcome in that state.



best <- function(state, outcome){
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = 'Not Available')
  
  ## Check that state and outcome are valid
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  states <- df[, 7]
  if (!is.element(state, states)){
    stop ('invalid state')
  }else if (!is.element(outcome, names(outcomes))){
    stop ('invalid outcome')
  }else{
    
    
    ## Return hospital name in that state with lowest 30-day death
    subset_df <- df[df[,7]==state, c(7, 2, outcomes[outcome])]
    names(subset_df) <- c('state','hospital', 'outcome')
    
    subset_df$outcome <- as.numeric(subset_df$outcome, na.rm = TRUE)
    subset_df <- subset_df[complete.cases(subset_df),]
    
    subset_df[subset_df$outcome == min(subset_df$outcome),]$hospital
  }
}