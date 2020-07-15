## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.



rankhospital <- function(state, outcome, num = "best") {
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
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    subset_df <- df[df[,7]==state, c(7, 2, outcomes[outcome])]
    names(subset_df) <- c('state','hospital', 'outcome')
    
    subset_df$outcome <- as.numeric(subset_df$outcome, na.rm = TRUE)
    subset_df <- subset_df[complete.cases(subset_df),]
    subset_df <- subset_df[order(subset_df$outcome, subset_df$hospital ),]
    
    if (num == 'best'){
      subset_df[1, 2]
    } else if (num == 'worst'){
      subset_df[nrow(subset_df), 2]
    } else {
      subset_df[num, 2]
    }
  }
}