## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function returns a 2-column data frame containing the hospital in each state that has the ranking specified in num.


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = 'Not Available')
  
  ## Check that state and outcome are valid
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  if (!is.element(outcome, names(outcomes))){
    stop ('invalid outcome')
  }
  ## For each state, find the hospital of the given rank
  rankstate <- function(df, n){
    if (n == 'best'){
      df[1, 2]
    } else if (n == 'worst'){
      df[nrow(df), 2]
    } else {
      df[n, 2]
    }
  }
  
  subset_df <- df[, c(7, 2, outcomes[outcome])]
  names(subset_df) <- c('state','hospital', 'outcome')
  
  subset_df$outcome <- as.numeric(subset_df$outcome, na.rm = TRUE)
  subset_df <- subset_df[complete.cases(subset_df),]
  subset_df <- subset_df[order(subset_df$state, subset_df$outcome, subset_df$hospital ),]
  
  result <- lapply(split(subset_df, subset_df$state), rankstate, num)
  unlisted_values <- unlist(result)
  list_names <- names(result)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result_df <- data.frame(hospital=unlisted_values, state=list_names, row.names=list_names)
  result_df
}
