rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  #read in the desired data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #create a list of states and initialize a character array to hold the
  #required hospital names
  states <- levels(factor(data[, 7]))
  hospital <- vector(mode="character") 
  
  for (i in seq(states)) {
    ## hospital[i] <- rankhospital(state[i], outcome, num)
    state<-states[i]
    
    ######################
    
    ## Read outcome data
    
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if ((outcome %in% outcomes) == FALSE) {
      stop(print("invalid outcome"))
    }
    
    #get the subset of the data with the desired state
    new_data <- subset(data, State == state)
    
    #get the desired outcome column from the data file
    if (outcome == "heart attack") {
      outcome_column <- 11
    }
    else if (outcome == "heart failure") {
      outcome_column <- 17
    }
    else {
      outcome_column <- 23
    }
    
    #if num is greater that the number of hospitals in the desired state,
    # return NA
    if (is.numeric(num) == TRUE) {
      if (length(data[,2]) < num) {
        return(NA)
      }
    }
    
    #get rid of the NA's in the desired outcome column
    new_data[, outcome_column] <- as.numeric(new_data[,outcome_column])
    bad <- is.na(new_data[, outcome_column])
    desired_data <- new_data[!bad, ]
    
    #arrange the modified dataframe in ascending order of the outcome values
    outcome_column_name <- names(desired_data)[outcome_column]
    hospital_column_name <- names(desired_data)[2]
    index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
    ordered_desired_data <- desired_data[index, ]
    
    
    #return the hospital name with the outcome ranking of num
    #ordered_desired_data[num, 2]
    hospital[i] <- ordered_desired_data[, 2]
    
    
    ###################
  }
  data.frame(hospital, states)
}