rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        medData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
        ## Check that state and outcome are valid
        stateCheck <- (medData$State == state)
        outcomeCheck <- (c("heart attack", "heart failure", "pneumonia") == outcome)
        if(any(stateCheck) == FALSE) {
                stop("invalid state")
        } else if(any(outcomeCheck) == FALSE) {
                stop("invalid outcome")
        }
  
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        stateData <- medData[medData$State == state,]
        if(outcome == "heart attack") {
                stateData <- stateData[stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
                stateData[, 11] <- as.numeric(stateData[, 11])
                stateData <- order(stateData[, 11], stateData[, 2])
        }
        print(stateData)
        if(num > length(stateData) & num != "worst") {
                return(NA)
        } else if(num == "best" | num == 1) {
                stateData[1, 2]
        } else if(num > 1 | num < length(stateData)) {
                stateData[num, 2]
        } else {
                stateData[length(stateData), 2]
        }
}