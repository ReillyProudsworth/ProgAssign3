## This function reads in data from outcome-of-care-measures.csv, checks that 
## the function arguments are valid, and returns the hospital in the specified 
## state with the best 30-day mortality rate for the specified outcome.
best <- function(state, outcome) {
        ## Read outcome data
        medData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        stateCheck <- (medData$State == state)
        outcomeCheck <- (outcome == c("heart attack", "heart failure", "pneumonia"))
        if(any(stateCheck) == FALSE) {
                stop("invalid state")
        } else if(any(outcomeCheck) == FALSE) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        stateData <- medData[medData$State == state,]
        if(outcome == "heart attack") {
                stateData <- stateData[stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
                stateData[, 11] <- as.numeric(as.character(stateData[, 11]))
                stateData <- stateData[stateData[, 11] == min(stateData[, 11]),]
        } else if(outcome == "heart failure") {
                stateData <- stateData[stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]
                stateData[, 17] <- as.numeric(as.character(stateData[, 17]))
                stateData <- stateData[stateData[, 17] == min(stateData[, 17]),]
        } else {
                stateData <- stateData[stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",]
                stateData[, 23] <- as.numeric(as.character(stateData[, 23]))
                stateData <- stateData[stateData[, 23] == min(stateData[, 23]),]
        }
        
        stateData <- stateData[order(stateData$Hospital.Name),]
        as.character(stateData[[1,2]])
}