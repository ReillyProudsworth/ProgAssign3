rankall <- function(outcome, num = "best") {
        ## Read outcome data
        medData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome are valid
        outcomeCheck <- (c("heart attack", "heart failure", "pneumonia") == outcome)
        if(any(outcomeCheck) == FALSE) {
                stop("invalid outcome")
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        if(outcome == "heart attack") {
                rankData <- medData[medData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
                rankData[, 11] <- as.numeric(as.character(rankData[, 11]))
                rankData <- rankData[order(rankData$State, rankData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, rankData$Hospital.Name),]
        } else if(outcome == "heart failure") {
                rankData <- medData[medData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]
                rankData[, 17] <- as.numeric(as.character(rankData[, 17]))
                rankData <- rankData[order(rankData$State, rankData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, rankData$Hospital.Name),]
        } else {
                rankData <- medData[medData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",]
                rankData[, 23] <- as.numeric(as.character(rankData[, 23]))
                rankData <- rankData[order(rankData$State, rankData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, rankData$Hospital.Name),]
                
        }
        
        stateVect <- levels(as.factor(rankData$State))
        storeData <- rankData
        hosp <- c()
        st <- c()
        for(i in stateVect) {
                rankData <- storeData[storeData$State == i,]
                if(num == "best" | num == 1) {
                        hosp <- c(hosp, rankData[1, 2])
                        st <- c(st, i)
                } else if(num == "worst" | num == length(rankData[, 11])) {
                        hosp <- c(hosp, rankData[length(rankData[, 11]), 2])
                        st <- c(st, i)
                } else if(num > 1 | num < length(rankData[, 11])) {
                        hosp <- c(hosp, rankData[num, 2])
                        st <- c(st, i)
                } else {
                        hosp <- c(hosp, "<NA>")
                        st <- c(st, i)
                }
        }
        
        data.frame(hospital = hosp, state = st)
}