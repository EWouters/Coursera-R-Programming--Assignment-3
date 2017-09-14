best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate

    #Make list of possible values of outcome and their index
    possible.outcomes <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcome.col <- possible.outcomes[[outcome]]

    #Stop if outcome was not in possible.outcomes
    if (is.null(outcome.col))
        stop("invalid outcome")

    #Read the csv
    hospital.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #Convert the desired column to numeric
    hospital.df[, outcome.col] <- suppressWarnings(sapply(hospital.df[, outcome.col], as.numeric))

    #Stop if state was not a possible value of states
    if (!state %in% unique(hospital.df[,7]))
        stop("invalid state")

    #hospital.df[[which.min(as.numeric(hospital.df[hospital.df$State == state, outcome.col])),2]]

    #Find best outcome
    outcome.min <- min(hospital.df[hospital.df$State == state, outcome.col], na.rm = T)

    #Get list of names of best hospitals in state for outcome
    best.list <- hospital.df[hospital.df$State == state &
                                 (hospital.df[, outcome.col] == outcome.min) &
                                 !is.na(hospital.df[, outcome.col]), 2]

    #best.list <- subset(hospital.df, State == state & names(outcomes[outcome]) == outcome.min, 2)

    #sort alphabetically and return first element
    sort(best.list)[1]
}
