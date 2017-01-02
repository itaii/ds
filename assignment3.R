partOne <- function()
{
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	head(outcome)
	
	outcome[, 11] <- as.numeric(outcome[, 11])
	## You may get a warning about NAs being introduced; that is okay
	hist(outcome[, 11])
}

best <- function(state, outcome)
{
	contents <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	#print(paste("after reading file, rows left: ", nrow(contents)))
	
	# validate state + outcome
	
	# filter records only for the specified state
	contents <- contents[contents[,7]==state,]
	
	if (nrow(contents) == 0) stop("invalid state")
	
	#print(paste("after filtering state, rows left: ", nrow(contents)))
	
	colIndex <- 0L
	
	if (outcome == "heart attack") { colIndex[1] <- 11 }
	else if (outcome == "heart failure") { colIndex[1] <- 17 }
	else if (outcome == "pneumonia") { colIndex[1] <- 23 }
	else stop("invalid outcome")
	
	contents[, colIndex] <- as.numeric(contents[, colIndex])
	
	#print(paste("colIndex is", colIndex))
	
	minValue <- min(contents[,colIndex], na.rm = TRUE)
	
	#print(paste("min is", minValue))
	
	# filter contents to remove NA
	contents <- contents[!is.na(contents[,colIndex]),]
	
	contents <- contents[contents[,colIndex] == minValue,]
	
	#print(paste("after filtering min, rows left: ", nrow(contents)))
	
	# keep only names, sorted 
	winnerName <- min(contents[,2])
	
	winnerName
}

rankHospital <- function(state, outcome, num = "best")
{
	contents <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#print(paste("after reading file, rows left: ", nrow(contents)))
	
	# validate state + outcome
	
	# filter records only for the specified state
	contents <- contents[contents[,7]==state,]
	
	if (nrow(contents) == 0) stop("invalid state")
	
	#print(paste("after filtering state, rows left: ", nrow(contents)))
	
	colIndex <- 0L
	
	if (outcome == "heart attack") { colIndex[1] <- 11 }
	else if (outcome == "heart failure") { colIndex[1] <- 17 }
	else if (outcome == "pneumonia") { colIndex[1] <- 23 }
	else stop("invalid outcome")
	
	contents[, colIndex] <- as.numeric(contents[, colIndex])
	
	# filter contents to remove NA
	contents <- contents[!is.na(contents[,colIndex]),]
	
	#print(paste("colIndex is", colIndex))
	
	sortIndex <- order(contents[, colIndex],contents[, 2])
	
	# sort rates from lowest, with secondary importance to name
	contents <- contents[sortIndex,]
	
	if (num == "best") num <- 1
	else if (num == "worst") num <- nrow(contents)
	else if (num > nrow(contents)) return(NA)
	
	contents[num,2]
}

rankall <- function(outcome, num = "best")
{
	contents <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#print(paste("after reading file, rows left: ", nrow(contents)))

	colIndex <- 0L
		
	if (outcome == "heart attack") { colIndex[1] <- 11 }
	else if (outcome == "heart failure") { colIndex[1] <- 17 }
	else if (outcome == "pneumonia") { colIndex[1] <- 23 }
	else stop("invalid outcome")
	
	contents[, colIndex] <- as.numeric(contents[, colIndex])
	
	result <- data.frame( "hospital" = character(), "state" = character(), stringsAsFactors=FALSE)
	#result <- data.frame(col.names = c("hospital","state"))
	
	for (state in levels(factor(contents[,7])))
	{
		stateContents <- contents[contents[,7]==state,]
		
		# filter contents to remove NA
		stateContents <- stateContents[!is.na(stateContents[,colIndex]),]
		
		#print(paste("colIndex is", colIndex))
		
		sortIndex <- order(stateContents[, colIndex],stateContents[, 2])
		
		# sort rates from lowest, with secondary importance to name
		stateContents <- stateContents[sortIndex,]
		
		if (num == "best") num <- 1
		else if (num == "worst") num <- nrow(stateContents)
		
		if (num > nrow(stateContents))
			result <- rbind(result, data.frame("hospital"=NA, "state"=state))
		else
			result <- rbind(result, data.frame("hospital"=stateContents[num,2], "state"=state))
	}
	
	result
}