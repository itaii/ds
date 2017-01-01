getFileContent <- function(directory, id)
{
	## condition(id >= 1 && id <=332)
	
	fileprefix <- "";
	
	if (id >=1 && id <= 9)
	{
		fileprefix <- "00"
	}
	else if (id >=10 && id <= 99)
	{
		fileprefix <- "0"
	}
	
	filename <- paste(directory, fileprefix, id, ".csv", sep="");
	
	contents <- read.csv(filename);
	
	contents
}

getFilesContent <- function(directory, id)
{
	content <- getFileContent(directory, id[1])
	
	for (i in 2:length(id))
	{
		newFileNumber = id[i];
		
		newContent = getFileContent(directory, newFileNumber)
		
		content = rbind(content, newContent)
	}
	
	content
}

pollutantMean <- function(directory, pollutant, id = 1:332)
{
	contents = getFilesContent(directory, id)
	
	values = contents[!is.na(contents[,pollutant]),pollutant]
	
	mean(values)
}

complete <- function(directory, id = 1:332)
{
	result <- matrix(NaN,nrow=0,ncol=2)
	
	for (currentFileID in id)
	{
		content <- getFileContent(directory, currentFileID)

		content = content[!is.na(content[,"sulfate"]),]
		contentAfterSulfate <- nrow(content)

		content = content[!is.na(content[,"nitrate"]),]
		contentAfterNitrate <- nrow(content)

#		if (contentAfterSulfate != contentAfterNitrate)
#		{
#			print(paste("file", currentFileID, ": counts ",contentAfterSulfate, "<>",contentAfterNitrate))	
#		}
		
		result <- rbind(result, c(currentFileID, nrow(content)))
	}
	
	colnames(result) <- c("id", "nobs")

	result
}

corr <- function(directory, threshold)
{
	result <- c()
	
	completeData <- complete(directory)
	
	for (i in 1:nrow(completeData))
	{
		if (completeData[i, "nobs"] > threshold)
		{
			content <- getFileContent(directory, i)
			
			cv <- cor(content[,"sulfate"], content[,"nitrate"], use="pairwise.complete.obs")
			
			result <- c(result, cv)
		}
	}
	
	result
}

drawPlot <- function()
{
	set.seed(15)
	x <- rnorm(100)
	e <- rnorm(100,0,2)
	y <- 0.5 + 2*x + e
	plot(x,y)
}