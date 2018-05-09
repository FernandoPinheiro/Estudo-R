pollutantmean <- function(directory, pollutant, id = 1:332){
	path <- file.path(getwd(), directory)
	vpollutant <- c()
	
	for(sensor in id){
		f <- paste(formatC(sensor, width=3, flag="0"), ".csv", sep="")
		file <- file.path(path, f)
		data <- read.csv(file)[ , c(pollutant)]
		vpollutant <- c(vpollutant, data)
	}
	
	isNA <- is.na(vpollutant)
	values <- vpollutant[!isNA]
	return(mean(values))
}

complete <- function(directory, id = 1:332){
  path <- file.path(getwd(), directory)
  
  result <- data.frame()

  for(sensor in id){
    f <- paste(formatC(sensor, width=3, flag="0"), ".csv", sep="")
    file <- file.path(path, f)
    data <- read.csv(file)
    nobs <- nrow(data[complete.cases(data), ])
    
    df <- data.frame(sensor, nobs)
    names(df) <- c("id", "nobs")
    
    result <- rbind(result, df)
  }
  
  return(result)
}
