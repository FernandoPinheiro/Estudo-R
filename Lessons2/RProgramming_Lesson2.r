pollutantmean <- function(directory, pollutant, id = 1:332){
	path <- file.path(getwd(), directory)
	vpollutant <- c()
	
	for(sensor in id){
		f <- paste(formatC(sensor, width=3, flag="0"), ".csv", sep="")
		file <- file.path(path, f)
		data <- read.csv(file)
		vpollutant <- c(vpollutant, data[pollutant])
	}
	
	return(is.na(vpollutant))
	## values <- vpollutant[!vNA]
}