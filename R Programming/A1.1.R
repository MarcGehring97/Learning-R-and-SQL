setwd("/Users/Marc/Desktop/Goals/CS & DS/R Programming")

pollutantmean <- function(directory, pollutant, id = 1:332) {
	list <- vector(mode = "numeric")
	for (id1 in id) {
		id2 <- id1
		if (floor(log10(id1)) != 2) {
			for (i in 0:(1-floor(log10(id1)))) {
				id2 <- paste("0", as.name(id2), sep = "")
			}
	}
	id2 <- as.name(id2)
		
	data <- read.csv(paste(directory, "/", as.name(id2), ".csv", 	sep = ""))
	i <- 3
	if (pollutant == "sulfate") {
		i <- 2
	}
	column <- data[[i]]
	bad <- is.na(column)
	column[!bad]
	list <- c(column, list)
	}
	mean(list, na.rm = TRUE)
}
pollutantmean("specdata","sulfate",23)