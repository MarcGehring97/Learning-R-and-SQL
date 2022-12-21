setwd("/Users/Marc/Desktop/Goals/CS & DS/R Programming")

complete <- function(directory, id = 1:332) {
	ids <- vector(mode = "numeric", length = length(id))
	nobs <- vector(mode = "numeric", length = length(id))
	counter <- 1
	for (id1 in id) {
		id2 <- id1
		if (floor(log10(id1)) != 2) {
			for (i in 0:(1-floor(log10(id1)))) {
				id2 <- paste("0", as.name(id2), sep = "")
			}
		}
		id2 <- as.name(id2)
		data <- read.csv(paste(directory, "/", as.name(id2), ".csv", sep = ""))
		nas <- 0
		for (i in 1:nrow(data)) {
			if (is.na(data[i,2]) == FALSE & is.na(data[i,3]) == FALSE) {
				nas <- nas + 1
				}
			}
		ids[counter] <- id1
		nobs[counter] <- nas
		counter <- counter + 1
		}
	data.frame(ids, nobs)
}
complete("specdata", 1)