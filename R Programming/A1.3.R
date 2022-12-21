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

corr <- function(directory, threshold = 0) {
	out <- vector(mode = "numeric")
	for (i in 1:332) {
		if (complete(directory, i)[2] > threshold) {
			j <- i
			if (floor(log10(i)) != 2) {
				for (k in 0:(1-floor(log10(i)))) {
					j <- paste("0", as.name(j), sep = "")
				}	
			}
			j <- as.name(j)
			data <- read.csv(paste(directory, "/", as.name(j), ".csv", sep = ""))
			counter <- 1
			list1 <- vector(mode = "numeric")
			list2 <- vector(mode = "numeric")
			for (l in 1:nrow(data)) {
				if (is.na(data[l, 2]) == FALSE & is.na(data[l, 3]) == FALSE) {
					list1[counter] <- data[l, 2]
					list2[counter] <- data[l, 3]
					counter <- counter + 1
				}
			}
			out <- c(out, cor(list1, list2))
		}
	}
	out
}
corr("specdata", 150)
