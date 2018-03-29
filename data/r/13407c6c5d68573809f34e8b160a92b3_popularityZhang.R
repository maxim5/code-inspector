library(ggplot2)
library(methods)

# variables

# where are the csv files located?
targetFolder = "./eval-output/"
chartTitleSize = 20

# functions

# draw a popularity chart
drawPopularityPlot  = function(popData, chartTitle) {

	chart <- ggplot(data = popData, aes(x = Bin, y = Popularity)) + opts(title = paste("Recommendation popularity of ",chartTitle), plot.title=theme_text(size=chartTitleSize))

	# bars
	# If you have presummarised data, use stat="identity" to turn off the default summary
	return (chart + geom_bar(stat="identity", binwidth = 1))

	# line
#	return (chart + geom_line())

	#bars + line
#	return (chart + geom_bar(stat="identity", binwidth = 1) + geom_line())
}


# locate the files
files <- list.files(targetFolder, pattern = "popularity-(.*)-([0-9]+).csv")

files.splitted <- strsplit(files, split = c("-"));

# list of algorithms and matching csv files
algorithms <- list();

if (length(files.splitted) > 1){
	for (i in 1:length(files)) {
			# add files[i] to the list of matching algorithm
			algorithmName = unlist(files.splitted[i])[2]

			currentFile <- paste(targetFolder, files[i], sep="")
			if (length(algorithms[[algorithmName]]) == 0) {
				algorithms[[algorithmName]] <- currentFile
			} else {
				algorithms[[algorithmName]] <- c(algorithms[[algorithmName]], currentFile)
			}

	}
}

data = list()
for (algorithm in names(algorithms)) {
	# read the files into a list of data.frames
	data[[algorithm]] <- lapply(algorithms[[algorithm]], read.csv)

	# concatenate into one big data.frame
	data[[algorithm]] <- do.call(rbind, data[[algorithm]])

	# aggregate
	data[[algorithm]] <- aggregate(Popularity ~ Bin, data[[algorithm]], mean)
	
	pdf(paste(targetFolder,"popularity-",algorithm,".pdf", sep=""), paper="a4r", width=0, height=0)
	print(drawPopularityPlot(data[[algorithm]], algorithm))
	dev.off()
}