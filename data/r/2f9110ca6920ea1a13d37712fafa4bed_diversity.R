library(ggplot2)
library(methods)

# variables

# where are the csv files located?
targetFolder = "./"
chartTitleSize = 20

# functions

# draw a popularity chart
drawDiversityPlot= function(divData, chartTitle) {
chart <- qplot(G,L, data = divData, xlim = c(0,1), ylim = c(0,1), main = chartTitle, xlab = "Hits Proportion per Product", ylab ="Hits") + geom_abline(stat="abline", position="identity", color="blue")

#	chart <- ggplot(data = divData, aes(x = Bin, y = Popularity)) + opts(title = paste("diversity of ",chartTitle), plot.title=theme_text(size=chartTitleSize))

	# bars
	# If you have presummarised data, use stat="identity" to turn off the default summary
#	return (chart + geom_bar(stat="identity", binwidth = 1))

	# line
#	return (chart + geom_line())

	#bars + line
#	return (chart + geom_bar(stat="identity", binwidth = 1) + geom_line())
}


aggregateFiles <- function(filePattern){
  
  # locate the files
  files <- list.files(targetFolder, pattern = filePattern)
  
  files.splitted <- strsplit(files, split = c("-"));
  
  # list of algorithms and matching csv files
  algorithms <- list();
  
  if (length(files.splitted) > 1){
  	for (i in 1:length(files)) {
  			# add files[i] to the list of matching algorithm
  	    metricType = paste(unlist(files.splitted[i])[1],"-",unlist(files.splitted[i])[2],sep="")
  			algorithmName = unlist(files.splitted[i])[3]
  			datasetName = unlist(files.splitted[i])[4]
  
  			currentFile <- paste(targetFolder, files[i], sep="")
        
        arrayIndex = paste(metricType,"-",algorithmName,"-",datasetName,sep="")
        
  			if (length(algorithms[[arrayIndex]]) == 0) {
  				algorithms[[arrayIndex]] <- currentFile
  			} else {
  				algorithms[[arrayIndex]] <- c(algorithms[[arrayIndex]], currentFile)
  			}
  
  	}
  }
  
  
  print(algorithms)
  
  data = list()
  for (algorithm in names(algorithms)) {
  	# read the files into a list of data.frames
  	data[[algorithm]] <- lapply(algorithms[[algorithm]], read.csv)
  
  	# concatenate into one big data.frame
  	data[[algorithm]] <- do.call(rbind, data[[algorithm]])
  
  	# aggregate
  	data[[algorithm]] <- aggregate(. ~ Index, data[[algorithm]], mean)
    write.csv2(data[[algorithm]], paste(algorithm,".csv", sep=""))
  	
  	pdf(paste(targetFolder,algorithm,".pdf", sep=""), paper="a4r", width=0, height=0)
  	  print(drawDiversityPlot(data[[algorithm]], algorithm))
  	dev.off()
  }

}

aggregateFiles("diversity-HITS-(.*)-(.*)-([0-9]+).csv")
aggregateFiles("diversity-POPULARITY-(.*)-(.*)-([0-9]+).csv")