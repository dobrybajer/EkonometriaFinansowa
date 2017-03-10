WigData <- read.csv(file="H:/Windows7/Documents/wig_d.csv", header=TRUE, sep=",")
DaxData <- read.csv(file="H:/Windows7/Documents/dax_d.csv", header=TRUE, sep=",")
CacData <- read.csv(file="H:/Windows7/Documents/cac_d.csv", header=TRUE, sep=",")
FtmData <- read.csv(file="H:/Windows7/Documents/ftm_d.csv", header=TRUE, sep=",")

MyDataWig <- WigData[,c(1,5)]
MyDataDax <- DaxData[,c(1,5)]
MyDataCac <- CacData[,c(1,5)]
MyDataFtm <- FtmData[,c(1,5)]

mergedIndices <- merge(MyDataWig, MyDataDax, by.x="Data", by.y="Data", all=TRUE)
colnames(mergedIndices) <- c("Data", "WIG", "DAX")
mergedIndices <- merge(mergedIndices, MyDataCac, by.x="Data", by.y="Data", all=TRUE)
colnames(mergedIndices) <- c("Data", "WIG", "DAX", "CAC")
mergedIndices <- merge(mergedIndices, MyDataFtm, by.x="Data", by.y="Data", all=TRUE)
colnames(mergedIndices) <- c("Data", "WIG", "DAX", "CAC", "FTSE")

