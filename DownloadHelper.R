
# data url - Electric power consumption for one house over 3 years from the UC Irvine Machine Learning Repo
zipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

dataDir <- "./data/"
#dataSetDir <- paste(dataDir, "UCI HAR Dataset", sep="")

# prep: download file and store in data dir
# returns the scoped name of the zip file to use for extraction
downloadFile <- function(zipURL, dataDir) {
        if (!file.exists(dataDir)) {
                dir.create(dataDir)
        }
        zipFileName <- "Dataset.zip"
        # always download fresh set and capture data
        dateDownloaded <- format(Sys.time(), "%y-%m-%d.%H-%M-%S")
        fileNameResult <- paste(paste(dataDir, dateDownloaded, sep=""), zipFileName, sep="-")
        download.file(zipURL, destfile=fileNameResult, method="curl") 
        fileNameResult
}

downloadAndUnzipData <- function() {
        zipFile <- downloadFile(zipURL, dataDir)
        unzip(zipFile, exdir = ".")
}

