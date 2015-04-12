library(Hmisc)

dirName = "/Users/krusheel/Projects/Hackathon/json_format/json"
fileList = list.files(dirName, pattern = "*.json", full.names = TRUE)

numFiles = 500 

sampleFiles = sample(fileList, numFiles, replace = FALSE)
fileData <- data.frame()
for(i in 1:numFiles) {
        fileName = sampleFiles[i]
        print(paste(fileName," file : ", i, sep = " "))
        fileData <- rbind(fileData, readDataFromJson(fileName))
        print(nrow(fileData))
}
 
overall <- "overall_rt"
fileData <- clean(fileData, overall)
print(nrow(fileData))

### Correlation between over all rating and different individual parameters
srcdims = c("service_rt", "cleanliness_rt", "value_rt",
     "sleepquality_rt", "rooms_rt", "location_rt", "business_rt", "frontdesk_rt")

corrData <- list()
for(dim in srcdims) {        
        indices = (fileData[,overall] == 1) | (fileData[,overall] == 2) | (fileData[,overall] == 3)
        corrData[paste(dim, "1 2 3", sep = " ")] = cor(fileData[indices, dim],fileData[indices,overall],use = "pairwise.complete.obs", method = "spearman")
        
        indices = (fileData[,overall] == 3) | (fileData[,overall] == 4) | (fileData[,overall] == 5)
        corrData[paste(dim, "3 4 5", sep = " ")] = cor(fileData[indices, dim],fileData[indices,overall],use = "pairwise.complete.obs", method = "spearman")
}