### Data Parsing

library(data.table)
library(jsonlite)

### Read data from Json file
readDataFromJson <- function(file = fileName) {                
        
        currNames = c("Reviews.AuthorLocation", "Reviews.Title", "Reviews.Author", "Reviews.ReviewID",
                      "Reviews.Content", "Reviews.Date", "HotelInfo.Name", "HotelInfo.HotelURL", "HotelInfo.Price", 
                      "HotelInfo.Address" ,"HotelInfo.HotelID", "HotelInfo.ImgURL", "Service", "Cleanliness", 
                      "Overall", "Value", "Sleep Quality", "Rooms", "Location", "Business service", 
                      "Check in / front desk")
        
        newNames = c("authlocation_rv", "title_rv", "author_rv", "reviewid_rv", "reviewcontent_rv", "date_rv", "name_hot", 
                     "url_hot", "price_hot", "address_hot", "hot_id", "imgurl_hot", "service_rt", "cleanliness_rt", "overall_rt",
                     "value_rt", "sleepquality_rt", "rooms_rt", "location_rt", "business_rt", "frontdesk_rt")
        
        reqNames = c("service_rt", "cleanliness_rt", "overall_rt", "value_rt", "sleepquality_rt", "rooms_rt", "location_rt", "business_rt", "frontdesk_rt")
        
        completeData <- data.frame(newNames)
        if(file.exists(fileName)) {
                jsonData <- fromJSON(fileName)
                if(length(jsonData$Reviews) != 0) {
                        jDataFrame <- as.data.frame(jsonData)
                }
                else {
                        emptyFrame <- rep(NA, length(newNames))
                        tm <- matrix(emptyFrame, nrow=1, byrow=T)
                        colnames(tm) <- newNames
                        return(as.data.frame(tm))
                }
                nonRatingsData <- jDataFrame[,!(names(jDataFrame) %in% c("Reviews.Ratings"))]                
                completeData <- cbind(nonRatingsData,jDataFrame$Reviews.Ratings)   
                completeData <- renameColumns(completeData, currNames, newNames)
                for(dim in reqNames) {
                        if(dim %in% names(completeData)) {
                                completeData[,dim] <- as.numeric(completeData[,dim])
                                completeData[(completeData[,dim] == -1) && (!is.na(completeData[,dim])), dim] <- NA  
                        }                        
                }
        }
        else {
                print(paste(fileName,"doesn't exist", sep = " "))
        }
        completeData
}

###  Change a numeric value to NA
changetoNA <- function(colnum,df, value) {
        col <- df[,colnum]
        if (is.numeric(col)) {  #edit: verifying column is numeric
                col[col == value & is.numeric(col)] <- NA
        }
        return(col)
}

### Change names
renameColumns <- function(completeData, currNames, newNames) {
        this.Names = names(completeData)
        for(name in this.Names) {                
                if(substr(name,1,8) == "Business") {
                        names(completeData)[names(completeData) == name] = "business_rt"
                }
                else {
                        names(completeData)[names(completeData) == name] = newNames[currNames == name]
                }                
        }
        
        colNotPresent <- setdiff(currNames, this.Names)
        for(name in colNotPresent) {
                if(substr(name,1,8) == "Business") {
                        if(!("business_rt" %in% names(completeData)))
                                completeData[,"business_rt"] = NA
                }
                else {
                        completeData[,newNames[currNames == name]] = NA
                }
        }
        completeData
}

### Clean for overall rating
clean <- function(data, dim) {
        data[complete.cases(data[,dim]),]
}