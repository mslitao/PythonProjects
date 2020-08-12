source("TSDecomposition.R")

getCategory = function(volume, growth) {
  category<-""
  
  if(is.infinite(growth) | is.infinite(volume) | is.na(volume) | is.na(growth)){
    return(category)
  }

  if (volume >= 50 & volume <= 500 & growth >= 0.3){
    category = "EarlyRiser"
  }
  else if (volume >= 500 & volume < 5000 & growth >= 0.1){
    category = "Contemporary"
  }
  else if (volume >= 5000){
    category = "Mature"
  }
  else if (volume > 500 & growth < -0.1){
    category = "Declining"
  }
  
  return(category)
}

Category <- function(x, volumeColumn, growthColumn){
  #print(x)
  #print(as.double(x[volumeColumn]))
  #print(as.double(x[growthColumn]))
  category <-getCategory(as.double(x[volumeColumn]),as.double(x[growthColumn]))
  #print(category)
  return (category)
}

Slope = function(x, y){
  n = length(x)
  #y=n*y/max(y) #1-24
  #plot(x,y)
  a =0
  b =0
  for (i in 1:n) {
    a = a+ (x[i]-mean(x))*(y[i]-mean(y))
    b = b+ (x[i]-mean(x))*(x[i]-mean(x))
  }
  return(a/b);
}

ComputeSlope <- function(data) {
  n = length(data)
  x <- seq(1, n, by = 1)
  y = n*data/max(data) #1-24
  return(Slope(x,y))
}

ComputeSlopeGrowth<-function(data){
  n = length(data)
  x <- seq(1, n, by = 1)
  y <- data
  #y <- n*data/max(data) #1-24
  #plot(x,y)
  
  slope <- Slope(x,y)
  base <- mean(y)-slope*mean(x)
  
  error <- 0
  for (i in 1:n) {
    error <- error + abs(slope*x[i]+base - y[i])/y[i]*100
  }
  error <- error/n
  
  growth = slope*(x[n]-x[1])/(y[1]+1)
  
  growth2 = growth+1
  if(growth2<0)
  {
    growth2 =0
  }
  growthYoY = abs((growth2)^(1/n))^12 -1
  #avgVolume <- mean(data)
  
  return(c(slope=slope, growth = growth, growthYoY = growthYoY, mape = error))
}


PopulateCategory <-function(data){
  n = length(data)
  
  status <- ""
  for (i in 1:n) {
    if(data[i]==""){
      data[i] <-status
    }
    else if(data[i]!=status){
      status <- data[i]
    }
  }
  
  return(data)
}

ComputeMAPE<- function(x, baseColumn, evaluateColumn){
  base <- as.double(x[baseColumn])
  data <- as.double(x[evaluateColumn])
  
  n = length(data)
  error <- 0
  for (i in 1:n) {
    error <- error + abs(data[i] - base[i])/base[i]*100
  }
  error <- error/n
  
  return(error)
}

LinearWeightAgg <- function(data){
  data <- data[data!=0]
  n <- length(data)
  result <-0
  for (i in 1:n) {
    weight <- 2 * i/n /(n+1)
    result = result+ weight * (data[i]-0.3)
  }
  result = result + data[i]
  return (result)
}
  

inputData <- read.table("<INPUTDATA>", header = T, sep = "\t")
inputData$Date <- as.Date(inputData$Date, format = "%m/%d/%Y")

entities <- as.list(unique(inputData$Entity))

startDate <- as.Date("2013-01-01")
endDate <- as.Date("2017-10-01")
length <- floor(as.numeric(difftime(endDate, startDate), units = "days")/30)+1
newData = NULL
newRow = NULL
for (entity in entities) {
  
    print(as.character(entity))
    tsData = inputData[inputData$Entity == as.character(entity),]
    if(is.null(tsData)){
      next
    }
    tsData <- subset(tsData, Date >= startDate & Date <= endDate)

    tsData.Decomposition <- TSDecomposition(tsData, timeCol = "Date", valueCol = "Query_Smooth", timeformat = "%m/%d/%Y", targetSeasonality = "year", customDecomposition = "remainder", scaleDecomposition = FALSE)
    tsData.Decomposition$TrendRemainder <- tsData.Decomposition$TSTrend + tsData.Decomposition$TSRemainder
    
    results12 <- rollapply(tsData.Decomposition[, "TSTrend"], width = 12, FUN = ComputeSlopeGrowth, fill = 0, align = "right")
    volume12 <- rollapply(tsData.Decomposition[, "Query_Smooth"], width = 12, FUN = mean, fill = 0, align = "right")
    

    tsData.Decomposition$TrendGrowthYoY12 <- results12[,3]
    tsData.Decomposition$TrendVolume12 <- volume12
    #tsData.Decomposition$TrendCategory12 <- apply(tsData.Decomposition,1, Category, volumeColumn="TrendVolume12", growthColumn="TrendGrowthYoY12")
    n <- length(tsData.Decomposition[,1])
    
    lastCategory <-""
    earlyriser <- FALSE
    for (i in 1:n) {
      growth <-tsData.Decomposition[i,"TrendGrowthYoY12"]
      volume <-tsData.Decomposition[i,"TrendVolume12"]
      
      detail <- "Growth:"
      detail <- paste(detail, paste(toString(round(growth*100,2)),"%", sep=""))
      detail <- paste(detail, "Volume:",sep=",")
      detail <- paste(detail, toString(round(volume,0)))
      
      category <-""
      if(is.infinite(growth) | is.infinite(volume) | is.na(volume) | is.na(growth)){
        category <-""
      }
      else if(volume >= 30 & volume <= 500 & growth >= 0.25){
        category <- "EarlyRiser"
        earlyriser <- TRUE
      }
      else if (volume >= 30 & volume <= 500 & growth >= 0.05 & lastCategory == "EarlyRiser"){
        category <- "EarlyRiser"
      }
      else if (volume >= 30 & volume <= 500 & growth >= 0.1 & earlyriser){
        category <- "EarlyRiser"
      }
      
      if(is.infinite(growth) | is.infinite(volume) | is.na(volume) | is.na(growth)){
        category <-""
      }
      else if(volume >= 500 & volume <= 3000 & growth >= 0.1){
        category <- "Contemporary"
      }
      else if (volume >= 500 & volume <= 3000 & growth >= 0 & lastCategory == "Contemporary"){
        category <- "Contemporary"
      }
      
      if(is.infinite(growth) | is.infinite(volume) | is.na(volume) | is.na(growth)){
        category <-""
      }
      else if (volume > 500 & growth < -0.1){
        category = "Declining"
      }
      else if (volume > 500 & growth < 0 & lastCategory == "Declining"){
        category = "Declining"
      }
      else if(volume >= 3000){
        category <- "Mature"
      }
      
      if(category==""){
        detail = ""
      }
      
      lastCategory <- category
      tsData.Decomposition[i,"TrendGrowth"] <- as.character(round(growth*100,2))
      tsData.Decomposition[i,"TrendGrowthAbs"] <- as.character(round(abs(growth)*100,2))
      tsData.Decomposition[i,"TrendVolume"] <- as.character(round(volume,0))
      
      tsData.Decomposition[i,"TrendCategory"] <- as.character(category)
      tsData.Decomposition[i,"TrendDetail"] <- as.character(detail)
    }
    
    isEarlyRiser = length(which(tsData.Decomposition$TrendCategory == "EarlyRiser")) >0
    dateEarlyRiser<-""
    if(isEarlyRiser){
      dateEarlyRiser <- as.character(subset(tsData.Decomposition, TrendCategory=="EarlyRiser")[1,"Date"])
    }
    
    isMature = length(which(tsData.Decomposition$TrendCategory == "Mature")) >0
    dateMature<-""
    if(isMature){
      dateMature <- as.character(subset(tsData.Decomposition, TrendCategory=="Mature")[1,"Date"])
    }
    
    isDeclining = length(which(tsData.Decomposition$TrendCategory == "Declining")) >0
    dateDeclining<-""
    if(isDeclining){
      dateDeclining <- as.character(subset(tsData.Decomposition, TrendCategory=="Declining")[1,"Date"])
    }
    
    isContemporary = length(which(tsData.Decomposition$TrendCategory == "Contemporary")) >0
    dateContemporary <-""
    if(isContemporary){
      dateContemporary <- as.character(subset(tsData.Decomposition, TrendCategory=="Contemporary")[1,"Date"])
    }
    
    results <- data.frame(Entity = as.character(tsData.Decomposition$Entity[1]),
                          EarlyRiser = isEarlyRiser,
                          EarlyRiser.Date = dateEarlyRiser,
                          Contemporary = isContemporary,
                          Contemporary.Date =dateContemporary,
                          Mature = isMature,
                          Mature.Date =dateMature,
                          Declining = isDeclining,
                          Declining.Date =dateDeclining
                          )
    
    newRow = rbind(newRow, results)

    newData = rbind(newData, tsData.Decomposition)
}
write.table(newRow, "<SUMMARY>", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(newData, "<TSDETAIL>", sep = "\t", row.names = FALSE, quote = FALSE)