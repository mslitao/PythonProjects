source("ModelTraining.R")
RollingTrainMetrics <- function(inputData, selectedFeatures, trainEndDate, algo = "lm") {
    length <- length(inputData[, 1])
    trainEndIndex <- which(inputData$Date == trainEndDate)

    results <- NULL
    if (algo == "lm") {
        features.lm <- as.formula(paste0("Value ~ ", paste(selectedFeatures, collapse = '+')))
    }
    inputData <- inputData[, c("Date", "Value", selectedFeatures)]

    for (i in trainEndIndex:(length - 1)) {
        trainData <- inputData[1:i,]
        
        testData <- inputData[(i + 1):(i + 1),]

        if (algo == "lm") {
            model <- lm(features.lm, trainData)
            pred = predict(model, newdata = testData)
            predictions <- data.frame(Date = as.Date((testData$Date)[1], format = "%Y-%m-%d"), Actual = as.numeric(testData$Value[1]), Forecast = pred)
        }
        else {
            trainData <- na.omit(trainData)
            model <- Train(trainData, algorithm = "lasso", columnIndexDate = 1, columnIndexValue = 2, parameters = NULL, timeFormat = "%Y-%m-%d")
            predictions <- Predict(model, testData, algorithm = "lasso", columnIndexValue = 2, columnIndexDate = 1, parameters = data.frame(lambda = c(0.01, 0.005)))
        }
        #print(predictions)
        results <- rbind(results, predictions)
    }
    if (algo == "lm") {
        model <- lm(features.lm, inputData)
    } else {
        model <- Train(inputData, algorithm = "lasso", columnIndexDate = 1, columnIndexValue = 2, parameters = NULL, timeFormat = "%Y-%m-%d")
    }
    mape <- round(ComputeTSMetrics(results, "Actual", "Forecast")$MAPE, 1)

    return(list(results, model, mape))
}

RollingTrainTSMetrics <- function(inputData, columnIndexValue, columnIndexDate, trainEndDate, make) {
    #Train Time series model(baseline)
    AlgoList <- c("arima", "stl", "stl+arima", "ets")

    trainFraction <- which(inputData[, columnIndexDate] == trainEndDate) / nrow(inputData)
    results <- NULL
    stat <- NULL
    for (a in 1:length(AlgoList)) {
        algo <- AlgoList[a]

        data <- data.frame(TrainPredict(
            inputData,
            columnIndexValue = columnIndexValue,
            columnIndexDate = columnIndexDate,
            dateFormat = "%Y-%m-%d",
            algorithm = algo,
            parameters = NULL,
            mode = "rolling",
            trainFraction = trainFraction,
            folds = -1)[1])
        #Date Actual Forecast
        
        if (is.null(results)) {
            results <- data
            names(results) <- c("Date", "Actual", algo)
        } else {
            data.tmp <- data[, c("Date", "Forecast")]
            names(data.tmp) <- c("Date", algo)
            results <- merge(results, data.tmp, by.x = "Date", by.y = "Date")
        }

        #Metrics
        mape <- round(ComputeTSMetrics(subset(data, Date > trainEndDate), "Actual", "Forecast")$MAPE, 1)
        metrics <- rbind(stat, data.frame(Comment = algo, MAPE = mape))
        print(paste(algo, mape))
    }

    #Forecast
    write.table(results, paste0(make, "_", names(inputData)[columnIndexValue], "_ts.txt"), sep = "\t", quote = F, row.names = F)

    #Metrics
    write.table(stat, paste0(make, "_", names(inputData)[columnIndexValue], "_ts_MAPE.txt"), sep = "\t", quote = F, row.names = F)
}