source("Winsorize.R")
source("Metrics.R")
source("IdentifyLagFeatures.R")
source("Regularization.R")
source("RollingTrainNMetrics.R")
#source("AutoSaleStat.R")
source("Depo.R")
library(zoo)
library(Hmisc)
library(mondate)
library(stringr)

datesSet <- data.frame(Date = seq(as.Date("2010-01-01", format = "%Y-%m-%d"), as.Date(Sys.Date(), format = "%Y-%m-%d"), by = "month"))
#General functions
MaxMinNorm <- function(data) {
    min <- min(data)
    max <- max(data)
    data <- round(100 * (data - min) / (max - min))
    return(data)
}
TrimHead <- function(data, col, minValid) {
    if (col == "") {
        fstValRow <- min(which(data[, -1] >= minValid))
    } else {
        fstValRow <- min(which(data[, col] >= minValid))
    }
    if (fstValRow > 1) {
        data <- tail(data, - (fstValRow - 1))
    }
    return(data)
}
TrimTail <- function(data, col, minValid) {
    if (col == "") {
        lastValRow <- max(which(data[, -1] >= minValid))
    } else {
        lastValRow <- max(which(data[, col] >= minValid))
    }
    data <- data[1:lastValRow, ]
    return(data)
}

GetLagFeatures <- function(data, col, maxlag) {
    lag <- data.frame(IdentifyLagFeatures(data, col, maxlag)[1])
    lag <- data.frame(lag[, c(names(lag)[grep("lag", names(lag))], "Date")])
    for (l in 1:maxlag) {
        names(lag)[l] <- paste0(col, l)
    }
    return(lag)
}

#Extract sales
GetModelSales <- function(salefile, minSale = 100) {
    salesData <- read.table(salefile, header = T, sep = "\t")
    salesData$Month <- as.Date(salesData$Month, format = "%m/%d/%Y")
    salesData <- subset(salesData[with(salesData, order(Month)),], Month < as.Date(Sys.Date(), format = "%Y-%m-%d"))
    names(salesData) <- c("Date", "Value")
    salesData <- TrimHead(salesData, "Value", minSale)
    return(salesData)
}

GetAggModelSales <- function(targetModels, saleFolder, make, minSale = 100) {
    salesAgg <- datesSet
    for (i in 1:length(targetModels)) {
        model <- as.character(targetModels[i])
        #print(model)
        modelSaleFile <- paste0(saleFolder, model, "-", make, ".txt")
        salesData <- GetModelSales(modelSaleFile)
        salesAgg <- merge(x = salesAgg, y = salesData, by = "Date", all.x = T)
        names(salesAgg)[length(names(salesAgg))] <- model
    }
    salesAgg[is.na(salesAgg)] <- 0
    if (length(targetModels) == 1) {
        names(salesAgg) <- c("Date", "Value")
    } else {
        salesAgg <- data.frame(Date = salesAgg$Date, Value = rowSums(salesAgg[, -1]))
    }
    salesAgg <- TrimHead(salesAgg, "Value", minSale)
    salesAgg <- TrimTail(salesAgg, "Value", minSale)
    return(salesAgg)
}

#Extract Bing
GetBingFeatures <- function(bingData, modelList = character(), minBing = 100, needNorm = T, needSmooth = T, bodyStyleList = character(), brdList = character()) {
    if (length(modelList) > 0) {
        bingData <- subset(bingData, (Model %in% modelList))
        bingData <- bingData[,-which(names(bingData) %in% c("Model"))]
    }
    if (length(bodyStyleList) > 0 & length(brdList) > 0) {
        bingData <- subset(bingData, (BodyStyle %in% bodyStyleList) & (Brand %in% brdList))
        bingData <- bingData[, - which(names(bingData) %in% c("BodyStyle", "Brand"))]
    }
    names <- names(bingData)
    bingData <- data.frame(aggregate(bingData[-1], by = list(bingData$Date), sum))
    names(bingData) <- names
    bingData <- merge(x = datesSet, y = bingData, by = "Date", all.x = T)
    bingData[is.na(bingData)] <- 0
    bingData <- TrimHead(bingData, "", minBing)

    needFill <- F
    if (bingData$Date[1] <= "2011-10-01") {
        bingData[which(bingData$Date == "2011-10-01" | bingData$Date == "2011-11-01" | bingData$Date == "2011-12-01"), -1] <- NA #Cleanse (2011-10~12 pulse data has issue)
        needFill <- T
    }

    for (i in 2:ncol(bingData)) {
        if (needFill == T) {
            bingData[, i] <- na.StructTS(ts(bingData[, i], frequency = 12)) #Fillin NA
        }
        if (needNorm == T) {
            bingData[, i] <- round(30 * (as.numeric(bingData[, i]) / monthDays(as.Date(bingData$Date)))) #Norm
        }
        if (needSmooth == T) {
            bingData[, i] <- Winsorize(bingData[, i], width = 0, sigma = 1.5, replacement = c("cutoff", "observed")[2]) #Outlier removal
        }
    }
    return(bingData)
}

GetBingDepo <- function(bingFeatures, bingDepoType) {

    rowCount <- nrow(bingFeatures)
    for (col in 2:ncol(bingFeatures)) {
        depo <- data.frame(TSDecomposition(bingFeatures, timeCol = "Date", valueCol = bingFeaturesNames[col], timeformat = "%Y-%m-%d")[1])
        bingFeatures[, col] <- rep(0, rowCount)
        if (length(grep('T', bingDepoType)) > 0) {
            bingFeatures[, col] <- bingFeatures[, col] + depo$TSTrend
        }
        if (length(grep('S', bingDepoType)) > 0) {
            bingFeatures[, col] <- bingFeatures[, col] + depo$TSSeasonal
        }
        if (length(grep('R', bingDepoType)) > 0) {
            bingFeatures[, col] <- bingFeatures[, col] + depo$TSRemainder
        }
    }
    return(bingFeatures)
}


#Correlations
GetDepoCorrelation <- function(rawData) {
    rawDataTS <- data.frame(TSDecomposition(rawData, "Date", "Value")[1])
    rawDataTS$TSST <- rawDataTS$TSSeasonal + rawDataTS$TSTrend
    rawDataTS$TSTR <- rawDataTS$TSRemainder + rawDataTS$TSTrend

    for (p in 1:length(paraBegValDateList)) {
        paraBegValDate <- as.Date(paraBegValDateList[p])
        rawDataTSTmp <- subset(rawDataTS, Date >= paraBegValDate & Value >= minSale)
        corT <- round(cor(rawDataTSTmp[, "Value"], rawDataTSTmp[, "TSTrend"]), 2)
        mapeT <- round(ComputeTSMetrics(rawDataTSTmp, "Value", "TSTrend")$MAPE, 2)
        corST <- round(cor(rawDataTSTmp[, "Value"], rawDataTSTmp[, "TSST"]), 2)
        mapeST <- round(ComputeTSMetrics(rawDataTSTmp, "Value", "TSST")$MAPE, 2)
        corTR <- round(cor(rawDataTSTmp[, "Value"], rawDataTSTmp[, "TSTR"]), 2)
        mapeTR <- round(ComputeTSMetrics(rawDataTSTmp, "Value", "TSTR")$MAPE, 2)
        print(paste(bodyStyle, paraBegValDate, nrow(rawDataTSTmp), mapeT, corT, mapeST, corST, mapeTR, corTR))
    }
    plot(x = rawDataTS[, 1], y = rawDataTS[, 2], type = "l", main = bodyStyle)
    lines(x = rawDataTS[, 1], y = rawDataTS[, "TSTrend"], col = "red")
}

ComputeBingCorrelation <- function(corSet) {
    #Feature Generation  & Correlation
    corSet <- na.omit(corSet)
    corCollection <- NULL
    corSetMoM <- corSet[, -1] - apply(corSet[, -1], 2, function(x) shift(1, x))
    corSetMoM <- cbind(corSet[, 1], corSetMoM)
    names(corSetMoM) <- names(corSet)
    corSetMoM <- na.omit(corSetMoM)

    for (f in 3:ncol(corSet)) {
        #Intent - Lag 
        corOri <- round(round(cor(corSet[, 2], corSet[, f]) * 20)/20, 2)
        corMoM <- round(round(cor(corSetMoM[-1, 2], corSetMoM[-1, f]) * 20) / 20, 2)
        rowNum <- nrow(corSetMoM)
        dirAcc <- round(round(length(which(corSetMoM[-1, 2] * corSetMoM[-1, f] > 0)) / (rowNum - 1) * 20) / 20, 2)

        features.lm <- as.formula(paste0("Value ~ ", names(corSet)[f]))
        model <- lm(features.lm, corSet)
        predictions <- data.frame(Date = as.Date(corSet$Date), Actual = as.numeric(corSet$Value), Forecast = predict(model, newdata = corSet))
        mape <- round(round(ComputeTSMetrics(predictions, "Actual", "Forecast")$MAPE * 2) / 2, 1)
        feature <- names(corSet)[f]
        corCollection <- rbind(corCollection,
            data.frame(Feature = str_extract(feature, "[A-Za-z]+"),
            Lag = str_extract(feature, "[[:digit:]]+"),
            Cor = corOri,
            MAPE = mape,
            MoMCor = corMoM,
            DirAcc = dirAcc,
            All = - mape + corMoM))

        #print(paste(names(corSet)[f], dirAcc, corMoM, paste(corVec[-1], collapse = ":"), gap))
    }
    return(corCollection)
}

ComputeBingCorrelationYoY <- function(corSet, actualData, isDelta = F) {
    #Feature Generation  & Correlation
    corSet <- na.omit(corSet)
    corCollection <- NULL

    for (f in 3:ncol(corSet)) {
        #Intent - Lag 
        corOri <- round(cor(corSet[, 2], corSet[, f]), 2)
        if (isDelta == T) {
            features.lm <- as.formula(paste0("Value ~ ", names(corSet)[f]))
            model <- lm(features.lm, corSet)
            predictions <- data.frame(Date = as.Date(corSet$Date), Forecast = predict(model, newdata = corSet))
        } else {
            #print(tail(actualData, 5))
            corSetTmp <- merge(corSet, actualData[, c("Date", "LastY")], by = "Date")
            features.lm <- as.formula(paste0("Value ~ ", names(corSetTmp)[f], " + LastY"))
            model <- lm(features.lm, corSetTmp)
            predictions <- data.frame(Date = as.Date(corSetTmp$Date), Forecast = predict(model, newdata = corSetTmp))
        }
        
        
        predictions <- merge(x = predictions, y = actualData, by = "Date")
        if (isDelta == T) {
            predictions$Forecast <- (1.0 + predictions$Forecast) * predictions$LastY
        } else {
            predictions$Forecast <- predictions$Forecast + predictions$LastY
        }

        rows <- nrow(predictions)
        mape <- round(ComputeTSMetrics(predictions, "Value", "Forecast")$MAPE, 1)
        feature <- names(corSet)[f]
        corCollection <- rbind(corCollection, data.frame(Feature = str_extract(feature, "[A-Za-z]+"), Lag = str_extract(feature, "[[:digit:]]+"), Ori = corOri, MAPE = mape, Num = rows, Actual = predictions[rows, "Value"], Forecast = predictions[rows, "Forecast"]))
        #print(paste(names(corSet)[f], dirAcc, corMoM, paste(corVec[-1], collapse = ":"), gap))
    }
    return(corCollection)
}

LagFeatureGen <- function(featureSet, maxlag = 12) {
    allLagFeatures <- data.frame(Date = featureSet$Date)
    for (b in 2:ncol(featureSet)) {
        feature <- names(featureSet)[b]
        lagFeatures <- GetLagFeatures(featureSet, feature, maxlag)
        allLagFeatures <- merge(x = allLagFeatures, y = lagFeatures, by = "Date", all.x = T)
    }
    return(allLagFeatures)
}


CorMainFunction <- function(saleAgg, bingFeatures, minSale = 100, maxlag = 12, paraBegValDate = "2011-01-01", paraEndValDate = "2017-07-01") {
    # could be single or multiple(agg)
    #Bing Lag Features
    corOutput <- NULL
    for (b in 2:ncol(bingFeatures)) {
        feature <- names(bingFeatures)[b]
        lagFeatures <- GetLagFeatures(bingFeatures, feature, maxlag)
        #joinedData <- merge(x = bingFeatures[, c("Date", feature)], y = lagFeatures, by = "Date", all.x = T) #lag0
        joinedData <- merge(x = saleAgg, y = lagFeatures, by = "Date", all.x = T)
        joinedData <- na.omit(joinedData)

        #Compute Correlation
        begValDate <- as.Date(max(joinedData$Date[1], as.Date(paraBegValDate)))
        endValDate <- as.Date(min(joinedData$Date[max(which(joinedData[, "Value"] >= minSale))], as.Date(paraEndValDate)))
        dataForCorrelation <- subset(joinedData, Date >= begValDate & Date <= endValDate & Value >= minSale)
        corRet <- ComputeBingCorrelation(dataForCorrelation)

        #corRet <- corRet[with(corRet, order(-Ori, MAPE, Lag)),]
        corRet <- subset(corRet, Cor > 0 & MoMCor > 0 & DirAcc > 0.5) #Trend
        if (nrow(corRet) > 0) {
            corRet <- corRet[with(corRet, order(-All, -Lag)),]
            corOutput <- rbind(corOutput, data.frame(corRet[1,])) #bestlag
            #corOutput <- rbind(corOutput, corRet) #bestlag
        }
    }

    return(corOutput)
}

CorMainFunctionYoY <- function(saleAgg, bingFeatures, minSale = 100, maxlag = 12, paraBegValDate = "2011-01-01", paraEndValDate = "2017-07-01") {
    # could be single or multiple(agg)
    #Bing Lag Features
    corOutput <- NULL
    for (b in 2:ncol(bingFeatures)) {
        feature <- names(bingFeatures)[b]
        lagFeatures <- GetLagFeatures(bingFeatures, feature, maxlag)
        #joinedData <- merge(x = bingFeatures[, c("Date", feature)], y = lagFeatures, by = "Date", all.x = T) #lag0
        joinedData <- merge(x = saleAgg, y = lagFeatures, by = "Date", all.x = T)
        #YoY
        actualData <- joinedData[, c("Date", "Value")]
        actualData$LastY <- shift(12, joinedData$Value)
        #print(tail(actualData, 5))
        joinedData <- GetYoY(joinedData, isDelta = F)
        joinedData <- na.omit(joinedData)

        #Compute Correlation
        begValDate <- as.Date(max(joinedData$Date[1], as.Date(paraBegValDate)))
        endValDate <- as.Date(min(joinedData$Date[max(which(joinedData[, "Value"] >= minSale))], as.Date(paraEndValDate)))
        dataForCorrelation <- subset(joinedData, Date >= begValDate & Date <= endValDate & Value >= minSale)
        corRet <- ComputeBingCorrelationYoY(dataForCorrelation, actualData, isDelta = F)

        if (b == 2) {
            predictions <- data.frame(Date = as.Date(dataForCorrelation$Date))
            predictions <- merge(x = predictions, y = actualData, by = "Date")
            #plot(predictions[, 1], predictions[, "Value"], "l", main = "YoY")
            #lines(predictions[, 1], predictions[, "LastY"], col = "red")
            mape <- round(ComputeTSMetrics(predictions, "Value", "LastY")$MAPE, 1)
            rows <- nrow(predictions)
            corOutput <- rbind(corOutput, data.frame(Feature = "Sale", Lag = "12", Ori = 0, MAPE = mape, Num = rows, Actual = predictions[rows, "Value"], Forecast = predictions[rows, "LastY"]))
            #print(corOutput)
        }

        #corOutput <- rbind(corOutput, data.frame(Date = paraBegValDate, corRet[1,])) #lag 0
        #corRet <- data.frame(subset(corRet[-1,], Ori > 0))
        #if (nrow(corRet) > 0) {
        corRet <- corRet[with(corRet, order(-Ori, MAPE, Lag)),]
        corOutput <- rbind(corOutput, data.frame(corRet[1,])) #bestlag

        #}
    }
print(corOutput)
    return(corOutput)
}


#YoY
GetYoY <- function(inputDataYoY, isDelta = F) {
    colNum <- ncol(inputDataYoY)
    retData <- data.frame(Date = inputDataYoY[, c("Date")])
    for (i in 2:colNum) {
        target <- as.numeric(inputDataYoY[, i])
        if (isDelta == T) {
            retData$yoy <- (target - shift(12, target)) / shift(12, target)
        } else {
            retData$yoy <- target - shift(12, target)
        }
        names(retData)[names(retData) == "yoy"] <- names(inputDataYoY)[i]
    }
    return(retData[-1:-12,])
}

LassoFeatureSelection <- function(featureSelectionData, iterations = 10, top = 1) {

    selectedFeaturesList <- NULL
    lambdas = NULL
    for (i in 1:iterations) {
        fit <- cv.glmnet(as.matrix(allFeatures[, -1:-2]), allFeatures$Value, alpha = 1, family = "gaussian")
        errors = data.frame(fit$lambda, fit$cvm)
        lambdas <- rbind(lambdas, errors)
    }
    # take mean cvm for each lambda
    lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)

    # select top ones
    lambdas <- lambdas[order(lambdas[, 2]),]
    for (i in 1:top) {
        bestlambda = lambdas[i, 1]
        fit <- glmnet(as.matrix(allFeatures[, -1:-2]), allFeatures$Value, alpha = 1, family = "gaussian", lambda = bestlambda)
        featureWeights <- GetFeatureWeights(fit, fit$lambda)
        selectedFeaturesList[[length(selectedFeaturesList) + 1]] <- as.vector(featureWeights$Feature)
    }

    return(selectedFeaturesList)
}

GetSalesCustomFeatures <- function(sales, freq = 12, period = 3) {
    
    span <- freq * period
    salesCustom <- sales
    if (nrow(sales) < span) {
        span <- nrow(sales)
    }
    depo <- data.frame(TSDecomposition(sales[1:span,], timeCol = "Date", valueCol = "Value", timeformat = "%Y-%m-%d")[1])$TSSeasonal
    salesCustom[1:span, "Value"] <- MaxMinNorm(depo)
    if (nrow(sales) > span) {
        for (i in (span + 1):nrow(sales)) {
            depo <- data.frame(TSDecomposition(sales[(i - span):(i-1),], timeCol = "Date", valueCol = "Value", timeformat = "%Y-%m-%d")[1])$TSSeasonal
            salesCustom[i, "Value"] <- MaxMinNorm(depo)[span - freq + 1]
        }
    }
    names(salesCustom) <- c("Date", "ValueSeasonal")

    salesCustom$ValueMA <- sales$Value
    salesCustom$ValueMAHalf <- sales$Value
    salesCustom$ValueMAQtr <- sales$Value

    for (i in 2:nrow(sales)) {
        salesCustom[i, "ValueMA"] <- mean(sales$Value[max(1:(i - freq)):(i - 1)])
        salesCustom[i, "ValueMAHalf"] <- mean(sales$Value[max(1:(i - freq/2)):(i - 1)])
        salesCustom[i, "ValueMAQtr"] <- mean(sales$Value[max(1:(i - freq/4)):(i - 1)])
    }
    return(salesCustom)
}
