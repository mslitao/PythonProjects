library(Rcpp)
library(prophet)
library(dplyr)
library(lubridate)


christmas <- data_frame(
  holiday = 'christmas',
  ds = as.Date(c('2014-12-01', '2015-12-01', '2016-12-01', '2017-12-01', '2018-12-01')),
  lower_window = -1,
  upper_window = 1
)
summer <- data_frame(
  holiday = 'summer',
  ds = as.Date(c('2014-06-01', '2015-06-01', '2016-06-01', '2017-06-01', '2018-06-01')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(christmas,summer)

ForcastValidation = function(data, logtransform, validation_periods, model_growth)
{
  df = data
  start_date = min(as.Date(df$ds))
  end_date = max(as.Date(df$ds))
  future_periods = validation_periods+1  #12 + validation_periods+1
  validation_date = end_date %m-% months(validation_periods)
  
  if(logtransform){
    df$y <- log(df$y)
  }
  
  df$cap <- mean(df$y)
  df1 = df[as.Date(df$ds) <= validation_date,]
  sink("file")
  m1 <- prophet(df1,growth = model_growth,changepoint.prior.scale = 0.5,n.changepoints=5,uncertainty.samples = 10, holidays = holidays, weekly.seasonality = FALSE)
  sink()
  future <- make_future_dataframe(m1, periods = future_periods,fre="month")
  future$cap <- mean(df$y)
  forecast <- predict(m1, future)
  #m <- prophet(df);
  #plot(m, forecast)
  
  validation_true = df[as.Date(df$ds) > validation_date & as.Date(df$ds) <= end_date,]
  validation_predict = forecast[as.Date(forecast$ds) > validation_date & as.Date(forecast$ds) <= end_date,]
  i = 0
  error = 0
  while(i<validation_periods){
    i=i+1
    value_true = validation_true[i,]$y
    value_predict = validation_predict[i,]$yhat
    if(logtransform){
      value_true = exp(value_true)
      value_predict = exp(value_predict)
    }
    
    error = error +  abs((value_predict - value_true)/(value_true+1))
  }
  error = error/validation_periods*100
  return(error)
}

ForcastValidationSweepModels = function(data, validation_periods){
  transforms = c(TRUE,FALSE)
  #models =c("logistic","linear")
  models =c("linear")
  
  lowest_error = 100
  best_transform = NULL
  best_models = NULL
  for(logtransform in transforms){
    for(model_growth in models){
      model_error =tryCatch({
        ForcastValidation(data,logtransform,validation_periods,model_growth )
      }, warning = function(war) {
        return(99)
      }, error = function(err) {
        return(98)
      }, finally = {
      })
      
      if(model_error<lowest_error){
        lowest_error = model_error
        best_transform = logtransform
        best_models = model_growth
      }
    }
  }
  
  result = c(lowest_error,
             validation_periods,
             best_transform,
             best_models)
  
  return(result)
}

#Read data here
Root = "D:\\BingPrediction\\Nestle\\NgramToTrend\\Nutrient\\Trend"
fileName = "Food.TrendStatistics.V2.txt"
inputFile = paste(Root, fileName, sep="\\")
inputData <- read.csv(file=inputFile, header=TRUE, sep="\t",stringsAsFactors = F)
inputData = inputData[inputData$VolumeFrom2014>50 & abs(inputData$GrowthFrom2014)>5 & inputData$GrowthFrom2014>0,]
ngrams <-as.list(unique(inputData$QueryNgram))

newRow=NULL
startIndex = 1221
index =0
for(ngram in ngrams){
  index = index +1
  if(index<startIndex){
    next
  }
  if(index %% 20 == 0){
    outputFile =paste("D:\\BingPrediction\\Nestle\\NgramToTrend\\Nutrient\\Trend\\Forcasting\\Food.TrendStatistics.ForecastingValidation_",as.character(index),".txt",sep = "")
    
    colnames(newRow)<-c("Trend","Error","Periods","LogTransform","ModelGrowth")
    write.table(newRow,outputFile,sep="\t",row.names=FALSE,quote=FALSE)
    
    newRow=NULL
  }
  
  writeLines(paste("Process", as.character(index), ngram))
  tsdata = inputData[inputData$QueryNgram==as.character(ngram),]
  row.names(tsdata) = 1:dim(tsdata)[1]
  
  #data = tsdata[,c('MonthDate','ImpressionsNorm')]
  #colnames(data)<-c("ds","y")
  tsdata = data.frame(ds=as.Date(tsdata[['MonthDate']]),y=tsdata[['ImpressionsNorm']])
  
  #writeLines(paste("Process", as.character(index),2, ngram))
  result = ForcastValidationSweepModels(tsdata, 12)
  #writeLines(paste("Process", as.character(index),3, ngram))
  newRow=rbind(newRow, c(as.character(ngram), 
                         result[1],
                         result[2],
                         result[3],
                         result[4]))
}
outputFile =paste("D:\\BingPrediction\\Nestle\\NgramToTrend\\Nutrient\\Trend\\Forcasting\\Food.TrendStatistics.ForecastingValidation_",as.character(index),".txt",sep = "")

colnames(newRow)<-c("Trend","Error","Periods","LogTransform","ModelGrowth")
write.table(newRow,outputFile,sep="\t",row.names=FALSE,quote=FALSE)
