libraryRequireInstall = function(packageName, ...) {
    if (!require(packageName, character.only = TRUE))
        warning(paste("*** The package: '", packageName, "' was not installed ***", sep = ""))
    }

libraryRequireInstall("zoo")
libraryRequireInstall("proto")

################Inner parameters #################################

Sys.setlocale("LC_ALL", "English") # internationalization

# minimum samples for analysis
minSamples2run = 9

# minimum unique values
minUniqueValues = 3

Slope = function(x, y) {
    n = length(x)
    #y=n*y/max(y) #1-24
    #plot(x,y)
    a = 0
    b = 0
    for (i in 1:n) {
        a = a + (x[i] - mean(x)) * (y[i] - mean(y))
        b = b + (x[i] - mean(x)) * (x[i] - mean(x))
    }
    return(a / b);
}

ComputeSlope <- function(data) {
    n = length(data)
    x <- seq(1, n, by = 1)
    y = n * data / max(data) #1-24
    return(Slope(x, y))
}

ComputeSlopeGrowth <- function(data, freq) {
    n = length(data)
    x <- seq(1, n, by = 1)
    y <- data
    #y <- n*data/max(data) #1-24
    #plot(x,y)

    slope <- Slope(x, y)
    base <- mean(y) - slope * mean(x)

    error <- 0
    for (i in 1:n) {
        error <- error + abs(slope * x[i] + base - y[i]) / y[i] * 100
    }
    error <- error / n

    growth = slope * (x[n] - x[1]) / (y[1] + 1)

    growth2 = growth + 1
    if (growth2 < 0) {
        growth2 = 0
    }
    growthYoY = abs((growth2) ^ (1 / n)) ^ freq - 1
    #avgVolume <- mean(data)

    return(c(slope = slope, growth = growth, growthYoY = growthYoY, mape = error))
}

################Inner functions #################################
    # tiny function to deal with verl long strings on plot
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 3, partAvailable = 1) {
    # partAvailable, wich portion of window is available, in [0,1]
    if (is.null(strText))
        return(NULL)

    SCL = 0.075 * strCex / 0.8
    pardin = par()$din
    gStand = partAvailable * (isH * pardin[1] + (1 - isH) * pardin[2]) / SCL

    # if very very long abbreviate
    if (nchar(strText) > abbrTo && nchar(strText) > 1)
        strText = abbreviate(strText, abbrTo)

    # if looooooong convert to lo...
    if (nchar(strText) > round(gStand) && nchar(strText) > 1)
        strText = paste(substring(strText, 1, floor(gStand)), "...", sep = "")

    # if shorter than maxChar remove 
    if (gStand <= maxChar)
        strText = NULL

    return(strText)
}

#Info string to be plotted under the chart
CreateInfo = function(modelType, freqv, evars, plotType) {
    #all, clean, trend, seasonal, remainder, byseason
    pbiInfo = ""
    if (plotType == 'all')
        pbiInfo = paste(paste(names(evars), ": ", evars, "%", sep = ""), collapse = ", ")
    else
        if (plotType %in% c('clean', 'byseasonClean'))
            pbiInfo = paste(paste('clean', ": ", 100 - evars[3], "%", sep = ""), collapse = ", ")
        else
            if (plotType %in% c('trend', 'seasonal', 'remainder'))
                pbiInfo = paste(paste(names(evars[plotType]), ":", evars[plotType], "%", sep = ""), collapse = ", ")
            else
                if (plotType == 'byseason')
                    pbiInfo = paste(paste(names(evars), ":", evars, "%", sep = ""), collapse = ", ")

    sN = ""
    if (!is.null(names(freqv)[1]) && !is.na(names(freqv)[1]))
        sN = paste(" / ", names(freqv)[1], sep = "")

    pbiInfo = paste("Model: ", modelType, ", freq = ", as.character(freqv), sN, ", ", pbiInfo, sep = "")
    #print(pbiInfo)
    return(pbiInfo)
}

# log on numeric array 
makeLog = function(val) {
    add = 1 + max(0, min(val)) - min(val) # heuristic
    transVal = val + add # shift to positive range (>1)
    logVal = log(transVal, base = exp(1))
    mul = norm(val, type = "2") / norm(logVal, type = "2")
    logVal = logVal * mul
    return(list(add = add, mul = mul, logVal = logVal, base = exp(1)))
}

# invert makeLog 
makeUnLog = function(logVal, add, mul, base = exp(1)) {
    transVal = exp(logVal / mul)
    val = transVal - add
    return(val)
}

#New Function
# Find best unit from dates which can be day/week/month/quater/year
    #
# @param dates - List of dates
#
# @return Dates unit day/week/month/quater/year
FindSeasonFromDates = function(dates) {
    seasons <- FindSeasonStatFromDates(dates)
    perSeason <- length(dates) / seasons

    unit <- 1
    minFreq <- 1000
    for (s in names(seasons)) {
        if (abs(perSeason[s] - 1) < minFreq) {
            minFreq <- abs(perSeason[s] - 1)
            unit <- perSeason[s]
        }
    }

    return(names(unit))
}

# Compute Season statistic for day/week/month/quater/year for given dates
    #
# @param dates - List of dates
#
# @return Season statistic for day/week/month/quater/year
FindSeasonStatFromDates = function(dates) {
    seasonNames <- c("day", "week", "month", "quater", "year")
    seasons <- rep(NaN, 5)
    names(seasons) <- seasonNames

    seasons["day"] <- round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "days"))
    seasons["week"] <- round(as.numeric(difftime(dates[length(dates)], dates[1]), units = "weeks"))
    seasons["month"] <- seasons["day"] / 30
    seasons["year"] <- seasons["day"] / 365.25
    seasons["quater"] <- seasons["year"] * 4

    #print(seasons)
    return(seasons)
}

# Find frequency for the given dates, which can be day/week/month/quater/year
    #
# @param dates - Dates for time series
# @param seasonality - Target seasionality. By default is "autodetect"
    #
# @return - Matched seasonality for given dates.
FindFreqFromDates = function(dates, seasonality = "autodetect") {
    seasons = FindSeasonStatFromDates(dates)
    perSeason = length(dates) / seasons

    freq = 1
    if (seasonality != "autodetect") # target 
        freq = perSeason[seasonality]

    if (freq < 2) {
        freq = 1
        for (s in rev(names(seasons))) {
            if ((seasons[s] > 5 && perSeason[s] > 3) || (seasons[s] > 2 && perSeason[s] > 7)) {
                freq = perSeason[s]
                break
            }
        }
    }

    return(round(freq))
}
#End New Function

#decompose into 3 components, known frequency 
flexTSdecomposition = function(Time, vals, freq, trendSmoothness, myts, robustToOutliers, degree) {
    N = length(Time)
    twin = getSTwindows(N, trendSmoothness = trendSmoothness, freq = freq)

    if (freq == 1) {
        s = (100 * trendSmoothness / 70)
        span = max(s, 0.2) # get from t smoothness
        fit <- loess(vals ~ seq(1, length(Time)), degree = 1 + degree, span = span)
        fit$time.series.df = data.frame(seasonal = rep(0, length(Time)), trend = fit$fitted, residuals = fit$residuals, data = vals)

    }
    if (freq > 1) {
        ## Convert to time series
        fit <- stl(myts, robust = robustToOutliers, s.degree = degree, t.degree = degree, s.window = "periodic", t.window = twin)
        fit$time.series.df = as.data.frame(fit$time.series)
        fit$fitted = fit$time.series.df$seasonal + fit$time.series.df$trend
        fit$residuals = vals - fit$fitted

    }

    clean = fit$time.series.df[, 1] + fit$time.series.df[, 2]
    seasonal = fit$time.series.df[, 1] + mean(fit$time.series.df[, 2])
    remainder = fit$time.series.df[, 3] + mean(vals)
    trend = fit$time.series.df[, 2]
    dfTSD = data.frame(clean = clean, seasonal = seasonal, trend = trend, remainder = remainder)

    rawseasonal = fit$time.series.df[, 1]
    rawremainder = fit$time.series.df[, 3]
    dfTSDRaw = data.frame(clean = clean, seasonal = rawseasonal, trend = trend, remainder = rawremainder)
    return(list(fit = fit, dfTSD = dfTSD, dfTSDRaw = dfTSDRaw))

}

#find relative part of signal
explained = function(sigModeled, sig) {
    sig = sig - mean(sig)
    sigModeled = sigModeled - mean(sigModeled)
    normL2sig = norm(sig, type = "2")
    normL2err = norm(sigModeled, type = "2")
    return((normL2err / (normL2sig + 0.00001)))
}

#next odd number
nextodd = function(num)
    return(round(num) + (round(num) %% 2 == 0))

#get smoothness parameters for STL function
getSTwindows = function(numSamples, trendSmoothness = 0.5, freq = 4) {
    getByPos = function(arr, frac)
        arr[max(1, round(length(arr) * frac))]

    t = nextodd(freq * 1.5) # default
    allTS = seq(3, max(7, max(t * 2, nextodd(numSamples / 2))), by = 2)
    return(getByPos(allTS, trendSmoothness))
}

#get valid frequency parameter, based on input from user 
getFrequency = function(parsed_dates, values, tS, f) {
    myFreq = f
    grp = c("autodetect", "none", "manual")

    if (!(tS %in% c("autodetect", "none", "manual"))) #detect from date
        {
        myFreq = FindFreqFromDates(parsed_dates, seasonality = tS)
    } else {
        if (tS == "none") { myFreq = 1 }
        else {
            if (tS == "autodetect")
                myFreq = FindFreqFromDates(parsed_dates, seasonality = "autodetect")
            }
    }
    numPeriods = floor(length(values) / myFreq)
    if (numPeriods < 2)
        myFreq = FindFreqFromDates(parsed_dates, seasonality = "autodetect")
    return(myFreq)
}

p <- function(msg) {
    cat(paste0(Sys.time(), "\t", msg, "\n"))
}

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
trim.slash <- function(x) sub("^/", "", x)

#********* PBI Parameters Block ***************

if (!exists("Time"))
    Time = NULL

if (!exists("Value"))
    Value = NULL

showWarnInfo = TRUE #default
if (exists("settings_extra_params_show")) {
    showWarnInfo = settings_extra_params_show
}

if (exists("settings_model_params_show") && settings_model_params_show == FALSE)
    rm(list = ls(pattern = "settings_model_params_"))

if (exists("settings_algo_params_show") && settings_algo_params_show == FALSE)
    rm(list = ls(pattern = "settings_algo_params_"))

if (exists("settings_plot_params_show") && settings_plot_params_show == FALSE)
    rm(list = ls(pattern = "settings_plot_params_"))

if (exists("settings_extra_params_show") && settings_extra_params_show == FALSE)
    rm(list = ls(pattern = "settings_extra_params_"))

##PBI_PARAM: display_name: Decomposition model, tooltip:Switch between additive and multiplicative decomposition models 
    # Type: enumeration, default:'automatic', 
# Min: , Max:
# enumeration options: additive ,multiplicative ,automatic ,
    modelType = 'automatic' #default
if (exists("settings_model_params_modelType")) {
    modelType = settings_model_params_modelType
}

##PBI_PARAM: display_name: Seasonal factor, tooltip:Specify recommended seasonal factor
    # Type: enumeration, default:'autodetect', 
# Min: , Max:
# enumeration options: autodetect ,none ,manual ,hour ,day ,week ,month ,quater ,year ,
    targetSeasonality = 'autodetect' #default
if (exists("settings_model_params_targetSeasonality")) {
    targetSeasonality = settings_model_params_targetSeasonality
}

##PBI_PARAM: display_name: Frequency, tooltip:Number of samples per season
    # Type: numeric, default:12, 
# Min: 1, Max:10000
freq = 12 #default
if (exists("settings_model_params_freq")) {
    freq = settings_model_params_freq
    freq = max(min(freq, 10000), 1)
}

##PBI_PARAM: display_name: Degree, tooltip:Degree of locally-fitted polynomial in seasonal extraction and trend extraction
    # Type: bool, default:FALSE, 
# Min: , Max:
degree = FALSE #default
if (exists("settings_algo_params_degree")) {
    degree = settings_algo_params_degree
}

##PBI_PARAM: display_name: Robust to outliers, tooltip:Indicating if robust fitting be used in the loess procedure
    # Type: bool, default:TRUE, 
# Min: , Max:
robustToOutliers = TRUE #default
if (exists("settings_algo_params_robustToOutliers")) {
    robustToOutliers = settings_algo_params_robustToOutliers
}

##PBI_PARAM: display_name: Trend smoothness, tooltip:Trend smoothness
    # Type: numeric, default:50, 
# Min: 1, Max:100
trendSmoothness = 50 #default
if (exists("settings_algo_params_percentile")) {
    trendSmoothness = settings_algo_params_percentile
    trendSmoothness = max(min(trendSmoothness, 100), 1)
}

##PBI_PARAM: display_name: Plot type, tooltip:specify the plot type
    # Type: enumeration, default:'all', 
# Min: , Max:
# enumeration options: all ,trend ,seasonal ,clean ,remainder ,byseason ,byseasonClean ,
    plotType = 'all' #default
if (exists("settings_plot_params_plotType")) {
    plotType = settings_plot_params_plotType
}

##PBI_PARAM: display_name: Line width, tooltip:line width
# Type: numeric, default:10, 
# Min: 1, Max:50
lineWidth = 10 #default
if (exists("settings_plot_params_weight")) {
    lineWidth = settings_plot_params_weight
    lineWidth = max(min(lineWidth, 50), 1)
}

##PBI_PARAM: display_name: Line color, tooltip:line color
# Type: fill, default:'red', 
# Min: , Max:
lineCol = 'red' #default
if (exists("settings_plot_params_lineCol")) {
    lineCol = settings_plot_params_lineCol
}

##PBI_PARAM: display_name: Labels color, tooltip:labels color
    # Type: fill, default:'orange', 
# Min: , Max:
labelsCol = 'orange' #default
if (exists("settings_plot_params_labelsCol")) {
    labelsCol = settings_plot_params_labelsCol
}

##PBI_PARAM: display_name: Labels font size, tooltip:labels font size
    # Type: numeric, default:10, 
# Min: 8, Max:40
labelsFont = 10 #default
if (exists("settings_plot_params_textSize")) {
    labelsFont = settings_plot_params_textSize
    labelsFont = max(min(labelsFont, 40), 8)
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 8, Max:40
infoFontSize = 10 #default
if (exists("settings_extra_params_textSize")) {
    infoFontSize = settings_extra_params_textSize
    infoFontSize = max(min(infoFontSize, 40), 10)
}

##PBI_PARAM: display_name: Text color, tooltip:Text color
# Type: fill, default:'brown', 
# Min: , Max:
infoCol = 'brown' #default
if (exists("settings_extra_params_infoCol")) {
    infoCol = settings_extra_params_infoCol
}
{

    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) == 0) {
        input <- ""
        output.Summary <- ""
        output.Decomposition <- ""
        entityCol <- "Terms"
        valueCol <- "Weight"
        timeCol <- "DateTimeStamp"
        DateFormat <- "%Y-%m-%d"
        targetSeasonality = "year"
        scaleDecomposition = "false"

    } else {
        pars = list()

        for (i in 1:length(args)) {
            parts <- strsplit(args[i], "=")[[1]]
            pars[[trim.slash(parts[1])]] = parts[2]
        }

        #==========================================
        # Print Args
        #==========================================
        print(pars)

        input <- (pars[["Input"]])
        output.Summary <- (pars[["OutputSummary"]])
        output.Decomposition <- (pars[["OutputDecomposition"]])

        entityCol <- (pars[["EntityCol"]])
        valueCol <- (pars[["ValueCol"]])
        timeCol <- (pars[["TimeCol"]])

        DateFormat <- (pars[["DateFormat"]])
        DateFormat <- gsub("\\+", " ", DateFormat)
        print(DateFormat)
        targetSeasonality <- (pars[["Seasonality"]])
        scaleDecomposition <- (pars[["ScaleDecomposition"]])
    }

    data <- read.csv(file = input, header = TRUE, sep = "\t")

    #ngrams <-as.list(unique(data$Trend))
    ngrams <- as.list(unique(data[, entityCol]))


    plotType = "all"


    #update params to correct scale
    trendSmoothness = trendSmoothness / 100 # from % to [0,1]
    labelsFont = labelsFont / 10 # convert from range 8-40
    lineWidth = lineWidth / 8 # convert from 1-50 
    infoFontSize = infoFontSize / 10


    newRow = NULL
    newData = NULL

    index = 0
    for (ngram in ngrams) {
        index = index + 1
        tsdata = data[data[, entityCol] == as.character(ngram),]
        #tsdata = tsdata[order(tsdata[,timeCol]),]
        tsdata = tsdata[order(as.Date(tsdata[, timeCol], format = DateFormat)),]
        Time = as.data.frame(tsdata[, timeCol])
        Value = as.data.frame(tsdata[, valueCol])
        #print(Time)
        #print(Value)
        if (nrow(Time) < 30) {
            next
        }
        pbiInfo = "" # warning or info string 
        plotType = "all"
        modelType = "automatic"

        #check if all Roles exist
        if (!(exists("Value") && exists("Time"))) {
            Value = NULL;
            Time = ts();
            plotType = "empty"
        }
        else {
            nameTime = names(Time)[1]
            NewTime = as.character(Time[, 1])
            N = length(NewTime)
            if (N < minSamples2run) {
                Value = NULL;
                Time = ts();
                plotType = "empty"
                pbiInfo = "Warning: Not enough samples for analysis"
            }
        }
        if (plotType != "empty") {
            #parsed_dates=strptime(Time,"%Y-%m-%dT%H:%M:%S",tz="UTC")
            #parsed_dates=strptime(NewTime,"%m/%d/%Y %H:%M",tz="UTC")
            parsed_dates = strptime(NewTime, DateFormat, tz = "UTC")
            if ((any(is.na(parsed_dates)))) {
                Value = NULL;
                parsed_dates = ts();
                plotType = "empty"
                pbiInfo = "Warning: Only 'Date', 'Time', 'Date/Time' types are allowed for Time"
            }
            else
                if (!is.numeric(Value[, 1])) {
                    Value = NULL;
                    Time = ts();
                    plotType = "empty"
                    pbiInfo = "Warning: Only numeric types are allowed for Value"
                }
                else
                    if (length(unique(Value[, 1])) < minUniqueValues) {
                        Value = NULL;
                        Time = ts();
                        plotType = "empty"
                        pbiInfo = "Warning: No sufficient variance in Value"
                    }
        }

        if (plotType != "empty") {
            interval = difftime(parsed_dates[length(parsed_dates)], parsed_dates[1]) / (length(parsed_dates) - 1) # force equal spacing 

            vals = avals = Value[, 1]
            mvals = NULL
            coefficient = cor(seq(1, length(vals), by = 1), vals)
            volume = mean(vals)

            if (modelType != "additive") {
                mvals = makeLog(vals)
                vals = mvals$logVal
            }

            #detect the frequency 
            season = FindSeasonFromDates(parsed_dates)
            freqv = getFrequency(parsed_dates, vals, targetSeasonality, freq)

            print(season)
            print(freqv)

            seasonFreq = 1;
            if (season == "day") seasonFreq = 365
            if (season == "week") seasonFreq = 52
            if (season == "month") seasonFreq = 12
            if (season == "quater") seasonFreq = 4
            if (season == "year") seasonFreq = 1
            growthResult.Raw <- ComputeSlopeGrowth(vals, seasonFreq)

            # Convert to time series
            mytsAdd = myts <- ts(vals, start = as.Date(min(parsed_dates)), frequency = freqv)

            if (!is.null(mvals)) {
                afreq = getFrequency(parsed_dates, avals, targetSeasonality, freq)
                mytsAdd <- ts(avals, start = as.Date(min(parsed_dates)), frequency = afreq)
            }

            # decompose (additive or multiplicative with or without seasonality)
            flexTSres <- flexTSdecomposition(parsed_dates, vals, freqv, trendSmoothness, myts, robustToOutliers, degree)

            dfTSD = flexTSres$dfTSD
            dfTSDaddRaw <- flexTSres$dfTSDRaw

            #explained variance
            if (is.null(mvals))
                evars = apply(dfTSD[, -1], 2, FUN = explained, sig = vals)
            else
                evars = apply(makeUnLog(dfTSD[, -1], mvals$add, mvals$mul), 2, FUN = explained, sig = avals)


            evars = round(100 * evars / sum(evars))

            if (modelType == "automatic") {
                #compute the same for additive model, compare and select one 
                flexTSresAdd <- flexTSdecomposition(parsed_dates, avals, afreq, trendSmoothness, mytsAdd, robustToOutliers, degree)
                dfTSDadd <- flexTSresAdd$dfTSD
                dfTSDaddRaw <- flexTSresAdd$dfTSDRaw
                evarsAdd = apply(dfTSDadd[, -1], 2, FUN = explained, sig = avals)
                evarsAdd = round(100 * evarsAdd / sum(evarsAdd))

                if (evarsAdd[3] < evars[3]) # do additive
                    {
                    modelType = "additive"
                    myts = mytsAdd
                    mvals = NULL
                    freqv = afreq
                    dfTSD = dfTSDadd
                    evars = evarsAdd
                    flexTSres = flexTSresAdd
                    vals = avals
                }
                else # do multiplicative
                    modelType = "multiplicative"

                growthResult.Trend <- ComputeSlopeGrowth(dfTSDaddRaw[, 3], seasonFreq)

                newRow = rbind(newRow, c(as.character(ngram),
                #modelType,
                #round(coefficient,digits=2),
                               as.double(volume),
                               as.double(growthResult.Raw[[2]]),
                               as.double(growthResult.Raw[[3]]),
                               as.double(growthResult.Trend[[2]]),
                               as.double(growthResult.Trend[[3]]),
                               as.double(evars["seasonal"]),
                               as.double(evars["trend"]),
                               as.double(evars["remainder"])))

                if (scaleDecomposition == "true") {
                    tsdata$TSSeasonal = dfTSDadd[, 2]
                    tsdata$TSTrend = dfTSDadd[, 3]
                    tsdata$TSRemainder = dfTSDadd[, 4]
                } else {
                    tsdata$TSSeasonal = dfTSDaddRaw[, 2]
                    tsdata$TSTrend = dfTSDaddRaw[, 3]
                    tsdata$TSRemainder = dfTSDaddRaw[, 4]
                }


                newData = rbind(newData, tsdata)
            }
        }
    }

    colnames(newRow) <- c("Trend","Volume", "Raw_Growth", "Raw_YoYGrowth","Trend_Growth", "Trend_YoYGrowth", "TSSeasonal", "TSTrend", "TSRemainder")
    write.table(newRow, output.Summary, sep = "\t", row.names = FALSE, quote = FALSE)
    write.table(newData, output.Decomposition, sep = "\t", row.names = FALSE, quote = FALSE)
}
