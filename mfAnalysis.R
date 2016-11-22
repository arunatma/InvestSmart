source("mfUtils.R")
dlStartDate <- "03-Apr-2006"
dlEndDate <- "17-Aug-2016"

# UTI Equity Fund
mfCode <- "28"
schemeCode <- "100669"
statURL <- paste0(
    "http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=", 
    dlStartDate,"&todate=", dlEndDate,"&mf=", mfCode, "&scm=", schemeCode)    
navTable <- getFromAmfi(statURL)

# Birla Sunlife Frontline Equity
mfCode <- "3"
schemeCode <- "103174"
statURL <- paste0(
    "http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=", 
    dlStartDate,"&todate=", dlEndDate,"&mf=", mfCode, "&scm=", schemeCode)    
navTableBfe <- getFromAmfi(statURL)

# Franklin India Bluechip
mfCode <- "27"
schemeCode <- "100471"
statURL <- paste0(
    "http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=", 
    dlStartDate,"&todate=", dlEndDate,"&mf=", mfCode, "&scm=", schemeCode)    
navTableFib <- getFromAmfi(statURL)

#------------------------------------------------------------------------------#
# Getting SIP XIRR calculation
startStr = "03-Apr-2006"
startDate = as.Date(startStr, "%d-%b-%Y") 
numYears = 7
freq = "week"  #month #day #quarter #year
sip = 500
getSipIrr(sip, startDate, numYears, freq, navTable)

startDates <- seq.Date(startDate, startDate + 365 * 7, "day")
irr3yr <- sapply(startDates, getSipIrr, sip=sip, numYears=3, 
    freq=freq, navTable=navTable)
    
startDates <- seq.Date(startDate, startDate + 365 * 6, "day")
irr4yr <- sapply(startDates, getSipIrr, sip=sip, numYears=4, 
    freq=freq, navTable=navTable)

startDates <- seq.Date(startDate, startDate + 365 * 5, "day")
irr5yr <- sapply(startDates, getSipIrr, sip=sip, numYears=5, 
    freq=freq, navTable=navTable)

startDates <- seq.Date(startDate, startDate + 365 * 3, "day")
irr7yr <- sapply(startDates, getSipIrr, sip=sip, numYears=7, 
    freq=freq, navTable=navTable)

#------------------------------------------------------------------------------#
# Lumpsum IRR calculation across different mutual funds
startDates <- seq.Date(startDate, startDate + 365 * 7, "day")
lumpsumIrr3yr <- sapply(startDates, getLumpsumIrr, invYears=3, navTable=navTable)
summary(lumpsumIrr3yr)

startDates <- seq.Date(startDate, startDate + 365 * 7, "day")
lumpsumIrr3yrBfe <- sapply(startDates, getLumpsumIrr, invYears=3, navTable=navTableBfe)
summary(lumpsumIrr3yrBfe)

startDates <- seq.Date(startDate, startDate + 365 * 7, "day")
lumpsumIrr3yrFib <- sapply(startDates, getLumpsumIrr, invYears=3, navTable=navTableFib)
summary(lumpsumIrr3yrFib)

# Lumpsum IRR calculation across different time periods
startDates <- seq.Date(startDate, startDate + 365 * 6, "day")
lumpsumIrr4yr <- sapply(startDates, getLumpsumIrr, invYears=4, navTable=navTable)
summary(lumpsumIrr4yr)

startDates <- seq.Date(startDate, startDate + 365 * 5, "day")
lumpsumIrr5yr <- sapply(startDates, getLumpsumIrr, invYears=5, navTable=navTable)
summary(lumpsumIrr5yr)

startDates <- seq.Date(startDate, startDate + 365 * 3, "day")
lumpsumIrr7yr <- sapply(startDates, getLumpsumIrr, invYears=7, navTable=navTable)
summary(lumpsumIrr7yr)
#------------------------------------------------------------------------------#
# Graphing CAGR of UTI Equity fund across years     
plot(ecdf(lumpsumIrr3yr), col="red", main="Lumpsum Return Distribution", 
	xlab="CAGR", ylab="Cumulative Frequency", lwd=3)
lines(ecdf(lumpsumIrr5yr), col="green", lwd=3)
lines(ecdf(lumpsumIrr7yr), col="blue", lwd=3)
abline(h=seq(0, 1, 0.05), col="lightgray")
abline(v=seq(-0.1, 0.35, 0.05), col="lightgray")
legend("topleft", legend = c("3 Year", "5 Year", "7 Year"), 
	col = c("red", "green", "blue"), lty = 1, merge = TRUE)   
#------------------------------------------------------------------------------#    
# Graphing CAGR of UTI Equity fund across years (using ggplot and plotly)     
returnDt <- data.table(Lumpsum3Yr = lumpsumIrr3yr, Lumpsum5Yr = lumpsumIrr5yr,
    Lumpsum7Yr = lumpsumIrr7yr)
gg <- ggplot(returnDt, aes(x = Lumpsum3Yr, color="red")) + 
  stat_ecdf(geom = "line") + 
  stat_ecdf(aes(x=Lumpsum5Yr), color="green", geom = "line") +
  stat_ecdf(aes(x=Lumpsum7Yr), color="blue", geom = "line")   
ggplotly(gg)

#------------------------------------------------------------------------------#
# Graphing 3 year CAGR across mutual funds     
plot(ecdf(lumpsumIrr3yr), col="red", main="3 Year Lumpsum Return Distribution", 
	xlab="CAGR", ylab="Cumulative Frequency", lwd=3)
lines(ecdf(lumpsumIrr3yrBfe), col="green", lwd=3)
lines(ecdf(lumpsumIrr3yrFib), col="blue", lwd=3)
abline(h=seq(0, 1, 0.05), col="lightgray")
abline(v=seq(-0.1, 0.35, 0.05), col="lightgray")
legend("topleft", 
    legend = c("UTI Equity", "BSL Frontline Equity", "Franklin India Bluechip"), 
	col = c("red", "green", "blue"), lty = 1, merge = TRUE)   
    
#------------------------------------------------------------------------------#
# Comparing the equity and fixed income returns
completeNavs <- fillMissingNavsFast(navTable)

# Using the NAVs for the equity investments    
# Normalizing it as Rs 10000 investment
equityEqv <- completeNavs$NetAssetValue * 10000 / completeNavs$NetAssetValue[1]
# Assuming a constant 8% return over the years
fixedIncomeEqv <- 10000 * (1.00022^(0:(nrow(completeNavs)-1)))

# Usual Plot
plot(equityEqv, col="green", type="l")
lines(fixedIncomeEqv, col="red")
legend("topleft", legend = c("Equity", "8% Bond"), 
	col = c("green", "red"), lty = 1, merge = TRUE)   

# Log Plot
plot(log(equityEqv), col="green", type="l")
lines(log(fixedIncomeEqv), col="red")
legend("topleft", legend = c("Equity", "8% Bond"), 
	col = c("green", "red"), lty = 1, merge = TRUE)   

  