library(shiny)

compose <- function(f,g){
    return(function(...) f(g(...)))
}

toDouble = compose(as.double, as.character)
toDate = compose(as.Date(..., "%d-%b-%Y"), as.character)

divBy <- function (b){
    if (b == 0) return (0)
    else return (1 / b)
}

isEmpty <- function(x) {
    return(length(x) == 0)
}

getNav <- function(inDate, navDf, keyHeader, valueHeader){
    minDate = unlist(navDf[1, ][keyHeader])
    
    maxRow = dim(navDf)[1]
    maxDate = unlist(navDf[maxRow, ][keyHeader])
    
    curDate = inDate
    if (curDate < minDate){
        return(0)
        break
    }
    
    dateVector = unlist(navDf[keyHeader])
    rowNum = which(dateVector == curDate)
    while (isEmpty(rowNum)){
        if (curDate > maxDate){
            return(0)
            break
        }
        curDate = curDate + 1
        rowNum = which(dateVector == curDate)
    }
    
    return(unlist(navDf[rowNum[1], ][valueHeader]))
}

fvFun <- function(i, cf, n) sum(cf * (1 + i) ^ (n/365)) 

xirr <- function(cfs, redeemDate, allDates) { 
    numDays = as.double(redeemDate - allDates)
    return(uniroot(fvFun, c(-1, 10), cf = cfs, n = numDays)$root) 
}

getCfsIrr <- function(invAmt, wdwAmt, invType, invDate, startDate, endDate, 
                      freq, redeemDate, navTable, keyCol, valueCol, 
                      reinv, reinvRate){

    if(invType == 'single'){
        startDate <- invDate
        endDate <- invDate
    }
    
    if(invType == 'swp'){
        invDate <- as.Date(invDate, "%Y-%m-%d")
        invNav <- getNav(invDate, navDf = navTable, keyHeader = keyCol,
                    valueHeader = valueCol)
        invUnit <- invAmt * divBy(invNav)
        amt <- (-wdwAmt)
    }else {
        invUnit <- 0
        amt <- invAmt
    }
    
    startDate <- as.Date(startDate, "%Y-%m-%d")
    endDate <- as.Date(endDate, "%Y-%m-%d")
    redeemDate <- as.Date(redeemDate, "%Y-%m-%d")
    
    txnDates <- seq.Date(startDate, endDate, freq)
    txnNavs <- sapply(txnDates, getNav, navDf = navTable, keyHeader = keyCol,
                    valueHeader = valueCol)
    swips = rep(amt, length(txnNavs))
    txnUnits <- swips * unlist(Map(divBy, txnNavs))
    zeroAdjSips = txnNavs * txnUnits
    redeemNav <- getNav(redeemDate, navTable, keyCol, valueCol)
    redeemUnits <- invUnit + sum(txnUnits)
    redeemValue <- redeemUnits * redeemNav

    allDates    <- c(txnDates, redeemDate)
    txns        <- c(rep("Purchase", length(txnNavs)), "Redemption")
    navs        <- c(txnNavs, redeemNav)
    cfs         <- -c(zeroAdjSips, -redeemValue)
    units       <- c(txnUnits, -redeemUnits)
    cumulCost   <- c(cumsum(zeroAdjSips), 0)
    
    if (invType == 'swp'){
        allDates    <- c(invDate, allDates)
        txns        <- c('Purchase', rep('SWP', length(txnNavs)), 'Redemption')
        navs        <- c(invNav, navs)
        cfs         <- c(-invAmt , cfs)
        units       <- c(invUnit, units)
        cumulCost   <- rep(invAmt, length(units))
    }
    
    cumulUnits  <- cumsum(units)
    cumulValue  <- cumulUnits * navs
    
    cfsDf <- data.frame(Date = allDates, Transaction = txns, 
        NAV     = round(navs, 3),       NetCF  = round(cfs, 2), 
        Cost    = round(cumulCost, 2),  Units  = round(units, 3),      
        UnitsBalance = round(cumulUnits, 3),
        UnitsValue   = round(cumulValue, 2))
    
    if(reinv == 'same'){
        irr <- xirr(cfs, redeemDate, allDates)
    } else {
        numDays <- as.double(redeemDate - allDates[-1])
        cf <- cfs[-1]
        finalAmt <- fvFun((reinvRate/100), cf, numDays)
        
        twoDates <- c(invDate, redeemDate)
        scfs <- c(-invAmt, finalAmt)
        irr <- xirr(scfs, redeemDate, twoDates)
    }
    
    return(list (cfsDf = cfsDf, irr = irr))
}


shinyServer(function(input, output, session) {
    load("bse30.RData")
    
    startDate <- reactive(function(){
        as.character(input$dateRange[1])
    })
    
    endDate <- reactive(function(){
        as.character(input$dateRange[2])
    })
    
    invDate <- reactive(function(){
        as.character(input$invDate)
    })
     
    redeemDate <- reactive(function(){
        as.character(input$redeemDate)
    })
    
    cfsirr <- reactive(function(){
        getCfsIrr(as.double(input$invAmount), as.double(input$wdwAmount), 
            input$invType, invDate(), startDate(), endDate(), input$freq, 
            redeemDate(), bse30df, "Date", "Sensex", input$reinv, 
            as.double(input$reinvRate))
    })
    
    xdf <- reactive(function(){
        data.frame(Date = input$redeemDate, Transaction = "Purchase", 
            NAV = 32.5, Amount = 1000, Units = 1000/32.5, 
            UnitBalance = 1000/32.5, Cost = 1000, Valuation = 1000)
    })
    
    output$navDf <- renderDataTable(bse30df)
    output$results <- renderDataTable(cfsirr()$cfsDf)
    output$irr <- 
        renderText(paste0("IRR = ", round(cfsirr()$irr * 100, 3), " %"))
        
    output$navChart <- renderChart({
        plotOut <- nPlot(Sensex ~ Date, data = bse30df, type = "lineChart")
        plotOut$xAxis(tickFormat = "#!function(d) {
            return d3.time.format('%d %b %Y')(new Date( d * 86400000 ));}!#" )
        plotOut$xAxis(axisLabel = 'Date')
        plotOut$yAxis(axisLabel = 'Sensex', width=56)
		plotOut$addParams(dom = 'navChart')
        return(plotOut)
    })
    
    output$invChart <- renderChart({
        curDf = cfsirr()$cfsDf
        len = dim(curDf)[1] - 1 # Leave the last one
        # curDf$Date <- as.numeric(as.POSIXct(curDf$Date))*1000
        plotOut <- Highcharts$new()
        plotOut$chart(type = "line")
        plotOut$series(name = "Cost", 
            data = c(curDf$Cost[1:len], curDf$Cost[len]))
        #plotOut <- hPlot(Cost ~ Date, data = curDf, 
        #    type = "line", 
        #    radius=6)
            
        plotOut$series(name = "UnitsValue", 
            data = c(curDf$UnitsValue[1:len], curDf$NetCF[len+1]))
        
        plotOut$legend(enabled = TRUE, layout = "vertical", 
            align = "right", verticalAlign = "middle", borderWidth  = 1)
        
        plotOut$xAxis(title = "#!{text: 'Date'}!#")
        
        #plotOut$xAxis(type = 'datetime', 
        #    labels = list(format = '{value:%Y-%m-%d}'))
        
        plotOut$yAxis(title = "#!{text: 'Amount (in Rupees)'}!#")
        plotOut$plotOptions(line=list(marker=list(enabled = F)))
        
        plotOut$addParams(dom = 'invChart')
        
        return(plotOut)
    })

})
