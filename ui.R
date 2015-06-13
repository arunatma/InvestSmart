library(shiny)
library(rCharts)

shinyUI(pageWithSidebar(
    headerPanel('Investment IRR'),
    sidebarPanel(
        selectInput('instrument', 'Instrument:', c('Sensex')),
        radioButtons('invType', 'Investment type:',
            c('Lump Sum' = 'single',
               'Systematic (SIP)' = 'sip',
               'Systematic (SWP)' = 'swp'), selected = 'sip'),
        textInput('invAmount', 'Investment Amount', value = 1000),
        conditionalPanel(
            condition = "input.invType == 'single' || input.invType == 'swp'",
            dateInput('invDate', 'Investment Date:', value = '2011-01-01',
                min = '1990-01-01', max = '2015-03-12')
        ),
        conditionalPanel(
            condition = "input.invType == 'sip' || input.invType == 'swp'",
            wellPanel(
                conditionalPanel(
                    condition = "input.invType == 'swp'",
                    textInput('wdwAmount', 'Withdrawal Amount', value = 100)
                ),
                selectInput('freq', 'Frequency', 
                    c('Daily' = 'day', 'Weekly' = 'week', 'Monthly' = 'month',
                    'Quarterly' = 'quarter', 'Yearly' = 'year'), selected='month'),
                dateRangeInput('dateRange', 'Start and Stop Dates:', 
                    min = '1990-01-01', max = '2015-03-12', 
                    start = '2005-01-01', end = '2015-01-20'),
                conditionalPanel(
                    condition = "input.invType == 'swp'",
                    selectInput('reinv', 'Reinvestment Rate:', 
                        c('Same as IRR' = 'same', 'Different' = 'diff')),
                    conditionalPanel(
                        condition = "input.reinv == 'diff'",
                        textInput('reinvRate', 'In (%)', value = "8")
                    )
                )
            )
        ),
        dateInput('redeemDate', 'Redeem Date:', value = '2015-01-01')
        
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel('Transaction', 
                br(),   wellPanel(h4(textOutput("irr")),
                            br(), h4(textOutput("redm"))
                        ),
                br(), dataTableOutput("results")
            ),
            tabPanel("Charting", 
                br(), showOutput('invChart', 'highcharts')
            ),
            tabPanel('Historical NAV', 
                br(), showOutput('navChart', 'nvd3'),
                br(), dataTableOutput('navDf')
            )
        )
    )
    
))