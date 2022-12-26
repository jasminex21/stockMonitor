############
###Server###
############

server = function(input, output, session) {
  observeEvent(input$help,
               introjs(session, options = list("showBullets" = "false", 
                                               "showProgress" = "true", 
                                               "showStepNumbers" = "false",
                                               "nextLabel" = "Next",
                                               "prevLabel" = "Prev",
                                               "skipLabel" = "Skip")))

  tabOne = reactive({
    validate(
      need(input$symbol != "", "Please enter a stock symbol. A full list of symbols can be found at https://www.nasdaq.com/market-activity/stocks/screener"))
    getSymbols(input$symbol, 
               src = "yahoo",
               from = input$dateRange[1], 
               to = input$dateRange[2],
               auto.assign = F)
    
  })
  output$plot = renderPlot({
    chartSeries(tabOne(),
                name = input$symbol,
                theme = "white", 
                TA = gsub(",", ";", toString(c("addVo()", input$indicators))),
                type = if (input$candlestick) {
                  "candlestick"
                } else { "line" })
  })
  
  observeEvent(c(input$multStocks, input$timeFrame), { 
    validate(
      need(input$multStocks != "", "Please select at least one stock to examine"))
    # Only keeping the symbol, removing the full company name
    selections = sub(" .*", "", input$multStocks)
    # Acquiring all data for the selected stocks
    prices = tq_get(selections, 
                    get = "stock.prices") %>%
      select(symbol, date, close)
    
    if (input$timeFrame == 1) {
      prices = prices %>% filter(date >= today() - months(1))
    }
    
    if (input$timeFrame == 2) {
      prices = prices %>% filter(date >= today() - months(3))
    }
    
    if (input$timeFrame == 3) {
      prices = prices %>% filter(date >= today() - months(6))
    }
    
    if (input$timeFrame == 4) {
      prices = prices %>% filter(date >= today() - months(12))
    }
    
    if (input$timeFrame == 5) {
      prices = prices %>% filter(year(date) == year(today()))
    }
    
    output$multiPlot = renderPlotly({
      print(
        ggplotly(prices %>% 
                   group_by(symbol) %>%
                   ggplot(aes(x = date, 
                              y = close, 
                              color = symbol)) + 
                   geom_line(size = 1, 
                             alpha = 0.9))
      )
    })
  })
}
