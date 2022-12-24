ui = navbarPage(theme = shinytheme("spacelab"),
                title = "Stock Monitor",
                introjsUI(),
                tabPanel(title = "View Stocks", 
                         sidebarLayout(
                           sidebarPanel(h3(strong("Input Your Stock")), 
                                        
                                        helpText("Input a stock and specify a date range to examine it upon. All data is taken from Yahoo Finance."),
                                        
                                        actionButton("help", 
                                                     "About this Page",
                                                     style = "padding:6px; font-size:80%"),
                                        
                                        hr(),
                                        
                                        introBox(textInput("symbol",
                                                           label = "Input stock symbol",
                                                           value = "AAPL"), 
                                                 data.step = 1, 
                                                 data.intro = "Enter the stock symbol here. A full list of stock symbols can be found at https://www.nasdaq.com/market-activity/stocks/screener."),
                                        
                                        dateRangeInput("dateRange", 
                                                       label = "Select date range",
                                                       start = as.character(today() - months(12)), 
                                                       end = as.character(Sys.Date())),
                                        
                                        introBox(checkboxInput("candlestick", 
                                                      label = "Display as candlestick plot",
                                                      value = F), 
                                                 data.step = 2, 
                                                 data.intro = "Candlestick plots should mainly be used when looking at short time frames (i.e., days, not years). A green candlestick means that the stock's price increased during the day, and an orange candlestick means that the price decreased."),
                                        ), 
                           mainPanel(
                             introBox(plotOutput("plot", height = 550) %>% 
                                        withSpinner(color = "#08361e"), 
                                      data.step = 3,
                                      data.intro = "The upper portion of the plot is the price chart, which displays the closing values of the chosen stock. The lower portion is the volume chart, which indicates the volume of shares traded. A green volume bar means that the stock closed at a higher value than it did the previous day, while an orange bar means the opposite."))
                         )), 
                
                tabPanel(title = "Analyse Stocks",
                         sidebarLayout(
                           sidebarPanel(h3(strong("Examining Multiple Stocks")),
                                        helpText("Select up to 7 stocks to look at simultneously. The options provided below are the 100 largest companies ranked by market capacity."),
                                        helpText("Please be patient as the graph may take several seconds to load."),
                                        hr(),
                                        tags$head(tags$style(HTML(my_css))),
                                        pickerInput("multStocks", 
                                                    choices = symbs, 
                                                    multiple = T, 
                                                    selected = c(symbs[1:7]), 
                                                    options = pickerOptions(
                                                      actionsBox = TRUE,
                                                      maxOptions = 7,
                                                      maxOptionsText = "You have selected the maximum number of options",
                                                      noneSelectedText = "Please select at least one stock",
                                                      liveSearch = T, 
                                                      liveSearchNormalize = T
                                                    )), 
                                        radioButtons("timeFrame", 
                                                     label = "Choose a time frame", 
                                                     choices = c("1 month" = 1, 
                                                                 "3 months" = 2, 
                                                                 "6 months" = 3, 
                                                                 "12 months" = 4, 
                                                                 "This year only" = 5), 
                                                     selected = 4)), 
                           mainPanel(
                             helpText("This is an interactive plot - you can hover over each line for detailed closing values, for example. Other functions can be found in the menu above the plot."),
                             plotlyOutput("multiPlot", height = 550) %>%
                               withSpinner(color = "#08361e"))
                         ))
)
