library(quantmod)
library(shiny)
library(rvest)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)
library(rintrojs)
library(shinycssloaders)

# Scraping stock symbols and company names
link = "https://companiesmarketcap.com/"
page = read_html(link)
symbs = html_text(html_elements(page, ".company-code"))
companies = stringr::str_trim(html_text(html_elements(page, ".company-name")))
for (i in 1:100) {
  symbs[i] = paste(symbs[i], " (", companies[i], ")", sep = "")
}

# Removing "select all" button and centering "deselect all" button
my_css = "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  left: 25%;
}"
