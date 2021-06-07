install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
devtools::install_github("jcheng5/bubbles")
devtools::install_github("hadley/shinySignals")
devtools::install_github(c('yihui/knitr', 'rstudio/flexdashboard'))
devtools::install_github("rstudio/rmarkdown")
install.packages('rsconnect')
install.packages("leaflet")
devtools::install_github("mtennekes/tmaptools")

rmarkdown::run("/Users/Joao/Desktop/v2_US_SAP_residents_dashboard.rmd")

1library(rsconnect)
rsconnect::setAccountInfo(name='joaovissoci', token='9CA50F37220AE6294E1A9F88B8FD8DAA', secret='LKNaDGp8omkqoaMF9597faKYo9KCGCzF7ZJgW0lJ')
rsconnect::deployApp('/Users/Joao/Desktop/v2_US_SAP_residents_dashboard.rmd')

library(flexdashboard)

rmarkdown::run("/Users/joaovissoci/Downloads/index_ptb_ufma_v2.5.Rmd")

rsconnect::deployApp('/Users/Joao/Downloads/index_ptb_ufma.Rmd')


install.packages("car")