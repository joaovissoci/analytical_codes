install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
devtools::install_github("jcheng5/bubbles")
devtools::install_github("hadley/shinySignals")
devtools::install_github(c('yihui/knitr', 'rstudio/flexdashboard'))
devtools::install_github("rstudio/rmarkdown")
install.packages('rsconnect')

rmarkdown::run("/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/v1_4_US_SAP_faculty_dashboard.rmd")

library(rsconnect)
rsconnect::deployApp('/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/v1_4_US_SAP_faculty_dashboard.rmd')

rsconnect::setAccountInfo(name='joaovissoci', token='9CA50F37220AE6294E1A9F88B8FD8DAA', secret='LKNaDGp8omkqoaMF9597faKYo9KCGCzF7ZJgW0lJ')

rmarkdown::run("/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/US_SAP_residents_dashboard.rmd")

rsconnect::deployApp('/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/US_SAP_residents_dashboard.rmd')
