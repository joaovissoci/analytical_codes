install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
devtools::install_github("jcheng5/bubbles")
devtools::install_github("hadley/shinySignals")
devtools::install_github(c('yihui/knitr', 'rstudio/flexdashboard'))
devtools::install_github("rstudio/rmarkdown")


rmarkdown::run("/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/US_SAP_faculty_dashboard.rmd")

rsconnect::deployApp('/Users/joaovissoci/Git/analytical_codes/Duke_DGNN/US_SAP_faculty_dashboard.rmd')