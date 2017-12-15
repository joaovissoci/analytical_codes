install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit"))
devtools::install_github("jcheng5/bubbles")
devtools::install_github("hadley/shinySignals")
install.packages("flexdashboard")

rmarkdown::run("/Users/Joao/Git/analytical_codes/Duke_DGNN/US_SAP_faculty_dashboard.rmd")
