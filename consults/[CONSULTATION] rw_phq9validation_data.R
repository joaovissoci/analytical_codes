install.packages("jsonlite")
library(jsonlite)
file <- file("/Users/joaovissoci/Downloads/exported_data.txt",
             open="r",
             encoding="UTF-8-BOM")

df <- read.delim(file=file,
                 header=FALSE,
                 fill=FALSE,
                 stringsAsFactors=FALSE,
                 na.strings=c("NULL", ""),
                 quote="",
                 colClasses=c("integer", "character", "factor", "POSIXct", "POSIXct", "integer", "integer", "character"))
close(file)
rm(file)

dataTransform <- function(df) {
  # Parses JSON string into data frames
  json_data <- lapply(df$V8, fromJSON)
  
  # Transpose data frames, dropping all columns except label and value
  json_data <- lapply(json_data, function(df) {
    l <- lapply(df$value, function(x) if (length(x) != 1) I(list(x)) else x)
    names(l) <- trimws(df$label)
    data.frame(l, stringsAsFactors = FALSE)
  })

  # All one-line data frames must have the same columns
  labels <- unique(c(sapply(json_data, colnames)))
  for (i in seq_along(json_data)) {
    json_data[[i]][setdiff(labels, colnames(json_data[[i]]))] <- NA
  }
  
  # Binds rows from all one-line data frames
  json_data <- do.call(rbind, json_data)
  
  # Binds columns back to original data frame
  df_complete <- cbind(df, json_data)
}

df_hdrs <- dataTransform(df[df$V3 == "HDRS",])
df_phq9 <- dataTransform(df[df$V3 == "PHQ9",])
df_questionnaire <- dataTransform(df[df$V3 == "Questionnaire",])
df_short_questionnaire <- dataTransform(df[df$V3 == "ShortQuestionnaire",])
rm(dataTransform)

