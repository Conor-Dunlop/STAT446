raw_data <- readLines("bank-additional-full.csv")
cleaned_data <- gsub('""', '"', raw_data)
cleaned_data <- gsub('"', '', cleaned_data)
writeLines(cleaned_data, "cleaned_file_full.csv")
