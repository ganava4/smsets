## code to prepare `sparrows` dataset goes here
sparrows <- read.csv("../Bumpus_sparrows.csv", header=TRUE,
                     stringsAsFactors = TRUE)
sparrows$Total_length <- as.numeric(sparrows$Total_length)
sparrows$Alar_extent <- as.numeric(sparrows$Alar_extent)
usethis::use_data(sparrows, overwrite = TRUE)

