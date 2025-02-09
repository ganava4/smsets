## code to prepare `skulls` dataset goes here
skulls <- read.csv("../Egyptian_skulls.csv", header = TRUE,
                   stringsAsFactors = TRUE)
skulls$Maximum_breadth <- as.numeric(skulls$Maximum_breadth)
skulls$Basibregmatic_height <- as.numeric(skulls$Basibregmatic_height)
skulls$Basialveolar_length <- as.numeric(skulls$Basialveolar_length)
skulls$Nasal_height <- as.numeric(skulls$Nasal_height)
usethis::use_data(skulls, overwrite = TRUE)
