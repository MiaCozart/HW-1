library(devtools)
library(roxygen2)

find_rtools()

setwd("~/MyPackages/ForClass")

create("Bio442", 
       rstudio = FALSE, 
       roxygen = TRUE, 
       open = FALSE)

setwd("Bio442")

document()

setwd("..")

install("Bio442")

library(Bio442)

raw_data <- scan("C:/Users/Vivienne/Desktop/Bio 442/Code_snippets/demo_data.csv", "character")

fastdemo <- better_split(raw_data, "[0-9.]+")


use_vignette("Bio442vignette")

use_data(raw_data)



