library(ref2014)
library(readxl)

# REF 2014 results
results_file <- download_REF('results')
results <- read_xlsx(results_file, skip = 7, guess_max = 1e5, na = '-')
usethis::use_data(results)

# REF 2014 'Research outputs' submissions
outputs_file <- download_REF('outputs')
outputs <- read_xlsx(outputs_file, skip = 4, guess_max = 1e5, na = 'n/a')
usethis::use_data(outputs)
