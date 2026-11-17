# Render the HiQBioDiv SDM results book.
# Run from the repository root:
# source("render_book.R")

source(file.path("R", "create_species_accounts.R"))

# Synchronize centrally controlled account structure while preserving
# all contributor-written USER blocks.
sync_species_accounts()

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

setwd("book")
bookdown::render_book("index.Rmd")
