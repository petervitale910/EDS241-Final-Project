# Run all merge codes ---------------------------------------------------------------------------------------

source("code/0_setup/02_merge/02b_merge_scripts/02_01_merge_county_year.R",local = knitr::knit_global())

source("code/0_setup/02_merge/02b_merge_scripts/02_02_merge_zip_year.R",local = knitr::knit_global())

source("code/0_setup/02_merge/02b_merge_scripts/02_03_merge_bigcell_year.R",local = knitr::knit_global())
source("code/0_setup/02_merge/02b_merge_scripts/02_04_merge_bigcell_quarter.R",local = knitr::knit_global())
source("code/0_setup/02_merge/02b_merge_scripts/02_05_merge_bigcell_month.R",local = knitr::knit_global())

source("code/0_setup/02_merge/02b_merge_scripts/02_06_merge_bigcell_year_subsets.R",local = knitr::knit_global())

source("code/0_setup/02_merge/02b_merge_scripts/02_07_merge_cell_year.R",local = knitr::knit_global())
source("code/0_setup/02_merge/02b_merge_scripts/02_08_merge_cell_quarter.R",local = knitr::knit_global())

