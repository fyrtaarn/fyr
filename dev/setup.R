
# usethis::create_package("fyrtaarn")

devtools::load_all()
roxygen2::roxygenise(clean = TRUE)
devtools::document()
devtools::check()


#Run once to configure your package to use pkgdown
# usethis::use_pkgdown()

unloadNamespace("fyr")
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)

usethis::use_r("case.R")
usethis::use_r("utils.R")
usethis::use_package("knitr", "Suggests")
usethis::use_package("data.table", min_version = TRUE)

usethis::use_build_ignore("dev")



## Data
pth <- "f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_"
sm <- fread(paste0(pth, "/Data/02_extracted/NPR20240711/24_01903_som_til_utlevering.csv"), encoding = "Latin-1")
fm <- fread(paste0(pth, "/Data/02_extracted/NPR20240711/24_01903_fmds_til_utlevering.csv"), encoding = "Latin-1")

## Test data
fmd <- readRDS(file.path(pth, "Data/test/test_fmds.rds"))
som <- readRDS(file.path(pth, "Data/test/test_somatic.rds"))

som[, lopenr := substr(lopenr, nchar(lopenr)-3, nchar(lopenr))]
fmd[, lopenr := substr(lopenr, nchar(lopenr)-3, nchar(lopenr))]
## saveRDS(som, "inst//testdata//som.RDS")
## saveRDS(fmd, "inst//testdata//fmd.RDS")


## Duplikater ---------
dim(fmd)
[duplicated(), .N]
dt1 <- [!duplicated()]
dim(dt1)
som[, yr := year(innDato)]
dt <- som[yr == 2024]

## Testing
usethis::use_github()
usethis::use_github_action("test-coverage")

tinytest::run_test_dir()
tinytest::build_install_test()
