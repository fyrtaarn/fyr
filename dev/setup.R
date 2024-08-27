
# usethis::create_package("fyrtaarn")

devtools::load_all()
roxygen2::roxygenise(clean = TRUE)
devtools::document()
devtools::check()


# #Run once to configure your package to use pkgdown
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
som <- fread(paste0(pth, "/Data/02_extracted/NPR20240711/24_01903_som_til_utlevering.csv"), encoding = "Latin-1")

# Duplikater ---------
dim(som)
som[duplicated(som), .N]
dt1 <- som[!duplicated(som)]
dim(dt1)
som[, yr := year(innDato)]
dt <- som[yr == 2024]
