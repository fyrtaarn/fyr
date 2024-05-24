# usethis::create_package("fyrtaarn")
usethis::use_r("case.R")
usethis::use_r("utils.R")

devtools::load_all()
roxygen2::roxygenise(clean = TRUE)
devtools::document()
devtools::check()



pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)

usethis::use_package("data.table", min_version = TRUE)

usethis::use_build_ignore("dev")
