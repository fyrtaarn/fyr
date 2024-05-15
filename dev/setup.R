usethis::create_package("fyrtaarn")
usethis::use_r("case.R")

devtools::load_all()
roxygen2::roxygenise(clean = TRUE)
devtools::document()
devtools::check()



pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)

usethis::use_build_ignore("dev")
