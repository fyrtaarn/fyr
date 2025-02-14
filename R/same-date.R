#' @title Same date
#' @description Keeping cases with similar date and health institution in FMDS and Somatic data only.
#' Nevertheless, patients registered in FMDS that aren't found in Somatic data will be kept.
#' @param d1 FMDS dataset
#' @param d2 Somatic dataset
#' @param key1 Columnames in `d1` for sorting the data. Default are `lopenr` and `skadeDato`
#' @param key2 Columnames in `d2` for sorting the data. Default are `lopenr` and `innDato`
#' @export
is_same_dates <- function(d1, d2,
                          key1 = c("lopenr", "skadeDato"),
                          key2 = c("lopenr", "innDato")){

  lopenr <- xx.fmds <- helseforetak_nr <- i.helseforetak_nr <- NULL

  d1 <- data.table::copy(d1)
  data.table::setkeyv(d1, key1 )
  data.table::setkeyv(d2, key2)

  lnr <- "lineNo"
  # Needs linenumber to select cases
  if (!any(names(d1) == lnr)){
    d1[, (lnr) := 1:.N]
  }

  # Identify FMDS patients that aren't in somatic dataset
  idSom <- d2[!duplicated(lopenr)][["lopenr"]]
  d1[!(lopenr %in% idSom), "xx.fmds" := 1L]
  dx <- d1[xx.fmds == 1L]
  d1 <- d1[is.na(xx.fmds)]

  selx <- d2[d1, nomatch = 0][helseforetak_nr == i.helseforetak_nr][[lnr]]
  d1[!( lnr %in% selx ), "xx.DEL0" := 1L, by = lnr, env = list(lnr = lnr)]

  d1 <- data.table::rbindlist(list(d1, dx), fill = TRUE)
  d1[, (lnr) := NULL]
  data.table::setkeyv(d1, key1)
}
