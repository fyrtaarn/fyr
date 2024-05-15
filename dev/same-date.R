#' @title Same date
#' @description Keeping cases with similar date and health institution in FMDS and Somatic data only.
#' Nevertheless, patients registered in FMDS that aren't found in Somatic data will be kept.
#' @param d1 FMDS dataset
#' @param d2 Somatic dataset
#' @export
is_same_dates <- function(d1, d2){

  keyFMDS <- c("lopenr", "skadeDato")
  keySOM <- c("lopenr", "innDato")

  d1 <- data.table::copy(d1)
  data.table::setkeyv(d1, keyFMDS )
  data.table::setkeyv(d2, keySOM)

  lnr <- "lineNo"
  # Needs linenumber to select cases
  if (!any(names(d1) == lnr)){
    d1[, (lnr) := 1:.N]
  }

  # Identify FMDS patients that aren't in somatic dataset
  idSom <- d2[!duplicated(lopenr)][["lopenr"]]
  d1[!(lopenr %in% idSom), xx.fmds := 1L]
  dx <- d1[xx.fmds == 1L]
  d1 <- d1[is.na(xx.fmds)]

  selx <- d2[d1, nomatch = 0][helseforetak_nr == i.helseforetak_nr][[lnr]]
  d1[!( lnr %in% selx ), xx.DEL0 := 1L, by = lnr, env = list(lnr = lnr)]

  d1 <- data.table::rbindlist(list(d1, dx), fill = TRUE)
  d1[, (lnr) := NULL]
  data.table::setkeyv(d1, keyFMDS)
}
