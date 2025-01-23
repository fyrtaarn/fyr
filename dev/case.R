#' Find cases
#'
#' Identify cases from FMDS dataset. Three possible methods is used:
#' 1. Keep only those that are found in FMDS and those found in both FMDS and Somatic when the date and health institution are matched
#' 2. If multiple registries in FMDS with similar dates and health institution, then keep the earlier date
#' 3. If mulitple registries in FMDS with same date but different health institutions, check with Somatic data. Matching
#'    health institution will be kept.
#'
#' @param d1 Dataset for FMDS ie. external explanation for injury
#' @param d2 Dataset for NPR ie. intery registration for injury
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param filter Column name for filtering inclusion and exclusion ie. `is.na(filter)`
#' @param days A selected time period to consider as similar injury eg. 3 days
#' @param verbose Keep variables for making the selection ie. xx.[var_name] to produce DELXX
#' @param clean Delete all the possible duplicated cases
#' @return A dataset with a DELXX column to indicate non-cases with value 1 if argument `clean = FALSE`. But if `verbose = TRUE`
#' the output will include all variables needed to create DELXX.
#' @examples
#' \dontrun{
#'  dd <- find_case(d1, d2)
#' }
#' @import data.table
#' @export
find_case <- function(d1, d2,
                      id = "lopenr",
                      skade = "skadeDato",
                      rhf = "helseforetak_nr",
                      filter = NULL,
                      days = 3,
                      verbose = FALSE,
                      clean = FALSE){

  if (clean)
    verbose = FALSE

  if (days == 0){
    d <- is_same_dates(d1, d2)
  } else {
    d <- is_dup_rhf(d = d1,
                    id = id,
                    skade = skade,
                    rhf = rhf,
                    suffix = days)

    d <- is_rhf(d1 = d,
                d2 = d2,
                id = id,
                skade = skade,
                rhf = rhf,
                filter = filter,
                days = days)
  }

  if (verbose){
    return(d)
  } else {
    d[, DELXX := NA_integer_]

    delCols <- grep("^xx.DEL", names(d), value = TRUE)

    for (i in delCols){
      d[, DELXX := data.table::fifelse(is.na(DELXX), i, DELXX), env = list(i = i)]
    }

    xxCols <- grep("^xx.*", names(d), value = TRUE)
    d[, (xxCols) := NULL]
  }

  if (clean){
    indx <- d[DELXX == 1, which = TRUE]
    d <- is_delete_index(d, indx)
    d[, DELXX := NULL]
  }

  return(d[])
}


#' @keywords internal
#' @title Duplicated cases from similar health institution
#' @description Identify cases with similar date of injury and if found,
#' check if they are registered from the same health institutions.
#' Keep the first entry only with similar health institutions.
#' @param d Dataset
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param suffix Surfix to be added to colum names for filtering eg. xx.var1
is_dup_rhf <- function(d, id, skade, rhf, suffix = 1){

  d <- data.table::copy(d)

  data.table::setkeyv(d, c(id, skade, "skadeTid" ))
  xdate <- paste0("xx.date", suffix)
  d[, (xdate) := .N, by = c(id, skade)]

  xrhf <- paste0("xx.rhf", suffix)
  d[xdate > 1, (xrhf) := .N, by = .(id, rhf),
    env = list(xdate = xdate, id = id, rhf = rhf)]

  xdel <- paste0("xx.DEL", suffix)
  dupRows <- duplicated(d, by = c(id, skade, rhf))
  d[dupRows, (xdel) := 1L][]
}


#' @keywords internal
#' @title Duplicated with different health institutions
#' @description Identify cases with similar date of injury but has different health institutions.
#' Need to check with injury register to identify which health institutions
#' the patients were registered to in NPR. If the health institution in FMDS isn't
#' similar to those registered in NPR (entry point) within a selected time period
#' then it's considered double registery. Double registery is marked with 1 in column "xx.DEL"
#' @param d1 Dataset for FMDS ie. external explanation for injury
#' @param d2 Dataset for NPR ie. intery registration for injury
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param filter Column name for filtering inclusion and exclusion ie. `is.na(filter)`
#' @param days A selected time period to consider as similar injury eg. 3 days
is_rhf <- function(d1, d2, id, skade, rhf, filter = NULL , days = 3){

  if (is.null(filter))
    filter <- grep("xx.DEL", names(d1), value = TRUE)

  data.table::setkeyv(d2, c(id, "innDato"))

  dx <- d1[!is.na(filter), env = list(filter = filter)]
  d <- d1[is.na(filter), env = list(filter = filter)]

  period <- as.integer(gsub("\\D", "", filter))
  if (is.integer(period)){
    sufx <- (period * 10) + period
  } else {
    sufx <- period
  }

  d <- is_dup_rhf(d, id, skade = skade, rhf = rhf, suffix = sufx)

  # Similar dates per lopenr ie. value of 2 or more in date2 > 2
  date2 <- paste0("xx.date", sufx)
  xDato <- d[!duplicated(id) & date2 > 1,
             .(dateFrom = skade, dateTo = data.table::as.IDate(skade) + days), by = id,
             env = list(id = id, date2 = date2, skade = skade, days = days)]

  cols <- c(id, rhf)
  vecRFH <- vector(mode = "list", length = nrow(xDato))

  for (i in seq_len(nrow(xDato))){

    idx <- xDato[[id]][i]
    dateFrom <- xDato[["dateFrom"]][i]
    dateTo <- xDato[["dateTo"]][i]

    ## include hoveddiagnoser? only similar diagnoser ie. first 3 codes, is considered as similar injury?
    x <- d2[lopenr == idx & innDato %between% c(dateFrom, dateTo),
            .(id, rhf), env = list(id = id, rhf = rhf)]

    vecRFH[[i]] <- x
  }

  dtRHF <- data.table::rbindlist(vecRFH)

  colDate <- c("dateFrom", rhf)
  dtRHF[xDato, on = id, (colDate) := mget(colDate), env = list(id = id)]

  sufDel <- paste0("xx.DEL", sufx)
  idVec <- unique(dtRHF[[id]])
  for (i in idVec){
    datoF <- xDato[id == i, env = list(id = id)][["dateFrom"]]
    datoT <- xDato[id == i, env = list(id = id)][["dateTo"]]
    rhfx <- dtRHF[id == i, env = list(id = id)][[rhf]]

    ## Identify RHF i FMDS that aren't in somatic within the same selected period
    d[id == i & skade %between% c(datoF, datoT) & !(rhf %in% rhfx), (sufDel) := 1L,
      env = list(id = id,skade = skade, rhf = rhf)]
  }

  DT <- data.table::rbindlist(list(dx, d), fill = TRUE)
  data.table::setkeyv(DT, c(id, skade, "skadeTid"))
  return(DT)
}


