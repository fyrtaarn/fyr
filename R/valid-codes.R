#' @title Get valid codes
#'
#' @description Select codes from column consisting ICD-10 codes that meet the requirement for injury codes ie. between S00 to T78
#' @param d Dataset
#' @param select.col Columnames from dataset with ICD codes to be controlled for. Default is `hoveddiagnoser`
#' @param create.col New columnames consisting logical value to indicate that at least one
#' of these codes S00 to T78 exists ie. TRUE means one or more of the codes in `select.col`
#' is between S00 to T78. Default is `hovdiag`
#' @param split Symbols for splitting the codes when there are multiple codes in the column. Default is whitespace ie. `" "`
#' @param keep Split multiple codes and keep in its own column. Default is FALSE.
#' @return Original column will have suffix `*.old`.
#' @examples
#' \dontrun{
#' d1 <- get_valid_codes(dt = dd, "hoveddiagnoser", "hovdiag")
#' }
#' @export

get_valid_codes <- function(d, select.col = "hoveddiagnoser", create.col = "hovdiag", split = " ", keep = FALSE){

  col1 <- colnr <- lnr <- old <- NULL
  dx <- data.table::copy(d)

  # Linenumber is needed for counting ICD codes by line
  if (!("lnr" %in% names(dx))){
    dx[, lnr := 1:.N] # linenumber
  }

  # Column can have multiple codes. Split them so each column has single code
  dx[, colnr := length(unlist(strsplit(x = col1, split = split))), by = lnr, env = list(col1 = select.col)]
  cols <- paste0("icd_", 1:max(dx$colnr))
  dx[, (cols) := data.table::tstrsplit(x = col1, " "), env = list(col1 = select.col)]

  # Trim codes to keep only the first 3 digits
  for (j in cols){
    if(is.character(dx[[j]]))
      data.table::set(dx, j = j, value = substr(dx[[j]], 1, 3))
  }

  tempCol <- paste0(select.col, "01")
  dx[, (tempCol) := do.call(paste, c(replace(.SD, is.na(.SD), ""), sep = " ")), .SDcols = cols]

  # Select only these codes S00 til T78
  codeURL <- system.file("icd", "validCodes.RDS", package = "fyr")
  codes <- readRDS(codeURL)

  # Keep the codes as it is
  if (!keep){
    for (j in cols){
      if (is.character(dx[[j]]))
        data.table::set(dx, j = j, value = dx[[j]] %chin% codes)
    }

    dx[ , (create.col) := rowSums(.SD) > 0, .SDcols = cols]
  }

  oldCol <- paste0(select.col, ".old")
  data.table::setnames(dx, old = c(select.col, tempCol), new = c(oldCol, select.col))
  dx[, (select.col) := trimws(col, which = "right"), env = list(col = select.col)]
  dx[col == "" | is.na(old), (select.col) := NA, env = list(col = select.col, old = oldCol)]

  xcols <- c("colnr", "lnr", cols)

  if (keep)
    xcols <- c("colnr", "lnr")

  dx[, (xcols) := NULL][]
}
