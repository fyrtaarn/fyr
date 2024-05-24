## delidx <- dt[colName %in% selecedValues, which = TRUE]
## Ref https://github.com/Rdatatable/data.table/issues/635#issuecomment-261473829
is_delete_index <- function(dt, delidx){
  # delidx - Row index to be deleted
  keepIdx <- setdiff(dt[, .I], delidx)
  cols = names(dt)
  dtSub <- data.table::data.table(dt[[1]][keepIdx]) #subsetted table
  data.table::setnames(dtSub, cols[1])

  for (col in cols[2:length(cols)]){
    dtSub[, (col) := dt[[col]][keepIdx]]
    dt[, (col) := NULL]
  }

  return(dtSub)
}
