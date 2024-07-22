
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


# Encoding solution with some modification from
# https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
is_encode <- function(x) gsub("\\u00c3\\u00a6|\xe6", "\u00e6", useBytes = TRUE,
                              gsub("\\u00c3\\u00a5|\xe5", "\u00e5", useBytes = TRUE,
                                   gsub("\\u00c3\\u00b8|\xf8", "\u00f8", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "\u00c5", useBytes = TRUE,
                                                  gsub("\xd8", "\u00d8", x, useBytes = TRUE))))))
