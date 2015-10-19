# Outliers LogMAD
OutliersLogMAD <- function(l){
  l <- log(l)
  med = median(l,na.rm = T)
  Low = med - 3*1.48*mad(l,na.rm = T)
  Up = med + 3*1.48*mad(l,na.rm = T)
  l[which(l < Low | l > Up)] = med
  l <- exp(l)
  return(l)
}