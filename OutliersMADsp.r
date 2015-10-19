# Check for outliers:
OutliersMADsp     <- function(vec, range = 3){
  print("----------------------------------------------------------------")
  print(substitute(vec))
  print("Before imputation: ")
  print(describe(vec))
  w0           = which(vec==0)
  vec[w0]      = NA # zeros and NA are excluded from imputation              
  med          = median(vec, na.rm = TRUE)
  high         = med + range*1.48*mad(vec,na.rm = T)
  low          = med - range*1.48*mad(vec,na.rm = T)
  ind          = which(vec <= low | vec >= high)
  vec[ind]     = med
  print(paste0("Number of imputed observations: ", length(ind)))
  vec[w0]      = 0 # put zeros back into vec
  print("After imputation:")
  print(describe(vec)) 
 return(vec)
}
