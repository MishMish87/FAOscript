# Check for outliers:
imputeout     <- function(vec, range = 3){
  print("----------------------------------------------------------------")
  print(substitute(vec))
  print("Before imputation: ")
  print(describe(vec))
  w0           = which(vec==0)
  vec[w0]      = NA # zeros and NA are excluded from imputation              
  med          = median(vec, na.rm = TRUE)
  sd           = sd(vec, na.rm = TRUE)
  high         = med + range*sd
  low          = med - range*sd
  ind          = which(vec <= low | vec >= high)
  vec[ind]     = med
  print(paste0("Number of imputed observations: ", length(ind)))
  vec[w0]      = 0 # put zeros back into vec
  print("After imputation:")
  print(describe(vec)) 
 return(vec)
}
