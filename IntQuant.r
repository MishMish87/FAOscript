# Check for the outliers in Social Protection:
IntQuant <- function(t){
  med = median(t,na.rm = T)
  Quant <- quantile(t,probs=seq(0,1,0.25),names = F,na.rm = T)
  Q1 <- Quant[2]
  Q3 <- Quant[4]
  IQR <- Q3 - Q1
  t[which(t < Q1 - 3*IQR| t > Q3 + 3*IQR)] = med
  return(t)
}
