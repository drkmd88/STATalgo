# assume normally distributed
conf_cal <- function(dmin,se,alpha=0.05) {
  zscore <- -qnorm(alpha/2)
  m <- se*zscore
  print (m)
  ci <- c(dmin-m,dmin+m)
  return (ci)
}

conf_cal(0.03,0.013)
conf_cal(-0.5,0.21)
conf_cal(0.01,0.0045)
conf_cal(10,6.85)

# bonferroni can be too conservative
conf_cal(0.03,0.013,0.05/4)
conf_cal(-0.5,0.21,0.05/4)
conf_cal(0.01,0.0045,0.05/4)
conf_cal(10,6.85,0.05/4)
