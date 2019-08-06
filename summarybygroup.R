#provides mean, standard deviation, and sample size by group
summarybygroup = function(dv, groupsinlist){
  m = aggregate(dv, by = groupsinlist, FUN = mean, na.rm=TRUE)
  s = aggregate(dv, by = groupsinlist, FUN = sd, na.rm=TRUE)
  count = function(x){length(x[!is.na(x)])}
  n = aggregate(dv, by = groupsinlist, FUN = count)
  d=cbind(m, s$x, n$x)
  colnames(d)[colnames(d)=="x"] <- "mean"
  colnames(d)[colnames(d)=="s$x"] <- "sd"
  colnames(d)[colnames(d)=="n$x"] <- "N"
  print(d)
}

#example:
#summarybygroup(data$reading, list(data$gender, data$grade))