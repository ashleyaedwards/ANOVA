#this function provides all pairwise comparisons for a mixed (between and within) ANOVA and provides the Tukey critical value (cv) to correct for multiple comparisons
#requires library(ez)
Mixedallpairwise = function(data, dv, within, between, id){
  cat("Group Means:\n")
  d = data.frame(id = data[,id], dv = data[,dv], within = data[,within], between = data[,between])
  print(aggregate(dv~within, d, mean))
  a = aggregate(dv~within, d, mean)
  pairs = combn(levels(d$within), 2)
  for(i in 1:ncol(pairs)){
    new = d[d$within==pairs[1,i]|d$within==pairs[2,i],]
    colnames(new) = c("id", "dv", "within", "between")
    library(ez)
    cat("\nNext Comparison:")
    cat(pairs[,i])
    est = a[a$within==pairs[1,i],2] -  a[a$within==pairs[2,i],2]
    cat("\nContrast Estimate:")
    cat(est)
    print(ezANOVA(data = new, dv, as.factor(id), within, between = between, type = 3)) 
    dfd = unlist(summary(aov(dv~within*between+Error(as.factor(id)/within), new)))["Error: as.factor(id):within.Df3"][[1]]
    tukey = (qtukey(.95, nlevels(data[,within]), dfd))^2/2
    cat("\ntukey cv:")
    cat(tukey)
    cat("\n")
  }
}


#example:
#Mixedallpairwise(long,"value","variable","Group","Subject")