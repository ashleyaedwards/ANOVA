#Test whether homogeneity of varience assumption is violated for each level of the within subjects variable in a mixed ANOVA design
#requires library(ez)
MixedHOV = function(data, dv, within, between, id){
  cat("Group Means:\n")
  d = data.frame(id = data[,id], dv = data[,dv], within = data[,within], between = data[,between])
  print(aggregate(dv~within, d, mean))
  a = aggregate(dv~within, d, mean)
  for(i in 1:nlevels(d$within)){
    new = d[d$within==levels(d$within)[i],]
    colnames(new) = c("id", "dv", "within", "between")
    library(ez)
    cat("\nNext Within-Subjects Level: ")
    cat(levels(d$within)[i])
    print(ezANOVA(data = new, dv, as.factor(id), between = between, type = 3)$`Levene's Test for Homogeneity of Variance`) 
    
  }
}

#example:
#MixedHOV(long,"value","variable","Group","Subject")