#computes all pairwise contrasts for a repeated measures ANOVA
#requires library(ez)
RMallpairwise = function(data, dv, iv, id){
  print("Group Means:")
  d = data.frame(id = data[,id], dv = data[,dv], iv = data[,iv])
  print(aggregate(dv~iv, d, mean))
  a = aggregate(dv~iv, d, mean)
  pairs = combn(levels(data[,iv]), 2)
  for(i in 1:ncol(pairs)){
    new = data[data[,iv]==pairs[1,i]|data[,iv]==pairs[2,i],]
    colnames(new) = c("id", "iv", "dv")
    library(ez)
    print("Next Comparison:")
    print(pairs[,i])
    est = a[a$iv==pairs[1,i],2] -  a[a$iv==pairs[2,i],2]
    print("Contrast Estimate:")
    print(est)
    print(ezANOVA(data = new, dv, as.factor(id), iv, return_aov = T)) 
    
  }
} 

#example:
#RMallpairwise(long, "value", "variable", "id")
