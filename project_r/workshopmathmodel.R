# setwd("C:/Users/Paulien/Downloads")
# setwd('/media/mynewdrive1/Documenten/Wiskunde/2017-2018/shared_math_mod/project_r')
load('allp.Rdata')

allp$p1

# histograms
par(mfrow=c(2,2))


for (i in seq(1,length(allp))){
  hist(allp[[i]],
       main=paste("Hist of pvector", i),xlab = "p-value",prob=T)
}

for (i in seq(1,length(allp))){
  plot(sort(allp[[i]]), main=paste("p-values of pvector"),
       xlab="", ylab="sorted p-values",col="blue", type="l")
}
