### R Code to fit data with mixture distribution consisting of Beta_j(alpha_j,beta_j) ###

# load data
load('allp.Rdata')
ls()

# plots
par(mfrow=c(2,2))

# histograms
for (vector_number in seq(1,length(allp))){
  hist(allp[[vector_number]], main=paste("Hist of pvector", vector_number),xlab = "p value")
}

# plot sorted p values
for (vector_number in seq(1,length(allp))){
  plot(sort(allp[[vector_number]]), main=paste("plot of sorted pvector", vector_number),xlab = "p value")
}
