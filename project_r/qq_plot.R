qqplot(data,test)

x1 <- seq(0,1,length=pi_vector_new[1]*length(data))
x2 <- seq(0,1,length=pi_vector_new[2]*length(data))

test1 <- rbeta(x1,alpha_new[1],beta_new[1])
test2 <- rbeta(x2,alpha_new[2],beta_new[2])

testdit <- c(test1,test2)

qqplot(data,testdit)

