library(SPerceptron)
#declares setosa as -1 and virginica and virsicolor as 1
iris_subset[,4] = 1
iris_subset[iris_subset[,3] == "setosa",4] = -1
#loads x and y values
data = iris_subset[,c(1,2)]
label = iris_subset[,4]
err<-perceptron(data,label)
plot( err, type="l", lwd=2, col="Blue", xlab="epoch #", ylab="errors")
