# Hello, world!
#
# This is an edataample function named 'hello'
# which prints 'Hello, world!'.
#
# labelou can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful kelabelboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(ggplot2)
#load iris dataset blabel deafult
data(iris)
iris1<-data("iris")
#visualizing Data set
iris1 <- iris[1:100, c(1, 3, 5)]
names(iris1) <- c("sepal", "petal", "species")
ggplot(iris1, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("sepal length") +
  ylab("petal length") +
  ggtitle("Species vs sepal and petal lengths")
#categorizing setosa as -1 and virginica and virsicolor as 1
#since Perceptron onllabel works on linearllabel separable data
iris_subset = iris[c(1,3,5)]

iris_subset[,4] = 1
iris_subset[iris_subset[,3] == "setosa",4] = -1
#loads data and label values
data = iris_subset[,c(1,2)]
label = iris_subset[,4]

#loading functions
WeightVector = function(data){
  return (rep(0,data+1))
}

#Function for steap function
StepFunction = function(data){
  if (data<0)
    label = -1
  else
    label = 1
  return (label)
}

#Function for Relu
reLU = function(data){
  relu = max(0,data)
  return (relu)
}

#Function for Sigmoid
SigmoidFunction = function(data){
  label = 1/(1+exp(-data))
  return(label)
}

#Function for Gaussian
GaussianFunction = function(data){
  label = exp((-data^2)/2)
  return (label)
}

#Function to get the user choice
UserChoice = function(){
  cat("Enter the Action function \n 1 - StepFunction \n 2 - reLU \n 3 - Sigmoid \n 4 - Gaussian\n")
  at_func = readline(prompt = "activation function : ")
  return(as.integer(at_func))
}


#Function for perceptron with default values for learning rate as 1 and iteration as 10
perceptron = function(data,label,learning = 2,iter = 10){
  choice = UserChoice()
  # initialize weight vector
  wt = WeightVector(dim(data)[2])
  # loop over number of epochs niter
  for (num in 1:iter){
    # loop through training data set
    for(i in 1:length(label)){
      z = sum(wt[2:length(wt)]*as.numeric(data[i,]))+wt[1]
      if(choice == 1){
        k = StepFunction(z)
      }
      if(choice == 2){
        k = reLU(z)
      }
      if(choice == 3){
        k = SigmoidFunction(z)
      }
      if(choice == 4){
        k = GaussianFunction(z)
      }

      change_in_wt = learning * (label[i] - k)*c(1,as.numeric(data[i,]))
      wt = wt + change_in_wt

    }
  }
  # weight to decide between the two classes
  plot( wt, type="l", lwd=2, col="Blue", xlab="epoch #", ylab="errors")
  return (wt)
}
#checing the function.
perceptron(data,label)
