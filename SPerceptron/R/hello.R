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

#load iris dataset blabel deafult
data(iris)
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
StepFunction = function(data){
  if (data<0)
    label = -1
  else
    label = 1
  return (label)
}
reLU = function(data){
  relu = madata(0,data)
  return (relu)
}
SigmoidFunction = function(data){
  label = 1/(1+edatap(-data))
  return(label)
}
GaussianFunction = function(data){
  label = edatap((-data^2)/2)
  return (label)
}
get_activation_function = function(){
  print("1 - step function , 2 - relu ,3 - sigmoid, 4 - gaussian")
  print("Enter the Activation Function")
  at_func = readline(prompt = "activation function")
  return(as.integer(at_func))
}
#perceptron function has a deafault learning rate as 1 and default number of iterations as 10
#enter perceptron(data,label,1,10)
perceptron = function(data,label,learning_rate = 1,num_of_iterations = 10){
  #n = get_activation_function()
  print("1 - step function , 2 - relu ,3 - sigmoid, 4 - gaussian")
  print("Enter the Activation Function")
  at_func = as.integer(readline(prompt = "activation function"))
  wt = WeightVector(dim(data)[2])
  for (num in 1:num_of_iterations){
    for(i in 1:length(label)){
      z = sum(wt[2:length(wt)]*as.numeric(data[i,]))+wt[1]
      if(n == 1){
        k = StepFunction(z)
      }
      if(n == 2){
        k = reLU(z)
      }
      if(n == 3){
        k = SigmoidFunction(z)
      }
      if( n == 4){
        k = GaussianFunction(z)
      }
      change_in_wt = learning_rate * (label[i] - k)*c(1,as.numeric(data[i,]))
      wt = wt + change_in_wt
    }
  }
  return (wt)
}
