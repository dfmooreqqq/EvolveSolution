# Define functions --------------------------------------------------------

breed <- function(i,j, solutionstokeep=solutionstokeep, variables=variables) {
  bredsolution<-matrix(0,nrow=1, ncol=variables)
  for(k in 1:variables)
  {
    ifelse(runif(1)<threshold,
           bredsolution[1,k]<-solutionstokeep[i,k],
           bredsolution[1,k]<-solutionstokeep[j,k]
    )
  }
  bredsolution
}



# Create data -------------------------------------------------------------
# set.seed(665544)
# Let's do 10 variables and 100 observations
variables = 10
observations = 100
error = 20
datacenters<-runif(variables,-100,100)

datacreate<-matrix(nrow=observations, ncol=variables)

for(i in 1:variables)
{
  datacreate[,i]<-rnorm(observations, mean=datacenters[i], sd = sqrt(error))
}

## Create a formula for a model with a large number of variables:
xnam <- paste0("x", 1:variables)
colnames(datacreate)<-xnam
df<-as.data.frame(datacreate)
#let's try this formula: x1+5*x2+10*x3-7*x4+10*x5-x6+13.3*x7-12.8*x8+1.45*x9-20*x10
solutionvector<-c(1, 5, 10, -7, 10, -1, 13.3, -12.8, 1.45, -20)
sm<-as.matrix(solutionvector)
yvalues<-datacreate%*%sm
xvalues<-datacreate

xnam <- paste0("x", 1:variables)
colnames(xvalues)<-xnam
colnames(yvalues)<-"y"

df<-as.data.frame(xvalues)
df$y<-yvalues

(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))


# Create First 100 solutions ----------------------------------------------
solutions = 100
newsolutions<-matrix(data=runif(solutions*variables,-100,100), nrow=solutions, ncol=variables)
RMSE<-data.frame(value=rep(-1000000, solutions))


# Go through iterations ---------------------------------------------------
numIterations = 100

for(iter in 1:numIterations)
{
print(paste(iter,dim(unique(solutionstokeep))[1], sep=" - "))
    
solutioncenters<-newsolutions

for(i in 1:solutions)
{
  smi<-as.matrix(solutioncenters[i,])
  yvi<-xvalues%*%smi
  RMSE[i,]<-sqrt(sum((yvi-yvalues)^2))/observations
}

#keep top ten solutions and breed them
solutionstokeep<-solutioncenters[order(RMSE)<11,]
newsolutions<-matrix(0,nrow=solutions, ncol=variables)
threshold<-0.5
for(i in 1:dim(solutionstokeep)[1])
{
  for(j in 1:dim(solutionstokeep)[1])
  {
    #breeding i and j. For each parameter, keep i if random value is less than threshold, j if greater
    #This is the breed function
    newsolutions[10*(i-1)+j,]<-breed(i,j,solutionstokeep, variables)
    
    # 
    # for(k in 1:variables)
    # {
    #   ifelse(runif(1)<threshold,
    #   newsolutions[10*(i-1)+j,k]<-solutionstokeep[i,k],
    #   newsolutions[10*(i-1)+j,k]<-solutionstokeep[j,k]
    #   )
    # }
    
  }
}




}

for(i in 1:solutions)
{
  smi<-as.matrix(solutioncenters[i,])
  yvi<-xvalues%*%smi
  RMSE[i,]<-sum((yvi-yvalues)^2)/observations
}

#keep top ten solutions and breed them
solutionvector
unique(solutionstokeep)
RMSE[1,]




