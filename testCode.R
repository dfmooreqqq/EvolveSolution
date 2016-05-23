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

mutate <- function(solution, mutatethreshold=0.1)
{
    mutatedsolution<-matrix(0, nrow=1,ncol=variables)
    for(k in 1:variables)
    {
        ifelse(runif(1)<mutatethreshold,
               mutatedsolution[1,k]<-solution[k],
               mutatedsolution[1,k]<-runif(1,-100,100)
               )
    }
    mutatedsolution
}

judgesolutions <- function(solutionset, yv=yvalues, xv=xvalues)
{
  metric<-rep(-1,dim(solutionset)[1])
  for(i in 1:dim(solutionset)[1])
  {
    solution1<-solutionset[i,]
    yvalues<-t(tcrossprod(solutionvector,as.matrix(datacreate)))
    yvi<-t(tcrossprod(solution1, as.matrix(xv)))
    metric[i]<-summary(lm(yv~yvi))$r.squared # this allows for there to be a big offset. Also, the correlation could be negative
  }    
  
  metric
}


# Create data -------------------------------------------------------------
# set.seed(665544)
# Let's do 10 variables and 100 observations
variables = 20
observations = 10
error = 5
datacenters<-runif(variables,-100,100)
loops = 100
iterations = 100
solutions = 100

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
#solutionvector<-c(1, 5, 10, -7, 10, -1, 13.3, -12.8, 1.45, -20)
solutionvector<-round(runif(variables,-20,20))
sm<-as.matrix(solutionvector)
yvalues<-t(tcrossprod(solutionvector,as.matrix(datacreate)))
xvalues<-datacreate

xnam <- paste0("x", 1:variables)
colnames(xvalues)<-xnam
colnames(yvalues)<-"y"

df<-as.data.frame(xvalues)
df$y<-yvalues

(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))

# Loop through the starting seeds
loops = 100 # for now, until I get mutate working, we're going to just do a bunch of loops
RMSEbest<-data.frame(RMSE=rep(-1000, loops))
Rsqbest<-data.frame(Rsq=rep(-1000, loops))
BestSolutions<-matrix(data=rep(-1000, variables*loops), nrow=loops, ncol=variables)


for(loop in 1:loops)
{

# Create First 100 solutions ----------------------------------------------
solutions = 100
newsolutions<-matrix(data=runif(solutions*variables,-100,100), nrow=solutions, ncol=variables)
RMSE<-data.frame(value=rep(-1000000, solutions))
Rsq<-data.frame(value=rep(-1000000, solutions))
solutionstokeep<-newsolutions


# Go through iterations ---------------------------------------------------
numIterations = 100
for(iter in 1:numIterations)
{

    print(paste("Loop", loop, "Iter", iter, "Curr. Solns",dim(unique(solutionstokeep))[1], "Best", max(Rsq$value), sep=" - "))
    (if (iter > 1 && dim(unique(solutionstokeep))[1]==1) break)
    solutioncenters<-newsolutions

    #recode Rsq df

#keep top ten solutions and breed them
    #reset Rsq and RMSE matrices
    Rsq<-data.frame(value=rep(2,dim(newsolutions)[1]))
    Rsq$value<-judgesolutions(newsolutions)
    solutionstokeep<-solutioncenters[rank(1-Rsq$value, ties.method = "first")<sqrt(solutions)+1,]

    newsolutions<-matrix(0,nrow=solutions, ncol=variables)

    threshold<-0.5
    mutatethreshold<-0.01
    for(i in 1:dim(solutionstokeep)[1])
      {
      for(j in 1:dim(solutionstokeep)[1])
        {
        #breeding i and j. For each parameter, keep i if random value is less than threshold, j if greater
        #This is the breed function
        newsolutions[10*(i-1)+j,]<-breed(i,j,solutionstokeep, variables)
        #Now mutate! if below threshold
        ifelse(runif(1)<mutatethreshold
               , newsolutions[10*(i-1)+j,]<-mutate(newsolutions[10*(i-1)+j,], mutatethreshold=0.1)
               , newsolutions[10*(i-1)+j,]<-newsolutions[10*(i-1)+j,]
        )
      }
    }
    
    
    
    #endIteration
}


solutionforloop<-unique(solutionstokeep)

for(i in 1:dim(solutionforloop)[1])
{
  smi<-solutionforloop[i,]
  yvi<-t(tcrossprod(smi,as.matrix(xvalues)))
  RMSE[i,]<-sum((yvi-yvalues)^2)/observations
  Rsq[i,]<-summary(lm(yvalues~yvi))$r.squared
}


#solutionvector
        BestSolutions[loop,]<-unique(solutionstokeep)[1,] #only one solution per loop
        RMSEbest[loop,]<-RMSE[1,] #only one solution per loop
        Rsqbest[loop,]<-Rsq[1,] #only one solution per loop

#end loop
}

# Show very best solution
barplot(sort(RMSEbest[[1]]))
barplot(sort(RMSEbest[[1]]), log="y")
barplot(sort(Rsqbest[[1]]), log="y")

solutiontocompare<-BestSolutions[rank(1-Rsqbest[[1]])==1,]

yvalscompare<-xvalues%*%solutiontocompare

plot(yvalues, yvalscompare)

plot(density(BestSolutions[,1]))
for(i in 2:dim(BestSolutions)[2])
{
  lines(density(BestSolutions[,i]))
}
  
