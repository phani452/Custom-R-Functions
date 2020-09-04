# Automating EDA process
# The function can be used :
# For Binary classification problem
# For Continous target variable
# The function plots and saves univariate and Bivariate analysis graphs
# Function gives us the importance of the variables
# The function imputes missing values using KNN
# The function gives the number outlier in variables

tests_and_graphs=function(data,num,col)
{
  setwd(readline(prompt = "Enter destination folder :")) # getting the directory
  tab=c()
  n=ncol(data)
  if (sum(is.na(data))>0)   # treating missing values
  {
    library(VIM)
    data <- kNN(data)               # Imputing missing values using KNN
    data<-subset(data[,1:n])
  }
  for(i in 1:ncol(data))
  {
    if (length(unique(data[,i]))<=num)              #checking whether it is categorical variable or not
    {
      data[,i]=as.factor(data[,i])                   # converting to factor type
    }
  }
  for(i in 1:ncol(data))
  {
    if(is.integer(data[,i])|is.numeric(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep=""))
      par(mfrow=c(2,1))
      a=data[,i][data[,col]==0]
      b=data[,i][data[,col]==1]
      tab[i]=t.test(a,b)['p.value']                #performing t-test
      hist(data[,i],main = paste('histogram of ',names(data)[i]),col = 'gold')    #histogram
      boxplot(data[,i],main = paste('histogram of ',names(data)[i]),col='maroon')      #boxplot
      dev.off()
    }
    else
    {
      png(paste(names(data)[i], ".png", sep=""))
      tab[i]=chisq.test(data[,i],data[,col])['p.value'] #performing-chisq-test
      barplot(table(data[,i]),main =paste('barplot of ',names(data)[i]),col='red') #barplot
      dev.off()
    }
  }
  tab=as.matrix(tab)
  rownames(tab)=names(data)
  colnames(tab)='p-value'
  print('-------------------------------------------')
  print('Variable Significance')
  return(tab)
}

#tests_and_graphs(fram,5,'TenYearCHD')
# Function when our target variable is a continous variable

tests_and_graphs_con=function(data,num,col)
{
  setwd(readline(prompt = "Enter destination folder :")) #getting the destination folder to store the graphs
  tab=c()
  n=ncol(data)
  if (sum(is.na(data))>0)
  {
    library(VIM)
    data <- kNN(data)                                 # Imputing missing values using KNN
    data<-subset(data[,1:n])
  }
  for(i in 1:ncol(data))
  {
    if (length(unique(data[,i]))<=num)              #checking for numerical categorical variable 
    {
      data[,i]=as.factor(data[,i])            # converting to factor type
    }
  }
  if(is.numeric(data[,col]))
  {
    for(i in 1:ncol(data))
    {
      if(is.integer(data[,i])|is.numeric(data[,i]))
      {
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(3,1))
        tab[names(data)[i]]=cor(data[,i],data[,col]) # Checking for correlation
        hist(data[,i],main = paste('histogram of ',names(data)[i]),col = 'gold')  #histogram
        boxplot(data[,i],main = paste('boxplot of ',names(data)[i]),col='maroon')#boxplot
        plot(data[,i],data[,col],xlabel=paste(names(data)[i],''),ylabel=paste(col,''),main = paste('scatterplot of ',names(data)[i],col))
        dev.off()
      }
      
      else
      {
        png(paste(names(data)[i], ".png", sep=""))
        barplot(table(data[,i]),main =paste('barplot of ',names(data)[i]),col='red') #barplot
        dev.off()
      }
    }
  }
  tab=as.matrix(tab)
  colnames(tab)='correlation'
  print('------------------------------------------')
  print(' Correlation')
  return(tab)
}



# Function for number of outliers in the data 

outlier=function(data,num)
{
  d=c()
  n = ncol(data)
  for(i in 1:ncol(data))
  {
    if (length(unique(data[,i]))<=num)              #checking whether it is categorical variable or not
    {
      data[,i]=as.factor(data[,i])
    }
  }
  for (i in 1:ncol(data))
  {
    if (sum(is.na(data))>0)
    {
      library(VIM)
      data <- kNN(data)
      data<-subset(data[,1:n])
    }
    if(is.numeric(data[,i])|is.integer(data[,i]))
    {
      q1=quantile(data[,i],0.25)
      q3=quantile(data[,i],0.75)
      iqr=q3-q1
      ub=q3+(1.5*iqr)
      lb=q1-(1.5*iqr)
      d[names(data)[i]]=sum(data[,i]>ub)+sum(data[,i]<lb) # Outlier condition
      
    }
    
  }
  return(d)
}
#outlier(fram)

# Final function

final_function=function(data,num,col)
{
  answer<-winDialog("yesno", "Your target variable is continous?")
  
  if(answer=='YES')
  {
    print(tests_and_graphs_con(data,num,col))
    
  }
  else
  {
    print(tests_and_graphs(data,num,col))
  }
  print('--------------------------------------')
  print(' Outliers in the data')
  print(outlier(data,num))
  print('--------------------------------------')
}

final_function(cars,5,'MPG')
#fram=read.csv('C:\\Users\\Phaneendra\\downloads\\framingham.csv')

final_function(fram,5,'TenYearCHD')

setwd('C:\\Users\\anand\\Downloads')
fram <- read.csv('framingham.csv')
cars <- read.csv('cars1.csv')
answer<-winDialog("yesno", "Your target variable is contnous?")
