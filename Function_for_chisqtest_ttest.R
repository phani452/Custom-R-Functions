########## A function where every thing comes into one place########################
# A function which conducts the all tests and return the respective p value
# And also does the univariate analysis and returns the histogram for the
# numerical columns and the bar plot for the categorical columns
#Arguments
#data=dataframe
#num-threshold value to identify whether it is a categorical variable or not
#col=target variable
fram=read.csv('C:\\Users\\Phaneendra\\Downloads\\breast_cancer\\data.csv')
tests_and_graphs=function(data,num,col,dir)
{
  setwd(dir)
  tab=c()
  for(i in 1:ncol(data))
  {
    if (length(unique(data[,i]))<=num)              #checking whether it is categorical variable or not
    {
      data[,i]=as.factor(data[,i])
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
      d=t.test(a,b)#performing t-test
      tab[i]=round(d$p.value,2)
      hist(data[,i],main = paste('histogram of ',names(data)[i]),col = 'gold')  #histogram
      boxplot(data[,i],main = paste('histogram of ',names(data)[i]),col='maroon')#boxplot
      dev.off()
    }
    else
    {
      png(paste(names(data)[i], ".png", sep=""))
      e=chisq.test(data[,i],data[,col])['p.value'] #performing-chisq-test
      tab[i]=round(e$p.value,2)
      barplot(table(data[,i]),main =paste('barplot of ',names(data)[i]),col='red') #barplot
      dev.off()
    }
  }
  tab=as.matrix(tab)
  rownames(tab)=names(data)
  colnames(tab)='p-value'
  return(tab)
}
tests_and_graphs(fram,5,'diagnosis',dir="C:/Users/Phaneendra/Desktop/R/EDA")
View(fram)
getwd()
setwd("C:/Users/Phaneendra/Desktop/R/EDA")
------------------------------------------------------------------------------------------
           