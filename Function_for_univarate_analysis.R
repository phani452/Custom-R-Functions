-------------------------------------------------------------------------------------
#function which performs the univariate analysis If variable is continous histogram and
#Boxplot are plotted and if variable is categorical barplot and pie chart are plotted
#And all graphs are stored in the user preferd folder.
-------------------------------------------------------------------------------------
#Example:
#Graphs(Boston, Variable = c(1,3,4), dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"
graphs5=function(data,var=c(),dir)
{
  setwd(dir)
  if (is.null(var))
  {
    for(i in 1:ncol(data))
    {
      par(mfrow=c(2,1))
      
      if (is.numeric(data[,i]))
      {
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        hist(data[,i],main = paste('histogram of',names(data)[i]),xlab = names(data)[i],col = 'gold')
        boxplot(data[,i],main=paste('boxplot of',names(data)[i]),xlab=names(data)[i],col='maroon',horizontal=T)
        dev.off()
      }
      else
      {
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        barplot(table(data[,i]),main=names(data)[i],col = 'gold')
        pie(table(data[,i]))
        dev.off()
      }
    }
  }
  else
  {
    for (i in var)
    {
      par(mfrow=c(2,1))
      if(is.numeric(data[,i]))
      {
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        hist(data[,i],main = paste('histogram of',names(data)[i]),xlab = names(data)[i],col = 'gold')
        boxplot(data[,i],main=paste('boxplot of',names(data)[i]),xlab=names(data)[i],col='maroon',horizontal=T)
        dev.off()
      }
      else
      {
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        barplot(table(data[,i]),main=names(data)[i],col='gold')
        pie(table(data[,i]))
        dev.off()
      }
    }
  }
}
graphs5(cars,dir='C:\\Users\\Phaneendra\\Desktop\\R\\EDA')
graphs5(cars,c(1,2),dir='C:\\Users\\Phaneendra\\Desktop\\R\\EDA')
--------------------------------------------------------------------------------------------