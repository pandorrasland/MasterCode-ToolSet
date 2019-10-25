##############
## Get Data ##
install.packages("bazar")
library(bazar)

setwd("C:/Users/Bornatico Adeline/Documents/HSLU/MachineLearning2/wine-quality-selection/")
white <- read.csv("winequality-white.csv")
str(white)
head(white)

#############
## fit PCA ##

pcafunction<- function(data){
data.scaled<- as.matrix(scale(data))
pca.data <- prcomp(data.scaled, scale = TRUE)



##############
## Loadings ##
cat("--------------------------\n
-------------------------------Overview od PCA---------------\n
---------------------------")
print(pca.data)
## PC1 ~= average
## PC2 ~= shape

cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
pause(duration = Inf)

########################
## variance explained ##
cat("-------------\n
---------------PCA Summary and Screeplot:--------\n
----------------")
print(summary(pca.data))

cat("-------------\n
---------------From PCA, get the standard deviation, and variance.--------\n
----------------")
print(pca.data$sdev)
pca.var=pca.data$sdev^2
print(pca.var)

screeplot(pca.data)

pause(duration = Inf)

###Screeplot
# In order to compute the proportion of variance explained by each principal component (variance explained
#by each principal component / total variance explained by all four principal components)
pve=pca.var/sum(pca.var)
pve
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="PrincipalComponent",ylab="Cumulative Proportion of Variance
Explained",ylim=c(0,1),type='b')




cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
pause(duration = Inf)

###########################
# Access all stuff computed by PCA.
cat("--------------\n
--------------------Means and standard deviations of the variables that were used for scaling prior to implementing PCA------\n
------------------")
print(pca.data$center)
print(pca.data$scale)

cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
pause(duration = Inf)

####### 
# Rotations
cat("----------\n
-------------Rotation matrix provides the principal component of the loadings.----------\n
------------")
print(dim(pca.data$rotation))
print(pca.data$rotation)

cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
pause(duration = Inf)

########
cat("--------------\n
----------------x matrix provides the principal component of the scores.-----\n
--------------")
print(dim(pca.data$x))
print((pca.data$x))

cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
pause(duration = Inf)

################################
## Visualisation##


####
cat("--------------\n
----------------Display 2 first components.-----\n
--------------")
biplot(pca.data, scale=0)

cat("\n\n\n\n\n\n\n Copy PCA into Global Dataset? \n\n\n
    Press ENTER to continue\
    Type 'stop' to stop \n\n\n\n\n\n")
pause(duration = Inf)

cat("--------------------------\n
-------------------------------PCA is copied into Environment:---------------\n
---------------------------")
pca.data<<-pca.data

}

pcafunction(data=white)







