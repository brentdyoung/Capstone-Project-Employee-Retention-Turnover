#Brent Young
#MSDS 498 Capstone Spring 2019

#Load Data
setwd("~/Brent's Documents/R Datasets/Cluster")

mydata <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(proxy)
library(VIM) #Missingness Map
library(mice)
library(plyr) 
library(likert) #Visualize Likert Scale Data
require(ggplot2)
library(factoextra) #Density Clustering
library(ggpubr) #Density Clustering
library(dbscan) #Density Clustering
library(fpc) #Density Clustering
library(reshape)
library(NbClust) #Provides 30 indexes for determining the optimal number of clusters in a data set

# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###### Data Preparation ######
mydata$EnvironmentSatisfaction <- as.numeric(mydata$EnvironmentSatisfaction)
mydata$JobSatisfaction <- as.numeric(mydata$JobSatisfaction)
mydata$RelationshipSatisfaction <- as.numeric(mydata$RelationshipSatisfaction)
mydata$WorkLifeBalance <- as.numeric(mydata$WorkLifeBalance)

mydata$JobInvolvement <- as.numeric(mydata$JobInvolvement)
mydata$Attrition <- as.numeric(mydata$Attrition)
mydata$BusinessTravel <- as.numeric(mydata$BusinessTravel)
mydata$Department <- as.numeric(mydata$Department)
mydata$EducationField <- as.numeric(mydata$EducationField)
mydata$Gender <- as.numeric(mydata$Gender)
mydata$JobRole <- as.numeric(mydata$JobRole)
mydata$MaritalStatus <- as.numeric(mydata$MaritalStatus)
mydata$Over18 <- as.numeric(mydata$Over18)
mydata$OverTime <- as.numeric(mydata$OverTime)

str(mydata)

###### Normalizing - Z score EXAMPLE ######
#mu=colMeans(mydata[,2:6])
#mu
#mu=matrix(mu,nrow=23,ncol=5,byrow=TRUE)
#sigma=apply(mydata[,2:6],2,"sd")
#sigma=matrix(sigma,nrow=23,ncol=5,byrow=TRUE)
#sigma
#mydata[,2:6]=(mydata[,2:6]-mu)/sigma
#mydata
#colMeans(mydata[,2:6])

###################################### Create subsets #####################################

#Subset of survey questions
numsub <- subset(mydata, select=c("EnvironmentSatisfaction","JobSatisfaction","RelationshipSatisfaction","WorkLifeBalance"))

str(numsub) #1470 observations and 4 variables
summary(numsub) #1,2,3,4

#Questions - Visualization
library(likert)

#Subset numsub
numsub_v <- subset(mydata, select=c("EnvironmentSatisfaction","JobSatisfaction","RelationshipSatisfaction","WorkLifeBalance"))

numsub_v$EnvironmentSatisfaction <- as.factor(numsub_v$EnvironmentSatisfaction)
numsub_v$JobSatisfaction <- as.factor(numsub_v$JobSatisfaction)
numsub_v$RelationshipSatisfaction <- as.factor(numsub_v$RelationshipSatisfaction)
numsub_v$WorkLifeBalance <- as.factor(numsub_v$WorkLifeBalance)

likert(numsub_v)
Result = likert(numsub_v)
plot(Result,type="bar")

### Correlation Matrix of subset ###

require(corrplot)
numsubcorrelation <- cor(numsub)

##Correlation Plot 3 w/ Numbers
corrplot(numsubcorrelation, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45,number.cex = 0.6,tl.cex = 0.6, addcolorlabel="no", order="AOE",insig = "p-value")

### PCA Plots ###

pca <-princomp(numsub)
plot(pca$scores[,1],pca$scores[,2]) #First 2 principal components only explain 0.5927590 of the variation in the data. 

names(pca)
str(pca)
summary(pca)
head(pca$scores)

###################################################################
############### Kmeans Cluster ###################
###################################################################

#Create a 'scree' plot to determine the num of clusters
#'Sweep' through 1 to 15 clusters (standard, see slide 22)
wssplot <- function(numsub, nc=15, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub) #Elbow at 4, although no clear elbow. 

# Elbow method (alternative); #intercept specifies elbow
fviz_nbclust(numsub, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method #Recommends 6
fviz_nbclust(numsub, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
#set.seed(123)
#fviz_nbclust(numsub, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
# labs(subtitle = "Gap statistic method")

#NbClust: Determining the Best Number of Clusters in a Data Set
#It provides 30 indexes for determining the optimal number of clusters in a data set and offers the best clustering scheme from different results to the user.
NbClust(data = numsub, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

### k-means with 3 clusters ###
set.seed(567)
clusterresults_3 <- kmeans(numsub,3) #k-means w/ 3 clusters makes the most sense based on the clusters
names(clusterresults_3) #sub objects of this result file and can access these with $
clusterresults_3$withinss #withinss for each of the clusters (e.g., sitting in cluster centroid, distance with all the people computing)
clusterresults_3$tot.withinss #Total withinss for the clusters
clusterresults_3$totss #Total withins for 1 cluster
clusterresults_3$betweenss #total withins-totss
clusterresults_3$size #Gives the count of people that are sitting in each cluster
rsquare <- clusterresults_3$betweenss/clusterresults_3$totss
rsquare #r-squared: 0.3518845

### Create a PC (Principal Componenet plot) ###

plot(clusterresults_3, data=numsub) #PCA analysis, plot PC on x and y axis and then will plot the clusters
clusterresults_3$centers 
head(clusterresults_3$cluster) #Shows cluster membership

### Create a Silhouette Plot ###
dissE <- daisy(numsub)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults_3$cluster, dE2)
str(sk2)
plot(sk2) #Average Silhouette: 0.33

#Values near one mean that the observation is well placed in its cluster; 
#values near 0 mean that it's likely that an observation might really belong in some other cluster. 
#Within each cluster, the value for this measure is displayed from smallest to largest. 
#If the silhouette plot shows values close to one for each observation, the fit was good; 
#if there are many observations closer to zero, it's an indication that the fit was not good. 
#The sihouette plot is very useful in locating groups in a cluster analysis that may not be doing a good job; 
#in turn this information can be used to help select the proper number of clusters. 
#If many points have a low or negative value, then the clustering configuration may have too many or too few clusters.
#1 = Perfect, 0 is on the wall, and -1 is completely wrong. 
#Good news, it's on positive side, more than 0 which is good. 

### Produce csv Files ###
newdf <- as.data.frame(clusterresults_3$cluster) #creates dataframe for k-means cluster
write.csv(newdf, file = "clusterresults_3.csv") #cluster results assigned to each respondent
write.csv(numsub, file = "numsub.csv") #File of responses for subset of questions

######################################################
################### PAM Method #######################
######################################################

my.k.choices <- 2:8 #Sweep through using average silhouette width (k-means uses WSS)
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(numsub, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width)) #Optimal number is 4, given average silo of 0.2440981, but 3 is second best

clusterresultsPAM <-pam(numsub,3)
summary(clusterresultsPAM)

#Create cluster plot of PAM
plot(clusterresultsPAM, which.plots=1) #Different symbols denote various clusters; overlap

#Create a Silhouette Plot
plot(clusterresultsPAM, which.plots=2) #Average Silhouette: 0.22
#Cluster sizes somewhat imbalanced.
#k-means is still better in terms of average silouette.

####################################### Profiling #######################################################
#Create a dataset that combines original data with cluster information, used to create profiles

newdf <- read.csv("clusterresults_3.csv") #File that contains cluster results assigned to each respondent

str(newdf)
describe(newdf)

##File that contains cluster results assigned to each respondent with demographics and survey
combdata1 <- cbind(numsub ,newdf,mydata)
str(combdata1)
describe(combdata1)
write.table(combdata1,'clusterresults_3_full.csv',col.names = TRUE, row.names = FALSE, sep = ',')

#Demographics: Numeric Data
combdata <- cbind(numsub ,newdf,mydata$Age,
                  mydata$DistanceFromHome,
                  mydata$Education,
                  mydata$JobLevel,
                  mydata$JobInvolvement,
                  mydata$DailyRate,
                  mydata$HourlyRate,
                  mydata$MonthlyIncome,
                  mydata$MonthlyRate,
                  mydata$NumCompaniesWorked,
                  mydata$PercentSalaryHike,
                  mydata$PerformanceRating,  
                  mydata$StockOptionLevel,
                  mydata$TotalWorkingYears,
                  mydata$TrainingTimesLastYear,
                  mydata$YearsAtCompany,
                  mydata$YearsInCurrentRole,
                  mydata$YearsSinceLastPromotion,
                  mydata$YearsWithCurrManager)

head(combdata)
require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
aggregate(combdata,by=list(byvar=combdata$cluster), mean) #For each cluster, show mean response for the subset of q's


#Demographics: Categorical Data
combdata <- cbind(numsub ,newdf,mydata$Attrition,
                  mydata$BusinessTravel,
                  mydata$Department,
                  mydata$EducationField,
                  mydata$Gender,
                  mydata$JobRole,
                  mydata$MaritalStatus,
                  mydata$Over18,
                  mydata$OverTime)

head(combdata)
require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
aggregate(combdata,by=list(byvar=combdata$cluster), mean) #For each cluster, show mean response for the subset of q's

#Graphs - EXAMPLE
#colnames(data)=c("ID","Utilization","Rev_Growth","Charge_Rate","ID2","Cluster","Region")
#write.csv(data, file = "Galaxy_combined_data_5_renamed.csv") #cluster results assigned to each respondent

#data$Cluster=as.factor(data$Cluster)

#library(ggplot2)
#ggplot(data=data,aes(x=Utilization,y=Rev_Growth,color=Cluster))+geom_point()
#ggplot(data=data,aes(x=Charge_Rate,y=Rev_Growth,color=Cluster))+geom_point()
#ggplot(data=data,aes(x=Utilization,y=Charge_Rate,color=Cluster))+geom_point()

#library(rgl)
#with(data,plot3d(Utilization,Rev_Growth,Charge_Rate,col=Cluster,type="s"))

############################################### Segment 1 #######################################################

#Subsetting on Segment 1 (numdata)
combdata_seg <- cbind(numsub ,newdf,mydata)
combdata_seg <- rename(combdata_seg, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment1<-combdata_seg[combdata_seg$cluster ==1, ]
str(combdata_segment1)

describe(combdata_segment1)

############################################### Segment 2 #######################################################

#Subsetting on Segment 1 (numdata)
combdata_seg <- cbind(numsub ,newdf,mydata)
combdata_seg <- rename(combdata_seg, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment2<-combdata_seg[combdata_seg$cluster ==2, ]
str(combdata_segment2)

describe(combdata_segment2)

############################################### Segment 3 #######################################################

#Subsetting on Segment 1 (numdata)
combdata_seg <- cbind(numsub ,newdf,mydata)
combdata_seg <- rename(combdata_seg, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment3<-combdata_seg[combdata_seg$cluster ==3, ]
str(combdata_segment3)

describe(combdata_segment3)
