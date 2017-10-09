######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)

# set seed to ensure consistent results
set.seed(100)

# Read data file
# When	submitting, ensure that the path to this file is just "hw2-data.csv" 
# and doesn't point	to a path on your machine 
data.df <- read.csv('hw2-data.csv')
# TODO: Implement bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Be mindful that there might be duplicates in the data.
# terminating condition: when k clusters have been found

bisectingkmeans <- function(data.df, trials.max, k){
 # start your implementation here
  keeps<-c('band1','band2')
  data<-data.df[keeps]
  num = 1
  selectedCluster<-data
  clustSSE = vector()
  clusterArray = list()
  clusterArray[[1]] = as.numeric(rownames(data))
  index = 1
  
  while(num!=k){
    
      # Doing one iteration outside the loop to initialize totClustSSE
      randomCent<-sample(nrow(selectedCluster), 2)
      fit<-kmeans(selectedCluster,centers = selectedCluster[randomCent,])
      totClustSSE <- fit$tot.withinss
      fitclusters = fit$cluster
      individClustSSE = fit$withinss
    
    for(j in 2:trials.max){
      
      # Selecting random centers
      randomCent<-sample(nrow(selectedCluster), 2)
      fit<-kmeans(selectedCluster,centers = selectedCluster[randomCent,])
      
      # Updating totalSSE 
      if(fit$tot.withinss < totClustSSE){
        
        totClustSSE = fit$tot.withinss
        
        # Storing individual Cluster SSE and corresponding cluster points
        individClustSSE = fit$withinss
        fitclusters = fit$cluster
        
      }
     
    }
      
      temp2 = as.numeric(rownames(data.frame(fitclusters)))
     
      # adding the two clusters with lowest SSE to cluster array
      clusterArray[[index]] = temp2[fitclusters == 1]
      clusterArray[[num+1]] = temp2[fitclusters == 2]
      
      print(index)
      # adding the SSE of the two lowestSSE clusters to cluster SSE array
      clustSSE[index] = individClustSSE[1]
      clustSSE[num+1] = individClustSSE[2]
      
      # selecting cluster with maxium sse
      index = which.max(clustSSE)
      selectedCluster = data[clusterArray[[index]],]
      
      num = num + 1    
      
  }
  
   newList = vector()
   
   for (i in 1:k) {
     for(j in 1:length(clusterArray[[i]])){
       test = clusterArray[[i]]
       newList[test[j]] = i
     }
   }
  return(list(newList, clustSSE))
}
# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
  
  keeps<-c('band1','band2')
  data<-data.df[keeps]
  randomCent = c(210, 247, 265, 278, 288)
  fit = kmeans(data,centers = data[randomCent,])
  
  # Plotting the K-means data
  plot(data, col = fit$cluster)
  # print(fit$tot.withinss)
  # print(fit$withinss)
  
}

k=5
result <- bisectingkmeans(data.df, trials.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
      cex = 3)

kmeans_comparison(data.df, result, k)

