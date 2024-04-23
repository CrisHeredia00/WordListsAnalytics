createClusterDataGraph <- function(t_start, t_end, t_step, data){
  #Threshold to consider
  thresholds=seq(t_start, t_end, t_step) #ESTO SERIA INPUT

  tempFinal=data.frame(matrix(data=0,nrow = length(thresholds),ncol = 4))
  colnames(tempFinal)=c("threshold","meanClusters","numClusters","meanShift")
  index=1
  for (i in c(1:length(thresholds))){
    tempDF=getValuesClusterKids(data,thresholds[i])
    tempFinal$meanClusters[index]=mean(tempDF$Clusters)
    tempFinal$maxClusters[index]=max(tempDF$Clusters)
    tempFinal$meanShift[index]=mean(tempDF$Shifts)
    tempFinal$threshold[index]=thresholds[i]
    index=index+1
  }
  return(tempFinal)
}




getValuesClusterKids <- function(data,simThreshold){

  ################CLEANING PROCESS################################
  #Creating an empty data frame with the clean data
  cleanData = data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(cleanData) = colnames(data)

  #For each subject, we extract their words, and check for repetitions
  for (i in unique(data[,1])){
    tempData=data[data[,1]==i,]
    if (nrow(tempData)>length(unique(tempData[,2]))){
      tempData=tempData[!duplicated(tempData[,2]),]
    }
    cleanData=rbind(cleanData,tempData)
  }
  data=cleanData

  #Transforming the data to indexes for the matrix visualization
  tempVectorIndexes=numeric(nrow(data))

  #Transforming the data to indexes to sort them
  #This means that a new column is added then the same number correspond to
  #the same "word" independently of the "subject"
  count=0
  for (i in unique(data[,2])){
    count=count+1
    tempVectorIndexes[which(data[,2]==i)]=count
  }
  data$indexes=tempVectorIndexes
  ################END OF CLEANING PROCESS#########################

  ################SIMILARITY PROCESS################################
  numMaxWords=length(unique(data[,2])) #Number of unique words
  numSubjects=length(unique(data[,1])) #Number of unique subjects
  allSims=matrix(0,numMaxWords,numMaxWords) #Matrix with all the similarities
  simWords=numeric(numMaxWords)

  #Analyzing the similarities for each subject
  for (eachSubject in unique(data[,1])){
    tempData=data[data[,1]==eachSubject,"indexes"] #Getting the data for the subject
    #Initializaing variables to calculate the similarities
    minDist=1/(length(tempData)-1)
    tempSim=matrix(0,numMaxWords,numMaxWords)
    minDist2=1/length(tempData)
    tempsimWords=numeric(numMaxWords)
    #Loop to calculate the similarities
    for (i in c(1:(length(tempData)-1))){
      #Similarity from the initial to the last, based on the number of words mentioned
      tempsimWords[tempData[i]]=minDist2*(length(tempData)-i+1)
      #This is the vector similairity of the i-th subject to the words that he mentioned

      #Similarity from each word to another
      countSim=1
      for (j in c((i+1):length(tempData))){
        tempSim[tempData[i],tempData[j]]=minDist*(length(tempData)-countSim)
        tempSim[tempData[j],tempData[i]]=minDist*(length(tempData)-countSim)
        countSim=countSim+1
      }
    }
    #Updating the similarity for the last words, for the i-th subject
    tempsimWords[tempData[length(tempData)]]=minDist2
    #Updating the similarity among all words
    allSims=allSims+tempSim
    simWords=simWords+tempsimWords
  }
  allSims=allSims/numSubjects
  simWords=simWords/numSubjects

  ################END OF SIMILARITY PROCESS################
  #simWords: vector with the average order of the subjects with respect to each word
  #A high value of simWords[i] means that the subject mentions this word at the beginning
  #allSims: Matrix with the similarity among all words
  #######################################################

  #Sorting the matrix from the highest to the lowest number of times that is mentioned
  tempIndex=sort(simWords,decreasing=T,index.return=T)
  allSims=allSims[tempIndex$ix,tempIndex$ix]
  sortedWords=unique(data[,2])[tempIndex$ix]

  ################ CLUSTERING PROCESS ################################
  #Sorting the matrix similarity and getting the indices
  origSims=allSims
  allSims[upper.tri(allSims)]=0
  temp=sort(allSims,index.return=T,decreasing = T)
  tempIndex=temp$ix
  indexMat=matrix(nrow=length(tempIndex),ncol = 2)
  indexMat[,1]=c(ceiling(tempIndex/numMaxWords))
  indexMat[,2]=c(tempIndex-(indexMat[,1]-1)*numMaxWords)
  #indexMat: Indexes of the matrix showing order from highest to lowest


  #Cluster 1, elements not important
  #From there, if two words are close and not of them belong to a cluster
  #a new cluster is generated
  #If one of the words bedongs to a cluster and not the another, it is
  #added to that cluster
  #If the similarity between words is too small (simThreshold), they are not considered (cluster 1)
  clusterX=numeric(numMaxWords) #Mean that word i belong to clusterX[i]
  count=0
  numCluster=1
  clusterList=list()
  #First element of the list is empty
  #If the first element does not go a to a cluster none of the elements go
  if (allSims[indexMat[1,2],indexMat[1,1]]>simThreshold){
    for (i in c(1:nrow(indexMat))){
      #Checking if word A OR word B does not belong to a cluster
      #If both properties belong to a cluster, they are not considered
      if ((clusterX[indexMat[i,1]]==0)|(clusterX[indexMat[i,2]]==0)){
        #Checking if word A AND word B does not belong to a cluster
        if ((clusterX[indexMat[i,1]]==0)&(clusterX[indexMat[i,2]]==0)){
          #Checking if the similarity is greater than a threshold.
          if (max(allSims[indexMat[i,1],indexMat[i,2]],allSims[indexMat[i,2],indexMat[i,1]])>simThreshold){
            #Generation of a new cluster
            numCluster=numCluster+1
            count=count+2
            clusterX[indexMat[i,1]]=numCluster
            clusterX[indexMat[i,2]]=numCluster
            clusterList[[numCluster]]=c(indexMat[i,1],indexMat[i,2])
          } else {
            #Assigning the word to a cluster that is not important
            count=count+2
            clusterX[indexMat[i,1]]=1
            clusterX[indexMat[i,2]]=1
            #Updating the list of the words belonging to the "rare" cluster
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,1],indexMat[i,2])
          }
        }

        #Word A does not belong to a cluster, but word B belong to one of them
        if ((clusterX[indexMat[i,1]]==0)&(clusterX[indexMat[i,2]]>0)){
          #Checking if the similarity is greater than a threshold.
          if (max(allSims[indexMat[i,1],indexMat[i,2]],allSims[indexMat[i,2],indexMat[i,1]])>simThreshold){
            #Assigning cluster of word B to word A
            count=count+1
            clusterX[indexMat[i,1]]=clusterX[indexMat[i,2]]
            #Updating the list with the elements of each cluster
            clusterList[[clusterX[indexMat[i,2]]]]=c(clusterList[[clusterX[indexMat[i,2]]]],indexMat[i,1])
          } else {
            #Assigning the word to a cluster that is not important
            clusterX[indexMat[i,1]]=1
            #Updating the list of the elements belonging to the "rare" cluster
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,1])
          }
        }

        #Word B does not belong to a cluster, but word A belong to a cluster
        if ((clusterX[indexMat[i,1]]>0)&(clusterX[indexMat[i,2]]==0)){
          if (max(allSims[indexMat[i,1],indexMat[i,2]],allSims[indexMat[i,2],indexMat[i,1]])>simThreshold){
            count=count+1
            clusterX[indexMat[i,2]]=clusterX[indexMat[i,1]]
            clusterList[[clusterX[indexMat[i,1]]]]=c(clusterList[[clusterX[indexMat[i,1]]]],indexMat[i,2])
          } else {
            clusterX[indexMat[i,2]]=1
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,2])
          }
        }
      }
      if (count==numMaxWords){
        #This break the cycle once all the words has been considered
        break
      }
    }
  } else {
    #This case applies when all the words go to the garbage cluster
    clusterX=clusterX+1
    clusterList[[1]]=c(1:numMaxWords)
  }

  ###############################
  #ORDERING BASED ON THE CLUSTER#
  ###############################
  finalOrder=numeric(numMaxWords)
  count=1
  for (i in clusterList){
    if (count==1){
      if (!is.null(i)){
        finalOrder[(numMaxWords-length(i)+1):numMaxWords]=i
        count=count+1
      }
    } else {
      finalOrder[which(finalOrder==0)[1]:(which(finalOrder==0)[1]+length(i)-1)]=i
    }
  }

  #Sorting the matrix with the new order
  origSims=origSims[finalOrder,finalOrder]
  sortedWords=sortedWords[finalOrder]
  ################END OF CLUSTERING PROCESS END############

  finalVector=numeric(2)
  finalVector[1]=numCluster-1
  index=1
  sum=0
  for (i in clusterList){
    if (index>1){
      sum=sum+length(i)
    }
    index=index+1
  }
  finalVector[2]=sum

  #NOW I HAVE TO MOVE FOR EACH CHILD AND CHECK THE CHANGE OF CLUSTERS

  #Adding a new column to facilitate the process of shifting
  newDataShift=data
  newDataShift["cluster"]=0
  for (i in unique(data$indexes)){
    #iterating over each property (index number)
    for (j in c(1:length(clusterList))){
      #selecting the cluster where the index number is assigned
      if (sum(clusterList[[j]]==i)>0){
        selectedCluster=j
      }
    }
    #Assigning the number of cluster to all the elements of the dataset
    newDataShift$cluster[newDataShift$indexes==i]=selectedCluster
  }
  #Variable with all the number of shifts
  numberShifts=numeric(length(unique(newDataShift[,1])))
  numberClusters=numeric(length(unique(newDataShift[,1])))
  numberProp=numeric(length(unique(newDataShift[,1])))
  varCluster=numeric(length(unique(newDataShift[,1])))
  indexChild=1
  for (i in unique(newDataShift[,1])){
    #Obtaining the data of each subject
    tempData=newDataShift[newDataShift[,1]==i,]

    if (nrow(tempData)==1){
      numberShifts[indexChild]=0
    } else {
      numberShifts[indexChild]=sum(tempData$cluster[c(1:(length(tempData$cluster)-1))]-tempData$cluster[c(2:length(tempData$cluster))]!=0)
    }

    numberClusters[indexChild]=length(unique(tempData$cluster))
    numberProp[indexChild]=nrow(tempData)
    if (numberClusters[indexChild] == 1){
      varCluster[indexChild]=0
    } else {
      varCluster[indexChild]=round(stats::sd(table(tempData$cluster)),2)
    }
    indexChild=indexChild+1
  }
  #Crear histogramas para mostrar los cambios y mencioanr números de clusters

  # tempDF=data.frame(numeric(4))
  # tempDF[1,1]=finalVector[1]
  # tempDF[2,1]=finalVector[2]
  # tempDF[3,1]=mean(numberShifts)
  # tempDF[4,1]=sd(numberShifts)

  tempDF=data.frame(matrix(data=0,nrow = length(unique(newDataShift[,1])),ncol = 4))
  tempDF[,1]=unique(newDataShift[,1])
  tempDF[,2]=numberClusters
  tempDF[,3]=numberShifts
  tempDF[,4]=numberProp
  #tempDF[,5]=varCluster
  colnames(tempDF) = c("Subjects","Clusters","Shifts","#Prop") #,"sd"
  return(tempDF)
}
