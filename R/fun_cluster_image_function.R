#' This function receives a property listing task, a given concept, and a threshold. It clusterizes
#'  the data according to the order of the listed properties. Given the mentioned properties of all
#'  users for a specific concept, the algorithm estimates a similarity among properties, based on
#'  the number of words mentioned between properties. For example, if the properties A and B are
#'  usually mentioned one after another, their similarity will be higher than the properties A and C
#'  which are usually not even mentioned together. The properties with low similarity to all other
#'  properties (below the user-defined threshold) are discarded from the plot.
#'
#' @param data Data frame with 3 columns: ID, Concept and Property
#' @param distThreshold Distance value. It assign properties to specific cluster if their similarity is greater than distThreshold
#' @param concept Text value. Clusters will only be generated with properties from this concept.
#' @return List with 2 elements: ggplot2 plot and data frame with cluster information
#' @export
#'
#' @examples
#' data_cpn = data.frame(CPN_27)
#' threshold = 0.061
#' concept = "Ability"
#' cluster_data = clusterImage(data_cpn, threshold, concept)
clusterImage <- function(data, distThreshold, concept = NULL){

  if (!is.null(concept)){
    data <- data[data[, 2] == concept, ]
  }
  if (nrow(data) <= 1){# If there is no data, it returns empty data frame, with 3 empty columns
    return(list(ggplot2::ggplot() + ggplot2::theme_void(), data_return <- data.frame(Var1= numeric(0),
                                                                   Var2 = character(0),
                                                                   value = character(0))))
  }

  data$indexes <- match(data$Property, unique(data$Property))


  ################DISTANCE PROCESS################################
  #Calculating the matrix distance
  numProperties=length(unique(data$Property))
  #numIDs=max(data$ID)
  #numIDs=length(unique(data$ID))
  numIDs=unique(data$ID)

  #Cicle over all Subjects
  allDist=matrix(0,numProperties,numProperties)
  distProperties=numeric(numProperties)
  #Analyzing the matrix of a single child O(tooExpensive)
  for (eachID in numIDs){

    # tempData=unlist(data[data$ID==eachID,"indexes"]) # SLOWER VERSION
    tempData <- data$indexes[data$ID == eachID]

    #Initializaing variables to calculate the distances
    minDist=1/(length(tempData)-1)
    tempDist=matrix(0,numProperties,numProperties)
    minDist2=1/length(tempData)
    tempdistProperties=numeric(numProperties)


    #Distance from the initial to the last, based on the number of properties mentioned
    seq_lengths <- seq_len(length(tempData) - 1)
    tempdistProperties[tempData[seq_lengths]] <- minDist2 * (length(tempData) - seq_lengths + 1)


    #Loop to create the distances
    for (i in seq_len(length(tempData)-1)){
      countDist <- seq_along((i+1):length(tempData))
      tempDist[tempData[i], tempData[(i+1):length(tempData)]] <- minDist * (length(tempData) - countDist)
      tempDist[tempData[(i+1):length(tempData)], tempData[i]] <- minDist * (length(tempData) - countDist)
    }
    #Updating the distance for the last property, for the i-th ID
    tempdistProperties[tempData[length(tempData)]]=minDist2
    #Updating the distance between all properties
    allDist=allDist+tempDist
    #Updating the distance between all properties
    distProperties=distProperties+tempdistProperties
  }
  allDist=allDist/length(unique(data$ID))
  distProperties=distProperties/length(unique(data$ID))

  ################DISTANCE PROCESS END ################################
  #distProperties: VECTOR WITH THE AVERAGE ORDER OF THE ID WITH RESPECT TO EACH PROPERTY
  #A HIGH VALUE OF distProperties[i] MEANS THAT THE ID USUALLY MENTION THIS PROPERTY AT THE BEGINNING.
  #allDist: MATRIX WITH THE DISTANCE AMONG ALL PROPERTIES
  #######################################################

  #Sorting the matrix from the highest to the lowest
  tempIndex=sort(distProperties,decreasing=T,index.return=T)
  allDist=allDist[tempIndex$ix,tempIndex$ix]
  sortedProperties=unique(data$Property)[tempIndex$ix]

  ################NEW CLUSTERING PROCESS ################################
  #Generating the new order
  #Sorting the matrix distance and getting the indices
  origDist=allDist
  allDist[upper.tri(allDist)]=0
  temp=sort(allDist,index.return=T,decreasing = T)
  tempIndex=temp$ix
  indexMat=matrix(nrow=length(tempIndex),ncol = 2)
  indexMat[,1]=c(ceiling(tempIndex/numProperties))
  indexMat[,2]=c(tempIndex-(indexMat[,1]-1)*numProperties)
  #indexMat: Indexes of the matrix showing order from highest to lowest


  #Cluster 1, elements not important
  #From there, if two properties are close and not of them belong to a cluster
  #a new cluster is generated
  #If one of the properties belongs to a cluster and not the other, it is
  #added to that cluster
  #If the distance between properties is too low (disThreshold), they are not considered (cluster 1)
  clusterX=numeric(numProperties) #Mean that property i belong to clusterX[i]
  count=0
  numCluster=1
  clusterList=list()
  #First element of the list is empty
  #If the first element does not go a to a cluster none of the elements go
  if (allDist[indexMat[1,2],indexMat[1,1]]>distThreshold){
    for (i in c(1:nrow(indexMat))){
      #Checking if property A OR property B does not belong to a cluster
      #If both properties belong to a cluster, they are not considered
      if ((clusterX[indexMat[i,1]]==0)|(clusterX[indexMat[i,2]]==0)){
        #Checking if property A AND property B does not belong to a cluster
        if ((clusterX[indexMat[i,1]]==0)&(clusterX[indexMat[i,2]]==0)){
          #Checking if the similarity is greater than a threshold.
          if (max(allDist[indexMat[i,1],indexMat[i,2]],allDist[indexMat[i,2],indexMat[i,1]])>distThreshold){
            #Generation of a new cluster
            numCluster=numCluster+1
            count=count+2
            clusterX[indexMat[i,1]]=numCluster
            clusterX[indexMat[i,2]]=numCluster
            clusterList[[numCluster]]=c(indexMat[i,1],indexMat[i,2])
          } else {
            #Assigning the element to a cluster that is not important
            count=count+2
            clusterX[indexMat[i,1]]=1
            clusterX[indexMat[i,2]]=1
            #Updating the list of the elements belonging to the "garbage" cluster
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,1],indexMat[i,2])
          }
        }

        #Property A does not belong to a cluster, but property B belong to one of them
        if ((clusterX[indexMat[i,1]]==0)&(clusterX[indexMat[i,2]]>0)){
          #Checking if the similarity is greater than a threshold.
          if (max(allDist[indexMat[i,1],indexMat[i,2]],allDist[indexMat[i,2],indexMat[i,1]])>distThreshold){
            #Assigning cluster of property B to property A
            count=count+1
            clusterX[indexMat[i,1]]=clusterX[indexMat[i,2]]
            #Updating the list with the elements of each cluster
            clusterList[[clusterX[indexMat[i,2]]]]=c(clusterList[[clusterX[indexMat[i,2]]]],indexMat[i,1])
          } else {
            #Assigning the element to a cluster that is not important
            clusterX[indexMat[i,1]]=1
            #Updating the list of the elements belonging to the "garbage" cluster
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,1])
          }
        }

        #Property B does not belong to a cluster, but property A belong to one of them
        if ((clusterX[indexMat[i,1]]>0)&(clusterX[indexMat[i,2]]==0)){
          if (max(allDist[indexMat[i,1],indexMat[i,2]],allDist[indexMat[i,2],indexMat[i,1]])>distThreshold){
            count=count+1
            clusterX[indexMat[i,2]]=clusterX[indexMat[i,1]]
            clusterList[[clusterX[indexMat[i,1]]]]=c(clusterList[[clusterX[indexMat[i,1]]]],indexMat[i,2])
          } else {
            clusterX[indexMat[i,2]]=1
            clusterList[[1]]=c(clusterList[[1]],indexMat[i,2])
          }
        }
      }
      if (count==numProperties){
        #This break the cycle once all the properties has been considered
        break
      }
    }
  } else {
    clusterX=clusterX+1
    clusterList[[1]]=c(1:numProperties)
    #Case where all the clusters go to the garbage cluster
  }

  ###############################
  #ORDERING BASED ON THE CLUSTER#
  ###############################
  finalOrder=numeric(numProperties)
  count=1
  for (i in clusterList){
    if (count==1){
      if (!is.null(i)){
        finalOrder[(numProperties-length(i)+1):numProperties]=i
        count=count+1
      }
    } else {
      finalOrder[which(finalOrder==0)[1]:(which(finalOrder==0)[1]+length(i)-1)]=i
    }
  }

  #Sorting the matrix with the new order
  origDist=origDist[finalOrder,finalOrder]
  sortedProperties=sortedProperties[finalOrder]

  ################NEW CLUSTERING PROCESS END################################

  ################PLOTTING################################
  #Plotting matrix
  tempDF=reshape2::melt(origDist)
  numCols=numProperties-length(clusterList[[1]]) #Number of minimum animals mentioned

  # erasing zero data
  # data_return  <- tempDF[tempDF$value != 0, ]
  data_return = tempDF
  data_return$Var1 <- factor(data_return$Var1, levels = 1:length(sortedProperties), labels = sortedProperties)
  data_return$Var2 <- factor(data_return$Var2, levels = 1:length(sortedProperties), labels = sortedProperties)

  grafico_clusters <- ggplot2::ggplot(tempDF) +
    ggplot2::aes_string(x="Var1",y="Var2",fill="value")+
    ggplot2::geom_raster(alpha=0.95)+
    ggplot2::scale_x_continuous(expand=c(0,0),breaks=seq(1,numProperties,1),labels = sortedProperties)+
    ggplot2::scale_y_continuous(expand=c(0,0),breaks=seq(1,numProperties,1),labels = sortedProperties)+
    #labs(title="Data points distance matrix",x="data points",y="data points",fill="distance")+
    #labs(title=nombre,x="data points",y="data points",fill="similarity")+
    ggplot2::labs(title=paste("Similarity matrix and clusters for concept: ", concept),x="data points",y="data points",fill="similarity")+
    #scale_fill_gradient2(low="blue",mid="cyan",high="white",midpoint = (max(allDist)+min(allDist))/2)+
    #scale_fill_gradient2(low="white",mid="cyan",high="blue",midpoint = (max(allDist)+min(allDist))/2)+
    ggplot2::scale_fill_gradientn(colors = c("white","white","cyan","blue"),values=c(0,0.1,0.15,1))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90,vjust=0.5,hjust=1))+
    ggplot2::theme(axis.text.y = ggplot2::element_text(vjust=0.3))+
    ggplot2::theme(axis.title = ggplot2::element_blank())+
    ggplot2::theme(legend.background = ggplot2::element_rect(size=0.5, linetype="solid", colour ="black"))+
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color="black"))+
    ggplot2::coord_cartesian(xlim=c(0.5,numCols+0.5),ylim=c(0.5,numCols+0.5))

  return(list(grafico_clusters, data_return))



}
