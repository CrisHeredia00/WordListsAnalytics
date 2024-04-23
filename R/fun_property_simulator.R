property_simulator <- function(orig_data, new_words, number_subjects) {

  #Obtaining the number of words per user
  concepts_per_subject=graphics::hist(orig_data[,1],breaks=unique(orig_data[,1]),plot=F)
  mean_cps=mean(concepts_per_subject$counts)
  std_cps=stats::sd(concepts_per_subject$counts)

  #Adding new words for the tail of the distribution
  temp_data=orig_data[,3]
  levels(temp_data)=c(levels(temp_data),as.character(c(1:new_words)))
  temp_data[(length(temp_data)+1):(length(temp_data)+new_words)]=as.character(c(1:new_words))

  #Estimating the empirical distribution using the neworiginal data and added new words
  emp_dist=table(temp_data)/length(temp_data)

  #Final sampling process
  #Generating an empty data frame
  new_properties=data.frame(ID=numeric(0),Concept=character(0),Property=character(0))
  for (i in c(1:number_subjects)){
    #Sampling the number of properties to be sampled
    num_properties=round(stats::rnorm(1,mean=mean_cps,sd=std_cps))

    #If the number of properties is less than 1, one property is sampled
    if (num_properties<1){
      num_properties=1
    }
    #If the number of properties is more than the possible number of properties, all properties are sampled
    if (num_properties>nlevels(temp_data)){
      num_properties=nlevels(temp_data)
    }

    #Sampling the new properties
    sampled_properties=sample(x=row.names(emp_dist),prob = emp_dist,replace=F,size=num_properties)

    #Creating the data frame qith the sampled properties
    temp_properties=data.frame(Subject=i,Concept=orig_data[1,2],Property=sampled_properties)

    #Adding the sampled properties to the data frame
    new_properties=rbind(new_properties, temp_properties)
  }
  return(new_properties)
}
