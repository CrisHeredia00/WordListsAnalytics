# Shiny version, not executable outside a shiny session.
pa_calculator = function(data_1, data_2 = NULL, nr_repetitions = 50, max_iterations = 500, nr_points_moving_avg = 100, show_repetition = FALSE){

  if(nrow(data_1)<2){
    return(data.frame())
  }

  #Information about P(a) calculation
  if (is.null(data_2)){
    data_2 = data_1
    print("Calculating P(a) of data_1 with data_1")
  } else {
    print("Calculating P(a) of data_1 with data_2")
  }

  #Data checking
  #Data must have for columns
  if(ncol(data_1) != 4) stop ("data_1 must be a dataframe with 4 columns: concept, id of property; frequency of property, s value")
  if(ncol(data_2) != 4) stop ("data_2 must be a dataframe with 4 columns: concept, id of property; frequency of property, s value")

  #Data cannot have NA values
  if(anyNA(data_1)) stop("data_1 cannot have na values")
  if(anyNA(data_2)) stop("data_2 cannot have na values")

  #Frequency values must be numeric
  if(!(all(sapply(data_1[3], is.numeric)))) stop("Frequency of data_1 must be numeric")
  if(!(all(sapply(data_2[3], is.numeric)))) stop("Frequency of data_1 must be numeric")

  #Frequency values must be positive
  if(!all(data_1[3] >= 0) ) stop("frequency column of data_1 cannot have negative values")
  if(!all(data_2[3] >= 0) ) stop("frequency column of data_2 cannot have negative values")


  #Hyperparameters checking
  #Hyperparameters cannot be decimal values
  if(nr_repetitions%%1 != 0) stop("nr_repetitions cannot have decimal values")
  if(max_iterations%%1 != 0) stop("max_iterations cannot have decimal values")
  if(nr_points_moving_avg%%1 != 0) stop("nr_points_moving_avg cannot have decimal values")

  #Hyperparameter must be positive values
  if(nr_repetitions<=0) stop("nr_repetitions must be greater than zero")
  if(max_iterations<=0) stop("max_iterations must be greater than zero")
  if(nr_points_moving_avg<=0) stop("nr_points_moving_avg must be greater than zero")


  #Comparison between concept list of data_1 and data_2
  concept_list_1 = unique(data_1[[1]])
  concept_list_2 = unique(data_2[[1]])
  if(length(concept_list_1) != length(concept_list_2)) stop("Number of concepts in data_1 must be the same as in data_2")

  #Creation of data frame with the values (mean, standard deviation) calculated for the pairs of concepts
  pair_dataframe = data.frame("pair" = paste(concept_list_1, "-", concept_list_2), "mean" = NA, "std" = NA)

  #Showing the pairs that will be calculated
  print("The pair of concepts that will be calculated are:")
  print(data.frame("C1" = concept_list_1, "C2" = concept_list_2))
  print("****************************************************")

  #Estimating the agreement probability for each concept
  for (concept_pos in 1:length(concept_list_1)) {
    #It obtains the data of the current concept in the loop, this information is used to calculate P(a) per concept
    temp_data_1 = data_1[data_1[1] == concept_list_1[concept_pos],]
    temp_data_2 = data_2[data_2[1] == concept_list_2[concept_pos],]

    #Check that s values are equal in every row of the current concept
    if(stats::var(temp_data_1[[4]]) != 0) stop(paste("All s values in data_1 of concept:", concept_list_1[concept_pos], "must be equal"))
    if(stats::var(temp_data_1[[4]]) != 0) stop(paste("All s values in data_2 of concept:", concept_list_2[concept_pos], "must be equal"))

    #Variables to save the probability agreement estimations
    values_pa = numeric(nr_repetitions)

    #Property list and each weight. It is used to calculate weighted sample
    property_list_C1 = temp_data_1[[2]]

    print(temp_data_1)

    weight_list_C1 = temp_data_1[[3]]

    property_list_C2 = temp_data_2[[2]]


    weight_list_C2 = temp_data_2[[3]]

    #Check that the frequency column at least one value more than zero
    if(sum(weight_list_C1) <= 0) stop(paste("frequency column in data_1 of concept:", concept_list_1[concept_pos], "must sum more than zero"))
    if(sum(weight_list_C2) <= 0) stop(paste("frequency column in data_2 of concept:", concept_list_2[concept_pos], "must sum more than zero"))


    #Generating vector to figure out how many samples it have to do per iteration
    s1 = temp_data_1[[4]][1]
    s2 = temp_data_2[[4]][1]

    print("**********")
    print(s1)
    print(s2)

    #vectorS is a vector with the number of samples from a list of words.
    #If s is not an integer, sometimes we sample s and another we sample s+1
    vectorS1=numeric(nr_repetitions)+floor(s1)
    if (s1%%1 != 0){
      extraNum=stats::rbinom(nr_repetitions,1,s1 - floor(s1))
      vectorS1=vectorS1+extraNum
    }
    vectorS2=numeric(nr_repetitions)+floor(s2)
    if (s2%%1 != 0){
      extraNum=stats::rbinom(nr_repetitions,1,s2 - floor(s2))
      vectorS2=vectorS2+extraNum
    }

    #Calculation of P(A)
    for (reps_counter in 1:nr_repetitions) {
      #For each repetition the values are set to zero
      pa_counter = 0
      list_values_pa = vector(mode = "double", length = nr_points_moving_avg)
      pos_pa = 0
      indexSample=sample(vectorS2[reps_counter], max_iterations,replace=TRUE)

      #Creating max_iterations to estimate the agreement probability
      for (counter_iters in 1:max_iterations) {

        #Get the weighted sample from property_list. The number of items is obtained from vectorS and the weight from weight_list_C
        sample_C1 = sample(property_list_C1, vectorS1[reps_counter], replace = FALSE, prob = weight_list_C1) #DEBERIA SER FALSE
        sample_C2 = sample(property_list_C2, vectorS2[reps_counter], replace = FALSE, prob = weight_list_C2) #DEBERIA SER FALSE

        #It obtains a sample from sample_C2
        one_prop_sample_C2 = sample_C2[indexSample[counter_iters]]

        #If one_prop_sample_C2 is contained in sample_c1 then add one to pa_counter
        if(one_prop_sample_C2 %in% sample_C1) {pa_counter = pa_counter + 1}

        #vector recording the average P(a) according the number of iterations.
        #The first values are lately overwritten, estimating the agreement probability with just the last nr_points_moving_avg
        list_values_pa[pos_pa%%nr_points_moving_avg+1] = pa_counter/counter_iters #pa = pa_counter/counter_iters
        pos_pa = pos_pa+1
      }
      #Condition to show information in the console
      if (show_repetition){
        print(paste("Repetition: ", reps_counter))
        print(paste("P(a) until repetition: ", mean(list_values_pa)))
      }

      #recording values_pa, the mean of P(a) values calculated in all the iterations
      values_pa[reps_counter] = mean(list_values_pa)
    }
    print(paste("Avg. P(a) for", concept_list_1[concept_pos], "-", concept_list_2[concept_pos], "  = ", mean(values_pa)))
    print(paste("Std. dev P(a) for", concept_list_1[concept_pos], "-", concept_list_2[concept_pos], " = ", stats::sd(values_pa)))
    print("----------------------------------------------------")

    #It adds the mean and the standard deviation of P(a) calculated per repetition
    pair_dataframe[pair_dataframe$pair == paste(concept_list_1[concept_pos], "-", concept_list_2[concept_pos]), "mean"] = mean(values_pa)
    pair_dataframe[pair_dataframe$pair == paste(concept_list_1[concept_pos], "-", concept_list_2[concept_pos]), "std"] = stats::sd(values_pa)
    shiny::incProgress(1/length(concept_list_1), detail = paste("Doing concepts: ", concept_list_1[concept_pos], "-", concept_list_2[concept_pos]))
  }
  return(pair_dataframe)
}



