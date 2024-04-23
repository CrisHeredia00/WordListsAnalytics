#' Calculate all the norms from a Conceptual properties
#'
#' @param orig_data Data frame with 3 columns: id, concept and properties
#' @return Data frame with all the estimations of norms
#' @export
#' @examples
#' data_test = data.frame(CPN_27)
#' generate_norms(data_test)

generate_norms <- function(orig_data) {

    # preprocessing the data eliminating repeated rows ------------------------
    #orig_data <- unique(orig_data)

    # changing all the word to lowercase and getting rid of spaces
    #orig_data[, 2] <- toupper(trimws(orig_data[, 2]))
    #orig_data[, 3] <- toupper(trimws(orig_data[, 3]))

    # Obtaining the unique concepts -------------------------------------------
    vConcept <- as.vector(unique(orig_data[, 2]))
    vConcept <- sort(vConcept)
    numConcepts <- length(vConcept)

    # Creating empty data frame with the elements to create and adding the concepts
    results <- data.frame(matrix(0, numConcepts, 11))
    names(results) <- c("Concept", "Q1", "Q2", "T", "S_obs", "U", "S_hat", "sd_S_hat", "CI_L", "CI_U",
        "C_T")
    results[, 1] <- vConcept

    # Estimating the characteristics for each concept ------------------------
    for (i in c(1:numConcepts)) {
        # Obtaining the set of data corresponding to each unique concept
        tempData = orig_data[orig_data[, 2] == vConcept[i], ]

        # list with unique and number of users for the concept
        users <- as.vector(unique(tempData[, 1]))
        numUsers <- length(users)
        results[i, 4] <- numUsers

        # List with unique features and number of features
        features <- as.vector(unique(tempData[, 3]))
        numFeatures <- length(features)  #S0
        results[i, 5] <- numFeatures

        # Transforming the data to a binary matrix of featuresXusers mtI[i,j]=1 implies feature i was
        # mentioned by user j
        lstI <- tapply(X = tempData[, 3], INDEX = tempData[, 1], FUN = function(x) {
            features %in% x
        })
        mtI <- matrix(unlist(lstI), ncol = numUsers, byrow = F)
        mtI <- mtI * 1

        # obtaining the value of U
        U <- sum(sum(mtI))
        results[i, 6] <- U

        # frequency of each feature
        frequencyFeature <- rowSums(mtI)

        # vector of Q vectorQ[2]=5 implies that 5 features were named only two times
        vectorQ <- numeric(numUsers)
        vectorQ[as.numeric(names(table(frequencyFeature)))] <- table(frequencyFeature)

        # Obtaining Q1 and Q2
        if (length(vectorQ)==1){
          vectorQ[2]=0 #in this case Q2 does not exist
        }
        results[i, 2:3] = vectorQ[1:2]

        # Calculating the varianza for the estimation of S (Shat)
        A <- (numUsers - 1)/numUsers

        # Analyzing the possible cases for Q
        if (vectorQ[2] > 0) {
            Q0hat <- A * (vectorQ[1]^2)/(2 * vectorQ[2])

            # estimating S and its variance
            Shat <- results[i, 5] + Q0hat
            Q1_2 <- vectorQ[1]/vectorQ[2]  #ratio entre Q1 y Q2
            varShat <- vectorQ[2] * ((A/2) * Q1_2^2 + A^2 * Q1_2^3 + (A^2)/4 * Q1_2^4)

        } else {
            Q0hat <- A * (vectorQ[1] * (vectorQ[1] - 1))/2

            # estimating S and its variance
            Shat <- results[i, 5] + Q0hat
            varShat <- A * vectorQ[1] * (vectorQ[1] - 1)/2 + A^2 * vectorQ[1] * (2 * vectorQ[1] - 1)^2/4 -
                A^2 * vectorQ[1]^4/(4 * Shat)
        }

        # Recording the obtained results
        results[i, 7] <- Shat
        results[i, 8] <- sqrt(varShat)

        # Estimating the confidence intervals Note, a log-normal distributuion is assumed
        D <- exp(1.96 * sqrt(log(1 + (varShat/(Shat - numFeatures)^2))))
        results[i, 9] <- numFeatures + ((Shat - numFeatures)/D)
        results[i, 10] <- numFeatures + ((Shat - numFeatures) * D)

        # Estimating the coverage
        results[i, 11] <- 1 - vectorQ[1]/U * ((vectorQ[1] * (numUsers - 1))/(vectorQ[1] * (numUsers -
            1) + 2 * vectorQ[2]))
    }
    row.names(results) <- NULL
    return(results)
}
