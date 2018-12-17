agreg <- function(rangs, threshold) {
    points <- threshold - rangs + 1
    points[points < 0] <- 0
    return(sum(points))
}

structuration <- function(inputData, neighborhoodSize, threshold) {
  
  num_elements <- dim(inputData)[1]
  num_features <- dim(inputData)[2]
  
  scores <- lapply(seq_len(num_elements), function(x) numeric(num_elements))
  
  for (dimension in seq_len(num_features)) {
    distance_matrix <- rdist(inputData[, dimension])

    rangs <- matrix(rep(NA, num_elements * num_elements), nrow = num_elements)
    for (i in seq_len(num_elements)) {
      rangs[, i] <- rank(distance_matrix[, i], ties.method = "average")
    }
    
    dimensionScores <- numeric(num_elements)
    for (i in seq_len(num_elements)) {
      dimensionScores[i] <- agreg(rangs[i,], threshold)
    }
    
    for (element in seq_len(num_elements)) {
      trueNeighbors <- inputData[, dimension] - inputData[element, dimension]
      trueNeighbors <- abs(trueNeighbors)
      trueNeighbors <- order(trueNeighbors)
      trueNeighbors <- trueNeighbors[trueNeighbors != element]
      trueNeighbors <- trueNeighbors[seq_len(neighborhoodSize)]
      scores[[element]][trueNeighbors] <- scores[[element]][trueNeighbors] + dimensionScores[trueNeighbors]
    }
  }
  
  return(map_int(scores, which.max))
  
}

detailing <- function(inputData, neighborhoodSize, threshold, start, end) {
  
  num_elements <- dim(inputData)[1]
  num_features <- dim(inputData)[2]
  
  scores <- lapply(seq_len(num_elements), function(x) numeric(num_elements))
  
  results <- rep(0, num_features)
  
  for (dimension in seq_len(num_features)) {
    distance_matrix <- rdist(inputData[, dimension])
    
    rangs <- matrix(rep(NA, num_elements * num_elements), nrow = num_elements)
    for (i in seq_len(num_elements)) {
      rangs[, i] <- rank(distance_matrix[, i], ties.method = "average")
    }
    
    dimensionScores <- numeric(num_elements)
    for (i in seq_len(num_elements)) {
      dimensionScores[i] <- agreg(rangs[i,], threshold)
    }
    
    for (element in seq_len(num_elements)) {
      trueNeighbors <- inputData[, dimension] - inputData[element, dimension]
      trueNeighbors <- abs(trueNeighbors)
      trueNeighbors <- order(trueNeighbors)
      trueNeighbors <- trueNeighbors[trueNeighbors != element]
      trueNeighbors <- trueNeighbors[seq_len(neighborhoodSize)]
      scores[[element]][trueNeighbors] <- scores[[element]][trueNeighbors] + dimensionScores[trueNeighbors]
      
      if(element == start && end %in% trueNeighbors) {
        results[dimension] <- dimensionScores[end]
      } else if(element == start) {
        results[dimension] <- 0
      }
    }
  }
  
  return(results)
  
}