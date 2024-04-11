#' Predict class labels for target points using the KNN algorithm implemented in C++
#'
#' This function takes a training set and a set of target points, prepares the data, and then
#' calls the C++ implemented KNN function for each target point to predict their classes.
#'
#' @param trainingSet A matrix or data frame of training points where the last column is the class label.
#' @param target_points A matrix or data frame of target points for which to predict the classes.
#' @param k The number of nearest neighbors to consider.
#' @return A numeric vector of predicted class labels for the target points.
#' @examples
#' trainingSet <- matrix(c(1, 2, 1, 3, 4, 2, 5, 6, 1, 7, 8, 2), ncol=3, byrow=TRUE)
#' target_points <- matrix(c(2, 3, 4, 5), ncol=2, byrow=TRUE)
#' k <- 3
#' predictions <- predict_knn_cpp(trainingSet, target_points, k)
#' @export


predict_knn_cpp <- function(trainingSet, target_points, k) {
  predictions <- numeric(length = nrow(target_points))

  # Preparation of the training set matrix, excluding the class label for KNN algorithm input.
  trainingSetMatrix <- as.matrix(trainingSet[, -ncol(trainingSet)])

  for (i in 1:nrow(target_points)) {
    x <- target_points[i, 1]
    y <- target_points[i, 2]

    # Call to the C++ KNN function for each target point.
    predictions[i] <- knnRcpp(trainingSetMatrix, x, y, k)
  }

  return(predictions)
}
