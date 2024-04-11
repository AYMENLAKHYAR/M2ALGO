#' Prédit la classe pour des points cibles en utilisant un KD-tree
#'
#' Cette fonction utilise un KD-tree implémenté en C++ pour prédire la classe
#' de points cibles donnés, basée sur un ensemble d'entraînement.
#'
#' @param dataset Un data frame ou une matrice représentant l'ensemble de données d'entraînement.
#' @param target_points Un data frame ou une matrice des points cibles pour lesquels prédire les classes.
#' @param k Le nombre de voisins les plus proches à considérer pour la prédiction.
#' @return Un vecteur numérique des classes prédites pour chaque point cible.
#' @examples
#' # Exemple d'utilisation ici
#' @export


predict_kdtree_cpp <- function(dataset, target_points, k) {
  pointsData <- as.matrix(dataset[, -ncol(dataset)])  # Préparation des données
  predictions <- numeric(length = nrow(target_points))

  for (i in 1:nrow(target_points)) {
    x <- target_points[i, 1]
    y <- target_points[i, 2]
    predictions[i] <- predictClassForPoint(pointsData, x, y, k)  # Appel direct de la fonction C++ comme une fonction R
  }
  return(predictions)
}
