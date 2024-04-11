

#' Calcul de la Distance Euclidienne
#'
#' Cette fonction calcule la distance euclidienne entre deux points, représentés par des vecteurs numériques.
#'
#' @param row1 Un vecteur numérique représentant le premier point.
#' @param row2 Un vecteur numérique représentant le deuxième point.
#' @return Retourne un unique nombre réel représentant la distance euclidienne entre les deux points.
#' @examples
#' euclidean_distance(c(1, 2), c(4, 6))
#' @export


euclidean_distance <- function(row1, row2) {
  return(sqrt(sum((row1 - row2) ^ 2)))
}

################################################################################################################################################################################



#' Trouver les k Plus Proches Voisins
#'
#' Identifie les k plus proches voisins d'une instance de test dans un ensemble d'entraînement donné,
#' en utilisant la distance euclidienne comme mesure de proximité.
#'
#' @param training_set Un data frame ou une matrice où chaque ligne est une instance d'entraînement
#' et les colonnes représentent les caractéristiques (la dernière colonne peut représenter la classe).
#' @param test_instance Un vecteur numérique représentant l'instance de test.
#' @param k Le nombre de voisins les plus proches à identifier.
#' @return Un sous-ensemble de `training_set` contenant les k voisins les plus proches de l'instance de test.
#' @examples
#' data(iris)
#' test_instance <- iris[100, -5]
#' neighbors <- get_neighbors(iris, test_instance, k = 5)
#' @export


get_neighbors <- function(training_set, test_instance, k) {
  distances <- array(dim = nrow(training_set))
  for (i in 1:nrow(training_set)) {
    distances[i] <- euclidean_distance(test_instance, training_set[i, 1:ncol(training_set)-1])
  }
  sorted_indices <- order(distances)
  neighbors <- training_set[sorted_indices[1:k], ]
  return(neighbors)
}
################################################################################################################################################################################

#' Prédiction de Classe avec K Plus Proches Voisins
#'
#' Prédit la classe d'une instance de test basée sur les votes majoritaires des k plus proches voisins
#' dans l'ensemble d'entraînement.
#'
#' @param training_set Un data frame ou une matrice de l'ensemble d'entraînement avec les dernières colonnes
#' représentant la classe de chaque instance.
#' @param test_instance Un vecteur numérique représentant l'instance de test.
#' @param k Le nombre de voisins à considérer pour la prédiction.
#' @return La classe prédite pour l'instance de test, en tant qu'entier.
#' @examples
#' data(iris)
#' test_instance <- iris[150, -5]
#' class_predicted <- predict_classification_KNN(iris, test_instance, k = 5)
#' @export
predict_classification_KNN <- function(training_set, test_instance, k) {
  neighbors <- get_neighbors(training_set, test_instance, k)
  outcomes <- table(neighbors[, ncol(neighbors)])
  return(as.integer(names(which.max(outcomes))))
}
################################################################################################################################################################################


#' Prédiction de Classe pour Plusieurs Instances avec KNN
#'
#' Applique la prédiction KNN à un ensemble de points cibles, en utilisant un ensemble d'entraînement donné.
#'
#' @param dataset Un data frame ou une matrice de l'ensemble d'entraînement avec les dernières colonnes
#' représentant la classe.
#' @param target_points Un data frame ou une matrice des points cibles pour lesquels effectuer des prédictions.
#' @param k Le nombre de voisins les plus proches à considérer pour chaque prédiction.
#' @return Un vecteur d'entiers représentant la classe prédite pour chaque point cible.
#' @examples
#' data(iris)
#' target_points <- iris[146:150, -5]
#' predictions <- predict_classification_KNN_R(iris, target_points, k = 5)
#' @export
predict_classification_KNN_R <- function(dataset, target_points, k) {
  sapply(1:nrow(target_points), function(i) {
    test_instance <- target_points[i, ]
    predict_classification_KNN(dataset, test_instance, k = k)
  })
}

