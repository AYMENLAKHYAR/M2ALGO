#' Classe Node pour l'Arbre KD
#'
#' Cette classe représente un nœud dans un arbre KD, avec des champs pour le point (coordonnées et classe),
#' les fils gauche et droit, et la dimension utilisée pour le split.
#'
#' @field point Un vecteur numérique incluant les coordonnées et la classe du point.
#' @field left Le fils gauche du nœud, représentant l'espace de partitionnement à gauche ou en dessous du point de split.
#' @field right Le fils droit du nœud, représentant l'espace de partitionnement à droite ou au-dessus du point de split.
#' @field dimension La dimension (indice de colonne) utilisée pour le split à ce nœud.
#' @examples
#' node <- Node_R$new(point = c(1.5, 2.5, 1), left = NULL, right = NULL, dimension = 1)



Node_R <- setRefClass("Node",
                      fields = list(point = "numeric", # Point includes coordinates and class
                                    left = "ANY",  # "ANY" type allows for recursive structures
                                    right = "ANY",
                                    dimension = "numeric"))
################################################################################################################################################################################

#' Construit un arbre KD
#'
#' Prend un ensemble de points et construit récursivement un arbre KD.
#'
#' @param points Un data.frame contenant les points et leurs classes. La dernière colonne est supposée être la classe.
#' @param depth La profondeur actuelle dans l'arbre, utilisée pour alterner les dimensions de split.
#' @return Un objet `Node_R` représentant la racine de l'arbre KD construit.
#' @export
#' @examples
#' data <- data.frame(x = runif(10), y = runif(10), class = sample(1:2, 10, replace = TRUE))
#' kd_tree <- build_kdtree_R(data)


build_kdtree_R <- function(points, depth = 0) {
  if (nrow(points) == 0) {
    return(NULL)
  }

  # Détermine l'axe de division à ce niveau de profondeur.

  k <- ncol(points) - 1 # Assuming last column is class
  axis <- depth %% k

  # Trie les points selon l'axe actuel avant de trouver le médian.
  points <- points[order(points[, axis + 1]), ]
  median_index <- ceiling(nrow(points) / 2)

  # Crée un nouveau noeud avec le point médian et récursivement construit les sous-arbres gauche et droit.
  node <- Node_R$new()
  node$point <- as.numeric(points[median_index, ])
  node$dimension <- axis

  node$left <- if (median_index - 1 > 0) build_kdtree_R(points[1:(median_index-1), , drop = FALSE], depth + 1) else NULL
  node$right <- if (median_index < nrow(points)) build_kdtree_R(points[(median_index+1):nrow(points), , drop = FALSE], depth + 1) else NULL

  return(node)
}

################################################################################################################################################################################

#' Calcule la Distance Euclidienne
#'
#' Calcule la distance euclidienne entre deux points.
#'
#' @param point1 Un vecteur numérique représentant le premier point.
#' @param point2 Un vecteur numérique représentant le second point.
#' @return La distance euclidienne entre `point1` et `point2`.
#' @export
#' @examples
#' euclidean_distance(c(1, 2), c(3, 4))

euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

################################################################################################################################################################################

#' Recherche des Plus Proches Voisins dans un Arbre KD
#'
#' Pour un point donné, trouve les k plus proches voisins dans un arbre KD.
#'
#' @param node Le nœud racine de l'arbre KD.
#' @param target_point Le point cible pour lequel trouver les plus proches voisins.
#' @param k Le nombre de voisins à trouver.
#' @param depth La profondeur actuelle, utilisée pour choisir la dimension du split.
#' @param neighbors Une liste initialement vide pour stocker les voisins trouvés.
#' @return Une liste contenant les k plus proches voisins du `target_point`.
#' @export
#' @examples
#' # Voir l'exemple de `build_kdtree_R` pour construire un arbre KD avant d'utiliser cette fonction.


nearest_neighbors_search_R <- function(node, target_point, k, depth = 0, neighbors = NULL) {
  if (is.null(neighbors)) {
    # Initialisation de la liste des voisins si elle n'est pas fournie.
    neighbors <- list(points = vector("list", k), distances = rep(Inf, k), classes = vector("list", k))
  }

  if (is.null(node)) {
    # Cas de base: si le nœud est NULL, retourne les voisins actuels.
    return(neighbors)
  }

  # Calcul de la distance entre le point cible et le point du nœud actuel.
  current_dist <- euclidean_distance(target_point, node$point[1:2])

  # Mettre à jour les voisins si le point actuel est plus proche que l'un des voisins actuels.
  max_dist_index <- which.max(neighbors$distances)
  if (current_dist < neighbors$distances[max_dist_index]) {
    neighbors$distances[max_dist_index] <- current_dist
    neighbors$points[max_dist_index] <- list(node$point[1:2])
    neighbors$classes[max_dist_index] <- node$point[3]

    # Trier les voisins par ordre de distance croissante
    order_indexes <- order(neighbors$distances)
    neighbors$points <- neighbors$points[order_indexes]
    neighbors$distances <- neighbors$distances[order_indexes]
    neighbors$classes <- neighbors$classes[order_indexes]
  }

  # Décide de la direction de la recherche (gauche ou droite) basée sur l'axe actuel et la distance.
  axis <- depth %% (length(target_point) - 1) # -1 car target_point n'inclut pas de classe
  if (!is.null(node$left) && (target_point[axis + 1] < node$point[axis + 1] || current_dist < neighbors$distances[k])) {
    neighbors <- nearest_neighbors_search_R(node$left, target_point, k, depth + 1, neighbors)
  }

  if (!is.null(node$right) && (target_point[axis + 1] >= node$point[axis + 1] || current_dist < neighbors$distances[k])) {
    neighbors <- nearest_neighbors_search_R(node$right, target_point, k, depth + 1, neighbors)
  }

  return(neighbors)
}

################################################################################################################################################################################


#' Prédit la Classe d'un Point Utilisant l'Arbre KD
#'
#' Utilise un arbre KD pour prédire la classe d'un ensemble de points cibles en trouvant leurs plus proches voisins.
#'
#' @param classes Les classes des k plus proches voisins trouvés.
#' @return La classe prédite pour le point cible.
#' @export
#' @examples
#' # Voir l'exemple de `nearest_neighbors_search_R` pour obtenir les classes avant d'utiliser cette fonction.


predict_class_KD_R <- function(classes) {

  # Compte le nombre d'occurrences de chaque classe parmi les voisins.
  table_classes <- table(unlist(classes))

  # Retourne la classe avec le plus grand nombre d'occurrences.
  return(names(which.max(table_classes)))
}

################################################################################################################################################################################

#' Prédit les Classes pour Plusieurs Points Utilisant l'Arbre KD
#'
#' @param dataset Le dataset d'entraînement utilisé pour construire l'arbre KD.
#' @param target_points Un data.frame de points cibles.
#' @param k Le nombre de plus proches voisins à considérer pour la prédiction.
#' @return Un vecteur des classes prédites pour chaque point cible.
#' @export
#' @examples
#' # Voir l'exemple de `build_kdtree_R` pour construire un arbre KD avant d'utiliser cette fonction.


predict_kdtree_R <- function(dataset, target_points, k) {
  kdtree <- build_kdtree_R(dataset) # Assumes build_kdtree_R is defined
  predictions <- numeric(length = nrow(target_points))
  for (i in 1:nrow(target_points)) {
    result <- nearest_neighbors_search_R(kdtree, target_points[i, ], k) # Assumes nearest_neighbors_search_R is defined
    predictions[i] <- predict_class_KD_R(result$classes) # Assumes predict_class_KD_R is defined
  }
  predictions
}
