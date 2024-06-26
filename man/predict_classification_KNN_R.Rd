% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KNN_R.R
\name{predict_classification_KNN_R}
\alias{predict_classification_KNN_R}
\title{Prédiction de Classe pour Plusieurs Instances avec KNN}
\usage{
predict_classification_KNN_R(dataset, target_points, k)
}
\arguments{
\item{dataset}{Un data frame ou une matrice de l'ensemble d'entraînement avec les dernières colonnes
représentant la classe.}

\item{target_points}{Un data frame ou une matrice des points cibles pour lesquels effectuer des prédictions.}

\item{k}{Le nombre de voisins les plus proches à considérer pour chaque prédiction.}
}
\value{
Un vecteur d'entiers représentant la classe prédite pour chaque point cible.
}
\description{
Applique la prédiction KNN à un ensemble de points cibles, en utilisant un ensemble d'entraînement donné.
}
\examples{
data(iris)
target_points <- iris[146:150, -5]
predictions <- predict_classification_KNN_R(iris, target_points, k = 5)
}
