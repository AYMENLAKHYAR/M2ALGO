library(microbenchmark)
library(ggplot2)

# Supposons que vos fonctions soient déjà disponibles dans l'environnement
# par exemple, en ayant chargé le package qui les contient.

# Fonction pour générer des données de test
generate_data <- function(n) {
  x <- rnorm(n, mean = 5, sd = 2)
  y <- rnorm(n, mean = 3, sd = 1.5)
  classes <- sample(c(rep(0, ceiling(n / 2)), rep(1, floor(n / 2))))
  dataset <- data.frame(x = x, y = y, class = classes)
  return(dataset)
}

run_and_save_benchmark <- function(function_name, dataset_sizes, target_points, k, file_name_prefix) {
  results_list <- list()

  for (size in dataset_sizes) {
    dataset <- generate_data(size)

    # Utiliser `do.call` pour appeler la fonction par son nom
    mbm_results <- microbenchmark(
      do.call(function_name, list(dataset, target_points, k = k)),
      times = 10
    )

    # Stocker les résultats
    results_list[[as.character(size)]] <- mbm_results
  }

  # Construire le chemin vers le dossier où enregistrer les résultats
  results_dir <- file.path(getwd(), "benchmark_results")
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }

  # Enregistrer les graphiques
  for (size in names(results_list)) {
    plot <- ggplot2::autoplot(results_list[[size]]) +
      ggtitle(paste("Performance de", function_name, "\nTaille du jeu de données:", size))

    ggsave(paste0(results_dir, "/", file_name_prefix, "_", size, ".png"), plot, path = results_dir)
  }
}

# Définir les points cibles une seule fois
target_points <- matrix(rnorm(60, mean = c(5, 3), sd = c(2, 1.5)), ncol = 2)

# Exécuter les benchmarks pour chaque fonction
run_and_save_benchmark("predict_classification_KNN_R", c(100, 300), target_points, 5, "KNN_R")
run_and_save_benchmark("predict_knn_cpp", c(100, 300), target_points, 5, "KNN_CPP")
run_and_save_benchmark("predict_kdtree_R", c(100, 300), target_points, 5, "KDTREE_R")
run_and_save_benchmark("predict_kdtree_cpp", c(100, 300), target_points, 5, "KDTREE_CPP")
