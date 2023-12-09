# Set of utility functions for lpa to work

get_neighbors_max_labels <- function(G, v) {
  labels <- neighbors(G, v)$group
  labels_freq <- table(labels)
  names(labels_freq)[which(labels_freq == max(labels_freq))]
}

c_max <- function(G) {
  if (is.null(V(G)$group)) stop("Graph without labels assigned")
  is_max <- vapply(1:length(V(G)), function(x) {
    max_labels <- get_neighbors_max_labels(G, V(G)[x])
    V(G)[x]$group %in% max_labels
  }, FUN.VALUE = logical(1))
  all(is_max)
} 

verbose_message <- function(..., verbose = TRUE) {
  if (verbose) message(...)
}