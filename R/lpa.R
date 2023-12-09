#' Label Propagation Algorithm
#' 
#' Run Label Propagation Algorithm on the igraph object. Depending on the method
#' parameter it will use either retention or original version of the 
#' algorithm. Currently setting initial state is not available and any
#' value of the "group" label will be overwritten. 
#' 
#' @param G igraph undirected graph.
#' @param method character value, either "retention" or "original".
#' @param max_i integer, maximum number of iterations for algorithm to run.
#' 
#' @import igraph
#' 
#' @return igraph object with "group" feature assigned to every node
#' @export
lpa <- function(G, method = c("retention", "original"), max_i = 50, verbose = TRUE) {
  if (igraph::is.directed(G)) {
    G <- igraph::as.undirected(G)
  }
  method <- match.arg(method, c("retention", "original"))
  n <- length(V(G))
  V(G)$group <- as.character(seq(n))
  
  G <- switch(method,
    retention = .lpa_retention(G, max_i, verbose),
    original = .lpa_original(G, max_i, verbose)
  )
  
  V(G)$group <- match(V(G)$group, unique(V(G)$group))
  G
}

#' Get LPA groups
#' 
#' Extract which nodes belong to which LPA cluster. Function requires
#' graph generated with the lpa() function.
#' 
#' @param G lpa() output - igraph object with "group" label assigned to each node
#' @export
get_lpa_group <- function(G) {
  if (is.null(V(G)$group)) {
    stop("Run lpa() first")
  }
  
  lapply(unique(V(G)$group), function(x) {
    which(V(G)$group == x)
  })
}



.lpa_original <- function(G, max_i, verbose) {
  n <- length(V(G))
  for (i in seq(max_i)) {
    
    if (c_max(G)) {
      verbose_message("LPA converged!", verbose = verbose)
      break()
    }
    
    verbose_message(sprintf("Iteration: %s", i), verbose = verbose)
    
    for (v in sample(seq(n), n)) {
      verbose_message(sprintf("Node %s of %s", v, n), verbose = verbose)
      max_labels <- get_neighbors_max_labels(G, V(G)[v])
      v_label <- V(G)[v]$group
      V(G)[v]$group <- sample(max_labels, 1)
    }
    
  }
  G
}

.lpa_retention <- function(G, max_i, verbose) {
  n <- length(V(G))
  for (i in seq(max_i)) {
    
    updated <- FALSE
    verbose_message(sprintf("Iteration: %s", i), verbose = verbose)
    
    for (v in sample(seq(n), n)) {
      verbose_message(sprintf("Node %s of %s", v, n), verbose = verbose)
      max_labels <- get_neighbors_max_labels(G, V(G)[v])
      v_label <- V(G)[v]$group
      if (!v_label %in% max_labels) {
        V(G)[v]$group <- sample(max_labels, 1)
        updated <- TRUE
      }
    }
    
    if (!updated) {
      verbose_message("LPA converged!", verbose = verbose)
      break()
    }
  }
  G
}

