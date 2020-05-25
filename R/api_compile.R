#' @export
cpt_list <- function(x) { 

  if (!is_named_list(x)) stop("x must be a named list. A name should be the name of the corresponding child node.")

  dim_names <- vector("list")
  
  y <- lapply(seq_along(x), function(i) {
    l <- x[[i]]
    class_allowed <- any(.map_lgl(.allowed_cpt_classes(), function(x) inherits(l, x)))
    if (!class_allowed) stop("one ore more elements in x is not an array-like object")
    if (!is_named_list(dimnames(l))) stop("one or more elements in x does not have proper dimnames")
    sptl <- as_sptable(l)
    diml <- dimnames(l) # used to populate empty potentials in new_charge
    lapply(diml, function(x) { # sptables cant handle lvls with nchar > 1L ! (thats why they are fast)
      for (e in x) {
        if (nchar(e) != 1L) {
          stop("dimnames of x is not on correct form. All levels are restricted to be a single character.")
        }
      }
    })
    for (k in seq_along(diml)) dim_names <<- push(dim_names, unname(diml[[k]]), names(diml)[k])
    return(sptl)
  })
  dim_names <- dim_names[unique(names(dim_names))]
  structure(y, names = names(x), class = c("cpt_list", class(x)), dim_names = dim_names)
}


## as2 <- dimnames_to_single_chars(asia2)
## cpt_list(as2)


#' @export
compile <- function(x, g = NULL, validate = TRUE) UseMethod("compile")


compile.cpt_list <- function(x, g = NULL) {
  # x is validated in cpt_list
  g       <- graph_from_cpt_list(x)
  parents <- parents_igraph(g)
  adj     <- adjacency_list_from_moralization_and_triangulation_igraph(g, parents)
  cliques <- construct_cliques_and_parents(adj)$cliques
  charge  <- new_charge(x, cliques, parents)
  structure(list(charge = charge, cliques = cliques), class = c("charge", "list"))
}


## compile(cpt_list(as2))
## compile(asia, g)

compile.data.frame <- function(x, g, validate = TRUE) {

  if (validate) {
    if( !only_single_chars(x)) {
      stop("All values in x must be represented as a single character. Use to_single_chars(x)")
    }
  }

  pe <- new.env()
  pe[["parents"]] <- NULL
  
  adj     <- adjacency_list_from_graph(g, pe)
  cp      <- construct_cliques_and_parents(adj)
  parents <- if (is.null(pe$parents))  cp$parents else pe$parents
  charge  <- new_charge(x, cp$cliques, parents)

  structure(list(charge = charge, cliques = cp$cliques), class = c("charge", "list"))
}


## ---------------------------------------------------------
##                     EXAMPLE
## ---------------------------------------------------------

## arr_lst <- lapply(asia2, function(x) {
##   x_arr <- as(x$prob, "array")
##   if (length(dim(x$prob)) == 1L) dimnames(x_arr) <- structure(dimnames(x_arr), names = x$node)
##   x_arr
## })

## spt_lst <- lapply(asia2, function(x) {
##   x_arr <- as(x$prob, "array")
##   if (length(dim(x$prob)) == 1L) dimnames(x_arr) <- structure(dimnames(x_arr), names = x$node)
##   as_sptable(x_arr, x$node)
## })


## # Make a print method for these.

## cl <- cpt_list(arr_lst)
## x  <- cpt_list(spt_lst)

## compile(spt_lst)

## cp <- compile(x)
