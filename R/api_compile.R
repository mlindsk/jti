## #' @export
## cpt_list2 <- function(x) { 
##   # x: list of cpts with dimnames

##   # TODO: Test if x is already a sptable and return it if so!
  
##   if (!is_named_list(x)) {
##     stop("x must be a named list of cpts. A name should be the name of the corresponding child node.")
##   }

##   lvls <- vector("list")
  
##   y <- lapply(seq_along(x), function(i) {
##     l <- x[[i]]
##     class_allowed <- any(.map_lgl(.allowed_cpt_classes(), function(x) inherits(l, x)))
##     if (!class_allowed) stop("one ore more elements in x is not an array-like object")
##     if (!is_named_list(dimnames(l))) stop("one or more elements in x does not have proper dimnames")


##     # TODO: make a char_array instead of dimnames_to_chars(cpts)

##     sptl <- as_sptable(l)
##     lu <- dimnames(l) # used to populate/create empty/unity potentials in new_charge

##     # Dont use lapply here

##     # Dont test!! Just make a char_frame - no matter if they all have nchar == 1 in advance!
##     lapply(lu, function(x) { # sptables cant handle lvls with nchar > 1L ! (thats why they are fast)
##       for (e in x) {
##         if (nchar(e) != 1L) {
##           stop("dimnames of x is not on correct form. All levels are restricted to be a single character.")
##         }
##       }
##     })

    
##     for (k in seq_along(lu)) lvls <<- push(lvls, unname(lu[[k]]), names(lu)[k])
##     return(sptl)
##   })

##   lvls <- lvls[unique(names(lvls))]

##   # TODO: lvls should now conform with char_frame api !!! It should be of class lookup!
##   # so; lvls <- lookup(lu)
##   structure(y, names = names(x), class = c("cpt_list", class(x)), lvls = lvls)
## }


#' @export
cpt_list <- function(x) { 
  # x: list of cpts with dimnames

  if (inherits(x, "sptable")) return(x)
  
  if (!is_named_list(x)) {
    stop("x must be a named list of cpts. A name should be the name of the corresponding child node.")
  }

  lookup <- vector("list")
  
  y <- lapply(seq_along(x), function(i) {

    l <- x[[i]]

    class_allowed <- any(.map_lgl(.allowed_cpt_classes(), function(x) inherits(l, x)))
    if (!class_allowed) stop("one ore more elements in x is not an array-like object")

    cal  <- char_array(l)
    sptl <- as_sptable(cal, validate = FALSE)
    lu   <- lookup(cal) # used to populate/create empty/unity potentials in new_charge

    for (k in seq_along(lu)) {
      if (names(lu)[k] %ni% names(lookup)) {
        lookup <<- push(lookup, lu[[k]], names(lu)[k])  
      }
    }
    
    return(sptl)
  })

  class(lookup) <- c("lookup", class(lookup))
  
  structure(y, names = names(x), lookup = lookup, class = c("cpt_list", class(x)))
}



# TODO: Make a vanilla print method avoiding all the sptables

#' @export
compile <- function(x, g = NULL) UseMethod("compile")


compile.cpt_list <- function(x, g = NULL) {
  # x is validated in cpt_list
  if (!is.null(g)) stop("g must be 'NULL'")
  g       <- graph_from_cpt_list(x)
  parents <- parents_igraph(g)
  adj     <- adjacency_list_from_moralization_and_triangulation_igraph(g, parents)
  cliques <- construct_cliques_and_parents(adj)$cliques
  charge  <- new_charge(x, cliques, parents)
  # TODO: Also output the lookup!
  structure(list(charge = charge, cliques = cliques), lookup = attr(x, "lookup"), class = c("charge", "list"))
}

compile.data.frame <- function(x, g) {

  # TODO: Make a vanilla print method

  x <- char_frame(x)

  # Used to store parents in construct_cliques_and_parents if g != igraph.
  pe <- new.env()
  pe[["parents"]] <- NULL
  
  adj     <- adjacency_list_from_graph(g, pe)
  cp      <- construct_cliques_and_parents(adj)
  parents <- if (is.null(pe$parents))  cp$parents else pe$parents
  charge  <- new_charge(x, cp$cliques, parents)

  structure(list(charge = charge, cliques = cp$cliques), lookup = lookup(x), class = c("charge", "list"))
}
