## JTI SPECIFIC
check_query <- function(prop, type, inconsistent, flow, nodes, evidence, clique_root) {
  if (prop == "no") {
    stop("It is not possible to query from a junction tree that ",
      "hasn't been propagated.", call. = FALSE
    )
  }
  
  if (type %ni% c("marginal", "joint")) {
    stop("Type must be 'marginal' or 'joint'.", call. = FALSE)
  }

  if (type == "joint" && inconsistent) {
    # TODO: It may be OK for some joints?
    stop(
      "It is not possible to make joint queries when ",
      "there is inconsistent evidence", call. = FALSE
    )
  }
  
  if (flow == "max") {
    stop(
      "It does not make sense to query probablities from a junction",
      "tree with flow = 'max'. ",
      "Use 'mpe' to obtain the max configuration.", call. = FALSE
    )
  }

  if (any(nodes %in% evidence)) {
    stop("It is not possible to query probabilities from",
      "evidence nodes", call. = FALSE)
  }
  
  if (prop == "collect") if (!all(nodes %in% clique_root)) {
    stop(
      "All nodes must be in the root clique",
      "since the junction tree has only collected! ",
      "See get_clique_root(x) to find the nodes in the root clique.",
      call. = FALSE
    )
  }    
}

valid_evidence <- function(dim_names, e) {
  nemvc <- neq_empt_vector_chr(e)
  e_conforms_with_dim_names <- !anyNA(mapply(match, e, dim_names[names(e)]))
  if (e_conforms_with_dim_names && nemvc) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# .tri_options <- function(tri) {
#   c(
#     "min_fill",
#     "min_rfill",
#     "min_elfill",
#     "min_efill",
#     "min_sfill",
#     "min_elsp",
#     "min_esp",
#     "min_sp",
#     "min_nei",
#     "minimal",
#     "alpha"
#   )
# }

check_params_compile <- function(tri, pmf_evidence, alpha, nodes, root_node) {

  if (root_node != "") {
    if (root_node %ni% nodes) {
      stop("Invalid root_node", call. = FALSE)      
    }
  }
  
  # if (!(tri %in% .tri_options())) {
  #   stop("tri must be one of ", paste(.tri_options(), collapse = ", "), call. = FALSE)
  # }

  if (tri %in% c("min_efill", "min_esp") && is.null(pmf_evidence)) {
    stop(
      "tri = " , tri,
      "requires that pmf_evidence is specified",
      call. = FALSE)
  }

  if (tri == "alpha")  {
    if (is.null(alpha)) {
      stop("tri = 'alpha' requires that the alpha parameter is specified", call. = FALSE)
    }
    if (!identical(sort(alpha), sort(nodes))) stop("The alpha parameter was not specified correctly")
  }
  invisible(NULL)
}


## SETS
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lgl <- function(x) !identical(x, logical(0))
neq_empt_lst <- function(x) !identical(x, list())
neq_null     <- function(x) !is.null(x)
'%ni%'       <- Negate('%in%')


# Used in connection to marginalization of numbers
is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L && (inherits(x, "numeric") || inherits(x, "integer"))
}


## GRAPHS
is_decomposable <- function(adj) {
  m <- try(mcs(adj), silent = TRUE)
  if( inherits(m, "list") ) return(TRUE)
    else return(FALSE)
}

## MISC
neq_empt_vector_chr <- function(x) is.character(x) && length(x) > 0L

is_named_list <- function(x) {
  if (is.null(names(x))) return(FALSE)
  if ("" %in% names(x)) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}
