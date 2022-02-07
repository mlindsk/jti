#' jti: Junction Tree Inference
#'
#' Minimal and memory efficient implementation of the junction tree
#' algorithm using the Lauritzen-Spiegelhalter scheme.
#'
#' The main functions are \code{cpt_list}, \code{compile},\code{jt}
#' and \code{query_belief} which together is sufficient to make
#' inference using the junction tree algorithm.
#'
#' @references Local Computations with Probabilities on Graphical Structures
#' and Their Application to Expert Systems by S. L. Lauritzen
#' and D. J. Spiegelhalter (1988). Journal of the Royal Statistical
#' Society: Series B (Methodological) volume 50, issue 2.
#' 
#' @importFrom Rcpp sourceCpp
#' @useDynLib jti, .registration = TRUE
"_PACKAGE"
