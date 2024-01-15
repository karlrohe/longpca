#' pick_dim
#'
#' This computes the Z-scores from cross-validated eigenvalues
#'
#' @param fo
#' @param tib
#' @param dimMax
#' @param num_bootstraps
#'
#' @return
#' @export
#'
#' @examples
pick_dim = function(fo, tib, dimMax=20, num_bootstraps=2){
  parsed_model =  parse_variables(fo, tib)
  outcome_variables = parsed_model[[1]]
  if(outcome_variables!="outcome_unweighted_1"){
    warning(" The left side of the formula for pick_dim should be 1. \n \n Other variables on the left-hand-side are likely to create unreliable p-values. In many settings it likely still makes a good-bit of sense to have 1.  This code will still run, but you have been warned!")
  }


  A = interaction2sparse(fo, tib)$A
  gdim::eigcv(A = A, k_max = dimMax, laplacian= TRUE,num_bootstraps = num_bootstraps)
}

#' pick_dim_text
#'
#' This computes the Z-scores from cross-validated eigenvalues for a bag-of-words matrix.
#'
#' The arguments ... get handed to unnest_tokens. See Details of pca_text for mmore.
#'
#' @param fo
#' @param tib
#' @param dimMax
#' @param num_bootstraps
#'
#' @return
#' @export
#'
#' @examples
pick_dim_text= function(fo, tib, dimMax=20, num_bootstraps=2, ...){
  A = make_sparse_text_matrix_raw(fo, tib, dropNA=TRUE, ...)$A
  gdim::eigcv(A = A, k_max = dimMax, laplacian= TRUE,num_bootstraps = num_bootstraps)
}
