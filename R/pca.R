
# glaplacian: this normalizes and regularizes the data matrix.

#' glaplacian
#'
#' Normalizes and regularizes a sparse adjacency Matrix with deg_row and deg_col as the number of non-zero elements in each row/column
#'
#' @param A
#' @param regularize
#'
#' @return
#' @export
#'
#' @examples
glaplacian <- function(A, regularize = TRUE) {
  # deg_row <- Matrix::rowSums(abs(A))
  # deg_col <- Matrix::colSums(abs(A))
  deg_row <- Matrix::rowSums(A!=0)
  deg_col <- Matrix::colSums(A!=0)

  tau_row <- mean(deg_row)
  tau_col <- mean(deg_col)

  D_row = Matrix::Diagonal(nrow(A), 1 / sqrt(deg_row + tau_row))
  D_col = Matrix::Diagonal(ncol(A), 1 / sqrt(deg_col + tau_col))
  L = D_row %*% A %*% D_col
  rownames(L) = rownames(A)
  colnames(L) = colnames(A)
  return(L)
}



#' get_Matrix
#'
#' @param interaction_model
#'
#' @return
#' @export
#' @importFrom dplyr rowwise transmute c_across all_of ungroup pull
#'
#' @examples
get_Matrix = function(interaction_model, import_names = FALSE){
  A = Matrix::sparseMatrix(
    i = interaction_model$interaction_tibble$row_num,
    j = interaction_model$interaction_tibble$col_num,
    x = interaction_model$interaction_tibble$outcome,
    dims = c(nrow(interaction_model$row_universe), nrow(interaction_model$column_universe)))

  if(import_names){

    these_row_names = interaction_model$row_universe |>
      rowwise() |>
      transmute(combined = paste(c_across(all_of(rev(interaction_model$settings$row_variables))), collapse = "/")) |>
      ungroup() |>
      pull(combined)

    these_col_names = interaction_model$column_universe |>
      rowwise() |>
      transmute(combined = paste(c_across(all_of(rev(interaction_model$settings$column_variables))), collapse = "/")) |>
      ungroup() |>
      pull(combined)

    rownames(A) = these_row_names
    colnames(A) = these_col_names
  }
  return(A)
}

get_Incomplete_Matrix = function(interaction_model){
  softImpute::Incomplete(i = interaction_model$interaction_tibble$row_num,
                         j = interaction_model$interaction_tibble$col_num,
                         x = interaction_model$interaction_tibble$outcome)

}





#' update_lhs_to_1 (internal to pca_count)
#'
#' @param formula
#'
#' @return
#'
#' @examples
update_lhs_to_1 <- function(formula, quiet = FALSE) {
  if (length(formula) == 3) {
    # If the formula has an LHS, replace it with "outcome"

    if(formula[[2]]!=1 ){

      formula[[2]] <- 1
      if(quiet) warning("left hand side of formula has been updated to 1 because this is a call to pca_count which requires 1. The formula is now:\n\n", deparse(formula))
      # print()
    }
  } else if (length(formula) == 2) {
    # If the formula does not have an LHS, add "outcome"
    formula <- stats::as.formula(paste("1", deparse(formula)))
  }
  return(formula)
}




#' pca_count
#'
#' this is the first user function to generate pcs.
#'
#' @param fo a formula, like  ~ (row_ids & context) * measurement_type. The left hand side should be left empty.  If it isn't, it will be converted to 1. Either way, the elements of the sparse matrix will just be counts.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#' @param k number of dimensions for the pca
#'
#' @return the output is a list of: (1) row_features for whichever term is first on the right hand side of the formula, (2) column_features for whichever term is second on the right hand side of the formula, (3) middle_B which is a tibble corresponding to a diagonal matrix in sparse triplet form, (4) settings which is a list of details.
#' @export
#'
#' @examples
pca_count = function(fo, tib, k){


  # first, ensure the outcome is 1:
  fo = update_lhs_to_1(fo)
  pcs = pca_sum(fo, tib, k)
  pcs$settings$fit_method = "pca_count"
  pcs
}

#' pca_sum
#'
#' This performs pca on a sparse Matrix specified by the formula and the tibble.
#'
#' @param fo a formula, like  outcome ~ (row_ids & context) * measurement_type.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#' @param k number of dimensions for the pca
#'
#' @return the output is a list of: (1) row_features for whichever term is first on the right hand side of the formula, (2) column_features for whichever term is second on the right hand side of the formula, (3) middle_B which is a tibble corresponding to a diagonal matrix in sparse triplet form, (4) settings which is a list of details.
#' @export
#'
#' @examples
pca_sum = function(fo, tib, k){
  im = make_interaction_model(tib, fo)
  pca(im, k)
}

#' pca
#'
#' @param im
#' @param k
#' @param method_prefix
#'
#' @return
#' @export
#'
#' @examples
pca = function(im,k, method_prefix = "pc", regularize = TRUE, sqrt_counts = TRUE){
  A = get_Matrix(im)
  # A = sp_A_dat$A
  if(sqrt_counts) A@x = sqrt(A@x)
  if(regularize) A = glaplacian(A)
  # s_svd = irlba::irlba(A,nu = k, nv = k)
  s_svd = RSpectra::svds(A,k = k)

  # dimension_prefix = paste0(im$settings$data_prefix, method_prefix)
  # print(length(dimension_prefix))
  # print(str(im$settings$data_prefix))

  dimension_prefix = method_prefix

  pcs = s_2_pc(interaction_model = im, s = s_svd, dimension_prefix=dimension_prefix)

  # parsed_model =  parse_variables(fo, tib)
  settings = list(fit_method = "pca_sum",
                  prefix_for_dimensions = stringr::str_glue(dimension_prefix, "_"),
                  prefix_for_data = im$settings$data_prefix,
                  prefix_for_method = method_prefix,
                  k = k,
                  normalized = TRUE,
                  reguarlized = TRUE,
                  outcome_variables = im$settings$outcome_variables,
                  row_variables  = im$settings$row_variables,
                  column_variables  = im$settings$column_variables)

  pcs[[4]] = settings
  names(pcs)[4] = "settings"
  class(pcs) = "pc"
  pcs
}



#' pca_text
#'
#' given formula ~ row_ids * text, where text is a character string, construct a matrix where row's are indexed by row_ids and columns are (default) bag-of-words. Perform PCA on this matrix.
#'
#' Extended arguments ... are passed to tidytext::unnest_tokens. for example ... as token = "ngrams", n=2 would construct matrix of bigrams.
#'
#' TODO: make diagnose_text
#'
#' @param fo
#' @param tib
#' @param k
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pca_text = function(fo, tib, k, ...){


  im = make_interaction_model(tib, fo, parse_text = TRUE, ...)
  pca(im, k)

  # A = sp_A_dat$A
  # A@x = sqrt(A@x)
  # L = glaplacian(A)
  # s_svd = irlba::irlba(L,nu = k, nv = k)
  #
  # pcs = s_2_pc(sparse_matrix_data = sp_A_dat, s = s_svd, dimension_prefix=dimension_prefix)
  #
  #
  # settings = list(fit_method = "pca_sum",
  #                 prefix_for_dimensions = stringr::str_glue(dimension_prefix, "_"),
  #                 k = k,
  #                 normalized = TRUE,
  #                 reguarlized = TRUE,
  #                 outcome_variables = parsed_model[[1]],
  #                 row_variables  = parsed_model[[2]],
  #                 column_variables  = parsed_model[[3]])
  #
  # pcs[[4]] = settings
  # names(pcs)[4] = "settings"
  # class(pcs) = "pc"
  # pcs
}









#' remove_L_normalization (internal to pca_mean/pca_average)
#'
#' @param s_svd
#' @param A
#' @param orthogonalize
#'
#' @return
#'
#' @examples
remove_L_normalization = function(s_svd, A, orthogonalize= FALSE){
  # if using L in  s_svd(L) ,
  #  then we might want to remove that normalization in the output.
  #  this function does that.
  # deg_row <- Matrix::rowSums(abs(A))
  # deg_col <- Matrix::colSums(abs(A))
  deg_row <- Matrix::rowSums(A!=0)
  deg_col <- Matrix::colSums(A!=0)


  tau_row <- mean(deg_row)
  tau_col <- mean(deg_col)

  D_row = Matrix::Diagonal(nrow(A), sqrt(deg_row + tau_row))
  D_col = Matrix::Diagonal(ncol(A), sqrt(deg_col + tau_col))
  s_svd$u = D_row %*% s_svd$u
  s_svd$v = D_col %*% s_svd$v
  s_svd$u = as.matrix(s_svd$u)
  s_svd$v = as.matrix(s_svd$v)

  if(!orthogonalize) return(s_svd)
  # if we want to re-orthogonalize s$u and s$v....
  #  then you need to do some algebra.
  #  I think this is right, but has not been tested:
  su= svd(s_svd$u)
  sv= svd(s_svd$v)
  b= diag(su$d) %*% t(su$v) %*% diag(s_svd$d) %*% sv$v %*% diag(sv$d)
  sb = svd(b)
  u = su$u%*% sb$u
  d = sb$d
  v = sv$u %*% sb$v
  return(list(u = as.matrix(u), d = d, v = as.matrix(v)))
}

# pca_average does low-rank matrix completion


#' pca_na
#'
#' This performs pca on an interaction_model, where if a (row_id,col_id) are not present in the data, then it is presumed NA and imputed.  This contrasts to the other `pca` function which imputes the missing values to zero.
#'
#' @param fo
#' @param tib
#' @param k
#'
#' @return
#' @export
#'
#' @examples
pca_na = function(im, k){


  # the output is a pc object... a list of:
  # row_features (for whichever term is first on the right hand side of the formula)
  # column_features (for whichever term is second on the right hand side of the formula)
  # middle_B (this is a diagonal matrix, but in a triplet form in a tibble)
  # settings (this is a list of details)


  # im = make_interaction_model(tib, fo, duplicates= "average")
  # sp_A_dat = make_incomplete_matrix_raw(fo, tib)
  cat("Taking", k, "core.  Starting with:\n", nrow(im$row_universe), "rows\n",
      nrow(im$column_universe), "columns\n",
      nrow(im$interaction_tibble), "observed values")
  im = core(im,core_threshold = k)
  cat("After taking", k, "core.  There remain:\n", nrow(im$row_universe), "rows\n",
      nrow(im$column_universe), "columns\n",
      nrow(im$interaction_tibble), "observed values")


  A = get_Incomplete_Matrix(im)
  # A = sp_A_dat$A
  # L = glaplacian(A)
  # s_svd = softImpute::softImpute(L,rank.max = k)
  # s_svd = remove_L_normalization(s_svd,A)
  s_svd = softImpute::softImpute(A, rank.max = k)

  dimension_prefix = "na_pc"
  pcs = s_2_pc(interaction_model = im, s = s_svd, dimension_prefix=dimension_prefix)


  settings = list(fit_method = "pca_average",
                  prefix_for_dimensions = stringr::str_glue(dimension_prefix, "_"),
                  prefix_for_data = im$settings$data_prefix,
                  prefix_for_method = dimension_prefix,
                  k = k,
                  normalized = FALSE,
                  reguarlized = FALSE,
                  outcome_variables = im$settings$outcome_variables,
                  row_variables  = im$settings$row_variables,
                  column_variables  = im$settings$column_variables)

  pcs[[4]] = settings
  names(pcs)[4] = "settings"
  class(pcs) = "pc"
  pcs

}
pca_average= pca_na
pca_mean = pca_average





#' print.pc
#'
#' @param pcs
#'
#' @return
#' @export
#'
#' @examples
print.pc = function(pcs){
  print(pcs$row_features)
  print(pcs$column_features)
}
