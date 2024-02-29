generate_colnames <- function(matrix_data, prefix) {
  num_cols = ncol(matrix_data)
  num_digits = nchar(as.character(num_cols))

  formatted_colnames = sprintf(paste0(prefix, "_%0", num_digits, "d"), 1:num_cols)
  return(formatted_colnames)
}

s_2_pc = function(interaction_model, s, dimension_prefix){
  # interaction_model is a list with all the good stuff...
  A = get_Matrix(interaction_model)
  row_universe = interaction_model$row_universe
  column_universe = interaction_model$column_universe

  #  s is output of svd with s$u, s$d, s$v.
  # dimension_prefix is a character string that will be put at the front (e.g. "pc" for pc_1,...)

  #  we want to make a tidy output.


  # sometimes first dimension is all negative and that's annoying...
  #  make it mostly positive...
  if(stats::median(s$u[,1])<0){
    s$u[,1]= -s$u[,1]
    s$v[,1]= -s$v[,1]
  }
  u = s$u  * sqrt(nrow(A))
  v = s$v  * sqrt(ncol(A))

  #  I currently naming dimensions/factors pc_1, pc_2, ... for both row and column pcs
  #    this has some risks.

  colnames(u) = generate_colnames(u, dimension_prefix)
  colnames(u) = paste0(colnames(u), "_rows")
  colnames(v) = generate_colnames(v, dimension_prefix)
  colnames(v) = paste0(colnames(v), "_columns")

  row_features = dplyr::bind_cols(row_universe,
                                  degree= Matrix::rowSums(A!=0),
                                  weighted_degree = Matrix::rowSums(abs(A)),
                                  tibble::as_tibble(u))

  column_features = dplyr::bind_cols(column_universe,
                                     degree= Matrix::colSums(A!=0),
                                     weighted_degree = Matrix::colSums(abs(A)),
                                     tibble::as_tibble(v))



  # keep_these_rows = Matrix::rowSums(abs(A))>0
  # row_features = dplyr::bind_cols(row_universe,
  #                          degree= Matrix::rowSums(A!=0)[keep_these_rows],
  #                          weighted_degree = Matrix::rowSums(abs(A))[keep_these_rows],
  #                          tibble::as_tibble(u[keep_these_rows,]))
  # keep_these_cols = Matrix::colSums(abs(A))>0
  # column_features = dplyr::bind_cols(column_universe,
  #                             degree= Matrix::colSums(A!=0)[keep_these_cols],
  #                             weighted_degree = Matrix::colSums(abs(A))[keep_these_cols],
  #                             tibble::as_tibble(v[keep_these_cols,]))

  middle_B = make_middle_B_tibble(B_matrix = diag(s$d / (sqrt(nrow(A))*sqrt(ncol(A)))), dimension_prefix = dimension_prefix)
  # middle_B= dplyr::bind_cols(row_factors = colnames(u),
  #                     column_factors = colnames(v),
  #                     value = s$d / (sqrt(nrow(A)*ncol(A))))
  list(row_features = row_features, column_features= column_features, middle_B=middle_B)
}


get_middle_matrix = function(pcs){
  b_el = pcs$middle_B
  B_list = make_interaction_model(value~row_factors*column_factors, tib = b_el)
  B_matrix = get_Matrix(B_list)
  rownames(B_matrix) = B_list$row_universe$row_factors
  colnames(B_matrix) = B_list$column_universe$column_factors
  return(Matrix::as.matrix(B_matrix))
}
make_middle_B_tibble = function(B_matrix, dimension_prefix){
  colnames(B_matrix) = paste(dimension_prefix,1:ncol(B_matrix), "columns", sep = "_")
  B_tib = tibble::as_tibble(as.matrix(B_matrix)) %>%
    mutate(row_factors = paste(dimension_prefix,1:nrow(B_matrix), "rows", sep="_")) %>%
    relocate(row_factors) %>%
    pivot_longer(-1, names_to="column_factors", values_to = "value") %>%
    dplyr::filter(value !=0)
  return(B_tib)

}



#' select_universe
#'
#' @param pcs
#' @param mode
#' @param any_dims
#'
#' @return
#' @export
#'
#' @examples
select_universe = function(pcs, mode = c("rows","columns"), any_dims = NA){
  if(class(pcs) == "interaction_model"){
    if(mode[1] =="rows") return(pcs$row_universe)
    if(mode[1] =="columns") return(pcs$column_universe)
  }

  dim_string= paste0(pcs$settings$prefix_for_dimensions, "0*",any_dims,"_")

  if(mode[1] =="rows"){
    tidy_universe =
      pcs$row_features |>
      dplyr::select(tidyselect::all_of(pcs$settings$row_variables),
                    "row_num",
                    dplyr::matches(dim_string))
  }
  if(mode[1] =="columns"){
    tidy_universe =
      pcs$column_features |>
      dplyr::select(tidyselect::all_of(pcs$settings$column_variables),
                    "col_num",
                    dplyr::matches(dim_string))
  }

  tidy_universe
}

# join_features <- function(x, ...) {
#   UseMethod("join_features")
# }
# join_features.interaction_model <- function(x, ...) {
#   # Your code for handling interaction_model objects goes here
#   cat("This function hasn't been built for interaction_models yet.  first compute pca and then, try joining to that pc object.\n")
# }
#
#
# # join_features.pc = function(pcs, feature_tib, mode = c("rows","columns"), by){
# #
# # }


mode_detector = function(this_mode){
  if(this_mode %in% c("r","row","rows")) return("row")
  if(this_mode %in% c("c","col", "column", "cols", "columns")) return("column")
  warning("this_mode not recognized:", this_mode, ". please set to 'row' or 'column' or leave empty to remove warning.  Continuing with this_mode = NULL")
}

join_features <- function(pcs, fresh_features, feature_tag = NULL, this_mode = NULL) {
  # Extracting row and column variable names from pcs settings
  if(!is.null(this_mode))  this_mode = mode_detector(this_mode)

  row_vars <- pcs$settings$row_variables
  column_vars <- pcs$settings$column_variables
  # Checking for common variables in row features and fresh_features
  common_row_vars <- intersect(row_vars, names(fresh_features))
  # Checking for common variables in column features and fresh_features
  common_column_vars <- intersect(column_vars, names(fresh_features))

  some_matching_rows = length(common_row_vars)!=0
  some_matching_columns = length(common_column_vars)!=0
  if(!is.null(this_mode)){
    if(this_mode == "row" & !some_matching_rows) stop("no matching row variables")
    if(this_mode == "column" & !some_matching_columns) stop("no matching row variables")
  }
  if(is.null(this_mode) & some_matching_columns & some_matching_rows){
    stop("this_mode not specified and there are both matching row and column variables.  please rerun and specify this_mode as 'row' or 'column'.")
  }
  # If common variables are found in row variables, perform a left join on those
  if (some_matching_rows) {
    pcs$row_features = left_join(pcs$row_features, fresh_features, by = common_row_vars)
    fresh_feature_names = setdiff(colnames(fresh_features),common_row_vars)
    if(is.null(feature_tag)){
      starting_here = length(pcs$settings$fresh_row_features)
      feature_tag = paste0("fresh_row_set_", starting_here+1)
    }
    pcs$settings$fresh_row_sets = c(pcs$settings$fresh_row_sets, feature_tag)
    pcs$settings$fresh_row_features[[feature_tag]] = list(fresh_feature_names = fresh_feature_names, joined_by = common_row_vars)

  }

  # If common variables are found in column variables, perform a left join on those
  else if (some_matching_columns) {
    pcs$column_features = left_join(pcs$column_features, fresh_features, by = common_column_vars)
    fresh_feature_names = setdiff(colnames(fresh_features),common_column_vars)
    if(is.null(feature_tag)){
      starting_here = length(pcs$settings$fresh_column_features)
      feature_tag = paste0("fresh_column_set_", starting_here+1)
    }
    pcs$settings$fresh_column_sets = c(pcs$settings$fresh_column_sets, feature_tag)
    pcs$settings$fresh_column_features[[feature_tag]] = list(fresh_feature_names = fresh_feature_names, joined_by = common_column_vars)

  }


  # If there are no common variables, return a message indicating no join was performed
  else {
    message("No common variables found for joining.")
    return(NULL)
  }

  return(pcs)
}




