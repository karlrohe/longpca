#' Title
#'
#' @param pcs
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
top = function(pcs, this_dim){
  dim_string= paste0(pcs$settings$prefix_for_dimensions, "0*",this_dim,"_")

  tidy_row_features = pcs %>%
    select_universe(mode = "rows", any_dims = this_dim)

  top_rows = top_features(tidy_row_features, dim_string)

  tidy_column_features = pcs %>%
    select_universe(mode = "columns", any_dims = this_dim)

  top_columns = top_features(tidy_column_features, dim_string)
  list(top_rows = top_rows, top_columns= top_columns)
}

#' Title
#'
#' @param tidy_features
#' @param dim_string
#'
#' @return
#' @export
#'
#' @examples
top_features = function(tidy_features, dim_string){


  tmp = tidy_features %>%
    dplyr::arrange(desc(pc))

  dplyr::bind_rows(
    tmp %>% dplyr::slice_head(n = 9),
    tmp %>% dplyr::slice_tail(n = 9)
  )
}
