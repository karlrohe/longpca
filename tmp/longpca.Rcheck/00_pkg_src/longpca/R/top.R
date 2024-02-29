#' Title
#'
#' @param pcs
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
top = function(pcs, this_dim, keep_how_many = 9, abs_cut_off = 3){
  dim_string= paste0(pcs$settings$prefix_for_dimensions, "0*",this_dim,"_")

  tidy_row_features = pcs %>%
    select_universe(mode = "rows", any_dims = this_dim) |>
    select(-any_of("row_num"))

  pc_name_for_rows = tidy_row_features |> select(matches(dim_string)) |> colnames()

  top_rows = top_features(tidy_row_features, pc_name_for_rows, keep_how_many)

  tidy_column_features = pcs %>%
    select_universe(mode = "columns", any_dims = this_dim) |>
    select(-any_of("col_num"))

  pc_name_for_cols = tidy_column_features |> select(matches(dim_string)) |> colnames()

  top_columns = top_features(tidy_column_features, pc_name_for_cols,keep_how_many)

  list(top_rows = top_rows %>%
         dplyr::filter(abs(!!sym(pc_name_for_rows))>abs_cut_off),
       top_columns= top_columns %>%
         dplyr::filter(abs(!!sym(pc_name_for_cols))>abs_cut_off))
}

#' top_features
#'
#' @param tidy_features
#' @param dim_string
#'
#' @return
#'
#' @examples
top_features = function(tidy_features, pc_name_for_mode, keep_how_many){


  tmp = tidy_features %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(pc_name_for_mode),
                                 dplyr::desc))

  dplyr::bind_rows(
    tmp %>% dplyr::slice_head(n = keep_how_many),
    tmp %>% dplyr::slice_tail(n = keep_how_many)
  )
}
