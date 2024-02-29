#' # join_features at the bottom.
#'
#'
#'
#'
#' #' bff
#' #'
#' #' best feature function; Proposed in ["Don't mind the eigen-gap"](https://www.stat.cmu.edu/~jiashun/Research/Selected/SCC-disc3.pdf) a comment on Ji and Jin's AOAS discussion paper.
#' #'
#' #'
#' #' @param this_pcs
#' #' @param text_im
#' #' @param chop_words
#' #' @param num_best
#' #'
#' #' @return
#' #' @export
#' #' @import dplyr
#' #' @import dplyr
#' #'
#' #' @examples
#' bff = function(this_pcs, text_im, chop_words = 5, num_best = 10){
#'   # identify if text_im is matching to rows or columns of this_pcs:
#'   match_to_pc_rows = any(text_im$settings$row_variables %in% this_pcs$settings$row_variables)
#'   match_to_pc_cols = any(text_im$settings$row_variables %in% this_pcs$settings$column_variables)
#'
#'   if(all(!match_to_pc_cols, !match_to_pc_rows)){
#'     no_match_warning(this_pcs)
#'   }
#'   if(match_to_pc_rows){
#'     pc_loadings = this_pcs$row_features |> rename(pc_index = row_num)
#'     print("Joining to this_pcs rows.")
#'   }
#'   if(match_to_pc_cols){
#'     pc_loadings = this_pcs$column_features |> rename(pc_index = col_num)
#'     print("Joining to this_pcs columns.")
#'   }
#'
#'   match_tib = pc_loadings |>
#'     select(-contains("vtextpc")) |>
#'     left_join(
#'       text_im$row_universe |>
#'         rename(text_row_num = row_num)
#'     ) |>
#'     drop_na() |>
#'     select(pc_index, text_row_num)
#'
#'   if(nrow(match_tib)<10){
#'     warning("there were ", nrow(match_tib), "matches between the pcs and rows in text_im.  exiting.")
#'     return(NULL)
#'   }
#'   # get_Matrix = longpca:::get_Matrix
#'   loadings_mat = pc_loadings |>
#'     select(contains("vtextpc"))
#'   loadings_mat = loadings_mat[match_tib$pc_index, ]
#'   features_mat = get_Matrix(text_im)
#'   features_mat = features_mat[match_tib$text_row_num,]
#'   colnames(features_mat) = text_im$column_universe |> mutate(cn = paste(token, from_text, sep = " | ")) |> pull(cn)
#'   cs = colSums(features_mat)
#'   features_mat = features_mat[,cs>chop_words]
#'   features_mat@x[] = 1
#'   bb = bff_raw(loadings_mat, features_mat, num_best = num_best)
#'   return(bb)
#' }
#'
#'
#'
#' no_match_warning = function(this_pcs){
#'   warning("the rows in text_im need to match either rows or columns in this_pcs; that is, either \n\n",
#'           paste(this_pcs$settings$row_variable, collapse =" "),
#'           "\n or \n",
#'           this_pcs$settings$column_variables,".\n\nWhen constructing text_im, the terms before the * should be one of those.  You might need to rename variables to make that work."
#'   )
#' }
#'
#'
#'
#' #' Title
#' #'
#' #' @param loadings
#' #' @param features
#' #' @param num_best
#' #'
#' #' @return
#' #' @import dplyr
#' #'
#' #' @examples
#' bff_raw = function (loadings, features, num_best)
#' {
#'   l1_normalize <- function(x) x/sum(x)
#'   loadings[loadings < 0] <- 0
#'   k <- ncol(loadings)
#'   best_feat <- matrix("", ncol = k, nrow = num_best)
#'   nc <- apply(loadings, 2, l1_normalize)
#'   nOutC <- apply(loadings == 0, 2, l1_normalize)
#'   inCluster <- sqrt(crossprod(features, nc))
#'   outCluster <- sqrt(crossprod(features, nOutC))
#'   diff <- inCluster - outCluster
#'   diff |>
#'     as.matrix() |>
#'     as_tibble(rownames = "word") |>
#'     tidyr::pivot_longer(-word, names_to = "factor", values_to = "importance") |>
#'     group_by(factor) |>
#'     top_n(num_best, importance) |>
#'     arrange(factor, desc(importance)) |>
#'     mutate(rank = row_number()) |>
#'     ungroup() |>
#'     tidyr::pivot_wider(id_cols = factor, names_from = rank,
#'                 names_prefix = "word", values_from = word)
#' }
#'
#'
#'
#' join_features <- function(pcs, fresh_features, by = NULL) {
#'   # extend function so that fresh_features could be a tibble or an interaction_model.
#'   # #  if we have an interaction model.  Then, we want longpca:::get_Matrix() to be useful for bff.
#'   #
#'   # left_side_row_vars <- pcs$settings$row_variables
#'   # left_side_column_vars <- pcs$settings$column_variables
#'   # if(class(fresh_features) == "interaction_model"){
#'   #
#'   # }
#'   # right_side_row_vars = fresh_features$settings$row_variables
#'   # right_side_col_vars = fresh_features$settings$column_variables
#'   #
#'
#'   if (!is.null(by)) {
#'     # Extract column names from the 'by' argument
#'     by_vars = extract_by_vars(by)
#'
#'     # Determine if the 'by' columns match row or column features based on their names
#'     if (all(by_vars %in% left_side_row_vars)) {
#'       message("Joining to $row_features")
#'       output = pcs |>
#'         select_universe("rows") |>
#'         left_join(fresh_features, by = by)
#'
#'       # return(left_join(pcs$row_features, fresh_features, by = by))
#'     } else if (all(by_vars %in% left_side_col_vars)) {
#'       message("Joining to $column_features")
#'       output = pcs |>
#'         select_universe("columns") |>
#'         left_join(fresh_features, by = by)
#'       # return(left_join(pcs$column_features, fresh_features, by = by))
#'     } else {
#'       message("The specified 'by' columns do not match exclusively to row or column features.")
#'       return(NULL)
#'     }
#'   } else {
#'     # Proceed as in the original function if 'by' is not provided
#'
#'     common_row_vars <- intersect(row_vars, names(fresh_features))
#'     common_column_vars <- intersect(column_vars, names(fresh_features))
#'
#'     if (length(common_row_vars) > 0) {
#'       message("Joining to $row_features")
#'       output = pcs |>
#'         select_universe("rows") |>
#'         left_join(fresh_features)
#'     } else if (length(common_column_vars) > 0) {
#'       message("Joining to $column_features")
#'       output = pcs |>
#'         select_universe("columns") |>
#'         left_join(fresh_features)
#'     } else {
#'       message("No common variables found for joining.")
#'       return(NULL)
#'     }
#'   }
#'   return(output)
#' }
#'
#' extract_by_vars <- function(by) {
#'
#'   # Directly specified matches without '=' are directly in 'by'
#'   direct_matches <- by[names(by) %in% ""]
#'
#'   # Matches with '=' have their names split into two parts; we need both
#'   specified_matches = names(by)[names(by) != ""]
#'
#'   # Combine both direct and specified matches, removing extra spaces and quotes
#'   combined_matches <- c(direct_matches, specified_matches)
#'   combined_matches <- trimws(gsub("\"", "", combined_matches))  # Clean up spaces and quotes
#'
#'   # Return unique, non-empty elements
#'   unique(combined_matches[combined_matches != ""])
#' }
#' #
#' # formula =  ~ (month & day)*(dest)
#' # im = make_interaction_model(formula, flights)
#' # flight_pcs = pca(im, k = 6)
#' # # join_features figures out to join to row_features or column_features
#' # # the by argument can be used exactly like in dplyr::left_join...
#' # join_features(flight_pcs, airports, by = c("dest" = "faa"))
#' # #  it can also guess what you want to do.
#' # #  here it guesses and might do something unexpected ...
#' # #     each row of flight_pcs$column_features gets multiple rows of weather.
#' # join_features(flight_pcs, weather)
#' #
#' #
#' # document_word_interaction_model = make_interaction_model(~Package*Imports,
#' #                                                          all_packages,
#' #                                                          parse_text = TRUE,
#' #                                                          to_lower = FALSE)
#' # pcs = pca(document_word_interaction_model, k = 25)
#' # spcs = rotate(pcs)
#' # top_packages
#' # join_features(spcs, top_packages, by = c("token" = "Package"))
#' #
#' #
#' #
#' # # tibble_to_interaction_model = function(tib, key_features){
#' # #   tib |> pivot_longer(tidyselect::all_of(!key_features))
#' # #   tib |> pivot_longer(!tidyselect::all_of(key_features), names_to = "variable_name", values_to = "value")
#' # # }
#' #
#' #
#' #
#' #
