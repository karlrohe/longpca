#' itty_pivot (internal)
#'
#' used for diagnose
#'
#' @param itty_tibby
#'
#' @return
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_longer
itty_pivot = function(itty_tibby){
  # if you have a column like this:
  #   # A tibble: 162,263 × 2
  #   userId        degree
  #         <dbl>     <int>
  #         1           7
  #         2           36
  #         3          108
  # and you want it like this:
  #   # A tibble: 162,263 × 3
  #   type      id degree
  #   <chr>  <dbl>  <int>
  #   userId     1      7
  #   userId     2     36

  itty_tibby %>%
    pivot_longer(1, names_to = "type", values_to = "id") %>%
    relocate(type, id) %>% select(-id)
}


#' #' list_2_tib (internal)
#' #'
#' #' used for diagnose
#' #'
#' #' @param list_of_data
#' #'
#' #' @return
#' #'
#' #' @examples
#' list_2_tib = function(list_of_data){
#'   dplyr::bind_rows(lapply(list_of_data, itty_pivot))
#' }

#'
#' #' make_degrees (internal)
#' #'
#' #' used for diagnose
#' #'
#' #' @param fo
#' #' @param tib
#' #'
#' #' @return
#' #'
#' #' @examples
#' #' @importFrom dplyr tibble
#' make_degrees = function(fo, tib){
#'   sparse_matrix_data = make_sparse_matrix_raw(fo, tib)
#'   row_degrees = tibble(row_id = 1:nrow(sparse_matrix_data$A), degree = Matrix::rowSums(sparse_matrix_data$A!=0))
#'   col_degrees = tibble(col_id = 1:ncol(sparse_matrix_data$A), degree = Matrix::colSums(sparse_matrix_data$A!=0))
#'   # edge_tib_list = make_edge_tib(fo,tib)
#'   # row_degrees = edge_tib_list$edge_tib %>% count(row_id) %>% rename(degree=n)
#'   # col_degrees = edge_tib_list$edge_tib %>% count(col_id) %>% rename(degree=n)
#'   list(row_degrees, col_degrees)
#' }
#'
#'




#' transpose_tibble (internal)
#'
#' used for diagnose
#'
#' @param data
#'
#' @return
#'
#' @examples
transpose_tibble <- function(data) {
  # Convert the first column to row names
  data <- tibble::column_to_rownames(data, var = names(data)[1])

  # Transpose the data frame
  data_transposed <- as.data.frame(t(data))

  # Convert row names back to a column
  data_transposed <- tibble::rownames_to_column(data_transposed, var = "measurement")

  return(tibble::as_tibble(data_transposed))
}


#' diagnose_formula
#'
#' This function helps to see that there is "enough data" for the pca to return reliable results. In particular, it examine the degree distribution with both a printout and a plot. Perhaps run this function before running pca.
#'
#' @param fo
#' @param tib
#' @param make_plot
#'
#' @return
#' @export
#'
#' @examples
#' library(nycflights13)
#' im = make_interaction_model(~(month&day)*dest, flights)
#' diagnose(im)
#' @importFrom ggplot2 ggplot geom_histogram scale_x_log10 scale_y_log10 facet_wrap aes
#' @importFrom dplyr group_by summarize n tibble
#' @importFrom magrittr %>%
diagnose = function(im, make_plot = TRUE, nbins = 30){

  A = get_Matrix(im)

  row_degrees = tibble(row_id = 1:nrow(A), degree = Matrix::rowSums(A!=0))
  col_degrees = tibble(col_id = 1:ncol(A), degree = Matrix::colSums(A!=0))

  degrees_data = bind_rows(itty_pivot(row_degrees), itty_pivot(col_degrees))

  # model_variables = parse_variables(fo,tib) %>% lapply(function(x) str_glue(x,sep = " & "))
  # model_variables = parse_formula(fo,tib) %>% lapply(function(x) paste0(x,collapse = " & "))
  # degrees_data = degrees_data %>% left_join(tibble(type=c("row_id", "col_id"),
  #                                                  type_label = c(model_variables[[2]],model_variables[[3]]))) %>%
  #   select(-type)

  degrees_data = degrees_data %>% left_join(tibble(type=c("row_id", "col_id"),
                                                   type_label = c(paste(im$settings$row_variables, collapse = " & "),
                                                                  im$settings$column_variables)), by = "type") %>%
    select(-type)



  if (make_plot) {
    p =  ggplot(degrees_data %>% dplyr::filter(degree>0), aes(x = degree)) +
      geom_histogram(bins = nbins)+
      # geom_density(aes(y = after_stat(count))) +
      scale_x_log10()+
      scale_y_log10()+
      facet_wrap(~type_label, scales="free")
    print(p)
  }

  dat = degrees_data %>%
    group_by(type_label) %>%
    summarize(number_of_items = n(),
              average_degree = round(mean(degree)),
              median_degree = median(degree),
              percent_le_1 = round(mean(degree<=1),2)*100,
              percent_le_2 = round(mean(degree<=2),2)*100,
              percent_le_3 = round(mean(degree<=3),2)*100)

  # dat %>% left_join(tibble(type=c("row_id", "col_id"),
  #                          type_label = c(model_variables[[2]],model_variables[[3]]))) %>%
  #   select(-type) %>%
  #   relocate(type_label) %>%
  dat %>%   transpose_tibble
  # transpose_tibble(dat)

}


# diagnose_text = function(fo, tib, make_plot = TRUE){
#   diagnose(fo, tib, is_text = TRUE, make_plot = make_plot)
# }
