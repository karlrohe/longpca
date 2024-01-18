#' make_interaction_model
#'
#' @param fo a formula, like outcome ~ (row_nums & context) * measurement_type.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#' @param dropNA recommended.  This drops rows of tib if there are any NA's among the essential variables.
#'
#' @return a list with four elements.  First, the sparse Matrix A. Second, row_universe which is akin to the row names of A, but in a tidy form.  Thir, column_universe which is like row_universe. Fourth, some settings.
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
make_interaction_model = function(fo, tib, duplicates = "add", parse_text= FALSE, dropNA = TRUE, data_prefix = NULL,...){
  # This returns a list with elements
  #  interaction_tibble: sparse matrix for formula fo on tibble tib.
  #  row_universe: a tibble of distinct row-variable values, with the row_num (i.e. corresponding row number in A)
  #  column_universe: a tibble of distinct column-variable values, with the col_num (i.e. corresponding column number in A)
  #  settings: a bunch of stuff.

  if(parse_text & (duplicates != "add")){
    duplicates = "add"
    warning("duplicates set to `add` because this is text and that's what we do.  ")
  }

  allowed_duplicate_values <- c("add", "average")
  if (!duplicates %in% allowed_duplicate_values) {
    stop("Invalid value for 'duplicates'. Choose either 'add' or 'average'.")
  }


  dumb_formula = ~1
  boolean_first_symbol_is_tilde = fo[[1]] == dumb_formula[[1]]
  # if no left hand side in formula, then make it a 1
  if(length(fo)==2 & boolean_first_symbol_is_tilde){
    # fo = update_lhs_to_1(fo, quiet = FALSE)
    fo = stats::as.formula(paste("1", deparse(fo)))
  }


  vars = parse_formula(fo, tib)
  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  if(parse_text){
    tib = tib %>%
      dplyr::select(all_of(row_column), text = tidyselect::all_of(column_column)) %>%
      tidytext::unnest_tokens(!!column_column, text,...) %>%
      dplyr::mutate(outcome_unweighted_1 = 1)

    data_prefix= "text"
  }

  im =make_interaction_model_from_variables(tib=tib,
                                            row_column=row_column,
                                            column_column=column_column,
                                            outcome_column=outcome_column,
                                            vars=NULL,
                                            dropNA=dropNA,
                                            duplicates=duplicates)

  outcome_aggregation = duplicates
  if(outcome_column == "outcome_unweighted_1") outcome_aggregation = "count"

  im$settings = list(fo = fo,
                     data_prefix = data_prefix,
                     outcome_aggregation = outcome_aggregation,
                     outcome_variables = outcome_column,
                     row_variables = row_column,
                     column_variables = column_column)
  # im = list(interaction_tibble = interaction_tibble_list$interaction_tibble,
  #           row_universe=interaction_tibble_list$row_index,
  #           column_universe = interaction_tibble_list$col_index,
  #           settings = list(fo = fo,
  #                           data_prefix = data_prefix,
  #                           outcome_variables = outcome_column,
  #                           row_variables = row_column,
  #                           column_variables = column_column))
  class(im) = "interaction_model"
  return(im)
}





#' make_interaction_model_from_variables (internal to make_interaction_tibble and text2sparse)
#'
#' @param tib
#' @param row_column
#' @param column_column
#' @param outcome_column
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select distinct row_number left_join group_by ungroup summarize n
#' @importFrom stats sd
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#'
#' @examples
make_interaction_model_from_variables = function(tib, row_column, column_column, outcome_column, vars=NULL, dropNA, duplicates){
  if(is.null(vars)){ vars = list(outcome_column, row_column, column_column)}
  row_index = tib %>%
    select(all_of(row_column)) %>%
    drop_na() %>%
    distinct() %>%
    mutate(row_num = row_number())
  col_index = tib %>%
    select(all_of(column_column)) %>%
    drop_na() %>%
    distinct() %>%
    mutate(col_num = row_number())

  # if a weighted graph...
  if(outcome_column != "outcome_unweighted_1"){
    edge_list_tib = tib %>% select(all_of(unlist(vars)))

    interaction_tibble = edge_list_tib %>%
      left_join(row_index, by = row_column) %>%
      left_join(col_index, by = column_column) %>%
      mutate(outcome= .data[[outcome_column]]) %>%
      select(row_num, col_num, outcome)
  }else{
    # if an un-weighted graph...
    edge_list_tib = tib %>% select(all_of(unlist(vars[-1])))

    interaction_tibble = edge_list_tib %>%
      left_join(row_index, by = row_column) %>%
      left_join(col_index, by = column_column) %>%
      mutate(outcome= 1) %>%
      select(row_num, col_num, outcome)
  }
  if(dropNA) interaction_tibble = interaction_tibble %>% tidyr::drop_na()
  if(duplicates == "add"){
    interaction_tibble = interaction_tibble %>%
      group_by(row_num,col_num) %>%
      summarize(outcome = sum(outcome)) %>%
      ungroup()
  }
  if(duplicates == "average"){
    interaction_tibble = interaction_tibble %>%
      group_by(row_num,col_num) %>%
      summarize(outcome = mean(outcome),
                n = n(),
                sd = if_else(n>1, sd(outcome),0),
                min = min(outcome),
                max = max(outcome)) %>%
      ungroup()
  }

  list(interaction_tibble = interaction_tibble,
       row_universe = row_index,
       column_universe = col_index)
}


