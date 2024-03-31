
#' core
#' Given and interaction_model, this returns a new interaction_model that is the "k-core" of the "largest connected component" of the original interaction_model.  This function is recommended when `diagnose(im)` shows that the majority of rows/columns have 1, 2, or 3 connections.  In this case, the data is potentially too sparse for pca.  If you simply throwing away the rows/columns that are weakly connected, then you will reduce the connections of those that remain.  The k-core is what you get if you keep on iterating.  In particular, it will find the largest subset of rows and columns from the interaction_model such that every row and column has at least `core_threshold` number of connections or "data points" in interaction_tibble.  This is exactly the k-core if the row and columns correspond to unique elements (non-overlapping).  If the elements in the rows match some elements in the columns, then those elements are represented twice... once for the row and once for the column.  It is possible that only one of those is retained.
#'
#' @param im The interaction_model to be cleaned.
#' @param core_threshold An integer value that sets the minimum number of connections a row or column must have to be included in the final interaction model. Defaults to 3.
#'
#' @return Returns a modified version of the input interaction model (of the same type as `im`), which represents the k-core of the largest connected component based on the specified `core_threshold`. This cleaned interaction model will only include rows and columns that meet the minimum number of connections defined by `core_threshold`.
#' @export
#' @importFrom dplyr count slice_head pull bind_rows select filter
#'
#' @examples
core = function(im_input, core_threshold = 3){


  if(!"coreness" %in% colnames(im_input$row_universe)){
    print("adding graph summaries (coreness and connected components).")
    im_input = add_bipartite_summaries(im_input)
  }

  row_LCC <- im_input$row_universe |>
    count(component_label, sort = TRUE) |>
    slice_head(n = 1) |>
    pull(component_label)

  col_LCC <- im_input$column_universe |>
    count(component_label, sort = TRUE) |>
    slice_head(n = 1) |>
    pull(component_label)

  LCC = col_LCC

  if(col_LCC != row_LCC){
    print("WEIRD THING: the rows and columns have different largest components.")
    LCC = bind_rows(im_input$row_universe |> select(component_label),
                    im_input$column_universe |> select(component_label)
    ) |>
      count(component_label, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(component_label)

  }

  im_input$row_universe = im_input$row_universe |>
    filter(component_label>=LCC) |>
    filter(coreness>=core_threshold)

  im_input$column_universe = im_input$column_universe |>
    filter(component_label>=LCC)|>
    filter(coreness>=core_threshold)

  im_input = subset_im(im_input)

  return(im_input)



}




#' add_bipartite_summaries
#' this function takes an interaction_model and  an interaction_model, where row_universe and column_universe have two additional columns $coreness and $component_label. importantly, strange behavior if im is a symmetric graph. this treats each row index and each column index as nodes in a graph.  So, if something appears in both rows and columns (e.g. as in symmetric graph), then this will be ignored.
#'
#' @param im
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
add_bipartite_summaries = function(im_input){

  # this function takes an interaction_model
  #  it returns an interaction_model, where row_universe and column_universe have two additional columns $coreness and $component_label
  # importantly, strange behavior if im is a symmetric graph.
  #   this treats each row index and each column index as nodes in a graph.  So, if something appears in both rows and columns (e.g. as in symmetric graph), then this will be ignored.

  # Step 1: Mutate the columns to make r_ and c_ and construct graph.
  g = im_input$interaction_tibble |>
    transmute(row_num = paste0("r_", row_num),
              col_num = paste0("c_", col_num)) |>
    distinct() |>
    igraph::graph_from_data_frame()




  # Compute coreness for each node
  node_coreness <- igraph::coreness(g)

  # Compute connected components
  components <- igraph::components(g)

  # Create a tibble that combines node names, their coreness, and their component label
  node_data <- tibble(
    name = names(node_coreness),
    coreness = node_coreness,
    component_label = components$membership
  )

  # Split the dataframe into two: one for rows and one for columns
  row_data <- node_data[grep("^r_", node_data$name), ]
  col_data <- node_data[grep("^c_", node_data$name), ]

  # Remove the 'r_' and 'c_' prefixes for the final tibbles
  row_data$name <- as.numeric(sub("^r_", "", row_data$name))
  col_data$name <- as.numeric(sub("^c_", "", col_data$name))

  # Convert to tibbles and rename columns appropriately

  row_tibble <- row_data %>% rename(row_num = name)
  col_tibble <- col_data %>% rename(col_num = name)

  im_input$row_universe = im_input$row_universe |> left_join(row_tibble, by = "row_num")
  im_input$column_universe = im_input$column_universe |> left_join(col_tibble, by = "col_num")

  # Now, 'row_tibble' and 'col_tibble' are your final tibbles with the desired information.
  return(im_input)
}


#' Add Graph Summaries to interaction_model
#'
#' This function computes graph summaries coreness, connected components, and personalized PageRank
#' for an interaction_model. It adds these summaries to the row and column universes of the input interaction_model.
#'
#' @param im_input An interaction_model object containing an interaction tibble, row universe, and column universe.
#' @param row_key The column name in the row universe that corresponds to the node names in the graph.
#' @param col_key The column name in the column universe that corresponds to the node names in the graph.
#' @param tib_with_weights A tibble containing node weights for personalized PageRank calculation.
#'
#' @return The input interaction_model object with added graph summaries in the row and column universes.
#' @importFrom dplyr left_join select any_of distinct mutate pull
#' @importFrom igraph graph_from_data_frame coreness components V page_rank
#' @importFrom tibble tibble
#' @importFrom rlang sym !!
#' @export
#'
#' @examples
#' im = make_interaction_model(all_packages, ~Package*Imports, parse_text = TRUE, to_lower = FALSE)
#' row_key = "Package"
#' col_key = "token"
#' tib_with_weights = all_packages |> dplyr::mutate(weights = nchar(Package)) |> dplyr::select(Package, weights)
#' add_graph_summaries(im, row_key, col_key, tib_with_weights)
add_graph_summaries <- function(im_input, row_key=NULL, col_key=NULL, tib_with_weights = NULL) {

  if(is.null(row_key)){
    if(length(im_input$settings$row_variables)==1){
      row_key = im_input$settings$row_variables
    }else{
      cat("row_key not provided and there is more than one row variable in im_input$settings$row_variables:\n\n print(im_input$settings$row_variables) \n")
      print(im_input$settings$row_variables)
      cat("in the argument to add_graph_summaries, specify row_key.")
      return(NA)
    }
  }

  if(is.null(col_key)){
    if(length(im_input$settings$column_variables)==1){
      col_key = im_input$settings$column_variables
    }else{
      cat("col_key not provided and there is more than one row variable in im_input$settings$column_variables:\n\n print(im_input$settings$column_variables) \n")
      print(im_input$settings$column_variables)
      cat("in the argument to add_graph_summaries, specify col_key.")
      return(NA)
    }
  }

  # Create an igraph object from the interaction tibble
  g <- im_input$interaction_tibble |>
    left_join(im$row_universe |> select(row_num, any_of(row_key)), by = "row_num") |>
    left_join(im$column_universe |> select(col_num, any_of(col_key)), by  = "col_num") |>
    select(-row_num, -col_num, -outcome) |>
    distinct() |>
    igraph::graph_from_data_frame()

  # Compute coreness for each node
  node_coreness <- igraph::coreness(g)

  # Compute connected components
  components <- igraph::components(g)

  # Create a tibble with node names
  node_tibble <- tibble(name = names(igraph::V(g)))


  # Check if tib_with_weights is provided
  if (!is.null(tib_with_weights)) {
    # Join the weights from tib_with_weights based on the row_key or col_key column
    personalized_weights <- node_tibble |>
      left_join(tib_with_weights, by = c("name" = ifelse(row_key %in% names(tib_with_weights), row_key, col_key))) |>
      mutate(weights = ifelse(is.na(weights), 0, weights)) |>
      pull(weights)

    # Compute PageRank with personalized weights
    pagerank <- igraph::page_rank(g, personalized = personalized_weights)
  } else {
    # Compute standard PageRank
    pagerank <- igraph::page_rank(g)
  }

  # Create a tibble that combines node names, their coreness, their component label, and PageRank
  node_data <- tibble(
    name = names(node_coreness),
    coreness = node_coreness,
    component_label = components$membership,
    pagerank = pagerank$vector * length(pagerank$vector)
  )

  # Add graph summaries to the row universe
  im_input$row_universe <- im_input$row_universe |>
    left_join(node_data, by = join_by(!!sym(row_key) == "name"))

  # Add graph summaries to the column universe
  im_input$column_universe <- im_input$column_universe  |>
    left_join(node_data, by = join_by(!!sym(col_key) == "name"))

  return(im_input)
}


#' #' core
#' #' Given an interaction_model, this will return another interaction_model that corresponds to the "k-core" of the input.  This function is recommended when `diagnose(im)` shows that the majority of rows/columns have 1, 2, or 3 connections.  In this case, the data is potentially too sparse for pca.  If you simply throwing away the rows/columns that are weakly connected, then you will reduce the connections of those that remain.  The k-core is what you get if you keep on iterating.  In particular, it will find the largest subset of rows and columns from the interaction_model such that every row and column has at least `core_threshold` number of connections or "data points" in interaction_tibble.  This is exactly the k-core if the row and columns correspond to unique elements (non-overlapping).  If the elements in the rows match some elements in the columns, then those elements are represented twice... once for the row and once for the column.  It is possible that only one of those is retained.
#' #'
#' #' @param im_input
#' #' @param core_threshold
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @import dplyr
#' #'
#' #' @examples
#' core = function(im_input, core_threshold){
#'
#'
#'   if(!"coreness" %in% colnames(im_input$row_universe)){
#'     print("adding graph summaries (coreness and connected components).")
#'     im = add_bipartite_summaries(im)
#'   }
#'
#'   im_input$row_universe = im_input$row_universe |>
#'     filter(coreness>=core_threshold)
#'
#'   im_input$column_universe = im_input$column_universe |>
#'     filter(coreness>=core_threshold)
#'
#'   im = make_new(im)
#'   return(im)
#'
#' }



#' subset_im
#' If you've removed some rows of row_universe or column_universe, then you want to redefine a bunch of things... you need new row/col numbering and you want to remove rows from interaction_tibble.  this does that.
#'
#' @param im_input
#'
#' @return
#' @export
#' @import dplyr
#'
#'
#' @examples
subset_im = function(im_input){
  im_input$row_universe = im_input$row_universe |>
    mutate(new_row_num = row_number())

  im_input$column_universe = im_input$column_universe |>
    mutate(new_col_num = row_number())

  im_input$interaction_tibble = im_input$interaction_tibble |>
    left_join(im_input$row_universe |> select(row_num, new_row_num), by = "row_num") |>
    left_join(im_input$column_universe |> select(col_num, new_col_num), by = "col_num") |>
    select(-row_num, -col_num) |>
    rename(row_num = new_row_num, col_num = new_col_num) |>
    drop_na()

  im_input$row_universe = im_input$row_universe |>
    select(-row_num) |>
    rename(row_num = new_row_num)

  im_input$column_universe = im_input$column_universe |>
    select(-col_num) |>
    rename(col_num = new_col_num)

  return(im_input)
}
