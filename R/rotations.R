

#' make_sparse_output (internal to rotate)
#'
#' @param sparse_pc_mat
#' @param rot_mat
#' @param old_prefix
#'
#' @return
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_glue
#'
#' @examples
make_sparse_output = function(sparse_pc_mat, rot_mat, new_prefix){
  #  used in rotate.
  # given the output of varimax rotated matrix... we want to
  # 1) make skew positive.
  # 2) give nice column names.
  # 3) make it a tibble
  # 4) output the rotation matrix (that make it skew positive)

  # sparse_pc_mat has already been rotated by rot_mat.

  # get columns skew sign:
  skew_sign = apply(sparse_pc_mat, 2, function(x) sign(sum(x^3)))
  #  multiply each column of sparse_pc_mat by corresponding skew_sign element.
  sparse_pc_mat_output = sweep(sparse_pc_mat, MARGIN = 2, STATS = skew_sign, FUN = "*")
  # same for the rotation
  rot_mat_output = sweep(rot_mat, MARGIN = 2, STATS = skew_sign, FUN = "*")

  # nice column names
  colnames(sparse_pc_mat_output) = generate_colnames(sparse_pc_mat_output, new_prefix)
  list(sparse_tib = as_tibble(sparse_pc_mat_output), rot_mat = rot_mat_output)
}

#' varimax_with_pre_rotation (internal to rotate)
#'
#' @param matrix_to_rotate
#' @param pre_rotation
#'
#' @return
#' @importFrom stats varimax
#'
#' @examples
varimax_with_pre_rotation = function(matrix_to_rotate, pre_rotation){
  # used in function rotate.
  #  varimax has multiple optima.  it is nice for rotation of both modes to find
  #  ~analogous~ modes of varimax optima.  we do that we pre-rotation.
  # before computing varimax on a pc matrix, pre-rotate with an optima from the other mode.
  #  then, iterate from there.  empirically, this should speed up convergence and improve interpretability.
  matrix_to_rotate_pre_rotated = matrix_to_rotate %*% pre_rotation
  next_varimax_rotation = varimax(matrix_to_rotate_pre_rotated, normalize = F)$rot
  list(data_after_rotation = matrix_to_rotate_pre_rotated %*% next_varimax_rotation,
       varimax_rotation = pre_rotation%*%next_varimax_rotation)
}



#' rotate
#'
#' perform varimax rotation on both rows and columns.
#'
#' @param pcs
#'
#' @return
#' @export
#' @importFrom stats varimax
#' @importFrom dplyr select bind_cols
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_glue
#'
#' @examples
rotate = function(pcs, mode = NULL){
  # rotate both modes with variamax.  return a pc object.
  #  we want the axes for rows and columns to roughly align with each other
  #    (makes other interpretation easier... tends to make B have strong diagonal).
  #  so, rotate the smaller one first... then use that rotation to pre-rotate the bigger one...
  #           .... then take that rotation back to pre-rotate the smaller one.
  #
  if(is.null(mode)){
    only_rotate_one = FALSE
    if(nrow(pcs[[1]]) < nrow(pcs[[2]])) mode_order_vector = c(1,2)
    if(nrow(pcs[[1]]) > nrow(pcs[[2]])) mode_order_vector = c(2,1)
  }else{
    only_rotate_one = TRUE
    if(mode %in% c("r","row","rows")){
      mode_order_vector = c(1, 2)
    }
    if(mode %in% c("c","col","cols", "columns")){
      mode_order_vector = c(2, 1)
    }
  }

  old_prefix = pcs$settings$prefix_for_dimensions %>% stringr::str_remove("_")
  new_prefix = str_glue("v",old_prefix)

  # rotate the first one (which is the smaller one if we haven't specified a mode)

  pc_mat_mode1 = pcs[[mode_order_vector[1]]] %>%
    select(starts_with(pcs$settings$prefix_for_dimensions)) %>%
    as.matrix()

  varimax_rotation_mode1 = varimax(pc_mat_mode1,normalize = F)$rot


  pc_mat_mode2 = pcs[[mode_order_vector[2]]] %>%
    select(starts_with(pcs$settings$prefix_for_dimensions)) %>%
    as.matrix()



  # if this is true: we need to rotate mode1 pcs and output data.
  #  otherwise, we will rotate twice more...
  if(only_rotate_one){

    sparse_pc_mat_mode1 = pc_mat_mode1%*%varimax_rotation_mode1
    sparse_pc_mat_mode2 = pc_mat_mode2
    null_rotation = diag(rep(1, ncol(pc_mat_mode2)))
    mode1_output = make_sparse_output(sparse_pc_mat_mode1,
                                      varimax_rotation_mode1,
                                      new_prefix = new_prefix)
    mode2_output = make_sparse_output(sparse_pc_mat_mode2,
                                      null_rotation,
                                      new_prefix = new_prefix)
  }
  if(!only_rotate_one){
    # pre-rotate the bigger one...

    # # pre-rotate with last varimax rotation:
    v_mode_2 = varimax_with_pre_rotation(pc_mat_mode2, varimax_rotation_mode1)
    sparse_pc_mat_mode2 = v_mode_2$data_after_rotation

    # pre-rotate first mode with both of the previous varimax rotations.
    v_mode_1 = varimax_with_pre_rotation(pc_mat_mode1, v_mode_2$varimax_rotation)
    sparse_pc_mat_mode1 = v_mode_1$data_after_rotation


    mode1_output = make_sparse_output(sparse_pc_mat_mode1,
                                      v_mode_1$varimax_rotation,
                                      new_prefix = new_prefix)
    mode2_output = make_sparse_output(sparse_pc_mat_mode2,
                                      v_mode_2$varimax_rotation,
                                      new_prefix = new_prefix)

  }

  #if we did 2 first, then 1... then we need to flop mode1_output and mode2_output.
  if(mode_order_vector[1] == 2){
    row_output = mode2_output
    column_output = mode1_output
  }
  if(mode_order_vector[1] == 1){ # if we did 1 first, then no swapping
    row_output = mode1_output
    column_output = mode2_output
  }
  row_features = bind_cols(
    pcs$row_features %>% select(-starts_with(pcs$settings$prefix_for_dimensions)),
    row_output$sparse_tib %>% dplyr::rename_with(~paste0(., "_rows"), everything())
  )

  column_features = bind_cols(
    pcs$column_features %>% select(-starts_with(pcs$settings$prefix_for_dimensions)),
    column_output$sparse_tib %>% dplyr::rename_with(~paste0(., "_columns"), everything())
  )


  # construct the new middle B matrix by passing through rotations from:
  #  this should likely be it's own function at some point.... would be useful
  #  to abstract away this construction of B
  # row_output$rot_mat
  # mode2_output$rot_mat
  # pcs$middle_B

  # first, $middle_B is stored as "edge list form".  So, convert to a matrix....

  old_B = get_middle_matrix(pcs)

  new_B = t(row_output$rot_mat) %*% old_B %*% column_output$rot_mat




  B_tib= make_middle_B_tibble(new_B, dimension_prefix = new_prefix)


  if(!only_rotate_one) fit_method = str_glue(pcs$settings$fit_method, "varimax (both)",.sep = " + ")
  if(only_rotate_one & mode_order_vector[1]==1) fit_method = str_glue(pcs$settings$fit_method, "varimax (rows)",.sep = " + ")
  if(only_rotate_one & mode_order_vector[1]==2) fit_method = str_glue(pcs$settings$fit_method, "varimax (columns)",.sep = " + ")



  # if we only rotate one, then let's get rid of the middle B matrix and put it into the other factors...
  if(only_rotate_one){
    # if we rotated the rows...
    if(mode_order_vector[1] == 1){
      # push B into the column_features
      column_matrix_old = column_features |> select(contains(new_prefix)) |> as.matrix()
      column_matrix = column_matrix_old %*% t(new_B)
      colnames(column_matrix) = colnames(column_matrix_old)
      column_features = bind_cols(column_features |> select(-contains(new_prefix)),
                                  as_tibble(column_matrix))

    }
    if(mode_order_vector[1] == 2){
      # push B into the column_features
      row_matrix_old = row_features |> select(contains(new_prefix)) |> as.matrix()
      row_matrix = row_matrix_old %*% t(new_B)
      colnames(row_matrix) = colnames(row_matrix_old)
      row_features = bind_cols(row_features |> select(-contains(new_prefix)),
                               as_tibble(row_matrix))

    }

    new_B = diag(rep(1,pcs$settings$k))
    B_tib= make_middle_B_tibble(new_B, dimension_prefix = new_prefix)

  }
  new_settings = c(
    list(
      fit_method = fit_method,
      prefix_for_dimensions = str_glue(new_prefix,"_")
    ),
    pcs$settings[-(1:2)])

  out_list = list(row_features = row_features,
                  column_features = column_features,
                  middle_B = B_tib,
                  settings = new_settings)

  class(out_list) = "pc"
  out_list

}

