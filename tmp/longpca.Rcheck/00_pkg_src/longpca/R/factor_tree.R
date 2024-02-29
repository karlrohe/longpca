# # # given a pc object (that has been rotated) and mode,
# # #  I want to build the NJ tree.
# # # default can use middle_B directly
# # #   todo: build out options for estiamting/ building b / sparsity threadholding factors/ etc
# #
#
# library(Matrix)
# mode = "columns"
#
#
# if(mode == "columns"){
#   if(!grepl(pattern = "columns|both", spcs$settings$fit_method)){
#     warning("We need to rotate the columns with varimax before constructing the tree. The default of the funtion rotate is to rotate both modes and that is acceptable also.")
#   }
# }
# if(mode == "rows"){
#   if(!grepl(pattern = "rows|both", spcs$settings$fit_method)){
#     warning("We need to rotate the columns with varimax before constructing the tree. The default of the funtion rotate is to rotate both modes and that is acceptable also.")
#   }
# }
#
# B = longpca:::get_middle_matrix(spcs)
# str(B)
# if(mode == "columns") B = t(B)
# B2 = B%*%t(B)
# B2 |> str()
# # hist(B2 |> as.vector(), breaks = 100)
# dd = 1/sqrt(diag(B2))
# Bs = diag(dd)%*%B2%*%diag(dd)
# # hist(Bs |> as.vector(), breaks = 100)
# # Bs[Bs<0] = NA
# Bs = abs(Bs)
# DistMat = -log(Bs)
# diag(DistMat)=0
# Tree = ape::nj(DistMat)
# ggtree(Tree) + geom_tiplab()
# ggtree(Tree) + geom_tiplab()+geom_point()
