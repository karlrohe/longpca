pkgname <- "longpca"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "longpca-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('longpca')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("diagnose")
### * diagnose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: diagnose
### Title: diagnose_formula
### Aliases: diagnose

### ** Examples

library(nycflights13)
im = make_interaction_model(flights, ~(month&day)*dest)
diagnose(im)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("diagnose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_interaction_model")
### * make_interaction_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_interaction_model
### Title: make_interaction_model
### Aliases: make_interaction_model

### ** Examples

library(nycflights13)
im = make_interaction_model(flights,~(month & day)*dest)
names(im)
im$row_universe
im$column_universe
im$interaction_tibble
im$settings
# you can extract the sparse Matrix:
A = longpca:::get_Matrix(im,  import_names = TRUE)
str(A)
im = make_interaction_model(all_packages, ~Package*Imports, parse_text = TRUE)
names(im)
im$row_universe
im$column_universe
im$interaction_tibble
im$settings



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_interaction_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("old_make_interaction_model")
### * old_make_interaction_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: old_make_interaction_model
### Title: old_make_interaction_model
### Aliases: old_make_interaction_model

### ** Examples

library(nycflights13)
im = make_interaction_model(~(month & day)*dest, flights)
names(im)
im$row_universe
im$column_universe
im$interaction_tibble
im$settings
# you can extract the sparse Matrix:
A = longpca:::get_Matrix(im,  import_names = TRUE)
str(A)
im = make_interaction_model(~Package*Imports, all_packages, parse_text = TRUE)
names(im)
im$row_universe
im$column_universe
im$interaction_tibble
im$settings



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("old_make_interaction_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pick_dim")
### * pick_dim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pick_dim
### Title: pick_dim
### Aliases: pick_dim

### ** Examples

library(nycflights13)
im = make_interaction_model(~(month&day)*dest, flights)
cveig = pick_dim(im, dimMax = 7)
plot(cveig)
cveig



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pick_dim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.pc")
### * plot.pc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.pc
### Title: plot.pc
### Aliases: plot.pc

### ** Examples

library(nycflights13)
pcs = pca_count(1 ~ (month & day)*(dest), flights, k = 6)
plot(pcs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.pc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rotate")
### * rotate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rotate
### Title: rotate
### Aliases: rotate

### ** Examples

im = make_interaction_model(~Package*Imports, all_packages, parse_text = TRUE)
pcs = pca(im, k = 10)
# streaks(pcs)
sparse_pcs = rotate(pcs)
# notice how the rotation aligns the streaks with the axes...
# streaks(sparse_pcs, "columns")
# if you do not specify a mode, then the middle B matrix will not be strictly diagonal...
image(longpca:::get_middle_matrix(sparse_pcs))
# if you rotate only the columns, then the middle B matrix is set to diagonal and this matrix is "pushed into" the other mode.
sparse_columns_pcs = rotate(pcs, mode = "columns")
# because we only rotated one mode, the B matrix is the identity matrix:
image(longpca:::get_middle_matrix(sparse_columns_pcs))
# these values were pushed into the row_features.  You can see that their scale is drastically reduced:
sparse_pcs$row_features$vpc_01_rows |> sd()
sparse_columns_pcs$row_features$vpc_01_rows |> sd()
# importantly, this is not simply the row pcs scaled by the singular values... it is also rotated by the varimax rotation for the columns...
# here is the algebra using the SVD:
# U D V' = (UDR)(VR)'
# after rotating only the columns...
# (UDR) gives the new row_features
# (VR) gives the new column_features



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rotate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
