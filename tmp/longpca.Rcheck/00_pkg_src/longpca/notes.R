use_package("tidyr")
use_package("dplyr")
use_package("stringr")
use_package("Matrix")
use_package("irlba")
use_package("gdim")
use_package("rlang")
use_package("magrittr")
use_package("ggplot2")
use_package("tidyselect")
# use_package("tibble")
use_package("tidytext")
use_package("nycflights13")
use_package("RSpectra")

make_sparse_matrix_raw
interaction2sparse


make_incomplete_matrix_raw
interaction2incomplete


make_sparse_text_matrix_raw
text2sparse
