PCA for the people
================
Karl Rohe
2024-01-03

This package introduces a novel formula syntax for PCA. In modern
applications (where data is often in “long format”), the formula syntax
helps to fluidly imagine PCA without thinking about matrices. In other
words, it provides a layer of abstraction above matrices. Given the
formula and the (long) data, the code in this package transforms your
data into a proper format for fast PCA via sparse linear algebra. The
package also provides code to 1) help pick the number of dimensions to
compute, 2) diagnose the suitability of PCA (both pre and post PCA), 3)
rotate the PCs with varimax, 4) visualize and interpret the dimensions
uncovered, and (not yet) 5) make predictions. This package uses “PCA” as
a broad term for computing the leading singular vectors of a normalized
(sometimes incomplete) matrix. Some might refer to specific instances as
factor analysis, correspondence analysis, latent symantic analysis,
social network analysis, or low-rank matrix completion, among other
possible terms. This is big-tent PCA, all included. `longpca` is in
development. So, functions and syntax might change.

The current approach to PCA (principal components analysis) is *matrix
first*. This note begins to explore an alternative path, one that is
*model first*. The formula syntax provides an alternative way to think
about PCA that makes matrices transparent; completely hidden, unless you
want to see the code.

I hope this makes PCA legible for folks that have not yet learned linear
algebra (just like linear models are legible without solving linear
systems of equations).

I am personally inspired by this approach because (despite the fact that
I love matrices and linear algebra) I find that this *model first* way
of thinking is so much easier and more direct.

This document gives an illustration with a data analysis of the popular
`nycflights13` data via PCA. Headline: we find two seasonal effects
(annual and weekly) and also the “fly-over-zone” (midwest 4ever. ride or
die \<3 much love to my midwest fam). Code details follow this analysis.

(Disclaimer: this is very early in this project. So, the syntax and the
code is likely to change a great deal. Input is very welcome about ways
to improve it.)

#### Install

The functions for PCA for the People are contained in an R package
`longpca`. If you do not already have `devtools` installed, you will
first need to install that:

``` r
install.packages("devtools")
devtools::install_github("karlrohe/longpca")
```

Thank you to Alex Hayes for helpful feedback in this process and
suggesting the name `longpca`.

### PCA the nycflights.

The code is fast and nimble. First you define “the model” with a
formula… and some data:

``` r
formula = 1 ~ (month & day)*(dest)
im = make_interaction_model(flights, formula)
pcs = pca(im, k = 6)
```

There are three functions to run on `im`: `diagnose`, `pick_dim`, and
`pca`.

There are three key functions to run on `pcs`: `plot`, `rotate`, and
`top`.

See the vignettes for further illustrations:

1)  [In depth example with nycflights13
    data](articles/Intro_to_longpca_with_nycflights_data.html)
2)  [Inside `make_interaction_model`, you can
    `parse_text`](articles/parse_text.html)

#### Slightly more detail…

The hope is that *model first* PCA with the formula makes interacting
with the matrix / linear algebra unnecessary. That said, it might be
instructive to understand the class `interaction_model` to see how it
represents a matrix “under the hood”.

The function `make_interaction_model` constructs a list with the class
`interaction_model`. You can think of this as an abstraction of a
matrix…

``` r
formula = 1 ~ (month & day)*(dest)
im = make_interaction_model(flights,formula)
names(im)
```

    ## [1] "interaction_tibble" "row_universe"       "column_universe"   
    ## [4] "settings"

``` r
class(im)
```

    ## [1] "interaction_model"

In this “matrix like thing,” the `month & day` index the rows and `dest`
indexes the columns. This is because `month & day` come before the
interaction `*` in the formula and `dest` comes afterwords.

``` r
im$row_universe
```

    ## # A tibble: 365 × 4
    ##    month   day     n row_num
    ##    <int> <int> <int>   <int>
    ##  1    11    27  1014       1
    ##  2     7    11  1006       2
    ##  3     7     8  1004       3
    ##  4     7    10  1004       4
    ##  5    12     2  1004       5
    ##  6     7    18  1003       6
    ##  7     7    25  1003       7
    ##  8     7    12  1002       8
    ##  9     7     9  1001       9
    ## 10     7    17  1001      10
    ## # ℹ 355 more rows

``` r
im$column_universe
```

    ## # A tibble: 105 × 3
    ##    dest      n col_num
    ##    <chr> <int>   <int>
    ##  1 ORD   17283       1
    ##  2 ATL   17215       2
    ##  3 LAX   16174       3
    ##  4 BOS   15508       4
    ##  5 MCO   14082       5
    ##  6 CLT   14064       6
    ##  7 SFO   13331       7
    ##  8 FLL   12055       8
    ##  9 MIA   11728       9
    ## 10 DCA    9705      10
    ## # ℹ 95 more rows

Then, “the matrix” is in sparse triplet form:

``` r
im$interaction_tibble
```

    ## # A tibble: 31,229 × 3
    ##    row_num col_num outcome
    ##      <int>   <int>   <dbl>
    ##  1       1       1      52
    ##  2       1       2      51
    ##  3       1       3      49
    ##  4       1       4      43
    ##  5       1       5      40
    ##  6       1       6      42
    ##  7       1       7      43
    ##  8       1       8      38
    ##  9       1       9      37
    ## 10       1      10      28
    ## # ℹ 31,219 more rows

If for any reason you actually wanted the sparse `Matrix`…

``` r
A = get_Matrix(im, import_names = TRUE)
str(A)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:31229] 0 1 2 3 4 5 6 7 8 9 ...
    ##   ..@ p       : int [1:106] 0 365 730 1095 1460 1825 2190 2555 2920 3285 ...
    ##   ..@ Dim     : int [1:2] 365 105
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:365] "11/27" "7/11" "7/8" "7/10" ...
    ##   .. ..$ : chr [1:105] "ORD" "ATL" "LAX" "BOS" ...
    ##   ..@ x       : num [1:31229] 52 55 55 55 49 54 55 55 54 55 ...
    ##   ..@ factors : list()

The hope is that model first PCA with the `interaction_model` makes data
analysis more direct, i.e. that you should not need to think about this
matrix (too much). Instead, this path is simply a way to estimate a “low
rank” statistical model via least squares.
