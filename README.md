dmtools <img src='man/figures/logo.png' align="right" height="170" />
=====================================================================

[![CRAN
status](https://www.r-pkg.org/badges/version/dmtools)](https://CRAN.R-project.org/package=dmtools)
[![Build
Status](https://travis-ci.com/chachabooms/dmtools.svg?token=pmH5ZxVz4xaZTjx5TDKs&branch=master)](https://travis-ci.com/chachabooms/dmtools)
[![codecov](https://codecov.io/gh/KonstantinRyabov/dmtools/branch/master/graph/badge.svg?token=AEKUFWUUXZ)](https://codecov.io/gh/KonstantinRyabov/dmtools)

Installation
------------

    install.packages("dmtools")

    # dev-version
    devtools::install_github("KonstantinRyabov/dmtools")

    library(dmtools)

Overview
--------

For checking the dataset from EDC in clinical trials. Notice, your
dataset should have a postfix( \_V1 ) or a prefix( V1\_ ) in the names
of variables. Column names should be unique.

-   `date()` - create object date to check dates in the dataset
-   `lab()` - create object lab to check lab reference range
-   `short()` - create object short to reshape the dataset in a tidy
    view.
-   `check()` - check objects
-   `get_result()` - get the final result of object
-   `choose_test()` - filter the final result of `check()`
-   `rename_dataset()` - rename the dataset

Usage
-----

For example, you want to check laboratory values, you need to create the
excel table like in the example.

-   AGELOW - number, &gt;= number
-   AGEHIGH - if none, type Inf, &lt;= number  
-   SEX - for both sex, use `|`
-   LBTEST - What was the lab test name? (can be any convenient name for
    you)
-   LBORRES\* - What was the result of the lab test?
-   LBNRIND\* - How \[did/do\] the reported values compare within the
    \[reference/normal/expected\] range?
-   LBORNRLO - What was the lower limit of the reference range for this
    lab test, &gt;=
-   LBORNRHI - What was the high limit of the reference range for this
    lab test, &lt;=

\*column names without prefix or postfix

<table>
<caption>lab reference ranges</caption>
<thead>
<tr class="header">
<th style="text-align: right;">AGELOW</th>
<th style="text-align: right;">AGEHIGH</th>
<th style="text-align: left;">SEX</th>
<th style="text-align: left;">LBTEST</th>
<th style="text-align: left;">LBORRES</th>
<th style="text-align: left;">LBNRIND</th>
<th style="text-align: left;">LBORNRLO</th>
<th style="text-align: left;">LBORNRHI</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">f|m</td>
<td style="text-align: left;">Glucose</td>
<td style="text-align: left;">GLUC</td>
<td style="text-align: left;">GLUC_IND</td>
<td style="text-align: left;">3.9</td>
<td style="text-align: left;">5.9</td>
</tr>
<tr class="even">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">Aspartate transaminase</td>
<td style="text-align: left;">AST</td>
<td style="text-align: left;">AST_IND</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">42</td>
</tr>
<tr class="odd">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">f</td>
<td style="text-align: left;">Aspartate transaminase</td>
<td style="text-align: left;">AST</td>
<td style="text-align: left;">AST_IND</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">39</td>
</tr>
</tbody>
</table>

<table>
<caption>dataset</caption>
<thead>
<tr class="header">
<th style="text-align: left;">ID</th>
<th style="text-align: left;">AGE</th>
<th style="text-align: left;">SEX</th>
<th style="text-align: left;">GLUC_V1</th>
<th style="text-align: left;">GLUC_IND_V1</th>
<th style="text-align: left;">AST_V2</th>
<th style="text-align: left;">AST_IND_V2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">01</td>
<td style="text-align: left;">19</td>
<td style="text-align: left;">f</td>
<td style="text-align: left;">5.5</td>
<td style="text-align: left;">norm</td>
<td style="text-align: left;">30</td>
<td style="text-align: left;">norm</td>
</tr>
<tr class="even">
<td style="text-align: left;">02</td>
<td style="text-align: left;">20</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">4.1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">48</td>
<td style="text-align: left;">norm</td>
</tr>
<tr class="odd">
<td style="text-align: left;">03</td>
<td style="text-align: left;">22</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">9.7</td>
<td style="text-align: left;">norm</td>
<td style="text-align: left;">31</td>
<td style="text-align: left;">norm</td>
</tr>
</tbody>
</table>

    # "norm" and "no" it is an example, necessary variable for the estimate, get from the dataset
    refs <- system.file("labs_refer.xlsx", package = "dmtools")
    obj_lab <- lab(refs, ID, AGE, SEX, "norm", "no")
    obj_lab <- obj_lab %>% check(df)

    # ok - analysis, which has a correct estimate of the result
    obj_lab %>% choose_test("ok")
    #>   ID AGE SEX                 LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES
    #> 1 01  19   f                Glucose     GLUC   _V1      3.9      5.9     5.5
    #> 2 01  19   f Aspartate transaminase      AST   _V2      0.0     39.0      30
    #> 3 03  22   m Aspartate transaminase      AST   _V2      0.0     42.0      31
    #>   LBNRIND RES_TYPE_NUM IND_EXPECTED
    #> 1    norm          5.5         norm
    #> 2    norm         30.0         norm
    #> 3    norm         31.0         norm

    # mis - analysis, which has an incorrect estimate of the result
    obj_lab %>% choose_test("mis")
    #>   ID AGE SEX                 LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES
    #> 1 02  20   m Aspartate transaminase      AST   _V2      0.0     42.0      48
    #> 2 03  22   m                Glucose     GLUC   _V1      3.9      5.9     9.7
    #>   LBNRIND RES_TYPE_NUM IND_EXPECTED
    #> 1    norm         48.0           no
    #> 2    norm          9.7           no

    # skip - analysis, which has an empty value of the estimate
    obj_lab %>% choose_test("skip")
    #>   ID AGE SEX  LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES LBNRIND
    #> 1 02  20   m Glucose     GLUC   _V1      3.9      5.9     4.1    <NA>
    #>   RES_TYPE_NUM IND_EXPECTED
    #> 1          4.1         norm

<table>
<caption>strange_dataset</caption>
<thead>
<tr class="header">
<th style="text-align: left;">ID</th>
<th style="text-align: left;">AGE</th>
<th style="text-align: left;">SEX</th>
<th style="text-align: left;">V1_GLUC</th>
<th style="text-align: left;">V1_GLUC_IND</th>
<th style="text-align: left;">V2_AST</th>
<th style="text-align: left;">V2_AST_IND</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">01</td>
<td style="text-align: left;">19</td>
<td style="text-align: left;">f</td>
<td style="text-align: left;">5,5</td>
<td style="text-align: left;">norm</td>
<td style="text-align: left;">&lt; 5</td>
<td style="text-align: left;">norm</td>
</tr>
<tr class="even">
<td style="text-align: left;">02</td>
<td style="text-align: left;">20</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">4,1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">48</td>
<td style="text-align: left;">norm</td>
</tr>
<tr class="odd">
<td style="text-align: left;">03</td>
<td style="text-align: left;">22</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">9,7</td>
<td style="text-align: left;">norm</td>
<td style="text-align: left;">31</td>
<td style="text-align: left;">norm</td>
</tr>
</tbody>
</table>

    # dmtools can work with the dataset as strange_df
    # parameter is_post has value FALSE because a dataset has a prefix( V1_ ) in the names of variables
    obj_lab <- lab(refs, ID, AGE, SEX, "norm", "no", is_post = F)
    obj_lab <- obj_lab %>% check(strange_df)

    # dmtools can understand the value with a comma like 6,6 
    obj_lab %>% choose_test("ok")
    #>   ID AGE SEX                 LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES
    #> 1 01  19   f                Glucose     GLUC   V1_      3.9      5.9     5,5
    #> 2 03  22   m Aspartate transaminase      AST   V2_      0.0     42.0      31
    #>   LBNRIND RES_TYPE_NUM IND_EXPECTED
    #> 1    norm          5.5         norm
    #> 2    norm         31.0         norm

    # Notice, if dmtools can't understand the value of lab_vals e.g. < 5, it puts Inf in the RES_TYPE_NUM
    obj_lab %>% choose_test("mis")
    #>   ID AGE SEX                 LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES
    #> 1 01  19   f Aspartate transaminase      AST   V2_      0.0     39.0     < 5
    #> 2 02  20   m Aspartate transaminase      AST   V2_      0.0     42.0      48
    #> 3 03  22   m                Glucose     GLUC   V1_      3.9      5.9     9,7
    #>   LBNRIND RES_TYPE_NUM IND_EXPECTED
    #> 1    norm          Inf           no
    #> 2    norm         48.0           no
    #> 3    norm          9.7           no

    obj_lab %>% choose_test("skip")
    #>   ID AGE SEX  LBTEST LBTESTCD VISIT LBORNRLO LBORNRHI LBORRES LBNRIND
    #> 1 02  20   m Glucose     GLUC   V1_      3.9      5.9     4,1    <NA>
    #>   RES_TYPE_NUM IND_EXPECTED
    #> 1          4.1         norm
