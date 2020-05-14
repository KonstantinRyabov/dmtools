dmtools <img src='man/figures/logo.png' align="right" height="170" />
=====================================================================

[![CRAN
status](https://www.r-pkg.org/badges/version/dmtools)](https://CRAN.R-project.org/package=dmtools)
[![Build
Status](https://travis-ci.com/chachabooms/dmtools.svg?token=pmH5ZxVz4xaZTjx5TDKs&branch=master)](https://travis-ci.com/chachabooms/dmtools)
[![codecov](https://codecov.io/gh/chachabooms/dmtools/branch/master/graph/badge.svg?token=AEKUFWUUXZ)](https://codecov.io/gh/chachabooms/dmtools)

Installation
------------

    install.packages("dmtools")

    # dev-version
    devtools::install_github("chachabooms/dmtools")

    library(dmtools)

Overview
--------

For checking the dataset from EDC in clinical trials. Notice, your
dataset should have a postfix( \_post ) or a prefix( pre\_ ) in the
names of variables. Column names should be unique.

-   `date()` - create object date, for check dates in the dataset
-   `lab()` - create object lab, for check lab results
-   `wbc()` - create object wbc, for check WBCs count: (all \* relative)
    / 100 = absolute
-   `short()` - create object short to transform the dataset(different
    events in one column)
-   `check()` - check objects
-   `get_result()` - get the final result of object
-   `choose_test()` - filter the final result of `check()`
-   `check_sites()` - check objects of different sites
-   `test_sites()` - filter the final result of `check_sites()`
-   `rename_dataset()` - rename dataset

Usage
-----

For example, you want to check laboratory values, you need to create the
excel table like in the example.

-   age\_min - whole number, &gt;= number
-   age\_max - if none, type Inf, &lt;= number  
-   sex - for both sex, use `|`
-   human\_name - friendly name for analysis
-   name\_lab\_vals - analysis from the dataset, without postfix or
    prefix
-   name\_is\_norm - estimate from the dataset, without postfix or
    prefix
-   lab\_vals\_min - lower limit of normal, &gt;=
-   lab\_vals\_max - upper limit of normal, &lt;=

<table>
<caption>lab reference ranges</caption>
<thead>
<tr class="header">
<th style="text-align: right;">age_min</th>
<th style="text-align: right;">age_max</th>
<th style="text-align: left;">sex</th>
<th style="text-align: left;">human_name</th>
<th style="text-align: left;">name_lab_vals</th>
<th style="text-align: left;">name_is_norm</th>
<th style="text-align: left;">lab_vals_min</th>
<th style="text-align: left;">lab_vals_max</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">f|m</td>
<td style="text-align: left;">gluc</td>
<td style="text-align: left;">gluc</td>
<td style="text-align: left;">gluc_res</td>
<td style="text-align: left;">3.9</td>
<td style="text-align: left;">5.9</td>
</tr>
<tr class="even">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">m</td>
<td style="text-align: left;">ast</td>
<td style="text-align: left;">ast</td>
<td style="text-align: left;">ast_res</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">42</td>
</tr>
<tr class="odd">
<td style="text-align: right;">18</td>
<td style="text-align: right;">45</td>
<td style="text-align: left;">f</td>
<td style="text-align: left;">ast</td>
<td style="text-align: left;">ast</td>
<td style="text-align: left;">ast_res</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">39</td>
</tr>
</tbody>
</table>

<table>
<caption>dataset</caption>
<thead>
<tr class="header">
<th style="text-align: left;">id</th>
<th style="text-align: left;">age</th>
<th style="text-align: left;">sex</th>
<th style="text-align: left;">gluc_post</th>
<th style="text-align: left;">gluc_res_post</th>
<th style="text-align: left;">ast_post</th>
<th style="text-align: left;">ast_res_post</th>
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
    obj_lab <- lab(refs, id, age, sex, "norm", "no")
    obj_lab <- obj_lab %>% check(df)

    # ok - analysis, which has a correct estimate of the result
    obj_lab %>% choose_test("ok")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 01  19   f      gluc gluc_post 3.9 - 5.9      5.5    norm         5.5
    #> 2 01  19   f       ast  ast_post    0 - 39       30    norm        30.0
    #> 3 03  22   m       ast  ast_post    0 - 42       31    norm        31.0
    #>   auto_norm
    #> 1      norm
    #> 2      norm
    #> 3      norm

    # mis - analysis, which has an incorrect estimate of the result
    obj_lab %>% choose_test("mis")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 02  20   m       ast  ast_post    0 - 42       48    norm        48.0
    #> 2 03  22   m      gluc gluc_post 3.9 - 5.9      9.7    norm         9.7
    #>   auto_norm
    #> 1        no
    #> 2        no

    # skip - analysis, which has an empty value of the estimate
    obj_lab %>% choose_test("skip")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 02  20   m      gluc gluc_post 3.9 - 5.9      4.1    <NA>         4.1
    #>   auto_norm
    #> 1      <NA>

<table>
<caption>strange_dataset</caption>
<thead>
<tr class="header">
<th style="text-align: left;">id</th>
<th style="text-align: left;">age</th>
<th style="text-align: left;">sex</th>
<th style="text-align: left;">gluc_post</th>
<th style="text-align: left;">gluc_res_post</th>
<th style="text-align: left;">ast_post</th>
<th style="text-align: left;">ast_res_post</th>
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
    obj_lab <- obj_lab %>% check(strange_df)

    # dmtools can understand the value with a comma like 6,6 
    obj_lab %>% choose_test("ok")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 01  19   f      gluc gluc_post 3.9 - 5.9      5,5    norm         5.5
    #> 2 03  22   m       ast  ast_post    0 - 42       31    norm        31.0
    #>   auto_norm
    #> 1      norm
    #> 2      norm

    # Notice, if dmtools can't understand the value of lab_vals e.g. < 5, it puts Inf in the vals_to_dbl
    obj_lab %>% choose_test("mis")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 01  19   f       ast  ast_post    0 - 39      < 5    norm         Inf
    #> 2 02  20   m       ast  ast_post    0 - 42       48    norm        48.0
    #> 3 03  22   m      gluc gluc_post 3.9 - 5.9      9,7    norm         9.7
    #>   auto_norm
    #> 1        no
    #> 2        no
    #> 3        no

    obj_lab %>% choose_test("skip")
    #>   id age sex human_lab  name_lab      refs lab_vals is_norm vals_to_dbl
    #> 1 02  20   m      gluc gluc_post 3.9 - 5.9      4,1    <NA>         4.1
    #>   auto_norm
    #> 1      <NA>
