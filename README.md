
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dxhelper

<!-- badges: start -->
<!-- badges: end -->

The goal of dxhelper is to provide an tidy interface to decoding raw
values from exported Apollo datasets, whether they are exported from
`dx extract_dataset` or through a SparkSQL query.

Given the data dictionary and the codings files, it will decode the
categorical values from the raw data.

## Installation

You can install the development version of dxhelper from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("laderast/dxr")
```

## Note about example data

All examples are shown with synthetic data. These are not actual
patients from UKB RAP.

## Read in CSV file or SQL generated `data.frame`

`{dxhelper}` works with both the CSV files that is generated from
`dx extrqct_dataset` or the `datq.frame` that is generated from running
the Spark SQL query.

If you need to load the CSV file in, `readr::read_csv` is recommended:

``` r
cohort <- readr::read_csv("my_dataset.csv", show_col_types=FALSE)
```

## Build coding dictionary

We’ll need two `data.frame`s: a `coding_dict` and a `data_dict`. These
are `data.frame`s from the `.coding.csv` and `data_dictionary.csv` files
that are generated with the `-ddd` (dump dataset dictionary) option of
`dx extract_dataset`. We read them in using `readr::read_csv`.

``` r
coding_dict <- readr::read_csv("my_dataset.coding.csv", show_col_types=FALSE)
data_dict <- readr::read_csv("my_dataset.data_dictionary.csv" , show_col_types=FALSE)
```

Then we’ll build a coding dictionary by combining `coding_dict` and
`data_dict`.

``` r
merged_code <- merge_coding_data_dict(coding_dict, data_dict)

head(merged_code)
#> # A tibble: 6 × 9
#>   title               ent_f…¹ entity name  codin…² code  meaning is_sp…³ is_mu…⁴
#>   <chr>               <glue>  <chr>  <chr> <chr>   <chr> <chr>   <chr>   <chr>  
#> 1 Coffee consumed | … partic… parti… p100… data_c… 0     No      <NA>    <NA>   
#> 2 Coffee consumed | … partic… parti… p100… data_c… 1     Yes     <NA>    <NA>   
#> 3 Coffee consumed | … partic… parti… p100… data_c… 0     No      <NA>    <NA>   
#> 4 Coffee consumed | … partic… parti… p100… data_c… 1     Yes     <NA>    <NA>   
#> 5 Sex of baby         partic… parti… p412… data_c… 9     Not sp… <NA>    yes    
#> 6 Sex of baby         partic… parti… p412… data_c… 3     Indete… <NA>    yes    
#> # … with abbreviated variable names ¹​ent_field, ²​coding_name,
#> #   ³​is_sparse_coding, ⁴​is_multi_select
```

## Decoding Integer Categories of Apollo Datasets

Categorical data is returned by `dx extract_dataset` as the integer
representation. To see the actual values, they must be decoded from the
`codings.csv` that is generated from `dx extract_dataset`.

Data fields that have multiple categories must also be parsed and
decoded. If the data is a sparse field, it is a combined
categorical/numerical field.

## Show Raw Cohort File

Here’s a cohort that was returned by `dx extract_dataset`. Note that
there are multiple columns encoded as categories. We need to decode
these values.

``` r
knitr::kable(head(cohort[,c(1:5,14)]))
```

| participant.p21022 | participant.p100240_i0 | participant.p100240_i1 | participant.p100240_i2 | participant.p100240_i3 | participant.p31 |
|-------------------:|-----------------------:|-----------------------:|-----------------------:|-----------------------:|----------------:|
|                 43 |                     NA |                      1 |                      1 |                     NA |               0 |
|                 60 |                     NA |                     NA |                     NA |                     NA |               0 |
|                 53 |                     NA |                      0 |                      0 |                      0 |               0 |
|                 62 |                     NA |                     NA |                     NA |                     NA |               0 |
|                 67 |                     NA |                     NA |                     NA |                     NA |               0 |
|                 61 |                     NA |                     NA |                     NA |                     NA |               0 |

## Decode Columns

We can decode the integer values into the actual values using
`decode_categories` and our `merged_code` object:

``` r
decoded_cohort <- cohort |>
  decode_df(merged_code)

knitr::kable(head(decoded_cohort[,c(1:5,14)]))
```

| participant.p21022 | participant.p100240_i0 | participant.p100240_i1 | participant.p100240_i2 | participant.p100240_i3 | participant.p31 |
|-------------------:|:-----------------------|:-----------------------|:-----------------------|:-----------------------|:----------------|
|                 43 | NA                     | Yes                    | Yes                    | NA                     | Female          |
|                 60 | NA                     | NA                     | NA                     | NA                     | Female          |
|                 53 | NA                     | No                     | No                     | No                     | Female          |
|                 62 | NA                     | NA                     | NA                     | NA                     | Female          |
|                 67 | NA                     | NA                     | NA                     | NA                     | Female          |
|                 61 | NA                     | NA                     | NA                     | NA                     | Female          |

## Decode Column Names

Optionally, you can also decode the column names in the table to be
R-friendly column names with the actual field titles:

``` r
final_cohort <- decoded_cohort |>
  decode_column_names(merged_code)

knitr::kable(head(final_cohort[,c(1:5,14)]))
```

| age_at_recruitment | coffee_consumed_instance_0 | coffee_consumed_instance_1 | coffee_consumed_instance_2 | coffee_consumed_instance_3 | sex    |
|-------------------:|:---------------------------|:---------------------------|:---------------------------|:---------------------------|:-------|
|                 43 | NA                         | Yes                        | Yes                        | NA                         | Female |
|                 60 | NA                         | NA                         | NA                         | NA                         | Female |
|                 53 | NA                         | No                         | No                         | No                         | Female |
|                 62 | NA                         | NA                         | NA                         | NA                         | Female |
|                 67 | NA                         | NA                         | NA                         | NA                         | Female |
|                 61 | NA                         | NA                         | NA                         | NA                         | Female |

If you want the original field names with no modifications, you can set
the argument `r_clean_names` to `FALSE`:

``` r
final_cohort2 <- decoded_cohort |>
  decode_column_names(merged_code, r_clean_names=FALSE)

knitr::kable(head(final_cohort2[,c(1:5,14)]))
```

| Age at recruitment | Coffee consumed \| Instance 0 | Coffee consumed \| Instance 1 | Coffee consumed \| Instance 2 | Coffee consumed \| Instance 3 | Sex    |
|-------------------:|:------------------------------|:------------------------------|:------------------------------|:------------------------------|:-------|
|                 43 | NA                            | Yes                           | Yes                           | NA                            | Female |
|                 60 | NA                            | NA                            | NA                            | NA                            | Female |
|                 53 | NA                            | No                            | No                            | No                            | Female |
|                 62 | NA                            | NA                            | NA                            | NA                            | Female |
|                 67 | NA                            | NA                            | NA                            | NA                            | Female |
|                 61 | NA                            | NA                            | NA                            | NA                            | Female |

## Multi-valued columns

Multivalued columns are returned as a pipe (`|`) delimited string (this
is to avoid issues with commas within categorical values):

``` r
knitr::kable(final_cohort[1:10,"diagnoses_main_icd10"])
```

| diagnoses_main_icd10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| R07.3 Other chest pain\|Z09.9 Follow-up examination after unspecified treatment for other conditions\|I83.9 Varicose veins of lower extremities without ulcer or inflammation\|Z30.5 Surveillance of (intra-uterine) contraceptive device\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|Z09 Follow-up examination after treatment for conditions other than malignant neoplasms\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|I83 Varicose veins of lower extremities\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system\|Z30 Contraceptive management\|Z30-Z39 Persons encountering health services in circumstances related to reproduction |
| J32.0 Chronic maxillary sinusitis\|R10.3 Pain localised to other parts of lower abdomen\|J32 Chronic sinusitis\|J30-J39 Other diseases of upper respiratory tract\|Chapter X Diseases of the respiratory system\|R10 Abdominal and pelvic pain\|R10-R19 Symptoms and signs involving the digestive system and abdomen\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| R07.3 Other chest pain\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| I87.1 Compression of vein\|I87 Other disorders of veins\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| K01.1 Impacted teeth\|K01 Embedded and impacted teeth\|K00-K14 Diseases of oral cavity, salivary glands and jaws\|Chapter XI Diseases of the digestive system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Q66.8 Other congenital deformities of feet\|G56.8 Other mononeuropathies of upper limb\|R07.0 Pain in throat\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|G56 Mononeuropathies of upper limb\|G50-G59 Nerve, nerve root and plexus disorders\|Chapter VI Diseases of the nervous system\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                     |
| I12.0 Hypertensive renal disease with renal failure\|N43.3 Hydrocele, unspecified\|G45.9 Transient cerebral ischaemic attack, unspecified\|I12 Hypertensive renal disease\|I10-I15 Hypertensive diseases\|Chapter IX Diseases of the circulatory system\|N43 Hydrocele and spermatocele\|N40-N51 Diseases of male genital organs\|Chapter XIV Diseases of the genitourinary system\|G45 Transient cerebral ischaemic attacks and related syndromes\|G40-G47 Episodic and paroxysmal disorders\|Chapter VI Diseases of the nervous system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Z03.9 Observation for suspected disease or condition, unspecified\|Q66.8 Other congenital deformities of feet\|M20.1 Hallux valgus (acquired)\|Z03 Medical observation and evaluation for suspected diseases and conditions\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|M20 Acquired deformities of fingers and toes\|M20-M25 Other joint disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue                                                                                                                                                                                                                                                                |
| Z87.1 Personal history of diseases of the digestive system\|C61 Malignant neoplasm of prostate\|M75.1 Rotator cuff syndrome\|R31 Unspecified haematuria\|D12.5 Sigmoid colon\|Z87 Personal history of other diseases and conditions\|Z80-Z99 Persons with potential health hazards related to family and personal history and certain conditions influencing health status\|Chapter XXI Factors influencing health status and contact with health services\|C60-C63 Malignant neoplasms of male genital organs\|Chapter II Neoplasms\|M75 Shoulder lesions\|M70-M79 Other soft tissue disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue\|R30-R39 Symptoms and signs involving the urinary system\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|D12 Benign neoplasm of colon, rectum, anus and anal canal\|D10-D36 Benign neoplasms                                                                                            |

## Returned tables from SparkSQL

If you use `dbGetQuery()` to fetch your table from Spark, multi-value
columns will be returned as list-columns. `decode_df` will return a
comma-delimited string for these columns as well.

``` r
tibble::tibble(mydf)
#> # A tibble: 100 × 11
#>    participant…¹ parti…² parti…³ parti…⁴ parti…⁵ parti…⁶ parti…⁷ parti…⁸ parti…⁹
#>    <chr>           <dbl>   <dbl>   <dbl>   <dbl> <list>    <dbl> <list>    <dbl>
#>  1 sample_100_1…      43       3      NA      NA <list>        0 <lgl>        NA
#>  2 sample_100_1…      60       3      NA      NA <list>        0 <lgl>        NA
#>  3 sample_100_2…      53       3      NA      NA <lgl>         0 <lgl>        NA
#>  4 sample_100_2…      62       2      NA      NA <list>        0 <lgl>        NA
#>  5 sample_100_3…      67       3      NA      NA <list>        0 <lgl>        NA
#>  6 sample_100_3…      61       1      NA      NA <list>        0 <lgl>        NA
#>  7 sample_100_3…      66       3      NA      NA <list>        0 <lgl>        NA
#>  8 sample_100_36      56       3       3      NA <list>        0 <lgl>        NA
#>  9 sample_100_3…      55       2      NA      NA <list>        0 <lgl>        NA
#> 10 sample_100_3…      53       1      NA      NA <list>        0 <lgl>        NA
#> # … with 90 more rows, 2 more variables: participant.p3159_i1 <dbl>,
#> #   participant.p3159_i2 <dbl>, and abbreviated variable names
#> #   ¹​participant.eid, ²​participant.p21022, ³​participant.p1508_i0,
#> #   ⁴​participant.p1508_i1, ⁵​participant.p1508_i2, ⁶​participant.p41202,
#> #   ⁷​participant.p31, ⁸​participant.p41226, ⁹​participant.p3159_i0
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```

``` r
from_db <- mydf |> 
  decode_df(merged_code)

knitr::kable(head(from_db))
```

| participant.eid | participant.p21022 | participant.p1508_i0                         | participant.p1508_i1 | participant.p1508_i2 | participant.p41202                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | participant.p31 | participant.p41226 | participant.p3159_i0 | participant.p3159_i1 | participant.p3159_i2 |
|:----------------|-------------------:|:---------------------------------------------|:---------------------|:---------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------|:-------------------|:---------------------|:---------------------|:---------------------|
| sample_100_116  |                 43 | Ground coffee (include espresso, filter etc) | NA                   | NA                   | R07.3 Other chest pain\|Z09.9 Follow-up examination after unspecified treatment for other conditions\|I83.9 Varicose veins of lower extremities without ulcer or inflammation\|Z30.5 Surveillance of (intra-uterine) contraceptive device\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|Z09 Follow-up examination after treatment for conditions other than malignant neoplasms\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|I83 Varicose veins of lower extremities\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system\|Z30 Contraceptive management\|Z30-Z39 Persons encountering health services in circumstances related to reproduction | Female          | NA                 | NA                   | NA                   | No                   |
| sample_100_142  |                 60 | Ground coffee (include espresso, filter etc) | NA                   | NA                   | J32.0 Chronic maxillary sinusitis\|R10.3 Pain localised to other parts of lower abdomen\|J32 Chronic sinusitis\|J30-J39 Other diseases of upper respiratory tract\|Chapter X Diseases of the respiratory system\|R10 Abdominal and pelvic pain\|R10-R19 Symptoms and signs involving the digestive system and abdomen\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Female          | NA                 | NA                   | NA                   | NA                   |
| sample_100_285  |                 53 | Ground coffee (include espresso, filter etc) | NA                   | NA                   | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | Female          | NA                 | NA                   | NA                   | NA                   |
| sample_100_290  |                 62 | Instant coffee                               | NA                   | NA                   | R07.3 Other chest pain\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Female          | NA                 | NA                   | NA                   | NA                   |
| sample_100_304  |                 67 | Ground coffee (include espresso, filter etc) | NA                   | NA                   | I87.1 Compression of vein\|I87 Other disorders of veins\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Female          | NA                 | NA                   | NA                   | NA                   |
| sample_100_328  |                 61 | Decaffeinated coffee (any type)              | NA                   | NA                   | K01.1 Impacted teeth\|K01 Embedded and impacted teeth\|K00-K14 Diseases of oral cavity, salivary glands and jaws\|Chapter XI Diseases of the digestive system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Female          | NA                 | NA                   | NA                   | NA                   |
