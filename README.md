
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xvhelper

<!-- badges: start -->
<!-- badges: end -->

Note: this package was recently named `dxhelper`. It has been renamed to
`xvhelper` to reflect that it is not software that is released by
DNAnexus product.

The goal of xvhelper is to provide an tidy interface to extracting and
decoding raw values from exported Apollo datasets, whether they are
exported from `dx extract_dataset` or through a SparkSQL query.

Given the data dictionary and the codings files, it will decode the
categorical values from the raw data.

## Installation

You can install the development version of xvhelper from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("laderast/xvhelper")
```

## Note about example data

All examples are shown with synthetic data. These are not actual
patients from UKB RAP.

## Read in CSV file or SQL generated `data.frame`

`{xvhelper}` works with both the CSV files that is generated from
`dx extract_dataset` or the `data.frame` that is generated from running
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
#>   title        ent_field entity name  coding_name code  meaning is_sparse_coding
#>   <chr>        <glue>    <chr>  <chr> <chr>       <chr> <chr>   <chr>           
#> 1 Coffee cons… particip… parti… p100… data_codin… 0     No      <NA>            
#> 2 Coffee cons… particip… parti… p100… data_codin… 1     Yes     <NA>            
#> 3 Coffee cons… particip… parti… p100… data_codin… 0     No      <NA>            
#> 4 Coffee cons… particip… parti… p100… data_codin… 1     Yes     <NA>            
#> 5 Sex of baby  particip… parti… p412… data_codin… 9     Not sp… <NA>            
#> 6 Sex of baby  particip… parti… p412… data_codin… 3     Indete… <NA>            
#> # ℹ 1 more variable: is_multi_select <chr>
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

## Separating out single and multi column decodes

You can have a little more control if you use the two `decode_`
functions, `decode_single()` and `decode_multi_purrr()`:

``` r
decoded_cohort <- mydf[1:10,] |>
  decode_single(merged_code) |>
  decode_multi_purrr(merged_code) |>
  decode_column_names(merged_code)

knitr::kable(decoded_cohort)
```

| participant_eid | age_at_recruitment | coffee_type_instance_0                       | coffee_type_instance_1                       | coffee_type_instance_2 | diagnoses_main_icd10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | sex    | sex_of_baby | smoked_cigarette_or_pipe_within_last_hour_instance_0 | smoked_cigarette_or_pipe_within_last_hour_instance_1 | smoked_cigarette_or_pipe_within_last_hour_instance_2 |
|:----------------|-------------------:|:---------------------------------------------|:---------------------------------------------|:-----------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------|:------------|:-----------------------------------------------------|:-----------------------------------------------------|:-----------------------------------------------------|
| sample_100_116  |                 43 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | R07.3 Other chest pain\|Z09.9 Follow-up examination after unspecified treatment for other conditions\|I83.9 Varicose veins of lower extremities without ulcer or inflammation\|Z30.5 Surveillance of (intra-uterine) contraceptive device\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|Z09 Follow-up examination after treatment for conditions other than malignant neoplasms\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|I83 Varicose veins of lower extremities\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system\|Z30 Contraceptive management\|Z30-Z39 Persons encountering health services in circumstances related to reproduction | Female | NA          | NA                                                   | NA                                                   | No                                                   |
| sample_100_142  |                 60 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | J32.0 Chronic maxillary sinusitis\|R10.3 Pain localised to other parts of lower abdomen\|J32 Chronic sinusitis\|J30-J39 Other diseases of upper respiratory tract\|Chapter X Diseases of the respiratory system\|R10 Abdominal and pelvic pain\|R10-R19 Symptoms and signs involving the digestive system and abdomen\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_285  |                 53 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_290  |                 62 | Instant coffee                               | NA                                           | NA                     | R07.3 Other chest pain\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_304  |                 67 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | I87.1 Compression of vein\|I87 Other disorders of veins\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_328  |                 61 | Decaffeinated coffee (any type)              | NA                                           | NA                     | K01.1 Impacted teeth\|K01 Embedded and impacted teeth\|K00-K14 Diseases of oral cavity, salivary glands and jaws\|Chapter XI Diseases of the digestive system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_348  |                 66 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | Q66.8 Other congenital deformities of feet\|G56.8 Other mononeuropathies of upper limb\|R07.0 Pain in throat\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|G56 Mononeuropathies of upper limb\|G50-G59 Nerve, nerve root and plexus disorders\|Chapter VI Diseases of the nervous system\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                     | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_36   |                 56 | Ground coffee (include espresso, filter etc) | Ground coffee (include espresso, filter etc) | NA                     | I12.0 Hypertensive renal disease with renal failure\|N43.3 Hydrocele, unspecified\|G45.9 Transient cerebral ischaemic attack, unspecified\|I12 Hypertensive renal disease\|I10-I15 Hypertensive diseases\|Chapter IX Diseases of the circulatory system\|N43 Hydrocele and spermatocele\|N40-N51 Diseases of male genital organs\|Chapter XIV Diseases of the genitourinary system\|G45 Transient cerebral ischaemic attacks and related syndromes\|G40-G47 Episodic and paroxysmal disorders\|Chapter VI Diseases of the nervous system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_362  |                 55 | Instant coffee                               | NA                                           | NA                     | Z03.9 Observation for suspected disease or condition, unspecified\|Q66.8 Other congenital deformities of feet\|M20.1 Hallux valgus (acquired)\|Z03 Medical observation and evaluation for suspected diseases and conditions\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|M20 Acquired deformities of fingers and toes\|M20-M25 Other joint disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue                                                                                                                                                                                                                                                                | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_384  |                 53 | Decaffeinated coffee (any type)              | NA                                           | NA                     | Z87.1 Personal history of diseases of the digestive system\|C61 Malignant neoplasm of prostate\|M75.1 Rotator cuff syndrome\|R31 Unspecified haematuria\|D12.5 Sigmoid colon\|Z87 Personal history of other diseases and conditions\|Z80-Z99 Persons with potential health hazards related to family and personal history and certain conditions influencing health status\|Chapter XXI Factors influencing health status and contact with health services\|C60-C63 Malignant neoplasms of male genital organs\|Chapter II Neoplasms\|M75 Shoulder lesions\|M70-M79 Other soft tissue disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue\|R30-R39 Symptoms and signs involving the urinary system\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|D12 Benign neoplasm of colon, rectum, anus and anal canal\|D10-D36 Benign neoplasms                                                                                            | Female | NA          | NA                                                   | NA                                                   | NA                                                   |

## Multi-valued columns

Multivalued columns are returned as a pipe (`|`) delimited string (this
is to avoid issues with commas within categorical values):

``` r
knitr::kable(decoded_cohort[1:10,"diagnoses_main_icd10"])
```

| x                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
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

If we want to split these out, we can use
`tidyr::separate_longer_delim()`:

``` r
decoded_cohort |>
  dplyr::select(participant_eid, diagnoses_main_icd10) |>
  tidyr::separate_longer_delim(diagnoses_main_icd10, delim="|") |>
  knitr::kable()
```

| participant_eid | diagnoses_main_icd10                                                                                                                  |
|:----------------|:--------------------------------------------------------------------------------------------------------------------------------------|
| sample_100_116  | R07.3 Other chest pain                                                                                                                |
| sample_100_116  | Z09.9 Follow-up examination after unspecified treatment for other conditions                                                          |
| sample_100_116  | I83.9 Varicose veins of lower extremities without ulcer or inflammation                                                               |
| sample_100_116  | Z30.5 Surveillance of (intra-uterine) contraceptive device                                                                            |
| sample_100_116  | R07 Pain in throat and chest                                                                                                          |
| sample_100_116  | R00-R09 Symptoms and signs involving the circulatory and respiratory systems                                                          |
| sample_100_116  | Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                 |
| sample_100_116  | Z09 Follow-up examination after treatment for conditions other than malignant neoplasms                                               |
| sample_100_116  | Z00-Z13 Persons encountering health services for examination and investigation                                                        |
| sample_100_116  | Chapter XXI Factors influencing health status and contact with health services                                                        |
| sample_100_116  | I83 Varicose veins of lower extremities                                                                                               |
| sample_100_116  | I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified                                                |
| sample_100_116  | Chapter IX Diseases of the circulatory system                                                                                         |
| sample_100_116  | Z30 Contraceptive management                                                                                                          |
| sample_100_116  | Z30-Z39 Persons encountering health services in circumstances related to reproduction                                                 |
| sample_100_142  | J32.0 Chronic maxillary sinusitis                                                                                                     |
| sample_100_142  | R10.3 Pain localised to other parts of lower abdomen                                                                                  |
| sample_100_142  | J32 Chronic sinusitis                                                                                                                 |
| sample_100_142  | J30-J39 Other diseases of upper respiratory tract                                                                                     |
| sample_100_142  | Chapter X Diseases of the respiratory system                                                                                          |
| sample_100_142  | R10 Abdominal and pelvic pain                                                                                                         |
| sample_100_142  | R10-R19 Symptoms and signs involving the digestive system and abdomen                                                                 |
| sample_100_142  | Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                 |
| sample_100_285  | NA                                                                                                                                    |
| sample_100_290  | R07.3 Other chest pain                                                                                                                |
| sample_100_290  | R07 Pain in throat and chest                                                                                                          |
| sample_100_290  | R00-R09 Symptoms and signs involving the circulatory and respiratory systems                                                          |
| sample_100_290  | Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                 |
| sample_100_304  | I87.1 Compression of vein                                                                                                             |
| sample_100_304  | I87 Other disorders of veins                                                                                                          |
| sample_100_304  | I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified                                                |
| sample_100_304  | Chapter IX Diseases of the circulatory system                                                                                         |
| sample_100_328  | K01.1 Impacted teeth                                                                                                                  |
| sample_100_328  | K01 Embedded and impacted teeth                                                                                                       |
| sample_100_328  | K00-K14 Diseases of oral cavity, salivary glands and jaws                                                                             |
| sample_100_328  | Chapter XI Diseases of the digestive system                                                                                           |
| sample_100_348  | Q66.8 Other congenital deformities of feet                                                                                            |
| sample_100_348  | G56.8 Other mononeuropathies of upper limb                                                                                            |
| sample_100_348  | R07.0 Pain in throat                                                                                                                  |
| sample_100_348  | Q66 Congenital deformities of feet                                                                                                    |
| sample_100_348  | Q65-Q79 Congenital malformations and deformations of the musculoskeletal system                                                       |
| sample_100_348  | Chapter XVII Congenital malformations, deformations and chromosomal abnormalities                                                     |
| sample_100_348  | G56 Mononeuropathies of upper limb                                                                                                    |
| sample_100_348  | G50-G59 Nerve, nerve root and plexus disorders                                                                                        |
| sample_100_348  | Chapter VI Diseases of the nervous system                                                                                             |
| sample_100_348  | R07 Pain in throat and chest                                                                                                          |
| sample_100_348  | R00-R09 Symptoms and signs involving the circulatory and respiratory systems                                                          |
| sample_100_348  | Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                 |
| sample_100_36   | I12.0 Hypertensive renal disease with renal failure                                                                                   |
| sample_100_36   | N43.3 Hydrocele, unspecified                                                                                                          |
| sample_100_36   | G45.9 Transient cerebral ischaemic attack, unspecified                                                                                |
| sample_100_36   | I12 Hypertensive renal disease                                                                                                        |
| sample_100_36   | I10-I15 Hypertensive diseases                                                                                                         |
| sample_100_36   | Chapter IX Diseases of the circulatory system                                                                                         |
| sample_100_36   | N43 Hydrocele and spermatocele                                                                                                        |
| sample_100_36   | N40-N51 Diseases of male genital organs                                                                                               |
| sample_100_36   | Chapter XIV Diseases of the genitourinary system                                                                                      |
| sample_100_36   | G45 Transient cerebral ischaemic attacks and related syndromes                                                                        |
| sample_100_36   | G40-G47 Episodic and paroxysmal disorders                                                                                             |
| sample_100_36   | Chapter VI Diseases of the nervous system                                                                                             |
| sample_100_362  | Z03.9 Observation for suspected disease or condition, unspecified                                                                     |
| sample_100_362  | Q66.8 Other congenital deformities of feet                                                                                            |
| sample_100_362  | M20.1 Hallux valgus (acquired)                                                                                                        |
| sample_100_362  | Z03 Medical observation and evaluation for suspected diseases and conditions                                                          |
| sample_100_362  | Z00-Z13 Persons encountering health services for examination and investigation                                                        |
| sample_100_362  | Chapter XXI Factors influencing health status and contact with health services                                                        |
| sample_100_362  | Q66 Congenital deformities of feet                                                                                                    |
| sample_100_362  | Q65-Q79 Congenital malformations and deformations of the musculoskeletal system                                                       |
| sample_100_362  | Chapter XVII Congenital malformations, deformations and chromosomal abnormalities                                                     |
| sample_100_362  | M20 Acquired deformities of fingers and toes                                                                                          |
| sample_100_362  | M20-M25 Other joint disorders                                                                                                         |
| sample_100_362  | Chapter XIII Diseases of the musculoskeletal system and connective tissue                                                             |
| sample_100_384  | Z87.1 Personal history of diseases of the digestive system                                                                            |
| sample_100_384  | C61 Malignant neoplasm of prostate                                                                                                    |
| sample_100_384  | M75.1 Rotator cuff syndrome                                                                                                           |
| sample_100_384  | R31 Unspecified haematuria                                                                                                            |
| sample_100_384  | D12.5 Sigmoid colon                                                                                                                   |
| sample_100_384  | Z87 Personal history of other diseases and conditions                                                                                 |
| sample_100_384  | Z80-Z99 Persons with potential health hazards related to family and personal history and certain conditions influencing health status |
| sample_100_384  | Chapter XXI Factors influencing health status and contact with health services                                                        |
| sample_100_384  | C60-C63 Malignant neoplasms of male genital organs                                                                                    |
| sample_100_384  | Chapter II Neoplasms                                                                                                                  |
| sample_100_384  | M75 Shoulder lesions                                                                                                                  |
| sample_100_384  | M70-M79 Other soft tissue disorders                                                                                                   |
| sample_100_384  | Chapter XIII Diseases of the musculoskeletal system and connective tissue                                                             |
| sample_100_384  | R30-R39 Symptoms and signs involving the urinary system                                                                               |
| sample_100_384  | Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                 |
| sample_100_384  | D12 Benign neoplasm of colon, rectum, anus and anal canal                                                                             |
| sample_100_384  | D10-D36 Benign neoplasms                                                                                                              |

## New (7/2423) Incremental Decoding of Very Large Data Frames

One thing to note is that decoding a large dataset (for UKB Pheno data,
this can be over 500K rows) takes time. The new function
`decode_multi_large_df()` will split the dataset into a number of data
frames, the size of which is specified by `df_size`, which is then
processed one data frame at a time, with a progress bar.

This is not necessarily faster, but it does give the user a sense of
progress.

Note that the default value for `df_size` is 1000. We’re only changing
it here because the example data is smaller.

``` r
decoded_cohort <- mydf[1:10,] |>
  decode_single(merged_code) |>
  decode_multi_large_df(merged_code, df_size = 2) |>
  decode_column_names(merged_code)


knitr::kable(decoded_cohort)
```

| participant_eid | age_at_recruitment | coffee_type_instance_0                       | coffee_type_instance_1                       | coffee_type_instance_2 | diagnoses_main_icd10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | sex    | sex_of_baby | smoked_cigarette_or_pipe_within_last_hour_instance_0 | smoked_cigarette_or_pipe_within_last_hour_instance_1 | smoked_cigarette_or_pipe_within_last_hour_instance_2 |
|:----------------|-------------------:|:---------------------------------------------|:---------------------------------------------|:-----------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------|:------------|:-----------------------------------------------------|:-----------------------------------------------------|:-----------------------------------------------------|
| sample_100_116  |                 43 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | R07.3 Other chest pain\|Z09.9 Follow-up examination after unspecified treatment for other conditions\|I83.9 Varicose veins of lower extremities without ulcer or inflammation\|Z30.5 Surveillance of (intra-uterine) contraceptive device\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|Z09 Follow-up examination after treatment for conditions other than malignant neoplasms\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|I83 Varicose veins of lower extremities\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system\|Z30 Contraceptive management\|Z30-Z39 Persons encountering health services in circumstances related to reproduction | Female | NA          | NA                                                   | NA                                                   | No                                                   |
| sample_100_142  |                 60 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | J32.0 Chronic maxillary sinusitis\|R10.3 Pain localised to other parts of lower abdomen\|J32 Chronic sinusitis\|J30-J39 Other diseases of upper respiratory tract\|Chapter X Diseases of the respiratory system\|R10 Abdominal and pelvic pain\|R10-R19 Symptoms and signs involving the digestive system and abdomen\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_285  |                 53 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_290  |                 62 | Instant coffee                               | NA                                           | NA                     | R07.3 Other chest pain\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_304  |                 67 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | I87.1 Compression of vein\|I87 Other disorders of veins\|I80-I89 Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified\|Chapter IX Diseases of the circulatory system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_328  |                 61 | Decaffeinated coffee (any type)              | NA                                           | NA                     | K01.1 Impacted teeth\|K01 Embedded and impacted teeth\|K00-K14 Diseases of oral cavity, salivary glands and jaws\|Chapter XI Diseases of the digestive system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_348  |                 66 | Ground coffee (include espresso, filter etc) | NA                                           | NA                     | Q66.8 Other congenital deformities of feet\|G56.8 Other mononeuropathies of upper limb\|R07.0 Pain in throat\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|G56 Mononeuropathies of upper limb\|G50-G59 Nerve, nerve root and plexus disorders\|Chapter VI Diseases of the nervous system\|R07 Pain in throat and chest\|R00-R09 Symptoms and signs involving the circulatory and respiratory systems\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                                                                                                                                                                                                                                                                                                     | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_36   |                 56 | Ground coffee (include espresso, filter etc) | Ground coffee (include espresso, filter etc) | NA                     | I12.0 Hypertensive renal disease with renal failure\|N43.3 Hydrocele, unspecified\|G45.9 Transient cerebral ischaemic attack, unspecified\|I12 Hypertensive renal disease\|I10-I15 Hypertensive diseases\|Chapter IX Diseases of the circulatory system\|N43 Hydrocele and spermatocele\|N40-N51 Diseases of male genital organs\|Chapter XIV Diseases of the genitourinary system\|G45 Transient cerebral ischaemic attacks and related syndromes\|G40-G47 Episodic and paroxysmal disorders\|Chapter VI Diseases of the nervous system                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_362  |                 55 | Instant coffee                               | NA                                           | NA                     | Z03.9 Observation for suspected disease or condition, unspecified\|Q66.8 Other congenital deformities of feet\|M20.1 Hallux valgus (acquired)\|Z03 Medical observation and evaluation for suspected diseases and conditions\|Z00-Z13 Persons encountering health services for examination and investigation\|Chapter XXI Factors influencing health status and contact with health services\|Q66 Congenital deformities of feet\|Q65-Q79 Congenital malformations and deformations of the musculoskeletal system\|Chapter XVII Congenital malformations, deformations and chromosomal abnormalities\|M20 Acquired deformities of fingers and toes\|M20-M25 Other joint disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue                                                                                                                                                                                                                                                                | Female | NA          | NA                                                   | NA                                                   | NA                                                   |
| sample_100_384  |                 53 | Decaffeinated coffee (any type)              | NA                                           | NA                     | Z87.1 Personal history of diseases of the digestive system\|C61 Malignant neoplasm of prostate\|M75.1 Rotator cuff syndrome\|R31 Unspecified haematuria\|D12.5 Sigmoid colon\|Z87 Personal history of other diseases and conditions\|Z80-Z99 Persons with potential health hazards related to family and personal history and certain conditions influencing health status\|Chapter XXI Factors influencing health status and contact with health services\|C60-C63 Malignant neoplasms of male genital organs\|Chapter II Neoplasms\|M75 Shoulder lesions\|M70-M79 Other soft tissue disorders\|Chapter XIII Diseases of the musculoskeletal system and connective tissue\|R30-R39 Symptoms and signs involving the urinary system\|Chapter XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified\|D12 Benign neoplasm of colon, rectum, anus and anal canal\|D10-D36 Benign neoplasms                                                                                            | Female | NA          | NA                                                   | NA                                                   | NA                                                   |

## Returned tables from SparkSQL

If you use `dbGetQuery()` to fetch your table from Spark, multi-value
columns will be returned as list-columns. `decode_df` will return a
pipe-delimited string for these columns as well.

``` r
tibble::tibble(mydf)
#> # A tibble: 100 × 11
#>    participant.eid participant.p21022 participant.p1508_i0 participant.p1508_i1
#>    <chr>                        <dbl>                <dbl>                <dbl>
#>  1 sample_100_116                  43                    3                   NA
#>  2 sample_100_142                  60                    3                   NA
#>  3 sample_100_285                  53                    3                   NA
#>  4 sample_100_290                  62                    2                   NA
#>  5 sample_100_304                  67                    3                   NA
#>  6 sample_100_328                  61                    1                   NA
#>  7 sample_100_348                  66                    3                   NA
#>  8 sample_100_36                   56                    3                    3
#>  9 sample_100_362                  55                    2                   NA
#> 10 sample_100_384                  53                    1                   NA
#> # ℹ 90 more rows
#> # ℹ 7 more variables: participant.p1508_i2 <dbl>, participant.p41202 <list>,
#> #   participant.p31 <dbl>, participant.p41226 <list>,
#> #   participant.p3159_i0 <dbl>, participant.p3159_i1 <dbl>,
#> #   participant.p3159_i2 <dbl>
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

## New (7/28/23) Extract data using `dx-toolkit`

`xvhelper` now contains a number of functions that help you extract and
download pheno data using the `dx-toolkit` from your project. It does
this by using `reticulate` to call the python `dxpy` package and using
system calls to `dx extract_dataset`.

These functions are designed to work within RStudio (on UKB RAP) or
JupyterLab (on DNAnexus/UKB RAP).

[See the
article](https://laderast.github.io/xvhelper/articles/downloading-pheno-data.html)
for more info.
