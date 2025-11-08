# Data from the General Social Survey (GSS) from the National Opinion Research Center of the University of Chicago

A subset of the data from the `carData::GSSvocab` dataset from the
`carData` package, containing observations from 2016 only.

## Format

A data frame with 1858 rows and 3 variables:

- `vocab`: numeric; the number of words out of 10 correct on a
  vocabulary test.

- `nativeBorn`: factor; Was the respondent born in the US? A factor with
  levels `no` and `yes`.

- `ageGroup`: factor; grouped age of the respondent with levels `18-29`
  `30-39`, `40-49`, `50-59`, and `60+`.
