# Return the reference or specific level of a factor

Extracts the reference or a specific level the supplied factor,
returning it as a factor with the same levels as the one supplied.

## Usage

``` r
ref_level(fct)

level(fct, level)
```

## Arguments

- fct:

  factor; the factor from which the reference or specific level will be
  extracted.

- level:

  character; the specific level to extract in the case of `level()`.

## Value

A factor with the selected values and the same levels, level order and
ordered status as `fct`. `ref_level()` returns the first level.

## Examples

``` r
f <- factor(sample(letters[1:5], 100, replace = TRUE))

# the reference level
ref_level(f)
#> [1] a
#> Levels: a b c d e

# a specific level
level(f, level = "b")
#> [1] b
#> Levels: a b c d e

# note that the levels will always match the input factor
identical(levels(f), levels(ref_level(f)))
#> [1] TRUE
identical(levels(f), levels(level(f, "c")))
#> [1] TRUE
```
