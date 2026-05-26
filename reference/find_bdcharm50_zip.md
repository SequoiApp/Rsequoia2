# Find BDCharm50 archive for a department

Finds the unique BDCharm50 ZIP archive matching a department code.

## Usage

``` r
find_bdcharm50_zip(dep, urls)
```

## Arguments

- dep:

  Department code.

- urls:

  Named `character` vector of archive URLs, usually returned by
  [`get_bdcharm50_url()`](https://sequoiapp.github.io/Rsequoia2/reference/get_bdcharm50_url.md).

## Value

A `character(1)` ZIP filename.
