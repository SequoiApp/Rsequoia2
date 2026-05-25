# Retrieve cadastral gaps around the \_PARCA\_

Retrieve cadastral gaps around the \_PARCA\_

## Usage

``` r
get_voids(x)
```

## Arguments

- x:

  An \`sf\` object used as the input area. It must contain a source
  field with \`bdp\` or \`etalab\` value.

## Value

An \`sf\` object of type \`POLYGON\` containing cadastral gaps with
standardized fields, including: \* \`TYPE\` - Empty, to complete \*
\`NATURE\` - Empty \* \`NAME\` - Empty, to complete \* \`SOURCE\` -
Empty
