# Helpers to search within a forest matrix

This function allows searching for parcels in a forest matrix based on
owner names or location (lieu-dit).

## Usage

``` r
search_legal_entity(x, prop = NULL, lieu_dit = NULL)
```

## Arguments

- x:

  `data.frame`; The matrix containing parcel legal entity information in
  a format readable by SEQUOIA.

- prop:

  `character`; Owner name(s) used to further filter results. Defaults to
  `NULL`. Multiple owners can be specified.

- lieu_dit:

  `character`; Locality name(s) used to further filter results. Defaults
  to `NULL`. Multiple locality names can be specified.

## Value

A `data.frame`.

## Details

The search relies on a text–normalization step applied to both the
inputs and the corresponding columns of the matrice. This makes the
search robust to accents, punctuation, spacing irregularities and case
differences.

**Examples of normalization:**

- `"État / Forêts"` → `"ETATFORETS"`

- `" Le Bois-de l'Orme "` → `"LEBOISDELORME"`

- `"Société du Chêne"` → `"SOCIETEDUCHENE"`

## Examples

``` r
if (FALSE) { # \dontrun{
legal_entity <- get_legal_entity(29158)
search_mat <- search_legal_entity(legal_entity, c("penmarch", "guenole"))
} # }
```
