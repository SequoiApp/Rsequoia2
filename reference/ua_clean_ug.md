# Clean management units (UG) by correcting minor inconsistencies in the *UA* sf object

Detects and corrects minor inconsistencies within management units (UG)
in *UA*. Lines with small surfaces relative to their UG are updated to
match the dominant description.

## Usage

``` r
ua_clean_ug(ua, atol = 0.5, rtol = 0.1)
```

## Arguments

- ua:

  `sf` object containing analysis units; with at least the UG identifier
  field and relevant attribute fields.

- atol:

  Absolute tolerance for surface correction (default 0.50 ha).

- rtol:

  Relative tolerance for surface correction within a UG (default 10%).

## Value

An `sf` object identical to `ua`, with minor inconsistent lines
corrected and an additional logical column `ug_valid` indicating UG
consistency.

## Details

The function works by:

1.  Checking UG consistency with
    [`ua_check_ug()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_check_ug.md).

2.  Identifying the dominant row per UG (largest total surface).

3.  Correcting rows that differ from the dominant description and whose
    surface is smaller than `atol` and represents less than `rtol` of UG
    total.

4.  Returning the corrected UA and rechecking inconsistencies.
