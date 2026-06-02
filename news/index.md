# Changelog

## Rsequoia2 (development version)

### v0.0.3

- Add custom formula capabilities in total row of synthese tables
- Add
  [`seq_cache_clear()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache_clear.md)
- Improve robustness and speed of MNH/MNT downloads
- Disable MNS LIDAR product download by default

### v0.0.2

#### Added

- Implemented package-wide cache configuration support;
  - Added cache configuration through `inst/config/seq_caches.yaml`
  - New helpers in `config-cache.R`;
- Added LIDAR support:
  - [`download_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/download_lidar.md)
    downloads LIDAR tiles in parallel and stores them in cache;
  - [`get_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/get_lidar.md)
    loads cached LIDAR tiles, then crops and masks them around an area;
  - [`seq_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_lidar.md)
    integrates LIDAR processing into the Sequoia workflow.
- Added shade raster generation:
  - [`get_shade()`](https://sequoiapp.github.io/Rsequoia2/reference/get_shade.md)
    computes multi-orientation hillshade rasters;
  - [`seq_terrain()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_terrain.md)
    now generates shade rasters from MNH data.
- Added
  [`seq_dir_rename()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_dir_rename.md)
  to rename a Sequoia folder and its associated layer.
- Added package version display in the
  [`sequoia2()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia2.md)
  menu.
- Added “ask for help” action in the
  [`sequoia2()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia2.md)
  menu.
- Added `unit` argument to
  [`get_slope()`](https://sequoiapp.github.io/Rsequoia2/reference/get_slope.md)
  with support for `"degrees"`, `"radians"` and `"percent"`.
- Added human-readable field labels in xlsx generated with
  [`seq_summary()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_summary.md)

#### Changed

- Replaced `download_brgm()` with more explicit download helpers:
  - [`download_bdcharm50()`](https://sequoiapp.github.io/Rsequoia2/reference/download_bdcharm50.md);
  - [`download_carhab()`](https://sequoiapp.github.io/Rsequoia2/reference/download_carhab.md).
- Split the former `seq_elevation()` workflow into:
  - [`seq_rgealti()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_rgealti.md)
    for classic RGE ALTI raster data;
  - [`seq_terrain()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_terrain.md)
    for terrain derivatives such as slope, aspect and shade.
- Updated the
  [`sequoia2()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia2.md)
  altimetry workflow to try LIDAR data first, with fallback to classic
  WMS raster data.
- Refactored
  [`seq_summary()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_summary.md)
  for clearer progress messages, improved table styling and more robust
  value calculations.
- Extended
  [`seq_summary()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_summary.md)
  with new summary tables.

#### Fixed

- Fixed slope unit handling to ensure percentage slope is correctly
  calculated.
- Abort summary generation when `cor_area` is missing, or contains only
  zero/`NA` values.

#### Removed

- Removed SCAN support to comply with IGN diffusion rules.
