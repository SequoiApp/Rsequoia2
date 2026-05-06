# Changelog

## Rsequoia2 (development version)

### v0.0.1

#### Added

- Added cache configuration in `inst/config/seq_caches.yaml`, with
  associated helpers in `config-cache.R`.
- Implemented cache configuration across the package.
- Added LIDAR tile support:
  - [`download_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/download_lidar.md)
    downloads LIDAR tiles in parallel and stores them in the cache;
  - [`get_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/get_lidar.md)
    loads LIDAR tiles in memory, then crops and masks the raster around
    an area;
  - [`seq_lidar()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_lidar.md)
    orchestrates LIDAR processing within a Sequoia workflow.

#### Changed

- Replaced `download_brgm()` with the more explicit
  [`download_bdcharm50()`](https://sequoiapp.github.io/Rsequoia2/reference/download_bdcharm50.md)
  and
  [`download_carhab()`](https://sequoiapp.github.io/Rsequoia2/reference/download_carhab.md)
  functions.
- Split the former `seq_elevation()` workflow into:
  - [`seq_rgealti()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_rgealti.md)
    for classic RGE ALTI raster data;
  - [`seq_terrain()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_terrain.md)
    for terrain derivatives such as slope and aspect.
- Updated the
  [`sequoia2()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia2.md)
  altimetry workflow: LIDAR data is now attempted first, with fallback
  to classic WMS raster data.

#### Removed

- Removed Scan support to comply with IGN diffusion rules.
