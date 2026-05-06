# Rsequoia2 (development version)

## v0.0.1

- Removed `download_brgm()` in favor of the more explicit `download_bdcharm50()` and `download_carhab()` functions.
- Added cache configuration in `inst/config/seq_caches.yaml`, with associated helper functions in `config-cache.R`.
- Added LIDAR tile support:
  - `download_lidar()` downloads LIDAR tiles in parallel and stores them in the cache;
  - `get_lidar()` loads LIDAR tiles in memory, then crops and masks the raster around an area;
  - `seq_lidar()` orchestrates LIDAR processing within a Sequoia workflow.
- Split the former `seq_elevation()` workflow into:
  - `seq_rgealti()` for classic RGE ALTI raster data;
  - `seq_terrain()` for slope/aspect
- Updated `sequoia2()` altimetry workflow: LIDAR data is now attempted first, with fallback to classic WMS raster data.
