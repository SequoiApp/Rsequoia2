# Rsequoia2 (development version)

## v0.0.1

### Added

- Added cache configuration in `inst/config/seq_caches.yaml`, with associated helpers in `config-cache.R`.
- Implemented cache configuration across the package.
- Added LIDAR tile support:
  - `download_lidar()` downloads LIDAR tiles in parallel and stores them in the cache;
  - `get_lidar()` loads LIDAR tiles in memory, then crops and masks the raster around an area;
  - `seq_lidar()` orchestrates LIDAR processing within a Sequoia workflow.

### Changed

- Replaced `download_brgm()` with the more explicit `download_bdcharm50()` and `download_carhab()` functions.
- Split the former `seq_elevation()` workflow into:
  - `seq_rgealti()` for classic RGE ALTI raster data;
  - `seq_terrain()` for terrain derivatives such as slope and aspect.
- Updated the `sequoia2()` altimetry workflow: LIDAR data is now attempted first, with fallback to classic WMS raster data.

### Removed

- Removed Scan support to comply with IGN diffusion rules.
