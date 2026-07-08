# Rsequoia2 (development version)

## v0.0.7
- Write matrice for `menu_rp` and `menu_pm` even if IDU are invalid
- Remove BDP support
- Better parca UI
- Auto correction of fully overlapped cadastral parcels
- New product `"v.cad.etalab.poly"` which is raw downloaded parca

## v0.0.6
- SEQ_FORET_point only return centroid of the biggest envelope

## v0.0.5
- Rework and reorganize sequoia2 menu (#77)
- Add tables in summary :
  - PLT_STR 
  - PLT_TYPE_RICH_STR 
  - PLT_TYPE_RICH_STR_ESS
  - PF_PLT_TYPE_RICH_STR

## v0.0.4
- Add new `DGD_SOUMIS` field and rename `BOISE` to `DGD_BOISE`
- Replace `ua_to_wooded()` and `seq_wooded()` with `ua_to_occupation()` and `seq_occupation()`
- Add UA consistency checks for DGD occupation:
  - force `DGD_BOISE = FALSE` when `DGD_SOUMIS = FALSE`
  - warn when non-wooded surfaces submitted to DGD exceed 10%

## v0.0.3
- Add custom formula capabilities in total row of synthese tables
- Add `seq_cache_clear()`
- Improve robustness and speed of MNH/MNT downloads
- Disable MNS LIDAR product download by default

### Added

- Implemented package-wide cache configuration support;
  - Added cache configuration through `inst/config/seq_caches.yaml`
  - New helpers in `config-cache.R`;
- Added LIDAR support:
  - `download_lidar()` downloads LIDAR tiles in parallel and stores them in cache;
  - `get_lidar()` loads cached LIDAR tiles, then crops and masks them around an area;
  - `seq_lidar()` integrates LIDAR processing into the Sequoia workflow.
- Added shade raster generation:
  - `get_shade()` computes multi-orientation hillshade rasters;
  - `seq_terrain()` now generates shade rasters from MNH data.
- Added `seq_dir_rename()` to rename a Sequoia folder and its associated layer.
- Added package version display in the `sequoia2()` menu.
- Added "ask for help" action in the `sequoia2()` menu.
- Added `unit` argument to `get_slope()` with support for `"degrees"`, `"radians"` and `"percent"`.
- Added human-readable field labels in xlsx generated with `seq_summary()`

### Changed

- Replaced `download_brgm()` with more explicit download helpers:
  - `download_bdcharm50()`;
  - `download_carhab()`.
- Split the former `seq_elevation()` workflow into:
  - `seq_rgealti()` for classic RGE ALTI raster data;
  - `seq_terrain()` for terrain derivatives such as slope, aspect and shade.
- Updated the `sequoia2()` altimetry workflow to try LIDAR data first, with fallback to classic WMS raster data.
- Refactored `seq_summary()` for clearer progress messages, improved table styling and more robust value calculations.
- Extended `seq_summary()` with new summary tables.

### Fixed

- Fixed slope unit handling to ensure percentage slope is correctly calculated.
- Abort summary generation when `cor_area` is missing, or contains only zero/`NA` values.

### Removed

- Removed SCAN support to comply with IGN diffusion rules.
