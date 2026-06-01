# comorosmaps 1.6.1

* Added `get_communes()`, `get_prefectures()`, and `get_cities()` to give
  direct access to the underlying spatial datasets as `sf` objects.

# comorosmaps 1.6.0

* Added choropleth support to both `plot_map()` and `view_map()` via the new
  `data`, `var`, `join_by`, and `fill_label` arguments.
* `plot_map()` uses a viridis colour scale rendered with **ggplot2** and
  **ggrepel** for repelled polygon labels.
* `view_map()` uses a `colorNumeric` palette with an interactive legend.

# comorosmaps 1.5.3

* Added `label_regions` argument to `view_map()` so permanent polygon labels
  can be toggled on or off for each administrative level.

# comorosmaps 1.5.2

* Added `plot_map()` for publication-ready static maps using **ggplot2**.
* Added `view_map()` for interactive Leaflet maps with layer controls.
* Added `commune()` to display commune-level boundaries.
* City markers (268 localities) can be shown or hidden via `city = TRUE/FALSE`.

# comorosmaps 1.0.0

* Initial CRAN release.
* `comoros()`, `grandeComore()`, `moheli()`, and `anjouan()` provide static
  maps of the country and each island with prefecture boundaries.
