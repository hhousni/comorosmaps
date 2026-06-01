# Python Version — To Do

Create a Python package `comorosmaps` on PyPI mirroring the R package.

## To Do

- [ ] Create a new repo `comorosmaps-py` on GitHub
- [ ] Convert shapefiles (adm0–adm3 + cities) to GeoPackage using R `sf::st_write()` or GDAL
- [ ] Set up Python package structure:
  ```
  comorosmaps-py/
    comorosmaps/
      __init__.py
      _data.py        # internal helpers to load bundled GeoPackage files
      data/           # bundled .gpkg files
    pyproject.toml
    README.md
  ```
- [ ] Implement functions returning `GeoDataFrame`:
  - `get_communes(island="all")`
  - `get_prefectures(island="all")`
  - `get_cities(island="all")`
  - `comoros()`, `grande_comore()`, `moheli()`, `anjouan()`
- [ ] Add a `plot()` convenience wrapper using `geopandas.GeoDataFrame.plot()`
- [ ] Write `pyproject.toml` with metadata (name, version, dependencies: geopandas)
- [ ] Write `README.md` with install + usage examples
- [ ] Publish to PyPI:
  - Create account at https://pypi.org
  - `pip install build twine`
  - `python -m build`
  - `twine upload dist/*`

## Notes

- Reuse shapefiles already in `data-raw/shp_files/`
- GeoPackage (.gpkg) is the best bundling format — one file per layer
- Target Python >= 3.9, geopandas >= 0.12
