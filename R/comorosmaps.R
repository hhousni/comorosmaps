#' Comoro Islands
#'
#' Draw  maps of Comoros Islands.
#'
#' `comoros()`uses the sf package to plot by plotting the geometry rather than every column and leave the plot pref ready for overplotting with other data
#'
#' @param x     Name of the data set to use. The default is `comoros()`, It draws Comoro Islands as one object without commune.
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city  Include all cities as point features (`TRUE`) or exclude them (`FALSE`). Default is `FALSE`.
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map Comoro Islands as one object without prefecture area
#' comoros()
#' ## Map Comoro Islands as one object with prefecture area
#' comoros(x="country",pref=TRUE)
#' ## Map Comoros Islands as 3 object (Grande Comore, Anjouan, Mohéli) without prefecture area.
#' comoros(x="island",pref=FALSE)
#' ## Map Comoros Islands as 3 object (Grande Comore, Anjouan, Mohéli) with prefecture area
#' comoros(x="island",pref=TRUE)
#'
comoros <- function(x = "country", pref = FALSE, city = FALSE) {
  switch(x,
         "country" = {
           codes <- c("KM")
           if (pref) codes <- c(codes, paste0("KM", 11:33))
           if (city) codes <- c(codes, "KM1c", "KM2c", "KM3c")
         },
         "island" = {
           codes <- c("KM1", "KM2", "KM3")
           if (pref) codes <- c(codes, paste0("KM", 11:33))
           if (city) codes <- c(codes, "KM1c", "KM2c", "KM3c")
         },
         stop("Invalid argument for 'x'")
  )
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}
#' Grande Comore
#'
#' Draw a map for Grande Comore Island
#'
#'
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city  Include all Grande Comore cities as point features (`TRUE`) or exclude them (`FALSE`). Default is `TRUE`.
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map Grande Comore Island
#' grandeComore ()
#' ## Map Grande Comore with prefecture area
#' grandeComore (pref = TRUE)
#'
grandeComore <- function(pref = FALSE, city = TRUE) {
  codes <- c("KM2", if (city) "KM2c", if (pref) paste0("KM", 21:29))
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}


#' Moheli
#'
#' Draw a map for Moheli Islands
#'
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city  Include all Mohéli cities as point features (`TRUE`) or exclude them (`FALSE`). Default is `TRUE`.
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map Moheli Island
#' moheli ()
#' ## Map Moheli Island with prefecture area
#' moheli (pref = TRUE)
moheli <- function(pref = FALSE, city = TRUE) {
  codes <- c("KM3", if (city) "KM3c", if (pref) paste0("KM", 31:33))
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}
#'
#' Anjouan
#'
#' Draw a map for Anjouan Island
#'
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city  Include all Anjouan cities as point features (`TRUE`) or exclude them (`FALSE`). Default is `TRUE`.
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map Anjouan Island.
#' anjouan ()
#' ## Map Anjouan Island with prefecture area.
#' anjouan (pref = TRUE)
#'
anjouan <- function(pref = FALSE, city = TRUE) {
  codes <- c("KM1", if (city) "KM1c", if (pref) paste0("KM", 11:15))
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}

#' Plot a styled map of the Comoro Islands
#'
#' Creates a publication-ready map using ggplot2 with non-overlapping city
#' labels via ggrepel.
#'
#' @param island        Which island to display: `"all"`, `"grande comore"`,
#'   `"anjouan"`, or `"moheli"`. Default is `"all"`.
#' @param pref          Show prefecture boundaries (`TRUE`) or not (`FALSE`). Default `FALSE`.
#' @param commune       Show commune boundaries (`TRUE`) or not (`FALSE`). Default `FALSE`.
#'   When `TRUE`, overrides `pref`.
#' @param label_regions Label prefecture or commune names inside their borders
#'   (`TRUE`) or not (`FALSE`). Default `TRUE` when `pref` or `commune` is `TRUE`.
#' @param city          Show city points and labels (`TRUE`) or not (`FALSE`). Default `TRUE`.
#' @param title         Map title. If `NULL` (default), a title is generated automatically.
#' @param data          Optional data frame to use for a choropleth fill. Must contain
#'   a column matching region names and a numeric column specified by `var`.
#' @param var           Name of the numeric column in `data` to use for choropleth fill.
#' @param join_by       Name of the column in `data` that matches the `name` column
#'   in the spatial data. Default is `"name"`.
#' @param fill_label    Legend title for the choropleth scale. Defaults to the value of `var`.
#'
#' @return A `ggplot` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_sf theme_void theme labs element_text
#'   element_rect geom_sf_text scale_fill_manual margin unit
#' @importFrom ggrepel geom_label_repel
#' @importFrom sf st_centroid st_coordinates
#' @examples
#' ## Styled map of all islands
#' plot_map()
#' ## Anjouan with prefecture names inside borders
#' plot_map(island = "anjouan", pref = TRUE)
#' ## Anjouan at commune level with names
#' plot_map(island = "anjouan", commune = TRUE)
plot_map <- function(island = "all", pref = FALSE, commune = FALSE,
                     label_regions = NULL, city = TRUE, title = NULL,
                     data = NULL, var = NULL, join_by = "name", fill_label = NULL) {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )

  # Commune overrides pref
  if (commune) pref <- FALSE

  # Default label_regions to TRUE when any region level is active
  if (is.null(label_regions)) label_regions <- (pref || commune)

  # Build polygon codes
  poly_codes <- island_codes
  region_codes <- character(0)

  if (commune) {
    comm_pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d{2}$")
    region_codes <- comoromaps_data$adminCode[grepl(comm_pattern, comoromaps_data$adminCode)]
    poly_codes   <- c(poly_codes, region_codes)
  } else if (pref) {
    pref_pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d$")
    region_codes <- comoromaps_data$adminCode[grepl(pref_pattern, comoromaps_data$adminCode)]
    poly_codes   <- c(poly_codes, region_codes)
  }

  polys        <- comoromaps_data %>% filter(adminCode %in% poly_codes)
  region_polys <- comoromaps_data %>% filter(adminCode %in% region_codes)
  cities       <- comoromaps_data %>% filter(adminCode %in% paste0(island_codes, "c"))

  # Choropleth: join user data to region polygons
  choropleth <- !is.null(data) && !is.null(var) && length(region_codes) > 0
  if (choropleth) {
    if (is.null(fill_label)) fill_label <- var
    region_polys <- dplyr::left_join(region_polys, data, by = c("name" = join_by))
    region_polys$.choro_var <- region_polys[[var]]
  }

  if (is.null(title)) {
    title <- switch(island,
      "all"           = "Comoro Islands",
      "grande comore" = "Grande Comore",
      "anjouan"       = "Anjouan",
      "moheli"        = "Moh\u00e9li"
    )
    if (commune)    title <- paste0(title, " \u2014 Communes")
    else if (pref)  title <- paste0(title, " \u2014 Prefectures")
    if (city)       title <- paste0(title, if (commune || pref) " & Cities" else " \u2014 Cities")
  }

  # Font size: smaller for communes (many polygons), larger for prefectures
  region_label_size <- if (commune) 2.0 else 2.8

  island_base <- comoromaps_data %>% filter(adminCode %in% island_codes)

  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5,
                                              margin = ggplot2::margin(b = 8)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin     = ggplot2::margin(10, 10, 10, 10)
    ) +
    ggplot2::labs(title = title)

  if (choropleth) {
    p <- p +
      ggplot2::geom_sf(data = island_base, fill = "#f5f0e8", colour = "grey40", linewidth = 0.4) +
      ggplot2::geom_sf(data = region_polys, ggplot2::aes(fill = .choro_var),
                       colour = "grey40", linewidth = 0.3) +
      ggplot2::scale_fill_viridis_c(name = fill_label, option = "plasma", na.value = "grey80")
  } else {
    p <- p +
      ggplot2::geom_sf(data = polys, fill = "#f5f0e8", colour = "grey40", linewidth = 0.4)
  }

  # Label region names at polygon centroids
  if (label_regions && nrow(region_polys) > 0) {
    suppressWarnings({
      centroids <- sf::st_centroid(region_polys)
    })
    p <- p +
      ggplot2::geom_sf_text(
        data     = centroids,
        ggplot2::aes(label = name),
        size     = region_label_size,
        colour   = "#2c3e50",
        fontface = "bold",
        check_overlap = FALSE
      )
  }

  if (city && nrow(cities) > 0) {
    p <- p +
      ggplot2::geom_sf(data = cities, colour = "#e74c3c", size = 1.0, shape = 21,
                       fill = "#e74c3c") +
      ggrepel::geom_label_repel(
        data               = cities,
        ggplot2::aes(label = name, geometry = geometry),
        stat               = "sf_coordinates",
        size               = 2.0,
        label.padding      = ggplot2::unit(0.10, "lines"),
        label.size         = 0.10,
        label.r            = ggplot2::unit(0.1, "lines"),
        fill               = "white",
        colour             = "#1a1a2e",
        segment.colour     = "grey60",
        segment.size       = 0.3,
        max.overlaps       = Inf,
        min.segment.length = 0.2,
        seed               = 42
      )
  }

  p
}

#' Comoro Islands Communes
#'
#' Draw a map of Comoros at the commune level (admin3).
#'
#' @param island  Filter by island: `"grande comore"`, `"anjouan"`, `"moheli"`, or `"all"` (default).
#' @param city    Include capital cities as point features (`TRUE`) or exclude them (`FALSE`). Default is `FALSE`.
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map all communes
#' commune()
#' ## Map only Grande Comore communes
#' commune(island = "grande comore")
#' ## Map Anjouan communes with cities
#' commune(island = "anjouan", city = TRUE)
commune <- function(island = "all", city = FALSE) {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )
  commune_codes <- comoromaps_data$adminCode[
    grepl(paste0("^(", paste(island_codes, collapse="|"), ")\\d{2}$"),
          comoromaps_data$adminCode)
  ]
  city_codes <- if (city) paste0(island_codes, "c")
  codes <- c(commune_codes, city_codes)
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}

#' Interactive map of the Comoro Islands
#'
#' Opens an interactive leaflet map in the viewer or browser. Click polygons
#' to see region names; hover over city markers to see city names.
#'
#' @param island  Which island to display: `"all"`, `"grande comore"`,
#'   `"anjouan"`, or `"moheli"`. Default is `"all"`.
#' @param pref    Show prefecture boundaries (`TRUE`) or not (`FALSE`). Default `FALSE`.
#' @param commune Show commune boundaries (`TRUE`) or not (`FALSE`). Default `FALSE`.
#'   When `TRUE`, overrides `pref`.
#' @param city    Show city markers (`TRUE`) or not (`FALSE`). Default `TRUE`.
#' @param label_regions Show permanent region name labels on polygons (`TRUE`) or
#'   not (`FALSE`). Default `TRUE`.
#' @param data          Optional data frame to use for a choropleth fill.
#' @param var           Name of the numeric column in `data` for choropleth fill.
#' @param join_by       Column in `data` matching the region `name` column. Default `"name"`.
#' @param fill_label    Legend title for the choropleth scale. Defaults to `var`.
#'
#' @return A `leaflet` map widget.
#' @export
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers addLayersControl
#'   layersControlOptions leafletOptions
#' @importFrom sf st_transform
#' @examples
#' ## Interactive map of all islands
#' \dontrun{
#' view_map()
#' view_map(island = "anjouan", commune = TRUE, city = TRUE)
#' }
view_map <- function(island = "all", pref = FALSE, commune = FALSE, city = TRUE, label_regions = TRUE,
                     data = NULL, var = NULL, join_by = "name", fill_label = NULL) {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )

  if (commune) pref <- FALSE

  # Island outlines
  islands <- comoromaps_data %>%
    filter(adminCode %in% island_codes) %>%
    sf::st_transform(4326)

  # Prefecture or commune polygons
  region_data <- NULL
  if (commune) {
    comm_pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d{2}$")
    region_codes <- comoromaps_data$adminCode[grepl(comm_pattern, comoromaps_data$adminCode)]
    if (length(region_codes) > 0)
      region_data <- comoromaps_data %>% filter(adminCode %in% region_codes) %>%
        sf::st_transform(4326)
  } else if (pref) {
    pref_pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d$")
    region_codes <- comoromaps_data$adminCode[grepl(pref_pattern, comoromaps_data$adminCode)]
    if (length(region_codes) > 0)
      region_data <- comoromaps_data %>% filter(adminCode %in% region_codes) %>%
        sf::st_transform(4326)
  }

  # Choropleth: join user data to region polygons
  choropleth <- !is.null(data) && !is.null(var) && !is.null(region_data)
  choro_pal  <- NULL
  if (choropleth) {
    if (is.null(fill_label)) fill_label <- var
    region_data <- dplyr::left_join(region_data, data, by = c("name" = join_by))
    region_data$.choro_var <- region_data[[var]]
    choro_pal <- leaflet::colorNumeric("viridis", domain = region_data$.choro_var, na.color = "#808080")
  }

  cities_data <- NULL
  if (city) {
    cities_data <- comoromaps_data %>%
      filter(adminCode %in% paste0(island_codes, "c")) %>%
      sf::st_transform(4326)
  }

  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 9)) %>%
    leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                      attribution = "\u00a9 OpenStreetMap contributors \u00a9 CARTO")

  # Draw regions FIRST (as fill layer), then island border on top
  if (!is.null(region_data)) {
    layer_name <- if (commune) "Communes" else "Prefectures"
    label_size  <- if (commune) "10px" else "12px"
    fill_col  <- if (choropleth) choro_pal(region_data$.choro_var) else "#d4e6f1"
    fill_opac <- if (choropleth) 0.8 else 0.6
    poly_args <- list(
      data        = region_data,
      fillColor   = fill_col,
      fillOpacity = fill_opac,
      color       = "#2471a3",
      weight      = 1.2,
      popup       = ~name,
      group       = layer_name
    )
    if (label_regions) {
      poly_args$label        <- ~name
      poly_args$labelOptions <- leaflet::labelOptions(
        permanent  = TRUE,
        direction  = "center",
        textOnly   = TRUE,
        style      = list(
          "font-size"   = label_size,
          "font-weight" = "bold",
          "color"       = "#1a1a2e",
          "text-shadow" = "1px 1px 2px white, -1px -1px 2px white"
        )
      )
    }
    m <- do.call(leaflet::addPolygons, c(list(m), poly_args))
  }

  # Island outline drawn on top — border only when regions are present
  m <- m %>%
    leaflet::addPolygons(
      data        = islands,
      fillColor   = "#f5f0e8",
      fillOpacity = if (!is.null(region_data)) 0 else 0.5,
      color       = "#333333",
      weight      = if (!is.null(region_data)) 2.0 else 1.5,
      popup       = ~name,
      group       = "Islands"
    )

  if (!is.null(cities_data)) {
    m <- m %>%
      leaflet::addCircleMarkers(
        data         = cities_data,
        radius       = 4,
        color        = "#e74c3c",
        fillColor    = "#e74c3c",
        fillOpacity  = 0.8,
        stroke       = TRUE,
        weight       = 1,
        opacity      = 1,
        label        = ~name,
        popup        = ~name,
        group        = "Cities"
      )
  }

  if (choropleth) {
    m <- m %>% leaflet::addLegend(
      position = "bottomright",
      pal      = choro_pal,
      values   = region_data$.choro_var,
      title    = fill_label,
      opacity  = 0.8
    )
  }

  layers <- c(if (!is.null(region_data)) if (commune) "Communes" else "Prefectures",
              "Islands",
              if (!is.null(cities_data)) "Cities")

  m %>% leaflet::addLayersControl(
    overlayGroups = layers,
    options       = leaflet::layersControlOptions(collapsed = FALSE)
  )
}

#' Get commune boundaries
#'
#' Returns commune (admin3) polygons as an `sf` object. Useful for custom
#' spatial analysis, joining your own data, or exporting to GeoJSON/CSV.
#'
#' @param island Filter by island: `"grande comore"`, `"anjouan"`, `"moheli"`,
#'   or `"all"` (default).
#' @return An `sf` data frame with columns `name`, `adminCode`, and `geometry`.
#' @export
#' @examples
#' communes <- get_communes()
#' anjouan_communes <- get_communes(island = "anjouan")
get_communes <- function(island = "all") {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )
  pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d{2}$")
  comoromaps_data %>%
    filter(grepl(pattern, adminCode)) %>%
    select(name, adminCode, geometry)
}

#' Get prefecture boundaries
#'
#' Returns prefecture (admin2) polygons as an `sf` object.
#'
#' @param island Filter by island: `"grande comore"`, `"anjouan"`, `"moheli"`,
#'   or `"all"` (default).
#' @return An `sf` data frame with columns `name`, `adminCode`, and `geometry`.
#' @export
#' @examples
#' prefectures <- get_prefectures()
#' gc_prefs <- get_prefectures(island = "grande comore")
get_prefectures <- function(island = "all") {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )
  pattern <- paste0("^(", paste(island_codes, collapse = "|"), ")\\d$")
  comoromaps_data %>%
    filter(grepl(pattern, adminCode)) %>%
    select(name, adminCode, geometry)
}

#' Get city locations
#'
#' Returns city point features as an `sf` object.
#'
#' @param island Filter by island: `"grande comore"`, `"anjouan"`, `"moheli"`,
#'   or `"all"` (default).
#' @return An `sf` data frame with columns `name`, `adminCode`, and `geometry`.
#' @export
#' @examples
#' cities <- get_cities()
#' moheli_cities <- get_cities(island = "moheli")
get_cities <- function(island = "all") {
  island_codes <- switch(island,
    "all"           = c("KM1", "KM2", "KM3"),
    "grande comore" = "KM2",
    "anjouan"       = "KM1",
    "moheli"        = "KM3",
    stop("Invalid 'island'. Use 'all', 'grande comore', 'anjouan', or 'moheli'.")
  )
  comoromaps_data %>%
    filter(adminCode %in% paste0(island_codes, "c")) %>%
    select(name, adminCode, geometry)
}
