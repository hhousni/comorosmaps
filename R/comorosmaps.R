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
