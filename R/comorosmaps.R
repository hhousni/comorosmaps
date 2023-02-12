#' Comoro Islands
#'
#' Draw  maps of Comoros Islands.
#'
#' `comoros()`uses the sf package to plot by plotting the geometry rather than every column and leave the plot pref ready for overplotting with other data
#'
#' @param x     Name of the data set to use. The default is `comoros()`, It draws Comoro Islands as one object without commune.
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city
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
           if (city) codes <- c(codes, "Moroni", "Mutsamudu", "Fomboni")
         },
         "island" = {
           codes <- c("KM1", "KM2", "KM3")
           if (pref) codes <- c(codes, paste0("KM", 11:33))
           if (city) codes <- c(codes, "Moroni", "Mutsamudu", "Fomboni")
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
#' @param x     Name of the data set to use. The default is `grandeComore()`, It draws Grande Comore Island as one object without prefecture.
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city
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
grandeComore <- function(x = "grande comore", pref = FALSE, city = TRUE) {
  codes <- switch(x,
                  "grande comore" = {
                    codes <- c("KM2", if (city) "Moroni", if (pref) paste0("KM", 21:29))
                    codes
                  },
                  stop("Invalid argument for 'x'")
  )
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}


#' Moheli
#'
#' Draw a map for Moheli Islands
#'
#' @param x     Name of the data set to use. The default is `moheli()`, It draws Moheli Islands as one object without prefecture.
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city
#' @return The data set used is in `sf` format
#' @export
#' @importFrom sf st_geometry
#' @importFrom graphics plot
#' @examples
#' ## Map Moheli Island
#' moheli ()
#' ## Map Moheli Island with prefecture area
#' moheli (pref = TRUE)
moheli <- function(x = "moheli", pref = FALSE, city = TRUE) {
  codes <- switch(x,
                  "moheli" = {
                    codes <- c("KM3", if (city) "Fomboni", if (pref) paste0("KM", 31:33))
                    codes
                  },
                  stop("Invalid argument for 'x'")
  )
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}
#'
#' Anjouan
#'
#' Draw a map for Anjouan Island
#'
#' @param x     Name of the data set to use. The default is `anjouan()`, It draws Anjouan Island as one object without prefecture.
#' @param pref  Choose to map with prefecture area ("pref" = TRUE) or without prefecture area ("pref" = FALSE)
#' @param city
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
anjouan <- function(x = "anjouan", pref = FALSE, city = TRUE) {
  codes <- switch(x,
                  "anjouan" = {
                    codes <- c("KM1", if (city) "Mutsamudu", if (pref) paste0("KM", 11:15))
                    codes
                  },
                  stop("Invalid argument for 'x'")
  )
  km <- comoromaps_data %>% filter(adminCode %in% codes) %>% select(name, geometry)
  plot(sf::st_geometry(km))
  invisible(unique(km))
}

