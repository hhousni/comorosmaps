# ─────────────────────────────────────────────────────────────────────────────
# create_dashboard.R
# Generates a standalone ministerial HTML dashboard for the Union des Comores.
# Run:  source("data-raw/create_dashboard.R")   (from project root)
# Output: comoros_dashboard.html
# ─────────────────────────────────────────────────────────────────────────────
library(comorosmaps); library(sf); library(dplyr)

# ── 1. Spatial data ──────────────────────────────────────────────────────────
communes <- get_communes() %>% sf::st_transform(4326)

# ── 2. Synthetic indicators (replace with real data) ─────────────────────────
set.seed(42); n <- nrow(communes)
communes$population    <- round(runif(n, 2000, 44000))
communes$literacy_rate <- round(runif(n, 40, 94), 1)
communes$health_index  <- round(runif(n, 28, 91), 1)
communes$poverty_rate  <- round(runif(n, 12, 72), 1)

# ── 3. GeoJSON ───────────────────────────────────────────────────────────────
tmp <- tempfile(fileext = ".geojson")
sf::st_write(communes, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
geojson_text <- paste(readLines(tmp, warn = FALSE), collapse = "\n"); unlink(tmp)

# ── 4. Summary stats ─────────────────────────────────────────────────────────
total_pop    <- format(sum(communes$population), big.mark = "\u202f", scientific = FALSE)
n_communes   <- nrow(communes)
avg_literacy <- round(mean(communes$literacy_rate), 1)
avg_health   <- round(mean(communes$health_index), 1)
report_date  <- format(Sys.Date(), "%d %B %Y")

top10 <- communes %>% as.data.frame() %>% arrange(desc(population)) %>% head(10)
js_labels <- paste0('["', paste(top10$name, collapse = '","'), '"]')
js_values <- paste0('[', paste(top10$population, collapse = ','), ']')

tbl_rows <- communes %>% as.data.frame() %>% arrange(desc(population)) %>% head(20) %>%
  select(name, population, literacy_rate, health_index, poverty_rate) %>%
  apply(1, function(r) paste0(
    "<tr><td>", r["name"], "</td><td>",
    format(as.integer(r["population"]), big.mark = "\u202f"), "</td><td>",
    r["literacy_rate"], "%</td><td>", r["health_index"], "</td><td>",
    r["poverty_rate"], "%</td></tr>")) %>% paste(collapse = "\n")

# ── 5. Load template & substitute tokens ─────────────────────────────────────
tmpl <- file.path("data-raw", "dashboard_template.html")
html <- paste(readLines(tmpl, warn = FALSE), collapse = "\n")

html <- gsub("{{DATE}}",         report_date,   html, fixed = TRUE)
html <- gsub("{{TOTAL_POP}}",    total_pop,     html, fixed = TRUE)
html <- gsub("{{N_COMMUNES}}",   n_communes,    html, fixed = TRUE)
html <- gsub("{{AVG_LITERACY}}", avg_literacy,  html, fixed = TRUE)
html <- gsub("{{AVG_HEALTH}}",   avg_health,    html, fixed = TRUE)
html <- gsub("{{TABLE_ROWS}}",   tbl_rows,      html, fixed = TRUE)
html <- gsub("{{GEOJSON}}",      geojson_text,  html, fixed = TRUE)
html <- gsub("{{JS_LABELS}}",    js_labels,     html, fixed = TRUE)
html <- gsub("{{JS_VALUES}}",    js_values,     html, fixed = TRUE)

# ── 6. Write output ──────────────────────────────────────────────────────────
out <- "comoros_dashboard.html"
writeLines(html, out, useBytes = TRUE)
message("Dashboard created: ", normalizePath(out))
