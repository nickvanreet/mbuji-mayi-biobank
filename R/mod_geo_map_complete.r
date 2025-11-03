# mod_geo_map.R
# Geography module with health zone choropleth and positivity rates
# =============================================================================

#' Geography Module - UI
#' @param id Module namespace ID
#' @export
mod_geo_map_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    
    # Settings card
    bslib::card(
      bslib::card_header("Map Settings"),
      
      shiny::checkboxInput(ns("map_use_online"), "Load GRID3 online", value = TRUE),
      shiny::fileInput(ns("map_upload"), "Or upload shapefile",
                       accept = c(".geojson", ".json", ".gpkg", ".shp", ".zip")),
      
      shiny::hr(),
      
      shiny::selectInput(ns("map_zone_col"), "Zone column", choices = NULL),
      shiny::selectInput(ns("map_prov_col"), "Province column", choices = NULL),
      
      shiny::selectInput(ns("map_metric"), "Color by",
                         choices = c(
                           "Sample count" = "n",
                           "DA samples" = "n_da",
                           "DP samples" = "n_dp",
                           "% Female" = "pct_female",
                           "PCR positivity %" = "pcr_pos_rate",
                           "iELISA positivity %" = "ielisa_pos_rate"
                         ),
                         selected = "n"),
      
      shiny::checkboxInput(ns("map_show_provinces"), "Show province outlines", 
                           value = TRUE),
      
      shiny::hr(),
      
      shiny::actionButton(ns("map_focus_kasai"), "Focus Kasaï Region", 
                          class = "btn-primary w-100 mb-2"),
      shiny::actionButton(ns("map_reload"), "Reload Map Data", 
                          class = "btn-secondary w-100 mb-2"),
      shiny::downloadButton(ns("map_dl_summary"), "Download Zone Summary", 
                            class = "btn-sm w-100")
    ),
    
    # Map card
    bslib::card(
      bslib::card_header("Health Zones Map"),
      leaflet::leafletOutput(ns("map_main"), height = 700)
    )
  )
}

#' Geography Module - Server
#' @param id Module namespace ID
#' @param biobank_filtered Reactive containing filtered biobank data
#' @param lab_joined Reactive containing joined lab results
#' @param config Reactive containing app configuration
#' @export
mod_geo_map_server <- function(id, biobank_filtered, lab_joined, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    shapefile_data <- shiny::reactiveVal(NULL)
    
    # Initialize settings from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "map" %in% names(cfg)) {
        shiny::updateCheckboxInput(session, "map_use_online", 
                                    value = cfg$map$use_grid3_online)
      }
    })
    
    # Load shapefile
    map_zones <- shiny::reactive({
      # Trigger on reload button or initial load
      input$map_reload
      
      shp <- NULL
      
      # Try online GRID3
      if (input$map_use_online) {
        cfg <- config()
        if (!is.null(cfg) && "map" %in% names(cfg)) {
          shp <- tryCatch({
            url <- cfg$map$grid3_url
            sf_data <- sf::read_sf(url, quiet = TRUE)
            
            # Flatten list columns
            for (col in names(sf_data)) {
              if (is.list(sf_data[[col]]) && col != attr(sf_data, "sf_column")) {
                sf_data[[col]] <- vapply(sf_data[[col]], 
                                         function(x) paste(as.character(x), collapse = "; "), 
                                         "")
              }
            }
            
            # Make valid
            sf_data <- sf::st_make_valid(sf_data)
            sf_data
          }, error = function(e) {
            shiny::showNotification(
              paste("Failed to load GRID3 data:", e$message),
              type = "error"
            )
            NULL
          })
        }
      }
      
      # Try uploaded file
      if (is.null(shp) && !is.null(input$map_upload)) {
        shp <- tryCatch({
          sf_data <- sf::read_sf(input$map_upload$datapath, quiet = TRUE)
          sf_data <- sf::st_make_valid(sf_data)
          sf_data
        }, error = function(e) {
          shiny::showNotification(
            paste("Failed to load uploaded file:", e$message),
            type = "error"
          )
          NULL
        })
      }
      
      shp
    })
    
    # Update column selectors
    shiny::observe({
      shp <- map_zones()
      if (is.null(shp)) return()
      
      cfg <- config()
      cols <- setdiff(names(shp), attr(shp, "sf_column"))
      
      # Auto-detect zone column
      zone_col <- if (!is.null(cfg) && "map" %in% names(cfg)) {
        detected <- grep(cfg$map$zone_field_regex, cols, 
                         ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(detected)) detected else cols[1]
      } else {
        cols[1]
      }
      
      # Auto-detect province column
      prov_col <- if (!is.null(cfg) && "map" %in% names(cfg)) {
        detected <- grep(cfg$map$province_field_regex, cols, 
                         ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(detected)) detected else cols[2]
      } else {
        cols[2]
      }
      
      shiny::updateSelectInput(session, "map_zone_col", 
                               choices = cols, selected = zone_col)
      shiny::updateSelectInput(session, "map_prov_col", 
                               choices = cols, selected = prov_col)
    })
    
    # Compute zone summaries
    zone_summary <- shiny::reactive({
      bio <- biobank_filtered()
      shiny::req(bio, nrow(bio) > 0)
      
      lab <- lab_joined()
      
      # Join lab results if available
      if (!is.null(lab) && nrow(lab) > 0) {
        bio <- bio %>%
          dplyr::left_join(
            lab %>% dplyr::select(barcode, lab_id, 
                                  is_pcr_pos, any_ielisa_pos = any_pos),
            by = c("barcode", "lab_id")
          )
      } else {
        bio <- bio %>%
          dplyr::mutate(is_pcr_pos = NA, any_ielisa_pos = NA)
      }
      
      # Normalize for joining
      bio <- bio %>%
        dplyr::mutate(
          zone_norm = normalize_text(zone),
          prov_norm = normalize_text(province)
        )
      
      # Compute summaries
      bio %>%
        dplyr::group_by(zone_norm, prov_norm, zone, province) %>%
        dplyr::summarise(
          n = dplyr::n(),
          n_da = sum(study == "DA", na.rm = TRUE),
          n_dp = sum(study == "DP", na.rm = TRUE),
          pct_female = mean(sex == "F", na.rm = TRUE) * 100,
          
          # PCR metrics
          pcr_n = sum(!is.na(is_pcr_pos)),
          pcr_pos = sum(is_pcr_pos %in% TRUE, na.rm = TRUE),
          pcr_pos_rate = if (pcr_n > 0) (pcr_pos / pcr_n * 100) else NA_real_,
          
          # iELISA metrics
          iel_n = sum(!is.na(any_ielisa_pos)),
          iel_pos = sum(any_ielisa_pos %in% TRUE, na.rm = TRUE),
          ielisa_pos_rate = if (iel_n > 0) (iel_pos / iel_n * 100) else NA_real_,
          
          # Latest sample
          last_sample = max(date_sample, na.rm = TRUE),
          
          # Transport medians
          med_field_hs = stats::median(transport_field_hs, na.rm = TRUE),
          med_hs_lsd = stats::median(transport_hs_lsd, na.rm = TRUE),
          med_lsd_inrb = stats::median(transport_lsd_inrb, na.rm = TRUE),
          
          .groups = "drop"
        )
    })
    
    # Build province outlines
    province_outlines <- shiny::reactive({
      shp <- map_zones()
      shiny::req(shp, input$map_prov_col)
      
      if (!input$map_show_provinces) return(NULL)
      
      tryCatch({
        shp %>%
          dplyr::mutate(prov_norm = normalize_text(.data[[input$map_prov_col]])) %>%
          dplyr::group_by(prov_norm) %>%
          dplyr::summarise(geometry = sf::st_union(sf::st_make_valid(geometry)),
                           .groups = "drop")
      }, error = function(e) {
        warning("Failed to create province outlines: ", e$message)
        NULL
      })
    })
    
    # Render map
    output$map_main <- leaflet::renderLeaflet({
      shp <- map_zones()
      summ <- zone_summary()
      
      if (is.null(shp) || is.null(summ)) {
        return(
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::setView(lng = 23, lat = -6, zoom = 6)
        )
      }
      
      shiny::req(input$map_zone_col, input$map_prov_col)
      
      # Normalize shapefile
      shp <- shp %>%
        dplyr::mutate(
          zone_norm = normalize_text(.data[[input$map_zone_col]]),
          prov_norm = normalize_text(.data[[input$map_prov_col]])
        )
      
      # Join summaries
      shp_data <- shp %>%
        dplyr::left_join(summ, by = c("zone_norm", "prov_norm"))
      
      # Get metric column
      metric_col <- switch(input$map_metric,
                           n = shp_data$n,
                           n_da = shp_data$n_da,
                           n_dp = shp_data$n_dp,
                           pct_female = shp_data$pct_female,
                           pcr_pos_rate = shp_data$pcr_pos_rate,
                           ielisa_pos_rate = shp_data$ielisa_pos_rate)
      
      # Color palette
      pal <- leaflet::colorBin("YlGnBu", domain = metric_col, 
                               bins = 5, na.color = "#cccccc")
      
      # Create labels
      labels <- sprintf(
        "<strong>%s</strong><br/>
        Province: %s<br/>
        <hr style='margin:3px 0'/>
        Samples: %s (DA: %s, DP: %s)<br/>
        Female: %.1f%%<br/>
        <hr style='margin:3px 0'/>
        <strong>Lab Results:</strong><br/>
        PCR: %s/%s positive (%.1f%%)<br/>
        iELISA: %s/%s positive (%.1f%%)<br/>
        <hr style='margin:3px 0'/>
        Last sample: %s<br/>
        <strong>Transport (median days):</strong><br/>
        Field→HS: %.1f | HS→LSD: %.1f | LSD→INRB: %.1f",
        shp_data[[input$map_zone_col]],
        shp_data[[input$map_prov_col]],
        scales::comma(shp_data$n),
        scales::comma(shp_data$n_da),
        scales::comma(shp_data$n_dp),
        shp_data$pct_female,
        scales::comma(shp_data$pcr_pos),
        scales::comma(shp_data$pcr_n),
        shp_data$pcr_pos_rate,
        scales::comma(shp_data$iel_pos),
        scales::comma(shp_data$iel_n),
        shp_data$ielisa_pos_rate,
        format(shp_data$last_sample, "%Y-%m-%d"),
        shp_data$med_field_hs,
        shp_data$med_hs_lsd,
        shp_data$med_lsd_inrb
      ) %>% lapply(htmltools::HTML)
      
      # Build map
      m <- leaflet::leaflet(shp_data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor = ~pal(metric_col),
          fillOpacity = 0.7,
          color = "#444",
          weight = 1,
          label = labels,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#000",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~metric_col,
          title = input$map_metric,
          position = "bottomright"
        )
      
      # Add province outlines if requested
      prov_outline <- province_outlines()
      if (!is.null(prov_outline)) {
        m <- m %>%
          leaflet::addPolylines(
            data = prov_outline,
            color = "#000",
            weight = 2,
            opacity = 0.8
          )
      }
      
      m
    })
    
    # Focus Kasaï button
    shiny::observeEvent(input$map_focus_kasai, {
      shp <- map_zones()
      shiny::req(shp, input$map_prov_col)
      
      shp_norm <- shp %>%
        dplyr::mutate(prov_norm = normalize_text(.data[[input$map_prov_col]]))
      
      kasai <- shp_norm %>%
        dplyr::filter(prov_norm %in% c("kasai oriental", "lomami"))
      
      if (nrow(kasai) > 0) {
        bbox <- sf::st_bbox(kasai)
        leaflet::leafletProxy("map_main") %>%
          leaflet::fitBounds(bbox["xmin"], bbox["ymin"], 
                             bbox["xmax"], bbox["ymax"])
      }
    })
    
    # Download handler
    output$map_dl_summary <- shiny::downloadHandler(
      filename = function() {
        paste0("zone_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        summ <- zone_summary()
        if (!is.null(summ)) {
          readr::write_csv(summ, file)
        }
      }
    )
    
    # Return zone summary for use by other modules
    return(list(
      zone_summary = zone_summary
    ))
  })
}
