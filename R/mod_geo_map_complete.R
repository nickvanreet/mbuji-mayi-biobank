# mod_geo_map_complete.R
# Geography module with health zone choropleth and lab results integration
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
      
      shiny::h6("Map Data Source"),
      shiny::checkboxInput(ns("map_use_online"), "Try GRID3 online first", value = FALSE),
      shiny::fileInput(ns("map_upload"), "Or upload shapefile/GeoPackage",
                       accept = c(".geojson", ".json", ".gpkg", ".shp", ".zip")),
      
      shiny::textInput(ns("local_gpkg_path"), "Or local .gpkg path",
                       placeholder = "testdata/your_file.gpkg"),
      
      shiny::actionButton(ns("map_load"), "Load Map Data", 
                          class = "btn-primary w-100 mb-2"),
      
      shiny::div(class = "mt-2", shiny::textOutput(ns("map_status"))),
      
      shiny::hr(),
      
      shiny::h6("Column Mapping"),
      shiny::selectInput(ns("map_zone_col"), "Zone column", choices = NULL),
      shiny::selectInput(ns("map_prov_col"), "Province column", choices = NULL),
      
      shiny::hr(),
      
      shiny::h6("Display Options"),
      shiny::selectInput(ns("map_metric"), "Color by",
                         choices = c(
                           "Sample count" = "n",
                           "DA samples" = "n_da",
                           "DP samples" = "n_dp",
                           "% Female" = "pct_female",
                           "PCR positivity %" = "pcr_pos_rate",
                           "ELISA PE positivity %" = "elisa_pe_pos_rate",
                           "ELISA VSG positivity %" = "elisa_vsg_pos_rate",
                           "iELISA positivity %" = "ielisa_pos_rate",
                           "Any test positivity %" = "any_pos_rate"
                         ),
                         selected = "n"),
      
      shiny::checkboxInput(ns("map_show_provinces"), "Show province outlines", 
                           value = TRUE),
      
      shiny::hr(),
      
      shiny::actionButton(ns("map_focus_kasai"), "Focus Kasaï Region", 
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
#' @param lab_joined Reactive containing joined lab results (optional)
#' @param config Reactive containing app configuration
#' @export
mod_geo_map_server <- function(id, biobank_filtered, lab_joined = NULL, config) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    shapefile_data <- shiny::reactiveVal(NULL)
    map_status <- shiny::reactiveVal("No map data loaded")
    
    # Initialize settings from config
    shiny::observe({
      cfg <- config()
      if (!is.null(cfg) && "map" %in% names(cfg)) {
        shiny::updateCheckboxInput(session, "map_use_online", 
                                    value = isTRUE(cfg$map$use_grid3_online))
        
        # Set local gpkg path if available
        if (!is.null(cfg$map$fallback_shapefile) && 
            nzchar(cfg$map$fallback_shapefile)) {
          shiny::updateTextInput(session, "local_gpkg_path",
                                 value = cfg$map$fallback_shapefile)
        }
      }
    })
    
    # Load shapefile/geopackage
# In mod_geo_map_complete.R, simplify the load logic:

shiny::observeEvent(input$map_load, {
  shiny::withProgress(message = 'Loading map data...', value = 0, {
    
    shp <- NULL
    error_msg <- NULL
    
    # REMOVE: Online GRID3 attempt
    # Just try uploaded file or local file
    
    # Try uploaded file
    if (!is.null(input$map_upload)) {
      shiny::incProgress(0.5, detail = "Reading uploaded file...")
      shp <- tryCatch({
        path <- input$map_upload$datapath
        sf_data <- sf::read_sf(path, quiet = TRUE)
        sf_data <- sf::st_make_valid(sf_data)
        map_status(paste("✓ Loaded:", input$map_upload$name))
        sf_data
      }, error = function(e) {
        error_msg <<- paste("Upload failed:", e$message)
        NULL
      })
    }
    
    # Try local path
    if (is.null(shp) && !is.null(input$local_gpkg_path) && 
        nzchar(trimws(input$local_gpkg_path))) {
      shiny::incProgress(0.5, detail = "Reading local file...")
      local_path <- safe_path(trimws(input$local_gpkg_path))
      
      shp <- tryCatch({
        if (file.exists(local_path)) {
          sf_data <- sf::read_sf(local_path, quiet = TRUE)
          sf_data <- sf::st_make_valid(sf_data)
          map_status(paste("✓ Loaded:", basename(local_path)))
          sf_data
        } else {
          error_msg <<- paste("File not found:", local_path)
          NULL
        }
      }, error = function(e) {
        error_msg <<- paste("Local file failed:", e$message)
        NULL
      })
    }
    
    # Final status
    if (is.null(shp)) {
      map_status(paste("⚠️ Failed to load map data.", error_msg))
      shiny::showNotification(error_msg, type = "error", duration = 10)
    } else {
      shapefile_data(shp)
      shiny::showNotification(
        sprintf("Map loaded: %s features", scales::comma(nrow(shp))),
        type = "message"
      )
    }
  })
})
    
    # Output map status
    output$map_status <- shiny::renderText({
      map_status()
    })
    
    # Update column selectors
    shiny::observe({
      shp <- shapefile_data()
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
        if (length(cols) > 1) cols[2] else cols[1]
      }
      
      shiny::updateSelectInput(session, "map_zone_col", 
                               choices = cols, selected = zone_col)
      shiny::updateSelectInput(session, "map_prov_col", 
                               choices = cols, selected = prov_col)
    })
    
    # Compute zone summaries with lab results integration
    zone_summary <- shiny::reactive({
      bio <- biobank_filtered()
      shiny::req(bio, nrow(bio) > 0)
      
      # Check if lab results are available
      lab <- NULL
      if (!is.null(lab_joined)) {
        lab <- tryCatch({
          lab_joined()
        }, error = function(e) {
          message("Lab results not available: ", e$message)
          NULL
        })
      }
      
      # Join lab results if available
      if (!is.null(lab) && is.data.frame(lab) && nrow(lab) > 0) {
        bio <- bio %>%
          dplyr::left_join(
            lab %>% dplyr::select(
              barcode, lab_id,
              dplyr::any_of(c(
                "PCR_pos", "ELISA_PE_pos", "ELISA_VSG_pos", 
                "iELISA_pos", "Any_pos"
              ))
            ),
            by = c("barcode", "lab_id")
          )
      } else {
        # Add dummy columns if no lab data
        bio <- bio %>%
          dplyr::mutate(
            PCR_pos = NA,
            ELISA_PE_pos = NA,
            ELISA_VSG_pos = NA,
            iELISA_pos = NA,
            Any_pos = NA
          )
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
          pcr_n = sum(!is.na(PCR_pos)),
          pcr_pos = sum(PCR_pos %in% TRUE, na.rm = TRUE),
          pcr_pos_rate = if (pcr_n > 0) (pcr_pos / pcr_n * 100) else NA_real_,
          
          # ELISA PE metrics
          elisa_pe_n = sum(!is.na(ELISA_PE_pos)),
          elisa_pe_pos = sum(ELISA_PE_pos %in% TRUE, na.rm = TRUE),
          elisa_pe_pos_rate = if (elisa_pe_n > 0) (elisa_pe_pos / elisa_pe_n * 100) else NA_real_,
          
          # ELISA VSG metrics
          elisa_vsg_n = sum(!is.na(ELISA_VSG_pos)),
          elisa_vsg_pos = sum(ELISA_VSG_pos %in% TRUE, na.rm = TRUE),
          elisa_vsg_pos_rate = if (elisa_vsg_n > 0) (elisa_vsg_pos / elisa_vsg_n * 100) else NA_real_,
          
          # iELISA metrics
          iel_n = sum(!is.na(iELISA_pos)),
          iel_pos = sum(iELISA_pos %in% TRUE, na.rm = TRUE),
          ielisa_pos_rate = if (iel_n > 0) (iel_pos / iel_n * 100) else NA_real_,
          
          # Any positivity
          any_tested = sum(!is.na(Any_pos)),
          any_pos = sum(Any_pos %in% TRUE, na.rm = TRUE),
          any_pos_rate = if (any_tested > 0) (any_pos / any_tested * 100) else NA_real_,
          
          # Latest sample
          last_sample = max(date_sample, na.rm = TRUE),
          
          # Transport medians (if available)
          med_field_hs = if ("transport_field_hs" %in% names(.)) {
            stats::median(transport_field_hs, na.rm = TRUE)
          } else NA_real_,
          med_hs_lsd = if ("transport_hs_lsd" %in% names(.)) {
            stats::median(transport_hs_lsd, na.rm = TRUE)
          } else NA_real_,
          med_lsd_inrb = if ("transport_lsd_inrb" %in% names(.)) {
            stats::median(transport_lsd_inrb, na.rm = TRUE)
          } else NA_real_,
          
          .groups = "drop"
        )
    })
    
    # Build province outlines
    province_outlines <- shiny::reactive({
      shp <- shapefile_data()
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
      shp <- shapefile_data()
      summ <- zone_summary()
      
      if (is.null(shp)) {
        # Show empty map with instructions
        return(
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::setView(lng = 23, lat = -6, zoom = 6) %>%
            leaflet::addControl(
              html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
                        <strong>No map data loaded</strong><br/>
                        Click 'Load Map Data' in settings
                      </div>",
              position = "topright"
            )
        )
      }
      
      if (is.null(summ) || !nrow(summ)) {
        # Show map without data overlay
        return(
          leaflet::leaflet(shp) %>%
            leaflet::addTiles() %>%
            leaflet::addPolygons(
              fillColor = "gray",
              fillOpacity = 0.3,
              color = "#444",
              weight = 1
            ) %>%
            leaflet::addControl(
              html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
                        <strong>No biobank data available</strong><br/>
                        Load biobank data first
                      </div>",
              position = "topright"
            )
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
                           elisa_pe_pos_rate = shp_data$elisa_pe_pos_rate,
                           elisa_vsg_pos_rate = shp_data$elisa_vsg_pos_rate,
                           ielisa_pos_rate = shp_data$ielisa_pos_rate,
                           any_pos_rate = shp_data$any_pos_rate)
      
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
        ELISA PE: %s/%s positive (%.1f%%)<br/>
        ELISA VSG: %s/%s positive (%.1f%%)<br/>
        iELISA: %s/%s positive (%.1f%%)<br/>
        Any test positive: %s/%s (%.1f%%)<br/>
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
        scales::comma(shp_data$elisa_pe_pos),
        scales::comma(shp_data$elisa_pe_n),
        shp_data$elisa_pe_pos_rate,
        scales::comma(shp_data$elisa_vsg_pos),
        scales::comma(shp_data$elisa_vsg_n),
        shp_data$elisa_vsg_pos_rate,
        scales::comma(shp_data$iel_pos),
        scales::comma(shp_data$iel_n),
        shp_data$ielisa_pos_rate,
        scales::comma(shp_data$any_pos),
        scales::comma(shp_data$any_tested),
        shp_data$any_pos_rate,
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
      if (!is.null(prov_outline) && nrow(prov_outline) > 0) {
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
      shp <- shapefile_data()
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
      } else {
        shiny::showNotification("Kasaï region not found in map data", 
                                type = "warning")
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
