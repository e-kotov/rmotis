#' Launch a GUI to View and Debug MOTIS Routing
#'
#' Launches a lightweight Shiny application to interactively visualize point-to-point
#' routing using a running MOTIS server. Supports left-click for start, right-click for end.
#'
#' @param input_motis Optional. Can be:
#'   \itemize{
#'     \item A `motis_server` object (process) returned by `motis_start_server()`.
#'     \item A path string to a MOTIS working directory (contains `config.yml` and `data/`).
#'     \item `NULL` (default): Auto-detects a running MOTIS server using `motis_servers()`.
#'   }
#' @param port Integer. The port the server is running on. If `NULL` (default), attempts to
#'   auto-detect from the server object or registry.
#' @param style Character. Map style for `mapgl`. Defaults to Carto Voyager.
#' @param center Numeric vector `c(lng, lat)`. Initial map center.
#' @param zoom Numeric. Initial zoom level.
#' @return No return value; launches a Shiny Gadget.
#' @export
motis_gui <- function(
  input_motis = NULL,
  port = NULL,
  style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  center = c(13.40, 52.52), # Default to Berlin
  zoom = 10
) {
  # 1. Check Dependencies
  .gui_check_dependencies()

  # 2. Manage Server Context
  srv_context <- .gui_setup_motis_server(input_motis, port)
  on.exit(srv_context$cleanup_fn(), add = TRUE)
  
  old_opts <- options(
    rmotis.url = paste0("http://127.0.0.1:", srv_context$active_port)
  )
  on.exit(options(old_opts), add = TRUE)

  # 3. UI Definition
  ui <- .gui_ui_layout()

  # 4. Server Logic
  server <- function(input, output, session) {
    
    # -- Map Initialization --
    output$map <- mapgl::renderMaplibre({
      mapgl::maplibre(
        style = style,
        center = center,
        zoom = zoom
      ) |>
        mapgl::add_navigation_control() |>
        mapgl::add_fullscreen_control() |>
        mapgl::add_scale_control()
    })
    
    # -- State --
    locations <- shiny::reactiveValues(
      start = NULL,
      end = NULL
    )
    
    init <- shiny::reactiveValues(
      route = FALSE
    )
    
    autozoom_enabled <- shiny::reactiveVal(TRUE)
    itineraries_res <- shiny::reactiveVal(NULL)
    motis_exec_time <- shiny::reactiveVal(NULL)
    
    # --- UI Helpers ---
    output$autozoom_button_ui <- shiny::renderUI({
      state <- autozoom_enabled()
      label <- if (state) "Autozoom: ON" else "Autozoom: OFF"
      color <- if (state) "#5cb85c" else "#777"
      shiny::actionButton(
        "toggle_autozoom",
        label,
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })
    
    shiny::observeEvent(input$toggle_autozoom, {
      autozoom_enabled(!autozoom_enabled())
    })

    # --- Marker Helpers ---
    update_start <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$start <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'start', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "start_coords_input",
        value = paste(coords$lat, coords$lng, sep = ", ")
      )
    }

    update_end <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$end <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'end', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "end_coords_input",
        value = paste(coords$lat, coords$lng, sep = ", ")
      )
    }

    # --- Interaction Handlers ---
    shiny::observeEvent(input$map_click, {
      shiny::req(input$map_click)
      update_start(input$map_click$lng, input$map_click$lat)
    })

    shiny::observeEvent(input$js_right_click, {
      shiny::req(input$js_right_click)
      update_end(input$js_right_click$lng, input$js_right_click$lat)
    })

    # Marker Dragged
    shiny::observeEvent(input$marker_dragged, {
      drag <- input$marker_dragged
      shiny::req(drag)
      if (drag$id == "start") {
        update_start(drag$lng, drag$lat)
      } else if (drag$id == "end") {
        update_end(drag$lng, drag$lat)
      }
    })

    # Manual Input Observers
    shiny::observeEvent(input$start_coords_input, {
      parts <- as.numeric(unlist(strsplit(input$start_coords_input, ",")))
      if (length(parts) == 2) {
        if (is.null(locations$start) || 
            !isTRUE(all.equal(as.numeric(c(locations$start$lat, locations$start$lng)), parts))) {
          update_start(parts[2], parts[1])
        }
      }
    })
    
    shiny::observeEvent(input$end_coords_input, {
      parts <- as.numeric(unlist(strsplit(input$end_coords_input, ",")))
      if (length(parts) == 2) {
        if (is.null(locations$end) || 
            !isTRUE(all.equal(as.numeric(c(locations$end$lat, locations$end$lng)), parts))) {
          update_end(parts[2], parts[1])
        }
      }
    })
    
    # -- Calculation --
    shiny::observeEvent(
      list(locations$start, locations$end, input$date, input$time, 
           input$direct_mode, input$transit_modes, input$timetable_view, 
           input$num_itineraries, input$arrive_by), {
      
      shiny::req(locations$start, locations$end)
      
      shiny::withProgress(message = "Routing...", {
        tryCatch({
          t0 <- Sys.time()
          res <- motis_plan(
            from = sprintf("%.6f,%.6f", locations$start$lat, locations$start$lng),
            to = sprintf("%.6f,%.6f", locations$end$lat, locations$end$lng),
            time = as.POSIXct(paste(input$date, input$time), tz = "UTC"),
            arrive_by = input$arrive_by,
            timetableView = input$timetable_view,
            numItineraries = input$num_itineraries,
            transitModes = input$transit_modes,
            directModes = input$direct_mode,
            output = "itineraries"
          )
          motis_exec_time(as.numeric(difftime(Sys.time(), t0, units = "secs")))
          
          itineraries_res(res)
          
          proxy <- mapgl::maplibre_proxy("map")
          
          if (!is.null(res) && nrow(res) > 0) {
            # Show only the first (best) itinerary on the map by default
            best_route <- res[1, ]
            
            if (!init$route) {
              mapgl::add_source(proxy, "route_source", best_route)
              mapgl::add_line_layer(proxy, "route_layer", "route_source", 
                                    line_color = "#3b82f6", line_width = 5, line_opacity = 0.8)
              init$route <- TRUE
            } else {
              mapgl::set_source(proxy, "route_layer", best_route)
              mapgl::set_layout_property(proxy, "route_layer", "visibility", "visible")
            }
            
            if (autozoom_enabled()) {
              bbox <- sf::st_bbox(res)
              mapgl::fit_bounds(proxy, bbox, animate = TRUE, padding = 100)
            }
          } else {
            shiny::showNotification("No routes found.", type = "warning")
            if (init$route) {
              mapgl::set_layout_property(proxy, "route_layer", "visibility", "none")
            }
          }
          
        }, error = function(e) {
          shiny::showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })

    # Highlighting selected itinerary
    shiny::observeEvent(input$itinerary_table_rows_selected, {
      proxy <- mapgl::maplibre_proxy("map")
      res <- itineraries_res()
      idx <- input$itinerary_table_rows_selected
      
      if (is.null(idx) || length(idx) == 0 || is.null(res)) {
        # Default back to the first one or clear if none
        if (!is.null(res) && nrow(res) > 0 && init$route) {
           mapgl::set_source(proxy, "route_layer", res[1, ])
        }
        return()
      }
      
      if (init$route) {
        selected_route <- res[idx[1], ]
        mapgl::set_source(proxy, "route_layer", selected_route)
      }
    })
    
    # -- Table & Stats --
    output$route_stats <- shiny::renderUI({
      res <- itineraries_res()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      
      idx <- input$itinerary_table_rows_selected
      if (is.null(idx) || length(idx) == 0) idx <- 1
      
      best <- res[idx[1], ]
      dur <- round(best$duration / 60)
      transfers <- best$transfers
      
      shiny::div(
        class = "route-stats-overlay",
        shiny::div(
          shiny::tags$b("Duration: ", style = "color: #3b82f6;"),
          shiny::span(paste(dur, "min"), class = "stat-val")
        ),
        shiny::div(
          shiny::tags$b("Transfers: ", style = "color: #3b82f6;"),
          shiny::span(transfers, class = "stat-val")
        )
      )
    })
    
    output$itinerary_table <- DT::renderDataTable({
      res <- itineraries_res()
      shiny::req(res)
      
      df <- as.data.frame(res)
      df <- df[, c("startTime", "endTime", "duration", "transfers")]
      df$duration <- round(df$duration / 60, 1)
      names(df) <- c("Start", "End", "Duration (min)", "Transfers")
      
      DT::datatable(
        df, 
        selection = 'single',
        options = list(pageLength = 5, dom = 'tp'),
        rownames = FALSE
      )
    })
    
    output$exec_time_overlay <- shiny::renderUI({
      et <- motis_exec_time()
      if (is.null(et)) return(NULL)
      shiny::div(
        class = "exec-time-overlay",
        shiny::div("MOTIS time:"),
        shiny::div(paste0(round(et * 1000, 0), "ms"), style = "font-weight: bold; color: #333;")
      )
    })

    # -- Reset --
    shiny::observeEvent(input$reset, {
      locations$start <- NULL
      locations$end <- NULL
      itineraries_res(NULL)
      motis_exec_time(NULL)
      shiny::updateTextInput(session, "start_coords_input", value = "")
      shiny::updateTextInput(session, "end_coords_input", value = "")
      
      proxy <- mapgl::maplibre_proxy("map")
      if (init$route) {
        mapgl::set_layout_property(proxy, "route_layer", "visibility", "none")
      }
      session$sendCustomMessage("clearAllMarkers", "clear")
    })

    shiny::observeEvent(input$quit_app, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
