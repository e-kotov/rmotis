#' Check for required GUI dependencies
#' @noRd
.gui_check_dependencies <- function() {
  required_pkgs <- c("shiny", "mapgl", "sf", "DT", "shinyjs")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required for the GUI but are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      ".
",
      "Please install them using install.packages(c(",
      paste(sprintf("'%s'", missing_pkgs), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
}

#' Manage MOTIS Server Lifecycle for GUI
#' @noRd
.gui_setup_motis_server <- function(input_motis, port) {
  server_process <- NULL
  kill_on_exit <- FALSE
  active_port <- port

  # 1. Existing Process Object passed
  if (inherits(input_motis, "motis_server") || inherits(input_motis, "process")) {
    if (!input_motis$is_alive()) {
      stop("The provided MOTIS server process is not running.", call. = FALSE)
    }
    
    # If it's a motis_server S3 object, it has the port attribute
    if (inherits(input_motis, "motis_server")) {
      active_port <- attr(input_motis, "motis_port")
    } else {
      # Raw process object? Check if port was manually supplied
      if (is.null(active_port)) {
        stop("When providing a raw process object, you must specify the 'port'.", call. = FALSE)
      }
    }
    debug_msg("Using provided MOTIS server process on port ", active_port)
    
  # 2. Path passed (Start temporary server)
  } else if (is.character(input_motis) && length(input_motis) == 1) {
    debug_msg("Starting temporary MOTIS server for directory: ", input_motis)
    
    # Auto-select port if null
    target_port <- if (is.null(active_port)) 8080L else as.integer(active_port)
    
    server_process <- motis_start_server(
      work_dir = input_motis,
      port = target_port,
      echo_cmd = FALSE
    )
    
    if (!server_process$is_alive()) {
      stop("Failed to start MOTIS server. Check config.", call. = FALSE)
    }
    active_port <- target_port
    kill_on_exit <- TRUE
    
  # 3. Auto-detect (Connect to existing)
  } else if (is.null(input_motis)) {
    servers <- motis_servers(include_all = TRUE)
    alive_servers <- servers[servers$alive, ]

    if (nrow(alive_servers) == 0) {
      stop(
        "No running MOTIS servers detected. ",
        "Start a server with `motis_start_server()` or provide a `work_dir` path.",
        call. = FALSE
      )
    } else {
      # If specific port requested
      if (!is.null(active_port)) {
        found <- alive_servers[alive_servers$port == active_port, ]
        if (nrow(found) == 0) stop("No running server found on port ", active_port)
        active_port <- found$port[1]
      } else {
        # Pick most recent
        most_recent <- alive_servers[nrow(alive_servers), ]
        active_port <- most_recent$port
        debug_msg("Connected to auto-detected MOTIS server on port ", active_port)
      }
    }
  }

  cleanup_fn <- function() {
    if (kill_on_exit && !is.null(server_process) && server_process$is_alive()) {
      debug_msg("Stopping temporary MOTIS server...")
      server_process$kill()
    }
  }

  list(
    active_port = active_port,
    server_process = server_process,
    cleanup_fn = cleanup_fn
  )
}

#' UI Resources (CSS/JS)
#' @noRd
.gui_ui_resources <- function() {
  css <- "
      html, body { height: 100%; margin: 0; overflow: hidden; }
      .container-fluid { height: 100%; display: flex; flex-direction: column; }
      #shiny-notification-panel { top: 70px; right: 10px; left: auto; bottom: auto; }
      
      .sidebar-layout { flex: 1; display: flex; overflow: hidden; min-height: 0; }
      .sidebar-panel { height: 100%; overflow-y: auto; padding: 15px; }
      .sidebar-panel h4:first-child { margin-top: 0; }
      .sidebar-panel h4 { margin-top: 20px; }
      
      .main-panel { 
        height: 100%; 
        display: flex; 
        flex-direction: column; 
        padding: 15px; 
        overflow: hidden;
      }
      
      .map-wrapper { position: relative; flex: 3; min-height: 300px; }
      #map { height: 100% !important; }
      
      .table-wrapper { 
        flex: 2; 
        overflow-y: auto; 
        margin-top: 10px; 
        border-top: 1px solid #eee;
        padding-top: 10px;
      }
      
      .route-stats-overlay {
        position: absolute;
        top: 10px;
        left: 10px;
        z-index: 1000;
        background: rgba(245, 245, 245, 0.9);
        padding: 8px 12px;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.15);
        pointer-events: none;
        display: flex;
        gap: 15px;
        font-size: 14px;
        border: 1px solid #ccc;
      }
      .stat-val { font-weight: bold; }

      .exec-time-overlay {
        position: absolute;
        top: 10px;
        right: 50px; /* Offset to avoid overlap with standard zoom/fullscreen controls */
        z-index: 1000;
        background: rgba(255, 255, 255, 0.8);
        padding: 2px 6px;
        border-radius: 4px;
        font-size: 11px;
        color: #777;
        pointer-events: none;
        border: 1px solid rgba(0,0,0,0.1);
      }
      
      .segments-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 5px;
      }
      
      /* Mobile/Responsive Styles */
      .hamburger-btn { display: none; margin-right: 10px; font-size: 20px; cursor: pointer; }
      
      @media (max-width: 768px) {
        .sidebar-panel {
          position: absolute;
          left: -100%;
          top: 50px; /* Below header */
          bottom: 0;
          width: 80% !important;
          z-index: 2000;
          transition: left 0.3s ease;
          box-shadow: 2px 0 5px rgba(0,0,0,0.2);
        }
        .sidebar-panel.show-sidebar {
          left: 0;
        }
        .main-panel {
          width: 100% !important;
        }
        .hamburger-btn {
          display: inline-block;
        }
        /* Overlay to close sidebar when clicking outside */
        .sidebar-overlay {
          display: none;
          position: fixed;
          top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(0,0,0,0.5);
          z-index: 1999;
        }
        .sidebar-overlay.show-overlay {
          display: block;
        }
        .route-stats-overlay {
          flex-direction: column;
          gap: 2px;
        }
      }
    "

  js <- "
    $(document).on('click', '#hamburger_btn', function() {
      $('.sidebar-panel').toggleClass('show-sidebar');
      $('.sidebar-overlay').toggleClass('show-overlay');
    });
    
    $(document).on('click', '.sidebar-overlay', function() {
      $('.sidebar-panel').removeClass('show-sidebar');
      $('.sidebar-overlay').removeClass('show-overlay');
    });

    function initializeMapListeners(mapId) {
      const mapElement = document.getElementById(mapId);
      if (!mapElement) return;

      const observer = new MutationObserver((mutations, obs) => {
        const map = mapElement.map;
        if (map) {
          // Disable default context menu to allow right-click
          map.getCanvas().addEventListener('contextmenu', (e) => e.preventDefault());

          map.on('contextmenu', (e) => {
            Shiny.setInputValue('js_right_click', {
              lng: e.lngLat.lng,
              lat: e.lngLat.lat,
              nonce: Math.random()
            });
          });

          let startMarker = null;
          let endMarker = null;

          Shiny.addCustomMessageHandler('updateMarker', function(message) {
            const lngLat = [message.lng, message.lat];
            const markerId = message.id;

            const createDragEndCallback = (id) => {
              return (marker) => {
                const coords = marker.getLngLat();
                Shiny.setInputValue('marker_dragged', {
                  id: id,
                  lng: coords.lng,
                  lat: coords.lat,
                  nonce: Math.random()
                });
              };
            };
            
            const createDragCallback = (id) => {
              return (marker) => {
                const coords = marker.getLngLat();
                Shiny.setInputValue('marker_moving', {
                  id: id,
                  lng: coords.lng,
                  lat: coords.lat,
                  nonce: Math.random()
                });
              };
            };

            if (markerId === 'start') {
              if (!startMarker) {
                startMarker = new maplibregl.Marker({ draggable: true, color: '#009E73' })
                  .setLngLat(lngLat)
                  .addTo(map);
                startMarker.on('dragend', () => createDragEndCallback('start')(startMarker));
                startMarker.on('drag', () => createDragCallback('start')(startMarker));
              } else {
                startMarker.setLngLat(lngLat);
              }
            } else if (markerId === 'end') {
              if (!endMarker) {
                endMarker = new maplibregl.Marker({ draggable: true, color: '#D55E00' })
                  .setLngLat(lngLat)
                  .addTo(map);
                endMarker.on('dragend', () => createDragEndCallback('end')(endMarker));
                endMarker.on('drag', () => createDragCallback('end')(endMarker));
              } else {
                endMarker.setLngLat(lngLat);
              }
            }
          });
          
          Shiny.addCustomMessageHandler('clearAllMarkers', function(message) {
              if(startMarker) {
                  startMarker.remove();
                  startMarker = null;
              }
              if(endMarker) {
                  endMarker.remove();
                  endMarker = null;
              }
          });

          obs.disconnect();
        }
      });

      observer.observe(mapElement, { childList: true, subtree: true });
    }

    $(document).on('shiny:connected', () => {
      initializeMapListeners('map');
    });
  "
  list(css = css, js = js)
}

#' Construct Main UI
#' @noRd
.gui_ui_layout <- function() {
  res <- .gui_ui_resources()

  shiny::fluidPage(
    shinyjs::useShinyjs(),
    # Header
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0; flex-wrap: wrap; gap: 10px;",
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::HTML(
          '<div id="hamburger_btn" class="hamburger-btn">&#9776;</div>'
        ),
        shiny::h3(
          shiny::HTML("<b>rmotis</b> GUI"),
          style = "margin: 0;"
        )
      ),
      shiny::div(
        style = "display: flex; gap: 10px; align-items: center;",
        shiny::uiOutput("autozoom_button_ui", inline = TRUE),
        shiny::actionButton(
          "quit_app",
          "Quit",
          style = "background-color: #d9534f; color: white; border-width: 0px;"
        )
      )
    ),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(res$css)),
      shiny::tags$script(shiny::HTML(res$js))
    ),

    shiny::div(
      class = "sidebar-overlay"
    ),

    shiny::div(
      class = "sidebar-layout",
      shiny::div(
        class = "sidebar-panel",
        style = "width: 25%;",
        shiny::h4("Locations"),
        shiny::helpText(
          "Left-click map: Start point",
          shiny::tags$br(),
          "Right-click map: End point",
          shiny::tags$br(),
          "Drag markers to adjust."
        ),
        shiny::actionButton(
          "reset",
          "Reset / Clear",
          style = "width: 100%; margin-bottom: 10px;"
        ),

        shiny::textInput(
          "start_coords_input",
          "Start (Lat, Lon)",
          placeholder = "52.52, 13.40"
        ),
        shiny::textInput(
          "end_coords_input",
          "End (Lat, Lon)",
          placeholder = "52.53, 13.41"
        ),

        shiny::hr(),
        shiny::h4("Street Routing"),
        shiny::numericInput("max_travel_time", "Max Travel Time (min)", value = 360, min = 1),
        shiny::numericInput("max_direct_time", "Max Direct Time (min)", value = 180, min = 1),
        shiny::numericInput("max_matching_dist", "Max Matching Dist (m)", value = 250, min = 10),
        shiny::sliderInput("fastest_direct_factor", "Fastest Direct Factor", min = 1, max = 5, value = 1.5, step = 0.1),
        
        shiny::hr(),
        shiny::h4("Time & Mode"),
        shiny::splitLayout(
          shiny::dateInput("date", "Date", value = Sys.Date()),
          shiny::textInput("time", "Time (HH:MM)", value = format(Sys.time(), "%H:%M"))
        ),
        shiny::checkboxInput("arrive_by", "Arrive By", value = FALSE),
        
        shiny::selectInput("direct_mode", "Direct Mode", 
                           choices = c("WALK", "BIKE", "CAR"), selected = "CAR"),
        
        shiny::checkboxGroupInput("transit_modes", "Transit Modes",
                                  choices = c("Train" = "RAIL", "Bus" = "BUS", "Tram" = "TRAM", "Subway" = "SUBWAY"),
                                  selected = c("RAIL", "BUS", "TRAM", "SUBWAY"),
                                  inline = TRUE),
        
        shiny::hr(),
        shiny::h4("Advanced Options"),
        shiny::checkboxInput("with_fares", "Include Fares", value = FALSE),
        shiny::checkboxInput("join_interlined", "Join Interlined Legs", value = FALSE),
        shiny::numericInput("max_transfers", "Max Transfers", value = 5, min = 0),
        shiny::checkboxInput("timetable_view", "Timetable View (Range)", value = TRUE),
        shiny::numericInput("num_itineraries", "Max Itineraries", value = 3, min = 1, max = 10)
      ),

      shiny::div(
        class = "main-panel",
        style = "width: 75%;",
        shiny::div(
          class = "map-wrapper",
          shiny::uiOutput("route_stats"),
          shiny::uiOutput("exec_time_overlay"),
          mapgl::maplibreOutput("map")
        ),
        shiny::div(
          class = "table-wrapper",
          shiny::div(
            class = "segments-header",
            shiny::h4("Itineraries", style = "margin: 0;")
          ),
          DT::dataTableOutput("itinerary_table")
        )
      )
    )
  )
}
