library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(dplyr)
library(DT)

geojson_dir <- "geojson"

carriles <- st_read(file.path(geojson_dir, "carrilesbici.geojson"), quiet = TRUE) %>%
  st_transform(4326)

stations <- st_read(file.path(geojson_dir, "valenbisi_stations.geojson"), quiet = TRUE) %>%
  st_transform(4326)

grid <- st_read(file.path(geojson_dir, "valencia_grid.geojson"), quiet = TRUE) %>%
  st_transform(4326)

grid$cell_id <- seq_len(nrow(grid))
grid_centroids <- suppressWarnings(st_centroid(grid))
grid_centroids$cell_id <- grid$cell_id

stations$station_id <- paste0("st_", seq_len(nrow(stations)))

bbox <- st_bbox(grid)

pal_pop <- colorNumeric("YlOrRd", domain = grid$Z, na.color = "transparent")

ui <- page_sidebar(
  title = "Valencia — Valenbisi & Cobertura",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    title = "Configuración",
    selectInput("map_style", "Estilo del mapa:",
                choices = c("OpenStreetMap" = "OpenStreetMap",
                            "Satélite"      = "Esri.WorldImagery",
                            "Minimalista"   = "CartoDB.Positron")),
    hr(),
    checkboxGroupInput("layers", "Capas visibles:",
                       choices  = c("Población (grid)" = "grid",
                                    "Carriles bici"    = "carriles",
                                    "Estaciones"       = "stations"),
                       selected = c("grid", "carriles", "stations")),
    hr(),
    radioButtons("station_color", "Color de estaciones:",
                 choices  = c("Uniforme" = "uniform",
                              "Por ocupación" = "ocupacion"),
                 selected = "uniform"),
    hr(),
    selectInput("grid_metric", "Color del grid según:",
                choices = c("Población (Z)"     = "Z",
                            "Cobertura buffer"  = "buffer_cov",
                            "Metros de carril"  = "car_cov")),
    hr(),
    p(tags$small("Haz clic en una estación o en un punto del grid para ver su información debajo."))
  ),

  layout_column_wrap(
    width = 1,
    card(
      card_header("Mapa de Valencia"),
      leafletOutput("mapa", height = "550px")
    ),
    card(
      card_header(uiOutput("info_header")),
      DTOutput("info_table")
    )
  )
)

server <- function(input, output, session) {

  selected <- reactiveVal(NULL)

  pal_grid <- reactive({
    metric <- input$grid_metric
    colorNumeric("YlOrRd", domain = grid[[metric]], na.color = "transparent")
  })

  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })

  observeEvent(input$map_style, {
    leafletProxy("mapa") %>%
      clearTiles() %>%
      addProviderTiles(input$map_style)
  })

  observe({
    proxy <- leafletProxy("mapa")
    proxy %>% clearGroup("grid") %>% clearGroup("grid_pts") %>%
      clearGroup("carriles") %>% clearGroup("stations") %>%
      removeControl("legend")

    if ("grid" %in% input$layers) {
      metric <- input$grid_metric
      pal <- pal_grid()
      vals <- grid[[metric]]

      proxy %>%
        addPolygons(
          data        = grid,
          layerId     = ~paste0("grid_", cell_id),
          group       = "grid",
          fillColor   = ~pal(vals),
          fillOpacity = 0.55,
          weight      = 0.4,
          color       = "#555",
          label       = ~sprintf("Pob: %.0f | buffer: %.2f | carril: %.0f m",
                                 Z, buffer_cov, car_cov)
        ) %>%
        addCircleMarkers(
          data        = grid_centroids,
          layerId     = ~paste0("gridpt_", cell_id),
          group       = "grid_pts",
          radius      = 3,
          color       = "#222",
          fillColor   = "#222",
          fillOpacity = 0.6,
          weight      = 1
        ) %>%
        addLegend("bottomright", pal = pal, values = vals,
                 title = metric, layerId = "legend", opacity = 0.8)
    }

    if ("carriles" %in% input$layers) {
      proxy %>% addPolylines(
        data    = carriles,
        group   = "carriles",
        color   = "#1f9d55",
        weight  = 2,
        opacity = 0.8
      )
    }

    if ("stations" %in% input$layers) {
      pal_st <- colorNumeric("RdYlGn", domain = c(0, 1), reverse = TRUE, na.color = "#aaaaaa")
      fill_colors <- if (input$station_color == "ocupacion") {
        pal_st(stations$ocupacion)
      } else {
        "#3498db"
      }
      proxy %>% addCircleMarkers(
        data        = stations,
        layerId     = ~station_id,
        group       = "stations",
        radius      = 6,
        color       = "#1c3d5a",
        fillColor   = fill_colors,
        fillOpacity = 0.85,
        weight      = 1.5,
        label       = ~address,
        popup       = ~sprintf("<b>%s</b><br>Disponibles: %s / %s<br>Ocupación: %s",
                               address,
                               ifelse(is.na(available), "?", available),
                               ifelse(is.na(total), "?", total),
                               ifelse(is.na(estado), "—", estado))
      )
      if (input$station_color == "ocupacion") {
        proxy %>% addLegend("bottomleft", pal = pal_st, values = c(0, 1),
                            title = "Ocupación", layerId = "legend_st",
                            labFormat = labelFormat(suffix = ""),
                            opacity = 0.8)
      } else {
        proxy %>% removeControl("legend_st")
      }
    } else {
      proxy %>% removeControl("legend_st")
    }
  })

  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click
    id <- click$id
    if (is.null(id)) return()
    if (startsWith(id, "gridpt_")) {
      selected(list(type = "grid", id = as.integer(sub("gridpt_", "", id))))
    } else {
      selected(list(type = "station", id = id))
    }
  })

  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    id <- click$id
    if (is.null(id)) return()
    if (startsWith(id, "grid_")) {
      selected(list(type = "grid", id = as.integer(sub("grid_", "", id))))
    }
  })

  output$info_header <- renderUI({
    sel <- selected()
    if (is.null(sel)) return("Información — selecciona algo en el mapa")
    if (sel$type == "station") {
      row <- stations[stations$station_id == sel$id, ]
      paste0("Estación: ", row$address)
    } else {
      paste0("Celda del grid #", sel$id)
    }
  })

  output$info_table <- renderDT({
    sel <- selected()
    if (is.null(sel)) {
      return(datatable(data.frame(Mensaje = "Haz clic en una estación o celda del grid."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    if (sel$type == "station") {
      row <- stations[stations$station_id == sel$id, ]
      df <- st_drop_geometry(row) %>%
        select(any_of(c("name", "address", "number", "open",
                        "available", "free", "total",
                        "ocupacion", "estado", "updated_at")))
      df_long <- data.frame(Campo = names(df), Valor = as.character(unlist(df[1, ])))
      datatable(df_long, options = list(dom = "t", pageLength = -1), rownames = FALSE)
    } else {
      row <- grid[grid$cell_id == sel$id, ]
      df <- st_drop_geometry(row) %>% select(X, Y, Z, buffer_cov, car_cov)
      df_long <- data.frame(
        Campo = c("Longitud (X)", "Latitud (Y)", "Población (Z)",
                  "Cobertura buffer estaciones", "Metros de carril bici"),
        Valor = c(sprintf("%.5f", df$X),
                  sprintf("%.5f", df$Y),
                  sprintf("%.0f", df$Z),
                  sprintf("%.3f", df$buffer_cov),
                  sprintf("%.2f", df$car_cov))
      )
      datatable(df_long, options = list(dom = "t", pageLength = -1), rownames = FALSE)
    }
  })
}

shinyApp(ui, server)
