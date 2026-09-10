# app.R
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(reshape2)
library(scales)
library(RColorBrewer)
library(shinyWidgets)
library(heatmaply)
#library(plotlyheatmaply)
#install.packages("plotlyheatmaply")

# ---------- Data: embedded from user's table ----------
station_df <- tribble(
  ~No., ~parameter, ~haz_B15, ~haz_A15, ~haz_Reservoir, ~haz_Source, ~hel_POND_2, ~helPOND_9, ~mr_SOURCE_1, ~ps_SOURCE_2,
  1, "Alkalinity (CaCO3) (mg/l)", 3, 10, 10, 10, 8, 14, 13, 13,
  2, "Colour (TRUE) (Pt/Co)", 24, 26, 15, 24, 14, 26, 16, 16,
  3, "pH", 7.67, 7.58, 7.8, 7.01, 7.8, 7.9, 7.64, 8.82,
  4, "Temperature (Deg C)", 30.24, 29.26, 32.42, 31.42, 29, 30, 30, 31,
  5, "Total suspended solids (TSS)", 10, 12, 8, 6, 6, 6, 2, 2,
  6, "Turbidity (NTU)", 7.7, 13.5, 14.2, 3.8, 11.1, 16.6, 4.6, 13.7,
  7, "Nitrate (NO3) (mg/l)", 2, 102, 8, 3, 4, 6, 1, 1.1,
  8, "Total Phosphate (PO4) (mg/l)", 6.68, 6.93, 5, 0.4, 2.12, 1.8, 0.14, 0.16,
  9, "Total Ammonia (NH3) (mg/l)", 0.19, 1.5, 0.12, 0.15, 0.12, 0.14, 0.17, 0.14,
  10, "BOD (mg/l)", 0.34, 0.55, 0.63, 0.52, 0.54, 1.03, 0.46, 0.37,
  11, "E. Coli (cfu/100 ml)", 0, 0, 0, 0, 0, 0, 0, 0,
  12, "oil and grease (mg/l)", 0.034, 0.053, 0.102, 0.147, 0.058, 0.048, 0.055, 0.036,
  13, "Total Chlorine (mg/l)", 0.14, 0.26, 0.21, 0.09, 0.19, 0.16, 0.05, 0.08,
  14, "CO2 (mg/l)", 3, 10, 10, 13, 8, 14, NA, 13,
  15, "amonia (mg/l)", 0.19, 0.12, 0.15, 0.17, 0.12, 0.14, NA, 0.14,
  16, "DO (mg/l)", 0.34, 0.63, 0.52, 0.46, 0.54, 1.03, NA, 0.37
) %>%
  rename(index = `No.`)

# ---------- Optimal reference ranges (example thresholds) ----------
# These are conservative example thresholds drawn from aquaculture/water quality guidance (see references).
# Values below/above target flagged in the app. Adjust to species-specific values as needed.

optimal_ranges <- tibble(
  parameter = c("Alkalinity (CaCO3) (mg/l)", "Colour (TRUE) (Pt/Co)", "pH",
                "Temperature (Deg C)", "Total suspended solids (TSS)", "Turbidity (NTU)",
                "Nitrate (NO3) (mg/l)", "Total Phosphate (PO4) (mg/l)", "Total Ammonia (NH3) (mg/l)",
                "BOD (mg/l)", "E. Coli (cfu/100 ml)", "oil and grease (mg/l)",
                "Total Chlorine (mg/l)", "CO2 (mg/l)", "amonia (mg/l)", "DO (mg/l)"),
  # For most parameters we give min and max; NA where not applicable
  min = c(20, NA, 6.5, 20, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, 5),
  max = c(200, NA, 8.5, 32, 50, 25, 3.0, 0.5, 0.05, 10, 1000, 0.2, 0.5, NA, 0.05, 8)
) 

# Notes: these example ranges reference FAO and Global Aquaculture Alliance and regional guidance.
# DO recommended commonly >5 mg/L for many culture species; ammonia (un-ionised) safe thresholds vary by species (FAO); total phosphorus targets for aquaculture effluents often <0.5 mg/L (GAA).

# ---------- Utility functions ----------
# Convert station data to long form
get_long <- function(df) {
  df %>%
    gather(key = "station", value = "value", -index, -parameter) %>%
    mutate(value = as.numeric(value))
}

long_df <- get_long(station_df)

# Combined dataset with optimal ranges
compare_df <- left_join(long_df, optimal_ranges, by = "parameter")

# ---------- UI modules ----------
parameter_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("param"), "Select parameter", choices = station_df$parameter,
                selected = "pH", multiple = FALSE, options = list(`live-search`=TRUE)),
    pickerInput(ns("stations"), "Select stations", choices = names(station_df)[3:ncol(station_df)],
                selected = names(station_df)[3:ncol(station_df)], multiple = TRUE,
                options = list(`actions-box` = TRUE))
  )
}

parameter_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(param = input$param, stations = input$stations))
  })
}

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = tagList(img(src = "4aa112f3-1d3b-43c1-a3d0-d3622453032c.png", height=40), "Aquaculture WQ — Brunei")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Parameter Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("Site Heatmap & Scores", tabName = "heat", icon = icon("th")),
      menuItem("Data & Download", tabName = "data", icon = icon("table")),
      hr(),
      htmlOutput("citation_box")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: linear-gradient(#e6f7ff,#f7fcff); }
      .box { border-radius: 8px; }
    "))),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(width=8, title="Project quick view", status="primary", solidHeader=TRUE,
                    p("This tool compares water quality parameters across aquaculture stations and flags deviations from recommended/optimal ranges. Use the 'Parameter Comparison' tab to drill into each variable."),
                    br(),
                    DT::dataTableOutput("summary_table")
                ),
                box(width=4, title="Guidance & thresholds", status="info", solidHeader=TRUE,
                    p(strong("Reference thresholds (editable):")),
                    DT::dataTableOutput("opt_table")
                )
              ),
              fluidRow(
                box(width=12, title="Interactive station map (abstract)", status="warning", solidHeader=TRUE,
                    p("If station coordinates are added, a map & spatial interpolation can be shown here. For now we show station score badges below."),
                    full_screen = TRUE,
                    plotlyOutput("station_score_plot", height = "250px")
                )
              )
      ),
      tabItem(tabName = "compare",
              fluidRow(
                box(width=4, title="Controls", status="primary", solidHeader=TRUE,
                    parameter_selector_ui("sel"),
                    hr(),
                    h5("Display options"),
                    switchInput("show_opt", "Show optimal ranges", value = TRUE),
                    numericInput("ymax_mult", "Y-axis multiplier (for visual scaling)", value = 1.2, min = 1, step = 0.1)
                ),
                box(width=8, title="Parameter charts", status="success", solidHeader=TRUE,
                    full_screen = TRUE,
                    plotlyOutput("param_bar", height = "360px"),
                    br(),
                    full_screen = TRUE,
                    plotlyOutput("param_radar", height="320px")
                )
              )
      ),
      tabItem(tabName = "heat",
              fluidRow(
                box(width=8, title="Heatmap: station vs parameters", status="primary", solidHeader=TRUE,
                    plotlyOutput("heatmap", height = "520px")
                ),
                box(width=4, title="Station scoring", status="info", solidHeader=TRUE,
                    p("Stations are assigned a simple 'score' based on how many parameters fall within the optimal min-max range."),
                    full_screen = TRUE,
                    DT::dataTableOutput("station_scores")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(width=12, title="Raw data and download", status="primary", solidHeader=TRUE,
                    DT::dataTableOutput("raw_table"),
                    br(),
                    downloadButton("download_csv", "Download CSV")
                )
              )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Citations (rendered in the sidebar)
  output$citation_box <- renderUI({
    tagList(
      p(strong("Key references:")),
      tags$ul(
        tags$li(HTML('FAO aquaculture guidance (Dissolved oxygen and general water quality). :contentReference[oaicite:0]{index=0}')),
        tags$li(HTML('FAO limits on ammonia and species-specific sensitivity. :contentReference[oaicite:1]{index=1}')),
        tags$li(HTML('Global Aquaculture Alliance targets for phosphorus and effluent standards. :contentReference[oaicite:2]{index=2}')),
        tags$li(HTML('Regional nitrate guidance (BC Water Quality Guidelines). :contentReference[oaicite:3]{index=3}')),
        tags$li(HTML('Practical turbidity ranges for aquaculture operations. :contentReference[oaicite:4]{index=4}'))
      )
    )
  })
  
  # Summary table in Overview
  output$summary_table <- DT::renderDataTable({
    station_df %>%
      select(-index) %>%
      datatable(rownames = FALSE, options = list(pageLength=5, scrollX=TRUE))
  })
  
  # editable opt table (shows the optimal ranges)
  output$opt_table <- DT::renderDataTable({
    optimal_ranges %>%
      datatable(options = list(pageLength = 8, dom='t'), rownames = FALSE)
  })
  
  # Station score: simple metric counting parameters inside optimal range
  compute_scores <- reactive({
    df <- compare_df
    df <- df %>%
      mutate(within = case_when(
        !is.na(min) & !is.na(max) ~ (value >= min & value <= max),
        !is.na(min) & is.na(max) ~ (value >= min),
        is.na(min) & !is.na(max) ~ (value <= max),
        TRUE ~ NA
      ))
    df %>%
      group_by(station) %>%
      summarise(params_checked = sum(!is.na(within)),
                within_ok = sum(within, na.rm = TRUE),
                score_pct = round(100 * within_ok / params_checked, 1)) %>%
      arrange(desc(score_pct))
  })
  
  
  
  
  # Optimized station_score_plot
  output$station_score_plot <- renderPlotly({
    sc <- compute_scores()
    req(sc, nrow(sc) > 0)
    
    # Prepare data with performance categories
    sc_processed <- sc %>%
      mutate(
        station = as.character(station),
        score_pct = as.numeric(score_pct),
        performance_category = case_when(
          score_pct >= 80 ~ "Excellent (≥80%)",
          score_pct >= 60 ~ "Good (60-79%)",
          score_pct >= 40 ~ "Fair (40-59%)",
          score_pct >= 20 ~ "Poor (20-39%)",
          TRUE ~ "Critical (<20%)"
        ),
        score_formatted = paste0(round(score_pct, 1), "%")
      ) %>%
      arrange(score_pct) %>%
      mutate(
        # Create ordered station names as character (not factor)
        station_ordered = factor(station, levels = station, ordered = TRUE),
        station_display = as.character(station_ordered)
      )
    
    # Create elegant color palette
    colors_gradient <- c("#d32f2f", "#ff5722", "#ff9800", "#ffc107", "#8bc34a", "#4caf50", "#2e7d32")
    
    # Create the plot
    p <- plot_ly(
      data = sc_processed,
      type = "bar",
      x = ~score_pct,
      y = ~station_display,  # Use character version instead of reorder()
      orientation = "h",
      color = ~score_pct,
      colors = colorRamp(colors_gradient),
      text = ~paste("Station:", station,
                    "<br>Quality Score:", score_formatted,
                    "<br>Performance:", performance_category),
      hovertemplate = "%{text}<extra></extra>",
      marker = list(
        line = list(color = "white", width = 1),
        opacity = 0.85
      )
    ) %>%
      layout(
        title = list(
          text = "Station Quality Performance Dashboard<br><span style='font-size:12px;color:gray'>Percentage of parameters within optimal ranges</span>",
          font = list(size = 16, family = "Arial"),
          x = 0.05
        ),
        xaxis = list(
          title = "Quality Score (%)",
          tickformat = ".0f",
          ticksuffix = "%",
          range = c(0, max(sc_processed$score_pct) * 1.1),  # Dynamic range based on data
          gridcolor = "#f0f0f0",
          gridwidth = 1,
          showgrid = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 11),
          gridcolor = "#f0f0f0",
          gridwidth = 1,
          showgrid = TRUE,
          categoryorder = "array",  # Specify category ordering
          categoryarray = sc_processed$station_display  # Use our ordered stations
        ),
        showlegend = FALSE,
        margin = list(t = 80, b = 60, l = 120, r = 60),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        hovermode = "closest"
      ) %>%
      add_annotations(
        x = pmax(sc_processed$score_pct + 2, 5),  # Ensure annotations don't go off chart
        y = sc_processed$station_display,
        text = sc_processed$score_formatted,
        showarrow = FALSE,
        font = list(color = "black", size = 10, family = "Arial", weight = "bold"),
        xanchor = "left"
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", "zoom2d")
      )
    
    return(p)
  })
  
  
  output$station_scores <- DT::renderDataTable({
    compute_scores() %>% datatable(options = list(pageLength=8))
  })
  
  # Raw table for download
  output$raw_table <- DT::renderDataTable({
    station_df %>% datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste0("aquaculture_stations_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(station_df, file, row.names = FALSE)
    }
  )
  
  # Parameter selection module
  sel <- parameter_selector_server("sel")
  
  # Parameter bar chart
  output$param_bar <- renderPlotly({
    sel_vals <- sel()
    req(sel_vals)
    param <- sel_vals$param
    stations <- sel_vals$stations
    
    data_plot <- long_df %>% filter(parameter == param, station %in% stations)
    
    # Merge optimal
    opt <- optimal_ranges %>% filter(parameter == param)
    ymin <- ifelse(!is.na(opt$min), opt$min, min(data_plot$value, na.rm=TRUE))
    ymax <- ifelse(!is.na(opt$max), opt$max, max(data_plot$value, na.rm=TRUE))
    ylimit <- max(ymax * input$ymax_mult, max(data_plot$value, na.rm=TRUE) * input$ymax_mult, na.rm=TRUE)
    
    p <- ggplot(data_plot, aes(x = station, y = value, fill = station)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Station comparison —", param), x = "", y = param) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # add ranges if selected
    if (input$show_opt & nrow(opt)>0) {
      if (!is.na(opt$min)) p <- p + geom_hline(yintercept = opt$min, linetype="dashed", color="blue", linewidth=0.8)
      if (!is.na(opt$max)) p <- p + geom_hline(yintercept = opt$max, linetype="dashed", color="red", linewidth=0.8)
    }
    
    p <- p + ylim(0, ylimit)
    ggplotly(p)
  })
  
 
  
 
  # Fixed param_radar plot
  output$param_radar <- renderPlotly({
    sel_vals <- sel()
    req(sel_vals)
    param <- sel_vals$param
    stations <- sel_vals$stations
    
    # Validate inputs
    req(param, stations)
    
    dfp <- tryCatch({
      compare_df %>% filter(parameter == param, station %in% stations)
    }, error = function(e) {
      return(data.frame())
    })
    
    if (nrow(dfp) == 0) {
      # Create empty plot using plotly directly
      p <- plot_ly() %>%
        add_annotations(
          x = 0, y = 0,
          text = "No data available for selected stations",
          showarrow = FALSE,
          font = list(size = 16, color = "gray50")
        ) %>%
        layout(
          title = paste("Normalized view —", param),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          showlegend = FALSE
        )
      return(p)
    }
    
    # Process data with simplified normalization
    dfp_processed <- tryCatch({
      dfp %>%
        mutate(
          # Ensure numeric values
          value = as.numeric(value),
          min_val = if("min" %in% colnames(dfp)) as.numeric(min) else NA_real_,
          max_val = if("max" %in% colnames(dfp)) as.numeric(max) else NA_real_
        ) %>%
        rowwise() %>%
        mutate(
          # Simple normalization logic
          norm = {
            val <- value
            min_v <- min_val
            max_v <- max_val
            
            if (!is.na(min_v) && !is.na(max_v) && max_v != min_v) {
              norm_val <- (val - min_v) / (max_v - min_v)
              max(0, min(1, norm_val))
            } else if (!is.na(min_v) && min_v > 0) {
              norm_val <- val / min_v
              max(0, min(1, norm_val))
            } else if (!is.na(max_v) && max_v > 0) {
              norm_val <- 1 - (val / max_v)
              max(0, min(1, norm_val))
            } else {
              0.5
            }
          },
          station_short = as.character(station),
          performance = {
            if (norm >= 0.8) "Excellent"
            else if (norm >= 0.6) "Good"
            else if (norm >= 0.4) "Fair"
            else if (norm >= 0.2) "Poor"
            else "Critical"
          },
          # Pre-format values for hover
          value_formatted = round(as.numeric(value), 3),
          norm_formatted = round(norm, 3),
          norm_percent = paste0(round(norm * 100, 1), "%")
        ) %>%
        ungroup() %>%
        filter(!is.na(norm), !is.na(value), is.finite(norm), is.finite(value))
    }, error = function(e) {
      return(data.frame())
    })
    
    if (nrow(dfp_processed) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          x = 0, y = 0,
          text = "No valid data after processing",
          showarrow = FALSE,
          font = list(size = 16, color = "gray50")
        ) %>%
        layout(
          title = paste("Normalized view —", param),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          showlegend = FALSE
        )
      return(p)
    }
    
    # Create plotly radar chart directly
    colors <- c("Critical" = "#d32f2f", "Poor" = "#ff9800", "Fair" = "#ffc107", 
                "Good" = "#8bc34a", "Excellent" = "#4caf50")
    
    # Create polar bar chart using plotly directly with fixed hover template
    p <- plot_ly(
      data = dfp_processed,
      type = "barpolar",
      r = ~norm,
      theta = ~station_short,
      color = ~performance,
      colors = colors,
      text = ~paste("Station:", station_short, 
                    "<br>Parameter:", param,
                    "<br>Value:", value_formatted,
                    "<br>Normalized:", norm_percent,
                    "<br>Performance:", performance),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste0("Performance Radar — ", param, "<br>",
                        "<span style='font-size:12px;color:gray'>Normalized values relative to optimal ranges</span>"),
          font = list(size = 16)
        ),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1),
            tickformat = ".0%",
            tickmode = "linear",
            tick0 = 0,
            dtick = 0.2,
            gridcolor = "#e0e0e0",
            gridwidth = 1,
            showticklabels = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 11, color = "black"),
            rotation = 90,
            direction = "clockwise",
            gridcolor = "#e0e0e0"
          ),
          bgcolor = "white"
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = 'center',
          y = -0.15,
          font = list(size = 11)
        ),
        margin = list(t = 80, b = 120, l = 60, r = 60),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", "zoom2d")
      )
    
    return(p)
  })
  
  
  
  output$heatmap <- renderPlotly({
    # Validate that station_df exists and has data
    req(station_df)
    
    if (nrow(station_df) == 0) {
      # Return informative empty plot
      empty_plot <- ggplot() + 
        geom_text(aes(x = 0, y = 0), label = "No data available for heatmap", 
                  size = 5, color = "gray50") +
        theme_void() +
        labs(title = "Parameter Heatmap - No Data Available")
      return(ggplotly(empty_plot))
    }
    
    # Create heatmap data with comprehensive error handling
    plot_data <- tryCatch({
      # Check available columns
      available_cols <- colnames(station_df)
      print(paste("Available columns:", paste(available_cols, collapse = ", "))) # Debug
      
      # Handle different data structures
      if ("parameter" %in% available_cols && "station" %in% available_cols && "value" %in% available_cols) {
        # Long format data
        station_df %>%
          select(station, parameter, value) %>%
          filter(!is.na(value), is.finite(value)) %>%
          group_by(parameter) %>%
          mutate(
            scaled_value = as.numeric(scale(value)),
            scaled_value = ifelse(is.na(scaled_value), 0, scaled_value)
          ) %>%
          ungroup()
      } else if ("parameter" %in% available_cols) {
        # Wide format with parameter column
        numeric_cols <- station_df %>% select(-parameter) %>% select_if(is.numeric) %>% colnames()
        if (length(numeric_cols) == 0) stop("No numeric columns found")
        
        station_df %>%
          select(parameter, all_of(numeric_cols)) %>%
          pivot_longer(-parameter, names_to = "station", values_to = "value") %>%
          filter(!is.na(value), is.finite(value)) %>%
          group_by(parameter) %>%
          mutate(
            scaled_value = as.numeric(scale(value)),
            scaled_value = ifelse(is.na(scaled_value), 0, scaled_value)
          ) %>%
          ungroup()
      } else {
        # All numeric columns are parameters
        numeric_cols <- station_df %>% select_if(is.numeric) %>% colnames()
        if (length(numeric_cols) == 0) stop("No numeric columns found")
        
        station_df %>%
          select(all_of(numeric_cols)) %>%
          mutate(station = paste("Station", row_number())) %>%
          pivot_longer(-station, names_to = "parameter", values_to = "value") %>%
          filter(!is.na(value), is.finite(value)) %>%
          group_by(parameter) %>%
          mutate(
            scaled_value = as.numeric(scale(value)),
            scaled_value = ifelse(is.na(scaled_value), 0, scaled_value)
          ) %>%
          ungroup()
      }
    }, error = function(e) {
      print(paste("Error in data preparation:", e$message)) # Debug
      return(NULL)
    })
    
    # Check if we have valid plot data
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      empty_plot <- ggplot() + 
        geom_text(aes(x = 0, y = 0), label = "Unable to prepare heatmap data", 
                  size = 5, color = "red") +
        theme_void() +
        labs(title = "Heatmap Error - Data Preparation Failed")
      return(ggplotly(empty_plot))
    }
    
    # Create the heatmap plot
    tryCatch({
      # Create elegant ggplot heatmap
      p <- ggplot(plot_data, aes(x = station, y = parameter, fill = scaled_value)) +
        geom_tile(color = "white", size = 0.2, alpha = 0.9) +
        scale_fill_gradient2(
          low = "#2166ac", 
          mid = "#f7f7f7", 
          high = "#762a83",
          midpoint = 0,
          name = "Scaled\nValue",
          guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            barwidth = 1,
            barheight = 8
          )
        ) +
        labs(
          title = "Station Performance Heatmap",
          subtitle = "Row-scaled values show relative performance patterns",
          x = "Stations",
          y = "Parameters"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
          plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray60", margin = margin(b = 20)),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 9),
          legend.position = "right",
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        )
      
      # Add custom tooltip for plotly
      p_with_tooltip <- p + aes(text = paste(
        "Station:", station,
        "\nParameter:", parameter,
        "\nOriginal Value:", round(value, 3),
        "\nScaled Value:", round(scaled_value, 3)
      ))
      
      # Convert to plotly
      ggplotly(p_with_tooltip, tooltip = "text") %>%
        layout(
          title = list(
            text = "Station Performance Heatmap<br><sub>Row-scaled values show relative performance patterns</sub>",
            font = list(size = 16),
            x = 0.5
          ),
          margin = list(t = 80, b = 100, l = 120, r = 100),
          hovermode = "closest"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d")
        )
      
    }, error = function(e) {
      print(paste("Error in plot creation:", e$message)) # Debug
      
      # Final fallback - simple static plot
      basic_plot <- ggplot() + 
        geom_text(aes(x = 0, y = 0), 
                  label = paste("Plot creation failed. Data shape:", 
                                nrow(plot_data), "rows,", 
                                length(unique(plot_data$parameter)), "parameters,",
                                length(unique(plot_data$station)), "stations"), 
                  size = 4, color = "red") +
        theme_void() +
        labs(title = "Heatmap Creation Error")
      
      return(ggplotly(basic_plot))
    })
  })
 
  
  
  
  # summary table of optimal compliance per param
  output$opt_summary <- DT::renderDataTable({
    compare_df %>%
      group_by(parameter) %>%
      summarise(checked = sum(!is.na(min) | !is.na(max)),
                within = sum((!is.na(min) & !is.na(max) & value >= min & value <= max) |
                               (!is.na(min) & is.na(max) & value >= min) |
                               (is.na(min) & !is.na(max) & value <= max), na.rm = TRUE)) %>%
      datatable()
  })
}

shinyApp(ui, server)


## optimise the output$opt_summary
## Add a new module visualize the relationship between stations in relation to optimal condition in ordination space, using informative anotated RDA and CCA
