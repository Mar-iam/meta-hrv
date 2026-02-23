# ------------------------------------------------------------
# Design Disclosure
# ------------------------------------------------------------
# The CSS styling and user interface formatting of this
# Shiny application were developed with the assistance of
# AI-based tools. All statistical modeling, analytical
# methods, and research content were designed, implemented,
# and validated independently by the authors.
# ------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(meta)
library(metafor)
library(networkD3)
library(dplyr)
library(htmlwidgets)
library(DT)
library(stringr)
library(bslib)

source("helpers/shiny_helper.R")

# ---------------------------
# Load data (NO restriction for overview/table/sankey when toggled)
# ---------------------------
data_all <- read.csv("data/data_es.csv")

# Arrange by effect size (keep NA last)
data_all <- data_all %>% arrange(is.na(yi), yi)

# Ensure categorical columns are characters
data_all <- data_all %>%
  mutate(
    COG_DOMAIN    = as.character(COG_DOMAIN),
    POPULATION    = as.character(POPULATION),
    HRV_MEASURE   = as.character(HRV_MEASURE),
    SUB_DOMAIN    = as.character(SUB_DOMAIN),
    STUDY_DESIGN  = as.character(STUDY_DESIGN),
    CITEKEY       = as.character(CITEKEY)
  )

# ---------------------------
# Theme (white background + blue hints; no green)
# ---------------------------
theme <- bs_theme(
  version = 5,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  primary = "#2D9CDB",
  secondary = "#56CCF2",
  bg = "#FFFFFF",
  fg = "#0F172A",
  success = "#2D9CDB",
  info    = "#2D9CDB",
  warning = "#2D9CDB",
  danger  = "#2D9CDB"
)

# ---------------------------
# UI
# ---------------------------
ui <- page_sidebar(
  theme = theme,
  title = "HRV Meta-Analysis",
  
  sidebar = sidebar(
    width = 360,
    class = "sidebar-card",
    
    tags$div(
      class = "sidebar-header",
      tags$div(class = "sidebar-title", "Filters"),
      tags$div(class = "sidebar-subtitle", "Explore studies and meta-analysis results")
    ),
    
    checkboxInput(
      inputId = "include_within",
      label   = "Include Within-design studies (show all in Sankey & Data)",
      value   = FALSE
    ),
    
    selectInput(
      inputId = "population",
      label   = "Population",
      choices = c("All", sort(unique(na.omit(data_all$POPULATION)))),
      selected = "All"
    ),
    
    selectInput(
      inputId = "hrv_measure",
      label   = "HRV Measure",
      choices = c("All", sort(unique(na.omit(data_all$HRV_MEASURE)))),
      selected = "All"
    ),
    
    selectInput(
      inputId = "cog_level",
      label   = "Cognitive Domain",
      choices = c("All", sort(unique(na.omit(data_all$COG_DOMAIN)))),
      selected = "All"
    ),
    
    awesomeRadio(
      inputId = "mod_level",
      label = "Moderator analysis",
      choices = list(
        "ASD vs. ADHD"     = "POPULATION",
        "HRV Measure"      = "HRV_MEASURE",
        "Cognitive Domain" = "COG_DOMAIN"
      ),
      status = "info"
    ),
    
    checkboxInput(
      inputId = "outlier",
      label = "Exclude Influential Study (Feeney2023)",
      value = FALSE
    ),
    
    checkboxInput(
      inputId = "apply_filters_to_plots",
      label = "Apply filters to plots",
      value = FALSE
    ),
    
    hr(),
    
    card(
      class = "prospero-card",
      card_header(
        tags$div(style = "font-weight:700; font-size:14px;", "Pre-Registered Protocol (PROSPERO 2024)")
      ),
      card_body(
        tags$div(
          style = "font-size:12px; opacity:.85; line-height:1.45;",
          "Mariam Bahameish, Dena Al-Thani, Maryam Al-Abdulla, Tony Stockman, Alaa Abd-Alrazaq. ",
          "Comparative Analysis of Heart Rate Variability across Cognitive Domains in Individuals with Autism and ",
          "Attention-Deficit/Hyperactivity Disorder: A Systematic Review and Meta-Analysis."
        ),
        tags$div(
          style = "margin-top:8px; font-size:12px;",
          tags$span(style="font-weight:700; opacity:.75;", "URL: "),
          tags$a(
            href="https://www.crd.york.ac.uk/PROSPERO/view/CRD42024521120",
            "CRD42024521120",
            target="_blank"
          )
        )
      )
    )
  ),
  
  # filter chips shown on ALL pages
  uiOutput("filter_chips"),
  
  # analysis exclusion note shown on ALL pages
  tags$div(
    class = "analysis-note",
    HTML("<strong>Note:</strong> Meta-analytic models in this dashboard focus on HRV during cognitive tasks in NDDs and controls. Baseline HRV and baseline-to-cognitive analyses are reported separately in the supplementary materials.")
  ),
  
  
  
  navset_card_tab(
    nav_panel(
      "Overview",
      
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        
        div(class = "metric-box",
            div(class = "metric-value", textOutput("n_studies")),
            div(class = "metric-label", "Studies")
        ),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("n_rows")),
            div(class = "metric-label", "Rows")
        ),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("n_hrv")),
            div(class = "metric-label", "HRV Measures")
        ),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("n_domains")),
            div(class = "metric-label", "Cognitive Domains")
        ),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("pooled_g")),
            div(class = "metric-label", "Pooled g (95% CI)")
        ),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("tau")),
            div(class = "metric-label", "τ (REML)")
        )
      ),
      
      card(
        card_header(
          tags$div(
            tags$div(style = "font-weight:700; font-size:16px;", "Sankey Diagram"),
            tags$div(style = "opacity:.70; font-size:12px;", "Evidence flow (updates with filters)")
          )
        ),
        card_body(
          sankeyNetworkOutput("sankey_plot", height = "calc(100vh - 450px)")
        )
      )
    ),
    
    nav_panel(
      "Data",
      card(
        card_header(
          tags$div(
            style="display:flex; align-items:center; justify-content:space-between; gap:12px;",
            tags$div(
              tags$div(style = "font-weight:700; font-size:16px;", "Included Studies"),
              tags$div(style = "opacity:.70; font-size:12px;", "Filtered view of studies (all designs)")
            ),
            tags$div(
              style="display:flex; gap:8px;",
              downloadButton("download_filtered", "Download filtered CSV", class="btn btn-sm btn-primary"),
              downloadButton("download_full", "Download full CSV", class="btn btn-sm btn-outline-primary")
            )
          )
        ),
        card_body(DTOutput("data_table"))
      )
    ),
    
    nav_panel(
      "Forest Plot",
      card(
        card_header(
          tags$div(
            tags$div(style = "font-weight:700; font-size:16px;", "Random-Effects Meta-Analysis"),
            tags$div(style = "opacity:.70; font-size:12px;", "Multilevel model (rma.mv); excludes Within + missing yi/vi")
          )
        ),
        card_body(plotOutput("forest_plot", height = "650px"))
      )
    ),
    
    nav_panel(
      "Funnel Plot",
      card(
        card_header(
          tags$div(
            tags$div(style = "font-weight:700; font-size:16px;", "Funnel Plot"),
            tags$div(style = "opacity:.70; font-size:12px;", "Visual check for asymmetry; excludes Within + missing yi/vi")
          )
        ),
        card_body(plotOutput("funnel_plot", height = "650px"))
      )
    ),
    
    nav_panel(
      "Moderator",
      card(
        card_header(
          tags$div(
            tags$div(style = "font-weight:700; font-size:16px;", "Moderator Analysis"),
            tags$div(style = "opacity:.70; font-size:12px;", "Subgroup forest plot; robust x-limits to avoid axis distortion")
          )
        ),
        card_body(plotOutput("forest_moderator", height = "650px"))
      )
    )
  ),
  
  # ---------------------------
  # CSS
  # ---------------------------
  tags$style(HTML("
    body { background: #FFFFFF; }

    .sidebar-card {
      background: #FFFFFF;
      border-radius: 18px;
      padding: 16px 16px 14px 16px;
      box-shadow: 0 10px 26px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(15, 23, 42, 0.08);
      position: relative;
    }
    .sidebar-card:before{
      content: '';
      position:absolute;
      top:0; left:0; right:0;
      height: 5px;
      border-radius: 18px 18px 0 0;
      background: linear-gradient(90deg, #2D9CDB, #56CCF2);
    }
    .sidebar-title { font-weight: 800; font-size: 16px; letter-spacing: -0.01em; }
    .sidebar-subtitle { opacity: .72; font-size: 12px; margin-top: 2px; }
    
    .sidebar-card {
    font-size: 12px;
  }
  
  .sidebar-card label {
    font-size: 12px !important;
    font-weight: 600;
  }
  
  .sidebar-card .form-control,
  .sidebar-card .selectize-input {
    font-size: 12px !important;
  }

    .card {
      border-radius: 18px !important;
      border: 1px solid rgba(45, 156, 219, 0.14);
      box-shadow: 0 12px 28px rgba(15, 23, 42, 0.06);
      background: #FFFFFF;
    }
    .card-header {
      background: linear-gradient(180deg, rgba(45,156,219,0.07), rgba(45,156,219,0.00));
      border-bottom: 1px solid rgba(45, 156, 219, 0.12);
    }

    .prospero-card {
      border: 1px solid rgba(45,156,219,0.20) !important;
      background: linear-gradient(180deg, rgba(45,156,219,0.05), rgba(86,204,242,0.02));
    }

    .form-control, .selectize-input { border-radius: 12px !important; }
    .selectize-input.focus {
      box-shadow: 0 0 0 4px rgba(45,156,219,0.14) !important;
      border-color: rgba(45,156,219,0.45) !important;
    }

    .chip-row { margin-bottom: 10px; display:flex; flex-wrap:wrap; gap:8px; }
    .chip {
      display:inline-flex;
      align-items:center;
      gap:8px;
      padding: 6px 10px;
      border-radius: 999px;
      border: 1px solid rgba(45,156,219,0.20);
      background: rgba(45,156,219,0.06);
      font-size: 12px;
      font-weight: 600;
      color: #0F172A;
    }
    .chip .k { opacity:.7; font-weight:700; }
    .chip .v { color:#1B6FB8; font-weight:800; }
    .chip .dot {
      width: 8px; height: 8px; border-radius: 999px;
      background: linear-gradient(90deg, #2D9CDB, #56CCF2);
      display:inline-block;
    }

    .analysis-note {
      font-size: 12px;
      opacity: 0.78;
      margin-bottom: 12px;
      padding: 8px 12px;
      border-left: 4px solid rgba(45,156,219,0.75);
      background: rgba(45,156,219,0.05);
      border-radius: 8px;
    }

    .metric-box {
      background: linear-gradient(135deg, rgba(45,156,219,0.09), rgba(86,204,242,0.06));
      border: 1px solid rgba(45,156,219,0.18);
      border-radius: 14px;
      padding: 10px 8px;
      text-align: center;
      box-shadow: 0 8px 18px rgba(15, 23, 42, 0.05);
      transition: 0.2s ease;
      margin-bottom: 6px;
      min-height: 64px;
    }
    .metric-value {
      font-size: 18px;
      font-weight: 850;
      color: #1B6FB8;
      line-height: 1.15;
      letter-spacing: -0.02em;
    }
    .metric-label {
      font-size: 12px;
      font-weight: 650;
      opacity: 0.72;
      margin-top: 2px;
    }

    table.dataTable { font-size: 12.5px !important; }
    table.dataTable thead th {
      font-size: 12.5px !important;
      font-weight: 800;
    }
    table.dataTable tbody td { padding: 6px 10px !important; }
    table.dataTable tbody tr:hover { background: rgba(45,156,219,0.05) !important; }

    .dataTables_filter input{
      border-radius: 12px !important;
      padding: 6px 10px !important;
      border: 1px solid rgba(15,23,42,0.12) !important;
    }
  "))
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # ---- analysis-safe base for ALL meta-analytic models ----
  analysis_base <- reactive({
    data_all %>%
      filter(STUDY_DESIGN != "Within") %>%
      filter(!is.na(yi), !is.na(vi))
  })
  
  # ---- base for UI/overview/table/sankey (toggle all vs analysis-ready) ----
  base_data <- reactive({
    if (isTRUE(input$include_within)) data_all else analysis_base()
  })
  
  # ---- apply dropdown filters (+ outlier) to the base_data ----
  filtered_data <- reactive({
    df <- base_data()
    
    if (input$population != "All")  df <- df %>% filter(POPULATION == input$population)
    if (input$hrv_measure != "All") df <- df %>% filter(HRV_MEASURE == input$hrv_measure)
    if (input$cog_level != "All")   df <- df %>% filter(COG_DOMAIN == input$cog_level)
    if (isTRUE(input$outlier))      df <- df %>% filter(CITEKEY != "Feeney2023")
    
    df
  })
  
  # ---- analysis-safe version of the CURRENT filters (for pooled g) ----
  filtered_analysis <- reactive({
    filtered_data() %>%
      filter(STUDY_DESIGN != "Within") %>%
      filter(!is.na(yi), !is.na(vi))
  })
  
  # ---- plot data toggle (ALWAYS analysis-safe) ----
  plot_data <- reactive({
    df <- analysis_base()
    
    # optional outlier exclusion
    if (isTRUE(input$outlier)) df <- df %>% filter(CITEKEY != "Feeney2023")
    
    if (isTRUE(input$apply_filters_to_plots)) {
      if (input$population != "All")  df <- df %>% filter(POPULATION == input$population)
      if (input$hrv_measure != "All") df <- df %>% filter(HRV_MEASURE == input$hrv_measure)
      if (input$cog_level != "All")   df <- df %>% filter(COG_DOMAIN == input$cog_level)
    }
    
    df
  })
  
  # ---------------------------
  # Filter chips (shown on all pages)
  # ---------------------------
  output$filter_chips <- renderUI({
    chip <- function(key, val) {
      tags$div(class="chip",
               tags$span(class="dot"),
               tags$span(class="k", key),
               tags$span(class="v", val))
    }
    
    tags$div(
      class="chip-row",
      chip("Population:", if (input$population == "All") "All" else input$population),
      chip("HRV:", if (input$hrv_measure == "All") "All" else input$hrv_measure),
      chip("Domain:", if (input$cog_level == "All") "All" else input$cog_level),
      chip("Outlier removed:", if (isTRUE(input$outlier)) "Yes" else "No"),
      chip("Filters → plots:", if (isTRUE(input$apply_filters_to_plots)) "On" else "Off"),
      chip("Include Within:", if (isTRUE(input$include_within)) "Yes" else "No")
    )
  })
  
  # ---------------------------
  # Metrics (Overview counts = filtered_data; model metrics = filtered_analysis)
  # ---------------------------
  output$n_studies <- renderText({
    df <- filtered_data()
    if (!"SID" %in% names(df)) return("—")
    length(unique(df$SID))
  })
  
  output$n_rows <- renderText({ nrow(filtered_data()) })
  
  output$n_hrv <- renderText({
    df <- filtered_data()
    length(unique(na.omit(df$HRV_MEASURE)))
  })
  
  output$n_domains <- renderText({
    df <- filtered_data()
    length(unique(na.omit(df$COG_DOMAIN)))
  })
  
  overview_model <- reactive({
    df <- filtered_analysis()
    validate(
      need(nrow(df) > 1, "Not enough analyzable effect sizes after exclusions.")
    )
    
    rand <- if ("UID" %in% names(df)) {
      ~ 1 | SID / UID
    } else if ("X" %in% names(df)) {
      ~ 1 | SID / X
    } else {
      ~ 1 | SID
    }
    
    rma.mv(yi, vi, random = rand, data = df)
  })
  
  output$pooled_g <- renderText({
    df <- filtered_analysis()
    if (nrow(df) <= 1) return("—")
    
    res <- overview_model()
    est <- as.numeric(res$b[1, 1])
    
    ci_b <- tryCatch(confint(res)$fixed, error = function(e) NULL)
    if (!is.null(ci_b) && nrow(ci_b) >= 1) {
      sprintf("%.2f [%.2f, %.2f]", est, ci_b[1, "ci.lb"], ci_b[1, "ci.ub"])
    } else {
      sprintf("%.2f", est)
    }
  })
  
  output$tau <- renderText({
    df <- filtered_analysis()
    if (nrow(df) <= 1) return("—")
    
    res <- overview_model()
    
    # Between-study tau (REML)
    tau_between <- sqrt(res$sigma2[1])
    
    sprintf("%.2f", tau_between)
  })
  
  # ---------------------------
  # Dependent dropdown updates (use ALL data for choices)
  # ---------------------------
  observeEvent(input$population, {
    df <- if (input$population == "All") data_all else data_all %>% filter(POPULATION == input$population)
    updateSelectInput(session, "hrv_measure",
                      choices = c("All", sort(unique(na.omit(df$HRV_MEASURE)))),
                      selected = "All")
  }, ignoreInit = TRUE)
  
  observeEvent(input$hrv_measure, {
    df <- data_all
    if (input$population != "All")  df <- df %>% filter(POPULATION == input$population)
    if (input$hrv_measure != "All") df <- df %>% filter(HRV_MEASURE == input$hrv_measure)
    
    updateSelectInput(session, "cog_level",
                      choices = c("All", sort(unique(na.omit(df$COG_DOMAIN)))),
                      selected = "All")
  }, ignoreInit = TRUE)
  
  # ---------------------------
  # Data table (no paging)
  # ---------------------------
  output$data_table <- renderDT({
    df <- filtered_data() %>%
      select(CITEKEY, POPULATION, HRV_MEASURE, COG_DOMAIN, SUB_DOMAIN, STUDY_DESIGN)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        dom = "ft"
      ),
      class = "compact stripe"
    )
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() paste0("hrv_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
  
  output$download_full <- downloadHandler(
    filename = function() paste0("hrv_full_", Sys.Date(), ".csv"),
    content = function(file) write.csv(data_all, file, row.names = FALSE)
  )
  
  # ---------------------------
  # Sankey (your palette + styling; updates with filters)
  # ---------------------------
  color_palette <- 'd3.scaleOrdinal()
    .domain(["POPULATION", "HRV_MEASURE", "COG_DOMAIN", "SUB_DOMAIN"])
    .range(["#1B3A57", "#2C5A78", "#3E8FA3", "#5DAFC1", "#8CC8D9", "#C2E5F2"])'
  
  build_sankey_widget <- function(df) {
    df <- df %>%
      select(POPULATION, HRV_MEASURE, COG_DOMAIN, SUB_DOMAIN) %>%
      filter(
        !is.na(POPULATION), POPULATION != "",
        !is.na(HRV_MEASURE), HRV_MEASURE != "",
        !is.na(COG_DOMAIN),  COG_DOMAIN  != "",
        !is.na(SUB_DOMAIN),  SUB_DOMAIN  != ""
      )
    
    if (nrow(df) == 0) return(NULL)
    
    links <- tibble(
      source = c(df$POPULATION, df$HRV_MEASURE, df$COG_DOMAIN),
      target = c(df$HRV_MEASURE, df$COG_DOMAIN, df$SUB_DOMAIN),
      value  = 1
    )
    
    nodes <- tibble(
      name  = unique(c(links$source, links$target)),
      group = NA_character_
    )
    
    nodes$group[match(unique(df$POPULATION), nodes$name)]  <- "POPULATION"
    nodes$group[match(unique(df$HRV_MEASURE), nodes$name)] <- ifelse(
      is.na(nodes$group[match(unique(df$HRV_MEASURE), nodes$name)]),
      "HRV_MEASURE",
      nodes$group[match(unique(df$HRV_MEASURE), nodes$name)]
    )
    nodes$group[match(unique(df$COG_DOMAIN), nodes$name)]  <- ifelse(
      is.na(nodes$group[match(unique(df$COG_DOMAIN), nodes$name)]),
      "COG_DOMAIN",
      nodes$group[match(unique(df$COG_DOMAIN), nodes$name)]
    )
    nodes$group[match(unique(df$SUB_DOMAIN), nodes$name)]  <- ifelse(
      is.na(nodes$group[match(unique(df$SUB_DOMAIN), nodes$name)]),
      "SUB_DOMAIN",
      nodes$group[match(unique(df$SUB_DOMAIN), nodes$name)]
    )
    
    nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    links <- as.data.frame(links, stringsAsFactors = FALSE)
    
    sankey <- sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "source", Target = "target",
      Value = "value", NodeID = "name",
      NodeGroup = "group",
      fontSize = 12,
      nodeWidth = 30,
      nodePadding = 14,
      colourScale = JS(color_palette)
    )
    
    htmlwidgets::onRender(sankey, "
      function(el, x) {
        var svg = d3.select(el).select('svg');

        svg.selectAll('.link')
          .style('stroke', '#A0A0A0')
          .style('stroke-opacity', 0.3)
          .style('fill', 'none');

        svg.selectAll('.node text')
          .style('font-family', 'Arial, sans-serif')
          .style('font-size', '12px');
      }
    ")
  }
  
  output$sankey_plot <- renderSankeyNetwork({
    w <- build_sankey_widget(filtered_data())
    validate(need(!is.null(w), "No data available for Sankey after filtering."))
    w
  })
  
  # ---------------------------
  # Forest plot
  # ---------------------------
  output$forest_plot <- renderPlot({
    df <- plot_data()
    validate(need(nrow(df) > 1, "Not enough analyzable effect sizes for meta-analysis."))
    
    rand <- if ("UID" %in% names(df)) ~ 1 | SID / UID else if ("X" %in% names(df)) ~ 1 | SID / X else ~ 1 | SID
    
    res <- rma.mv(yi, vi, random = rand, data = df, slab = paste0(CITEKEY))
    
    metafor::forest(
      res,
      slab = df$CITEKEY,
      ilab = cbind(df$HRV_MEASURE, df$COG_DOMAIN),
      ilab.xpos = c(-6.75, -4.75),
      xlab = "Hedges' g",
      order = df$yi,
      header = "Study",
      mlab = "",
      cex = 1,
      xlim = c(-10, 4),
      pch = 19
    )
    
    text(c(-6.75, -4.75), res$k + 2, c("HRV Measure", "Cognitive Domain"), cex = 1, font = 2)
    text(-6, 0, pos = 1, cex = 1, mlabfun("Multilevel RE Model", res))
  })
  
  # ---------------------------
  # Funnel plot
  # ---------------------------
  output$funnel_plot <- renderPlot({
    df <- plot_data()
    validate(need(nrow(df) > 1, "Not enough analyzable effect sizes to draw a funnel plot."))
    
    rand <- if ("X" %in% names(df)) ~ 1 | SID / X else if ("UID" %in% names(df)) ~ 1 | SID / UID else ~ 1 | SID
    
    res <- rma.mv(yi = yi, V = vi, random = rand, data = df, slab = paste0(CITEKEY))
    
    funnel.rma(
      res,
      pch = 21,
      legend = FALSE,
      xlab = "Hedge's g",
      cex = 1,
      digits = 2L,
      cex.lab = 1,
      cex.axis = 1,
      level = c(90, 95, 99),
      back = "grey95",
      shade = c("gray70", "gray80", "gray90")
    )
  })
  
  # ---------------------------
  # Moderator plot (robust x-limits)
  # ---------------------------
  output$forest_moderator <- renderPlot({
    df <- plot_data()
    
    validate(
      need(nrow(df) > 1, "Not enough data for moderator analysis."),
      need(all(c("yi","vi","CITEKEY","SID") %in% names(df)),
           "Missing required columns: yi, vi, CITEKEY, SID.")
    )
    
    mod_var <- input$mod_level
    validate(need(mod_var %in% names(df), paste("Missing moderator column:", mod_var)))
    
    df <- df %>%
      mutate(.MOD = as.factor(.data[[mod_var]])) %>%
      filter(!is.na(.MOD), .MOD != "")
    
    validate(need(nlevels(df$.MOD) >= 2, "Not enough groups after filtering for moderator analysis."))
    
    rand_formula <- if ("UID" %in% names(df)) {
      ~ 1 | SID / UID
    } else if ("X" %in% names(df)) {
      ~ 1 | SID / X
    } else {
      ~ 1 | SID
    }
    
    df <- df %>% arrange(.MOD, yi)
    
    groups <- levels(droplevels(df$.MOD))
    k <- nrow(df)
    
    gap_rows <- 2
    total_rows <- k + length(groups) * 2 + (length(groups) - 1) * gap_rows
    
    current <- total_rows
    study_rows <- integer(k)
    header_row <- setNames(integer(length(groups)), groups)
    summary_row <- setNames(integer(length(groups)), groups)
    
    for (g in groups) {
      header_row[g] <- current; current <- current - 1
      
      idx_g <- which(df$.MOD == g)
      for (j in idx_g) { study_rows[j] <- current; current <- current - 1 }
      
      summary_row[g] <- current; current <- current - 1
      if (g != tail(groups, 1)) current <- current - gap_rows
    }
    
    se_all <- sqrt(df$vi)
    ci_lb_all <- df$yi - 1.96 * se_all
    ci_ub_all <- df$yi + 1.96 * se_all
    
    lower <- as.numeric(quantile(ci_lb_all, 0.025, na.rm = TRUE))
    upper <- as.numeric(quantile(ci_ub_all, 0.975, na.rm = TRUE))
    alim <- c(lower - 0.20, upper + 0.20)
    
    op <- par(mar = c(4.5, 12.0, 3.0, 2.5))
    on.exit(par(op), add = TRUE)
    
    metafor::forest(
      x = df$yi,
      vi = df$vi,
      slab = df$CITEKEY,
      rows = study_rows,
      header = c("Study", "g [95% CI]"),
      xlab = "Hedges' g",
      alim = alim,
      cex = 0.95,
      pch = 19,
      ylim = c(0, total_rows + 3)
    )
    
    title(main = paste("Moderator:", mod_var), font.main = 2)
    
    usr <- par("usr")
    x_left_text <- usr[1]
    
    for (g in groups) {
      df_g <- df %>% filter(.MOD == g)
      
      text(
        x = x_left_text,
        y = header_row[g],
        labels = paste0(as.character(g), "  (k=", nrow(df_g), ")"),
        pos = 4, font = 2, cex = 1.05, xpd = NA
      )
      
      fit <- tryCatch(
        rma.mv(yi, vi, random = rand_formula, data = df_g),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        fit <- tryCatch(rma.uni(yi, vi, data = df_g, method = "REML"),
                        error = function(e) NULL)
      }
      if (is.null(fit)) next
      
      metafor::addpoly(fit, row = summary_row[g], mlab = "", cex = 0.95)
      abline(h = summary_row[g] - 0.5, col = "gray88")
    }
  })
}

shinyApp(ui = ui, server = server)