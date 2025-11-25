library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(biscale)
library(ggplot2)
library(cowplot)
library(plotly)
library(DT)

# Define custom colors
royal_blue <- "#4169E1" 
light_blue <- "#E8F0FF" 
dark_blue  <- "#1E3A8A"

# State dropdown choices (includes DC)
state_choices <- c(sort(c(state.name, "District of Columbia")))

shinyUI(
  navbarPage(
    title = "BIOL-185 Project - Melanoma Case Studies",
    id = "main_navbar",
    inverse = TRUE,
    
    header = tags$style(HTML(paste0("
      /* ========== NAVBAR STYLING ========== */
      .navbar {
        background-color: ", royal_blue, ";
        border-color: ", royal_blue, ";
      }
      .navbar-brand, .navbar-nav li a {
        color: white !important;
        font-weight: 500;
      }
      .navbar-nav li a:hover {
        background-color: ", dark_blue, " !important;
      }
      
      /* ========== GLOBAL STYLING ========== */
      body {
        background-color: ", light_blue, ";
        color: #0A0A0A;
        font-family: 'Segoe UI', 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      h2, h3 {
        color: ", dark_blue, ";
      }
      .well, .panel {
        background-color: white !important;
        border: 1px solid ", royal_blue, ";
        border-radius: 10px;
        box-shadow: 0 0 8px rgba(0,0,0,0.05);
      }
      
      /* ========== VISUALIZATION TAB STYLING ========== */
      
      /* Sidebar panel */
      .viz-sidebar {
        background: white;
        border-radius: 12px;
        padding: 20px 18px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        border: 1px solid #e0e5ec;
      }
      
      .viz-sidebar .form-group {
        margin-bottom: 0;
      }
      
      /* Section titles */
      .section-title {
        font-size: 13px;
        font-weight: 700;
        color: ", dark_blue, ";
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 12px;
      }
      
      /* Divider */
      .section-divider {
        height: 1px;
        background: linear-gradient(to right, transparent, #d0d5dd, transparent);
        margin: 18px 0;
      }
      
      /* Radio button groups */
      .layer-group {
        margin-bottom: 15px;
      }
      
      .layer-group-label {
        font-size: 11px;
        font-weight: 600;
        color: #777;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        margin-bottom: 8px;
        padding-left: 2px;
      }
      
      /* Radio button styling */
      .viz-sidebar .radio {
        margin: 0;
      }
      
      .viz-sidebar .radio label {
        padding: 10px 14px;
        margin: 3px 0;
        border-radius: 8px;
        display: flex;
        align-items: center;
        cursor: pointer;
        font-weight: 400;
        font-size: 14px;
        color: #444;
        transition: all 0.15s ease;
        border: 1px solid transparent;
      }
      
      .viz-sidebar .radio label input[type='radio'] {
        margin-right: 10px;
        margin-top: 0;
        flex-shrink: 0;
      }
      
      .viz-sidebar .radio label:hover {
        background-color: ", light_blue, ";
        border-color: #c8d4e8;
      }
      
      .viz-sidebar .radio input[type='radio']:checked + span {
        font-weight: 600;
        color: ", royal_blue, ";
      }
      
      .viz-sidebar label:has(input:checked) {
        background-color: ", light_blue, ";
        border-color: ", royal_blue, ";
        border-left: 3px solid ", royal_blue, ";
      }
      
      /* Select input styling */
      .viz-sidebar .selectize-input {
        border-radius: 8px;
        border: 1px solid #c8d4e8;
        padding: 10px 14px;
        font-size: 14px;
        box-shadow: none;
      }
      
      .viz-sidebar .selectize-input.focus {
        border-color: ", royal_blue, ";
        box-shadow: 0 0 0 3px rgba(65, 105, 225, 0.12);
      }
      
      /* Map container */
      .map-wrapper {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        border: 1px solid #e0e5ec;
      }
      
      .map-header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 15px;
        padding-bottom: 12px;
        border-bottom: 2px solid ", light_blue, ";
      }
      
      .map-header h4 {
        margin: 0;
        color: ", dark_blue, ";
        font-weight: 600;
        font-size: 18px;
      }
      
      /* Intro banner */
      .intro-banner {
        background: linear-gradient(135deg, ", royal_blue, " 0%, ", dark_blue, " 100%);
        color: white;
        padding: 25px 30px;
        border-radius: 12px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(65, 105, 225, 0.25);
      }
      
      .intro-banner h3 {
        color: white;
        margin: 0 0 10px 0;
        font-size: 24px;
        font-weight: 600;
      }
      
      .intro-banner p {
        margin: 0;
        opacity: 0.92;
        line-height: 1.6;
        font-size: 15px;
      }
      
      .stats-row {
        display: flex;
        gap: 15px;
        margin-top: 18px;
      }
      
      .stat-badge {
        background: rgba(255,255,255,0.18);
        padding: 8px 16px;
        border-radius: 20px;
        font-size: 13px;
        font-weight: 500;
      }
      
      /* Explanation box */
      .explanation-box {
        margin-top: 20px;
      }
      
      /* Helper text */
      .helper-text {
        background: ", light_blue, ";
        padding: 12px 15px;
        border-radius: 8px;
        margin-top: 15px;
        font-size: 13px;
        color: #555;
        line-height: 1.5;
      }
      
      .helper-text strong {
        color: ", royal_blue, ";
      }
      
      /* ========== HOME PAGE STYLING ========== */
      .home-hero {
        background: linear-gradient(135deg, ", royal_blue, " 0%, ", dark_blue, " 100%);
        color: white;
        padding: 40px;
        border-radius: 12px;
        margin-bottom: 30px;
        box-shadow: 0 4px 15px rgba(65, 105, 225, 0.25);
        text-align: center;
      }
      
      .home-hero h1 {
        color: white;
        margin: 0 0 15px 0;
        font-size: 32px;
        font-weight: 700;
      }
      
      .home-hero p {
        margin: 0;
        opacity: 0.95;
        line-height: 1.7;
        font-size: 16px;
        max-width: 800px;
        margin: 0 auto;
      }
      
      .home-section {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border: 1px solid #e0e5ec;
      }
      
      .home-section h3 {
        color: ", dark_blue, ";
        margin-top: 0;
        margin-bottom: 20px;
        padding-bottom: 12px;
        border-bottom: 2px solid ", light_blue, ";
        font-size: 20px;
      }
      
      .home-section p {
        line-height: 1.7;
        color: #444;
      }
      
      .image-row {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 40px;
        flex-wrap: wrap;
        margin: 25px 0;
      }
      
      .image-row img {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .feature-cards {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 20px;
        margin-top: 20px;
      }
      
      .feature-card {
        background: ", light_blue, ";
        border-radius: 10px;
        padding: 20px;
        border-left: 4px solid ", royal_blue, ";
      }
      
      .feature-card h4 {
        color: ", dark_blue, ";
        margin: 0 0 10px 0;
        font-size: 16px;
      }
      
      .feature-card p {
        margin: 0;
        font-size: 14px;
        color: #555;
        line-height: 1.6;
      }
      
      .data-sources {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 50px;
        flex-wrap: wrap;
        margin: 20px 0;
        padding: 20px;
        background: #f8f9fa;
        border-radius: 8px;
      }
      
      .data-sources img {
        max-height: 80px;
        opacity: 0.9;
      }
      
      /* ========== DATA EXPLORER TAB STYLING ========== */
      .data-intro {
        background: linear-gradient(135deg, ", royal_blue, " 0%, ", dark_blue, " 100%);
        color: white;
        padding: 25px 30px;
        border-radius: 12px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(65, 105, 225, 0.25);
      }
      
      .data-intro h3 {
        color: white;
        margin: 0 0 10px 0;
        font-size: 24px;
        font-weight: 600;
      }
      
      .data-intro p {
        margin: 0;
        opacity: 0.92;
        line-height: 1.6;
        font-size: 15px;
      }
      
      .data-section {
        background: white;
        border-radius: 12px;
        padding: 20px 25px;
        margin-bottom: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border: 1px solid #e0e5ec;
        border-left: 4px solid ", royal_blue, ";
      }
      
      .data-section-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 12px;
        padding-bottom: 12px;
        border-bottom: 1px solid ", light_blue, ";
        flex-wrap: wrap;
        gap: 10px;
      }
      
      .data-section-title {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .data-section-title h4 {
        margin: 0;
        font-size: 17px;
        font-weight: 600;
        color: ", dark_blue, ";
      }
      
      .data-section-meta {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .data-badge {
        background: ", light_blue, ";
        color: ", dark_blue, ";
        padding: 5px 12px;
        border-radius: 15px;
        font-size: 12px;
        font-weight: 500;
      }
      
      .data-section p.description {
        color: #666;
        margin: 0 0 15px 0;
        font-size: 14px;
        line-height: 1.5;
      }
      
      .download-btn {
        background: ", royal_blue, ";
        color: white;
        border: none;
        padding: 8px 16px;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      
      .download-btn:hover {
        background: ", dark_blue, ";
        color: white;
      }
      
      .table-container {
        border: 1px solid #e0e5ec;
        border-radius: 6px;
        overflow: hidden;
      }
    "))),
    
    # ==================== HOME TAB ====================
    tabPanel(
      "Home",
      fluidPage(
        style = "padding: 20px 15px;",
        
        # Hero banner
        div(class = "home-hero",
            h1("Melanoma Case Studies"),
            p("Welcome to our BIOL-185 project exploring melanoma incidence patterns across the United States. 
             This interactive dashboard visualizes county-level data on melanoma cases, UV exposure, 
             demographics, and healthcare access.")
        ),
        
        # Overview section
        div(class = "home-section",
            h3("Overview"),
            p("Melanoma is the most serious type of skin cancer, developing from the cells that give skin its color. 
             Below are examples of invasive melanoma and an overview of the general development process from the 
             epidermal region of the skin to the fourth stage, which involves complete invasion and spread to other organs."),
            div(class = "image-row",
                tags$img(src = "cs-Accuracy-Dermoscopic-Criteria-Diagnosis-Melanoma-Situ-600x400.jpg", 
                         width = "320px", alt = "Melanoma Examples"),
                tags$img(src = "melanoma-stages.jpeg", 
                         width = "320px", alt = "Melanoma Stages")
            )
        ),
        
        # Data Sources section
        div(class = "home-section",
            h3("Data Sources"),
            p("The data in this project comes from the National Cancer Institute (NCI) in conjunction with the 
             Centers for Disease Control and Prevention (CDC). All melanoma incidence data covers the years 2017-2021."),
            div(class = "data-sources",
                tags$img(src = "National_Cancer_Institute_logo.svg.png", alt = "NCI Logo"),
                tags$img(src = "CDC_logo.png", alt = "CDC Logo")
            )
        ),
        
        # Available Visualizations section
        div(class = "home-section",
            h3("Available Visualizations"),
            p("Navigate to the Visualizations tab to explore the following data layers:"),
            div(class = "feature-cards",
                div(class = "feature-card",
                    h4("Melanoma Cases by County"),
                    p("View the average annual count of invasive melanoma cases (2017-2021) for each county across the United States.")
                ),
                div(class = "feature-card",
                    h4("Melanoma Rate (Age-Adjusted)"),
                    p("Explore age-adjusted incidence rates per 100,000 population, accounting for differences in population age distributions.")
                ),
                div(class = "feature-card",
                    h4("UV Intensity Measurement"),
                    p("See UV radiation intensity (W/m²) by county, with data from the NCI GIS Portal covering 2020-2024.")
                ),
                div(class = "feature-card",
                    h4("Physician Availability"),
                    p("Examine healthcare access through the number of physicians (MDs) per 100,000 population by county.")
                ),
                div(class = "feature-card",
                    h4("Bivariate Analysis Maps"),
                    p("Explore relationships between multiple variables simultaneously using advanced 3×3 color schemes.")
                ),
                div(class = "feature-card",
                    h4("Statistical Analysis"),
                    p("Dive into regression models, Simpson's Paradox, and other statistical insights on the Statistical Analysis tab.")
                )
            )
        )
      )
    ),
    
    # ==================== VISUALIZATIONS TAB ====================
    tabPanel(
      "Visualizations",
      fluidPage(
        style = "padding: 20px 15px;",
        
        # Intro banner
        div(class = "intro-banner",
            h3("Interactive Melanoma Visualization"),
            p("Explore melanoma incidence patterns across the United States. Select a state and choose 
             from various data layers including case counts, incidence rates, UV exposure, 
             physician availability, and advanced bivariate analyses."),
            div(class = "stats-row",
                span(class = "stat-badge", "3,000+ Counties"),
                span(class = "stat-badge", "7 Data Layers"),
                span(class = "stat-badge", "2017-2021 Data")
            )
        ),
        
        # Main layout
        fluidRow(
          # Sidebar controls
          column(3,
                 div(class = "viz-sidebar",
                     
                     # State selection
                     div(class = "section-title",
                         "Select State"
                     ),
                     selectInput(
                       inputId = "state_select",
                       label = NULL,
                       choices = state_choices,
                       selected = "Alabama",
                       width = "100%"
                     ),
                     
                     div(class = "section-divider"),
                     
                     # Data layers
                     div(class = "section-title",
                         "Map Layer"
                     ),
                     
                     # Single variable maps
                     div(class = "layer-group",
                         div(class = "layer-group-label", "Single Variable"),
                         radioButtons(
                           inputId = "melanoma_view",
                           label = NULL,
                           choices = c(
                             "Melanoma Cases (Count)" = "count",
                             "Melanoma Rate (per 100k)" = "rate",
                             "UV Intensity (W/m²)" = "uv",
                             "Physician Availability" = "md_availability"
                           ),
                           selected = "count"
                         )
                     ),
                     
                     # Bivariate analysis maps
                     div(class = "layer-group",
                         div(class = "layer-group-label", "Bivariate Analysis"),
                         radioButtons(
                           inputId = "bivariate_view",
                           label = NULL,
                           choices = c(
                             "UV × Melanoma Rate" = "bivariate",
                             "UV × Melanoma (Risk-Adj)" = "bivariate_weighted",
                             "MD Access × Melanoma" = "bivariate_md"
                           ),
                           selected = character(0)
                         )
                     ),
                     
                     # Helper text
                     div(class = "helper-text",
                         tags$strong("Tip:"), " Bivariate maps show relationships between two variables. 
                Hover over counties for detailed information."
                     )
                 )
          ),
          
          # Main map panel
          column(9,
                 div(class = "map-wrapper",
                     div(class = "map-header",
                         h4("County-Level Data")
                     ),
                     leafletOutput("map", height = "550px")
                 ),
                 div(class = "explanation-box",
                     uiOutput("viz_explanation")
                 )
          )
        )
      )
    ),
    
    # ==================== DATA EXPLORER TAB ====================
    tabPanel(
      "Data Explorer",
      fluidPage(
        style = "padding: 20px 15px;",
        
        # Intro banner
        div(class = "data-intro",
            h3("Data Explorer"),
            p("Browse, search, filter, and download the raw datasets used in this project. 
             Each table is fully interactive - click column headers to sort, use the search box to find specific entries, 
             and download the complete dataset as a CSV file."),
            div(class = "stats-row",
                span(class = "stat-badge", "4 Datasets"),
                span(class = "stat-badge", "3,000+ Counties"),
                span(class = "stat-badge", "CSV Downloads")
            )
        ),
        
        # Melanoma Data Section
        div(class = "data-section",
            div(class = "data-section-header",
                div(class = "data-section-title",
                    h4("Melanoma Incidence Data")
                ),
                div(class = "data-section-meta",
                    span(class = "data-badge", textOutput("data_summary", inline = TRUE)),
                    downloadButton("download_data", "Download CSV", class = "download-btn")
                )
            ),
            p(class = "description", 
              "County-level melanoma incidence from the National Cancer Institute (2017-2021). Includes case counts and age-adjusted rates."),
            div(class = "table-container",
                reactableOutput("data_table", height = 300)
            )
        ),
        
        # UV Data Section
        div(class = "data-section",
            div(class = "data-section-header",
                div(class = "data-section-title",
                    h4("UV Exposure Data")
                ),
                div(class = "data-section-meta",
                    span(class = "data-badge", textOutput("uv_summary", inline = TRUE)),
                    downloadButton("download_uv", "Download CSV", class = "download-btn")
                )
            ),
            p(class = "description", 
              "UV radiation intensity (W/m²) by county from the NCI GIS Portal (2020-2024). Higher values indicate greater UV exposure."),
            div(class = "table-container",
                reactableOutput("uv_table", height = 300)
            )
        ),
        
        # Demographics Data Section
        div(class = "data-section",
            div(class = "data-section-header",
                div(class = "data-section-title",
                    h4("County Demographics")
                ),
                div(class = "data-section-meta",
                    span(class = "data-badge", textOutput("demographics_summary", inline = TRUE)),
                    downloadButton("download_demographics", "Download CSV", class = "download-btn")
                )
            ),
            p(class = "description", 
              "Population demographics by county including racial composition percentages. Used for risk-adjusted analyses."),
            div(class = "table-container",
                reactableOutput("demographics_table", height = 300)
            )
        ),
        
        # MD Availability Data Section
        div(class = "data-section",
            div(class = "data-section-header",
                div(class = "data-section-title",
                    h4("Physician Availability")
                ),
                div(class = "data-section-meta",
                    span(class = "data-badge", textOutput("md_summary", inline = TRUE)),
                    downloadButton("download_md", "Download CSV", class = "download-btn")
                )
            ),
            p(class = "description", 
              "Number of physicians (MDs) per 100,000 population by county. Indicates healthcare access and availability."),
            div(class = "table-container",
                reactableOutput("md_table", height = 300)
            )
        )
      )
    ),
    
    # ==================== STATISTICAL ANALYSIS TAB ====================
    tabPanel(
      "Statistical Analysis",
      fluidPage(
        tags$head(
          tags$style(HTML("
            .stat-card {
              padding: 20px;
              border-radius: 10px;
              color: white;
              text-align: center;
              margin-bottom: 20px;
              box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            }
            .stat-value {
              font-size: 36px;
              font-weight: bold;
              margin-bottom: 5px;
            }
            .stat-label {
              font-size: 14px;
              opacity: 0.9;
              text-transform: uppercase;
            }
            .insight-box {
              padding: 15px;
              margin-bottom: 15px;
              border-radius: 8px;
            }
            .section-header {
              color: #1976D2;
              font-weight: bold;
              margin-top: 30px;
              margin-bottom: 20px;
              padding-bottom: 10px;
              border-bottom: 2px solid #E0E0E0;
            }
            .info-box {
              background: #E1F5FE;
              border-left: 4px solid #039BE5;
              padding: 15px;
              margin: 15px 0;
              border-radius: 4px;
            }
          "))
        ),
        
        h2("Statistical Analysis of UV-Melanoma Relationship", 
           style = "color: #1565C0; font-weight: bold; text-align: center; margin-bottom: 30px;"),
        
        uiOutput("key_metrics"),
        br(),
        
        h3("1. Interactive Data Exploration", class = "section-header"),
        wellPanel(
          plotlyOutput("interactive_scatter", height = "500px"),
          p("Hover over points to see county details. Color indicates white population percentage.", 
            style = "text-align: center; color: #666; margin-top: 10px;")
        ),
        
        h3("2. Simpson's Paradox Revealed", class = "section-header"),
        wellPanel(
          plotOutput("simpsons_paradox", height = "500px"),
          HTML("<div class='alert alert-warning' style='margin-top: 15px;'>
                <strong>Key Finding:</strong> The overall UV-melanoma relationship appears negative (red dashed line), 
                but within demographic groups, it's positive. This is Simpson's Paradox - a statistical phenomenon 
                where trends reverse when data is aggregated.</div>")
        ),
        
        h3("3. Regression Analysis (Linear Model)", class = "section-header"),
        wellPanel(
          plotOutput("regression_visual", height = "400px"),
          HTML("<div class='info-box'>
                <strong>What is Linear Regression?</strong><br>
                We're using a mathematical equation to predict melanoma rates based on UV exposure and demographics:<br>
                <code>Melanoma Rate = β₀ + β₁(UV) + β₂(White%) + error</code><br><br>
                The bars show how much each factor increases melanoma rates. Error bars show uncertainty.</div>")
        ),
        
        h3("4. ANOVA - Analysis of Variance", class = "section-header"),
        wellPanel(
          plotOutput("anova_visual", height = "400px"),
          HTML("<div class='info-box'>
                <strong>What is ANOVA?</strong><br>
                ANOVA tells us how much of the variation in melanoma rates each factor explains. 
                Think of it like dividing a pie - how big is each factor's slice?<br><br>
                <strong>F-Statistics:</strong> Measure how much better each predictor is than random chance. 
                Bigger F = more important predictor.</div>")
        ),
        
        h3("5. Model Performance", class = "section-header"),
        wellPanel(
          plotOutput("model_comparison", height = "400px"),
          HTML("<div style='padding: 10px; background: #F5F5F5; border-radius: 5px; margin-top: 15px;'>
                <strong>Interpretation:</strong> Adding demographics to UV dramatically improves model performance. 
                The interaction model (UV × Demographics) performs best, suggesting UV effects vary by population composition.</div>")
        ),
        
        h3("6. Effect Size Analysis", class = "section-header"),
        wellPanel(
          plotOutput("effect_comparison", height = "350px"),
          HTML("<div style='padding: 10px; background: #E3F2FD; border-radius: 5px; margin-top: 15px;'>
                <strong>What are Standardized Effects?</strong><br>
                These show the relative importance of each factor when both are measured on the same scale. 
                A larger effect size means that factor has more influence on melanoma rates.</div>")
        ),
        
        h3("7. True UV Effect (Controlling for Demographics)", class = "section-header"),
        wellPanel(
          plotOutput("partial_plot", height = "450px"),
          HTML("<div class='alert alert-success' style='margin-top: 15px;'>
                <strong>What This Shows:</strong> After removing demographic influence (statistical control), 
                UV exposure shows a positive relationship with melanoma rates - exactly what we'd expect biologically. 
                This is the 'true' UV effect.</div>")
        ),
        
        h3("8. Model Predictions - How Accurate Are We?", class = "section-header"),
        wellPanel(
          plotlyOutput("residual_map", height = "500px"),
          HTML("<div class='info-box'>
                <strong>What are Model Predictions?</strong><br>
                Using our equation from the regression, we predict what each county's melanoma rate should be 
                based on its UV exposure and demographics. Points close to the diagonal line = good predictions. 
                Points far away = something else is affecting melanoma rates there (healthcare access, behaviors, etc.).</div>")
        ),
        
        h3("9. Model Diagnostics - Is Our Model Valid?", class = "section-header"),
        wellPanel(
          plotOutput("diagnostic_plots", height = "600px"),
          HTML("<div class='info-box'>
                <strong>Understanding Diagnostic Plots:</strong><br>
                <b>1. Linearity:</b> Checks if the relationship is straight-line (linear) vs curved<br>
                <b>2. Normality:</b> Checks if prediction errors follow a bell curve (important for statistics)<br>
                <b>3. Equal Variance:</b> Checks if our predictions are equally good across all values<br>
                <b>4. Influential Points:</b> Identifies counties that might be skewing our results<br><br>
                Our model passes most checks but shows some counties with unusual patterns.</div>")
        ),
        
        h3("10. Summary of Findings", class = "section-header"),
        wellPanel(
          uiOutput("insights_summary")
        ),
        
        h3("11. Counties of Interest", class = "section-header"),
        wellPanel(
          h4("Counties with Largest Prediction Errors", style = "color: #424242; text-align: center;"),
          DTOutput("county_table"),
          HTML("<div style='padding: 10px; background: #F5F5F5; border-radius: 5px; margin-top: 15px;'>
                <strong>Why These Matter:</strong> Counties with large residuals (prediction errors) may have unique 
                factors not captured by our model - perhaps differences in healthcare access, screening practices, 
                sun protection behaviors, or occupational exposures.</div>")
        ),
        
        br(), br()
      )
    )
  )
)