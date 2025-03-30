
library(shinydashboard)

library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(CausalImpact)
library(forecast)
library(plotly)
library(corrplot)

# Load your dataset (replace this with your actual data path)
happiness_df <- read.csv("data/world_happiness.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Happiness Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-line")),
      menuItem("EDA", tabName = "eda", icon = icon("search")),
      menuItem("CDA", tabName = "cda", icon = icon("cogs")),
      menuItem("Geospatial", tabName = "geospatial", icon = icon("globe")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Time Series Tab
      tabItem(tabName = "time_series",
              fluidPage(
                # UI components for Time Series
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("country", "Search and Select Countries:", 
                                   choices = unique(happiness_df$country), 
                                   multiple = TRUE, 
                                   options = list(maxItems = 5, placeholder = "Select countries")),
                    sliderInput("year_range", "Select Year Range:", 
                                min = 2014, max = 2024, value = c(2014, 2024),
                                step = 1, animate = TRUE)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Trend", plotlyOutput("trend_plot")),
                      tabPanel("Forecast", plotlyOutput("forecast_plot")),
                      tabPanel("Causal Impact", plotOutput("causal_impact_plot"))
                    )
                  )
                )
              )
      ),
      
      # EDA Tab
      tabItem(tabName = "eda",
              fluidPage(
                titlePanel("Exploratory Data Analysis"),
                plotOutput("eda_plot"),
                dataTableOutput("eda_table")
              )
      ),
      
      # CDA Tab
      tabItem(tabName = "cda",
              fluidPage(
                titlePanel("Causal Data Analysis"),
                tabsetPanel(
                  tabPanel("Correlation Heatmap", plotlyOutput("corr_plot")),
                  tabPanel("Feature Importance", plotlyOutput("feature_importance_plot")),
                  tabPanel("Stationarity Check", plotOutput("stationary_plot"))
                )
              )
      ),
      
      # Geospatial Tab
      tabItem(tabName = "geospatial",
              fluidPage(
                titlePanel("Geospatial Analysis"),
                plotOutput("geo_plot")
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidPage(
                titlePanel("About"),
                p("This is a dashboard to analyze happiness data across different countries and years.")
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive function to filter data based on selected country and year range
  filtered_data <- reactive({
    happiness_df %>%
      filter(country %in% input$country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  # Generate Trend Plot
  output$trend_plot <- renderPlotly({
    filtered <- filtered_data()  # Get filtered data
    
    # Check if filtered data is empty
    if (nrow(filtered) > 0) {
      plot_ly(data = filtered, x = ~year, y = ~ladder_score, color = ~country, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = "Happiness Trend",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Happiness Score"),
          legend = list(title = list(text = "Country"))
        )
    } else {
      plot_ly() %>%
        add_text(text = "No data available for the selected country/year range", x = 1, y = 1, showlegend = FALSE) %>%
        layout(title = "No Data Available", xaxis = list(title = "Year"), yaxis = list(title = "Happiness Score"))
    }
  })
    
  

  
  # Generate Forecast Plot
  output$forecast_plot <- renderPlotly({
    filtered <- filtered_data()  # Get user-selected data
    forecast_plots <- list()
    
    p <- plot_ly()  # Initialize empty plot
    
    for (country_name in unique(filtered$country)) {
      country_data <- filtered %>% filter(country == country_name)
      ts_data <- ts(country_data$ladder_score, start = min(country_data$year), frequency = 1)
      
      if (length(ts_data) > 5) {  # Ensure enough data points
        model <- auto.arima(ts_data)
        forecast_data <- forecast(model, h = 5)  # Forecast next 5 years
        
        # Add observed data
        p <- p %>%
          add_lines(x = country_data$year, y = ts_data, name = paste(country_name, "- Observed"), line = list(color = "blue"))
        
        # Add forecasted data
        future_years <- seq(max(country_data$year) + 1, max(country_data$year) + 5, by = 1)
        p <- p %>%
          add_lines(x = future_years, y = forecast_data$mean, name = paste(country_name, "- Forecasted"), 
                    line = list(dash = "dash", color = "red"))
      }
    }
    
    p %>%
      layout(title = "Happiness Forecast Comparison",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Happiness Score"),
             legend = list(title = list(text = "Country Forecasts")))
  })
  
  
  # Generate Causal Impact Plot
  output$causal_impact_plot <- renderPlot({
    pre_period <- c(2014, 2019)
    post_period <- c(2020, 2024)
    
    impact_plots <- list()  # Store plots for each country
    
    for (country_name in input$country) {
      impact_data <- happiness_df %>%
        filter(country == country_name, year >= 2014 & year <= 2024) %>%
        na.omit()  # Remove NAs if any
      
      if (nrow(impact_data) > 5) {  # Ensure enough data points
        ts_data <- zoo(impact_data$ladder_score, order.by = impact_data$year)
        impact <- CausalImpact(ts_data, pre.period = pre_period, post.period = post_period)
        
        impact_df <- data.frame(
          year = impact_data$year,
          actual = impact$series$response,
          predicted = impact$series$point.pred,
          lower = impact$series$point.pred.lower,
          upper = impact$series$point.pred.upper
        )
        
        impact_plots[[country_name]] <- ggplot(impact_df, aes(x = year)) +
          geom_line(aes(y = actual, color = "Actual"), size = 1) +
          geom_line(aes(y = predicted, color = "Predicted"), linetype = "dashed", size = 1) +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "gray") +
          labs(title = paste("Causal Impact -", country_name), x = "Year", y = "Happiness Score") +
          scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
          theme_minimal()
      }
    }
    
    # Combine all country plots into facets
    if (length(impact_plots) > 0) {
      do.call(gridExtra::grid.arrange, c(impact_plots, ncol = 2))
    } else {
      ggplot() + ggtitle("Not enough data for CausalImpact analysis")
    }
  })
  
  output$corr_plot <- renderPlotly({
    library(ggcorrplot)
    library(plotly)
    
    # Select only numeric columns
    num_data <- world_happiness_data[, c("ladder_score", "economy_score",
                                         "social_score", "lifeexpectancy_score",
                                         "freedom_score", "generosity_score",
                                         "corrperception_score", "residual_score")]
    
    # Remove rows with NA or Inf values
    num_data <- num_data[complete.cases(num_data), ]
    num_data <- num_data[apply(num_data, 1, function(row) all(is.finite(row))), ]
    
    # Compute correlation
    correlation_matrix <- cor(num_data, use = "pairwise.complete.obs")
    
    # Create heatmap plot using ggcorrplot
    heatmap_plot <- ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = FALSE)
    
    # Convert to plotly for interactivity and customize hover data
    plotly_heatmap <- ggplotly(heatmap_plot) %>%
      layout(
        title = "Correlation Heatmap",
        xaxis = list(title = "Features", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "Features", showgrid = FALSE, zeroline = FALSE),
        margin = list(t = 50, b = 50, l = 50, r = 50),  # Adjust margin to avoid overlap
        coloraxis = list(colorscale = 'RdYlBu', colorbar = list(title = "Correlation", tickvals = seq(-1, 1, 0.5)))
      ) %>%
      add_trace(
        type = "heatmap",
        z = correlation_matrix,
        colorscale = "RdYlBu",
        hoverinfo = "text",
        text = ~paste("Correlation: ", round(correlation_matrix, 2))  # Display value only on hover
      )
    
    plotly_heatmap
  })
  

  
  
  
  output$feature_importance_plot <- renderPlotly({
    library(randomForest)
    library(vip)
    library(plotly)
    
    # Select relevant columns (excluding categorical ones)
    feature_data <- world_happiness_data[, c("ladder_score", "economy_score",
                                             "social_score", "lifeexpectancy_score",
                                             "freedom_score", "generosity_score",
                                             "corrperception_score", "residual_score")]
    
    # Remove rows with NA values
    feature_data <- na.omit(feature_data)
    
    # Train Random Forest model
    rf_model <- randomForest(ladder_score ~ ., data = feature_data, importance = TRUE, na.action = na.omit)
    
    # Create Feature Importance Plot
    importance_data <- vip(rf_model, geom = "point")
    
    # Make it interactive
    ggplotly(importance_data) %>%
      layout(
        title = "Feature Importance",
        xaxis = list(title = "Variables"),
        yaxis = list(title = "Importance")
      )
  })
  
  output$stationary_plot <- renderPlot({
    library(tseries)
    library(ggplot2)
    library(zoo)
    
    # Choose a country, e.g., United States
    country_data <- subset(world_happiness_data, country == "Mexico")  # Example country
    
    # Check if there's data for the country
    if (nrow(country_data) == 0) {
      return(NULL)  # If no data for the country, return nothing
    }
    
    # Create time series object
    time_series <- ts(country_data$ladder_score, start = min(country_data$year), frequency = 1)
    
    # Check if time series has enough data
    if (length(time_series) < 2) {
      return(NULL)  # If not enough data, return nothing
    }
    
    # Perform ADF test
    adf_result <- adf.test(time_series)
    
    # Rolling mean and std calculation
    df <- data.frame(Year = country_data$year,
                     Ladder_Score = time_series,
                     Rolling_Mean = rollmean(time_series, k = 3, fill = NA),
                     Rolling_Std = rollapply(time_series, width = 3, FUN = sd, fill = NA))
    
    # Plot the rolling mean/std along with ladder score
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Ladder_Score, color = "Ladder Score"), linewidth = 1) +  # Changed size to linewidth
      geom_line(aes(y = Rolling_Mean, color = "Rolling Mean"), linetype = "dashed", linewidth = 1) +  # Changed size to linewidth
      geom_line(aes(y = Rolling_Std, color = "Rolling Std"), linetype = "dotted", linewidth = 1) +  # Changed size to linewidth
      labs(title = paste("Stationary Check - ADF p-value:", round(adf_result$p.value, 3)),
           y = "Ladder Score / Rolling Stats") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
