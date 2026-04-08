# Author: Quinn Asena
library(shiny)
library(ggplot2)

if (!file.exists("exp_model_title_text.html")) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# --- Growth model functions ---------------------------------------------------

# Discrete exponential growth: N[t+1] = N[t] + r * N[t]
exp_growth <- function(r = 0.05, N_pop = 10, t = 100) {
  N <- numeric(t)
  N[1] <- N_pop
  for (i in 2:t) {
    N[i] <- N[i - 1] + N[i - 1] * r
  }
  N
}

# Discrete logistic growth: N[t+1] = N[t] + r * N[t] * (1 - N[t] / K)
logistic_growth <- function(r = 0.05, K = 1000, N_pop = 10, t = 100) {
  N <- numeric(t)
  N[1] <- N_pop
  for (i in 2:t) {
    N[i] <- N[i - 1] + r * N[i - 1] * (1 - N[i - 1] / K)
  }
  N <- ifelse(N <= 0, 0, N)
  N
}

# Stochastic logistic growth: r drawn from N(r_mu, r_sd) each time-step
stochastic_growth <- function(r_mu = 0.05, r_sd = 0.15, K = 1000,
                              N_pop = 10, t = 250) {
  pop_df <- data.frame(
    r_vec = rnorm(t, r_mu, r_sd),
    N     = N_pop,
    time  = seq_len(t)
  )
  for (i in 2:nrow(pop_df)) {
    pop_df$N[i] <- pop_df$N[i - 1] +
      pop_df$r_vec[i - 1] * pop_df$N[i - 1] * (1 - pop_df$N[i - 1] / K)
  }
  pop_df$N <- ifelse(pop_df$N <= 0, 0, pop_df$N)
  pop_df
}

# Rate of change between consecutive time-steps
rate_of_change <- function(pop_vec) c(NA, diff(pop_vec))

# --- Shared ggplot theme ------------------------------------------------------

theme_app <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold")
    )
}

# --- UI -----------------------------------------------------------------------

ui <- fluidPage(
  withMathJax(),
  titlePanel("Discrete Population Growth Models"),
  tabsetPanel(

    # Tab 1 — Exponential growth -----------------------------------------------
    tabPanel(
      "Exponential model",
      includeHTML("exp_model_title_text.html"),
      sidebarLayout(
        sidebarPanel(
          h4("Model Parameters"),
          sliderInput("r_exp_N", "Starting Population (N)",
                      value = 10, min = 0, max = 1000),
          sliderInput("r_exp", "Discrete Growth Rate (r)",
                      value = 0.05, min = -1, max = 2, step = 0.01),
          sliderInput("r_exp_t", "Time (t)",
                      value = 300, min = 1, max = 1000),
          includeHTML("exp_equation_guide.html")
        ),
        mainPanel(
          plotOutput("exp_pop_plot"),
          plotOutput("exp_pop_roc_plot")
        )
      )
    ),

    # Tab 2 — Logistic growth --------------------------------------------------
    tabPanel(
      "Logistic model",
      includeHTML("r_model_title_text.html"),
      sidebarLayout(
        sidebarPanel(
          h4("Model Parameters"),
          sliderInput("r_N", "Starting Population (N)",
                      value = 10, min = 0, max = 999),
          sliderInput("r_K", "Carrying Capacity (K)",
                      value = 1000, min = 1, max = 1000),
          sliderInput("r", "Discrete Growth Rate (r)",
                      value = 0.05, min = -2, max = 4, step = 0.01),
          sliderInput("r_t", "Time (t)",
                      value = 300, min = 1, max = 1000),
          includeHTML("r_equation_guide.html")
        ),
        mainPanel(
          plotOutput("pop_plot"),
          plotOutput("pop_roc_plot")
        )
      )
    ),

    # Tab 3 — Stochastic growth ------------------------------------------------
    tabPanel(
      "Stochastic model",
      includeHTML("stoch_r_model_title_text.html"),
      sidebarLayout(
        sidebarPanel(
          h4("Model Parameters"),
          sliderInput("r_stoch_N", "Starting Population (N)",
                      value = 10, min = 0, max = 999),
          sliderInput("r_stoch_K", "Carrying Capacity (K)",
                      value = 1000, min = 1, max = 1000),
          sliderInput("r_stoch_t", "Time (t)",
                      value = 300, min = 1, max = 1000),
          sliderInput("r_mu", "Mean Discrete Growth Rate (r)",
                      value = 0.05, min = -1, max = 1, step = 0.01),
          sliderInput("r_sd", "Standard Deviation of r",
                      value = 0.15, min = 0, max = 0.5, step = 0.01),
          sliderInput("no_runs", "Number of runs",
                      value = 10, min = 1, max = 10, step = 1),
          includeHTML("stoch_r_equation_guide.html")
        ),
        mainPanel(
          plotOutput("stoch_pop_plot"),
          plotOutput("stoch_pop_roc_plot")
        )
      )
    ),

    # Tab 4 — Lecture slides ---------------------------------------------------
    tabPanel(
      "Lecture slides",
      tags$iframe(
        style = "height:800px; width:100%; border:none;",
        src = "pop_growth.html"
      )
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output) {


  # Exponential model ----------------------------------------------------------

  r_exponential_data <- reactive({
    req(input$r_exp_t >= 1)
    exp_growth(r = input$r_exp, N_pop = input$r_exp_N, t = input$r_exp_t)
  })

  output$exp_pop_plot <- renderPlot({
    N <- r_exponential_data()
    df <- data.frame(time = seq_along(N), N = N)
    ggplot(df, aes(x = time, y = N)) +
      geom_line(colour = "#0077b6", linewidth = 0.9) +
      labs(x = "Time", y = "Population size (N)") +
      theme_app()
  })

  output$exp_pop_roc_plot <- renderPlot({
    N <- r_exponential_data()
    df <- data.frame(N = N, dN = rate_of_change(N))
    ggplot(df, aes(x = N, y = dN)) +
      geom_line(colour = "#0077b6", linewidth = 0.9) +
      labs(x = "Population size (N)",
           y = expression(paste("Change in population (", Delta, "N)"))) +
      theme_app()
  })

  # Logistic model -------------------------------------------------------------

  r_logistic_data <- reactive({
    req(input$r_t >= 1)
    logistic_growth(r = input$r, K = input$r_K, N_pop = input$r_N, t = input$r_t)
  })

  output$pop_plot <- renderPlot({
    N <- r_logistic_data()
    df <- data.frame(time = seq_along(N), N = N)
    line_colour <- if (input$r > 2.45) "#d62828" else "#0077b6"
    ggplot(df, aes(x = time, y = N)) +
      geom_line(colour = line_colour, linewidth = 0.9) +
      labs(x = "Time", y = "Population size (N)") +
      theme_app()
  })

  output$pop_roc_plot <- renderPlot({
    N <- r_logistic_data()
    df <- data.frame(N = N, dN = rate_of_change(N))
    ggplot(df, aes(x = N, y = dN)) +
      geom_line(colour = "#0077b6", linewidth = 0.9) +
      labs(x = "Population size (N)",
           y = expression(paste("Change in population (", Delta, "N)"))) +
      theme_app()
  })

  # Stochastic model -----------------------------------------------------------

  r_stochastic_data <- reactive({
    req(input$r_stoch_t >= 1)
    runs <- replicate(
      input$no_runs,
      stochastic_growth(
        r_mu  = input$r_mu,
        r_sd  = input$r_sd,
        K     = input$r_stoch_K,
        N_pop = input$r_stoch_N,
        t     = input$r_stoch_t
      ),
      simplify = FALSE
    )
    for (i in seq_along(runs)) {
      runs[[i]]$run_id <- factor(i)
    }
    do.call(rbind, runs)
  })

  output$stoch_pop_plot <- renderPlot({
    ggplot(r_stochastic_data(), aes(x = time, y = N,
                                     group = run_id, colour = run_id)) +
      geom_line(linewidth = 0.7, show.legend = FALSE) +
      scale_colour_viridis_d(option = "plasma") +
      labs(x = "Time", y = "Population size (N)") +
      theme_app()
  })

  output$stoch_pop_roc_plot <- renderPlot({
    stoch_data <- r_stochastic_data()
    stoch_data$dN <- ave(stoch_data$N, stoch_data$run_id,
                         FUN = function(x) c(NA, diff(x)))
    ggplot(stoch_data, aes(x = N, y = dN,
                            group = run_id, colour = run_id)) +
      geom_line(linewidth = 0.7, show.legend = FALSE) +
      scale_colour_viridis_d(option = "plasma") +
      labs(x = "Population size (N)",
           y = expression(paste("Change in population (", Delta, "N)"))) +
      theme_app()
  })
}

shinyApp(ui = ui, server = server)
