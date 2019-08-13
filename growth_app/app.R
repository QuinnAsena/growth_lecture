# Author: Quinn Asena
library(shiny)
library(grDevices)
library(knitr)
library(tidyverse)
library(data.table)
#setwd("C:/Users/qase352/Dropbox/QuinnAsenaPhD/R/pop_growth_lecture/growth_app")
ui <- fluidPage(
  h1("Deterministic Discrete Population Growth Models"), #Main page title
  tabsetPanel(
    ##################################################
    #####     First tab for exp growth model     #####
    ##################################################
    tabPanel("r exponential model",
             h1(includeHTML("exp_model_title_text.html")),
             sidebarLayout(position = c("left"),
                           sidebarPanel(
                             h4("Model Parameters"),
                             
                             sliderInput(inputId = "r_exp_N",
                                         label = "Starting Population (N)",
                                         value = 10, min = 0, max = 1000),
                             sliderInput(inputId = "r_exp",
                                         label = "Discrete Growth Rate (r)",
                                         value = 0.05, min = -1, max = 2, step = 0.01, round = FALSE),
                             sliderInput(inputId = "r_exp_t",
                                         label = "Time (t)",
                                         value = 300, min = 0, max = 1000),
                             h6(includeHTML("exp_equation_guide.html"))
                           ), #close sidebarPanel
                           
                           mainPanel(
                             plotOutput("exp_pop_plot"),
                             plotOutput("exp_pop_roc_plot")
                           )
             )# close sidebarLayout
    ), #close tabPanel1
##################################################
#####     Second tab for r growth model      #####
##################################################
    tabPanel("r discrete model",
             h1(includeHTML("r_model_title_text.html")),
  sidebarLayout(position = c("left"),
                sidebarPanel(
                  h4("Model Parameters"),
                
                sliderInput(inputId = "r_N",
                            label = "Starting Population (N)",
                            value = 10, min = 0, max = 999),
                sliderInput(inputId = "r_K",
                            label = "Carrying Capacity (K)",
                            value = 1000, min = 0, max = 1000),
                sliderInput(inputId = "r",
                            label = "Discrete Growth Rate (r)",
                            value = 0.05, min = -2, max = 4, step = 0.01, round = FALSE),
                sliderInput(inputId = "r_t",
                            label = "Time (t)",
                            value = 300, min = 0, max = 1000),
                h6(includeHTML("r_equation_guide.html"))
                ), #close sidebarPanel
                
            mainPanel(
            plotOutput("pop_plot"),
            plotOutput("pop_roc_plot")
            )
  )# close sidebarLayout
    ), #close tabPanel1
##################################################
#####     Third tab for stochastic model     #####
##################################################
tabPanel("r stochastic model",
         h1(includeHTML("r_model_title_text.html")),
         sidebarLayout(position = c("left"),
                       sidebarPanel(
                         h4("Model Parameters"),
                         
                         sliderInput(inputId = "r_stoch_N",
                                     label = "Starting Population (N)",
                                     value = 10, min = 0, max = 999),
                         sliderInput(inputId = "r_stoch_K",
                                     label = "Carrying Capacity (K)",
                                     value = 1000, min = 0, max = 1000),
                         sliderInput(inputId = "r_stoch_t",
                                     label = "Time (t)",
                                     value = 300, min = 0, max = 1000),
                         sliderInput(inputId = "r_mu",
                                     label = "Mean Discrete Growth Rate (r)",
                                     value = 0.05, min = -1, max = 1, step = 0.01, round = FALSE),
                         sliderInput(inputId = "r_sd",
                                     label = "r SD",
                                     value = 0.15, min = 0, max = 0.5, step = 0.01),
                         sliderInput(inputId = "no_runs",
                                     label = "Number of runs",
                                     value = 10, min = 1, max = 10, step = 1),
                         h6(includeHTML("stoch_r_equation_guide.html"))
                       ), #close sidebarPanel
                       
                       mainPanel(
                         plotOutput("stoch_pop_plot"),
                         plotOutput("stoch_pop_roc_plot")
                       )
         )# close sidebarLayout
), #close tabPanel1

##################################################
#####   Fourth tab for model descriptions     #####
##################################################
tabPanel("Lecture slides",
         tags$iframe(style="height:800px; width:100%;",  src="pop_growth.pdf"))# close tabPanel3

  ) #close tabsetPanel
) #close fluidPage
##################################################
#####              Functions                 #####
##################################################
# exponential model
r_exp.fun <- function(r = 0.05, N_pop = 10, t = 100)
{
  N <- vector("numeric", length = t)
  N[1] <- N_pop
  for (i in 2:t)
  {
    N[i] <- N[i-1] + (N[i-1] * r)
  }
  return(N)
}

##### function for r model (Nt+1 = Nt + rdNt * (1-Nt/K))
r_logistic.fun <- function(r = 0.05, K = 100, N_pop = 10, t = 100)
{
  N <- vector("numeric", length = t)
  N[1] <- N_pop
  for (i in 2:t)
  {
    N[i] <- N[i-1] + (r * N[i-1] * (1 - N[i-1] / K))   # version 2 with R
  }
  N <- ifelse(N <= 0, 0, N)
  return(N)
}
##### stochastic model
r_stochastic.fun <- function(r_mu = 0.05, r_sd = 0.15, K = 1000, N_pop = 10, t = 250)
{
  pop_df <- data.frame(r_vec = rnorm(t, r_mu, r_sd),
                       N = N_pop,
                       time = 1:t)
  
  for (i in 2:length(pop_df$N))
  {
    pop_df$N[i] <- pop_df$N[i-1] + (pop_df$r_vec[i-1] * pop_df$N[i-1] * (1 - pop_df$N[i-1] / K))   # version 2 with R
  }
  pop_df$N <- ifelse(pop_df$N <= 0, 0, pop_df$N)
  
  # pop_df <- pop_df %>% 
  #   mutate(stoch_pop = N + (r_vec * N) * (1-N / K),
  #          time = 1:n())
  # pop_df$stoch_pop[1] <- N_pop
  
  return(pop_df)
}
##### Function for rate of change (Nt - Nt+1)
pop_roc_fun <- function(pop_vec){
  for (i in length(pop_vec):2)
  {
    pop_vec[i] <- pop_vec[i] - pop_vec[i-1]
  }
  pop_vec[1] <- NA
  return(pop_vec)
}
##################################################
#####                Outputs                 #####
##################################################
# Create reactive data from function so input values are the same for both following plots and update dynamically
server <- function(input, output){
  
  r_exponential_data <- reactive({
    r_exp.fun(r = input$r_exp, N_pop = input$r_exp_N, t=input$r_exp_t)
  })
  # Plot logistic function  
  output$exp_pop_plot <- renderPlot(
    if (input$r > 2.45){
      plot(r_exponential_data(),
           type = 'l', lwd = 1, col = 'red', ylab = 'Population size (N)', xlab='Time', 
           cex.lab = 1, cex.axis = 1)
    } else {
      plot(r_exponential_data(),
           type = 'l', lwd = 1, col = 'darkblue', ylab = 'Population size (N)', xlab='Time', 
           cex.lab = 1, cex.axis = 1)
    }
  )
  
  # Plot rate of change function  
  output$exp_pop_roc_plot <- renderPlot(
    plot(y = pop_roc_fun(r_exponential_data()), x = r_exponential_data(),
         type = 'l', lwd = 1, col = 'darkblue', ylab = expression(paste("Change in population (",Delta, "Nt)")), xlab='Population size (N)', 
         cex.lab = 1, cex.axis = 1))
  
##################################################  
  r_logistic_data <- reactive({
    r_logistic.fun(r = input$r, K = input$r_K, N_pop = input$r_N, t=input$r_t)
  })
  # Plot logistic function  
  output$pop_plot <- renderPlot(
    if (input$r > 2.45){
    plot(r_logistic_data(),
         type = 'l', lwd = 1, col = 'red', ylab = 'Population size (N)', xlab='Time', 
         cex.lab = 1, cex.axis = 1)
    } else {
      plot(r_logistic_data(),
           type = 'l', lwd = 1, col = 'darkblue', ylab = 'Population size (N)', xlab='Time', 
           cex.lab = 1, cex.axis = 1)
    }
  )
    
  # Plot rate of change function  
  output$pop_roc_plot <- renderPlot(
    plot(y = pop_roc_fun(r_logistic_data()), x = r_logistic_data(),
         type = 'l', lwd = 1, col = 'darkblue', ylab = expression(paste("Change in population (",Delta, "Nt)")), xlab='Population size (N)', 
         cex.lab = 1, cex.axis = 1))
##################################################
  # Create reactive data from function so input values are the same for both following plots and update dynamically
  r_stochastic_data <- reactive({rbindlist(
    replicate(input$no_runs,
    r_stochastic.fun(r_mu = input$r_mu, r_sd = input$r_sd, K = input$r_stoch_K, N_pop = input$r_stoch_N, t=input$r_stoch_t),
    simplify = FALSE),
    idcol = "run_id")
  })
  #r_stochastic_data() <- rbindlist(r_stochastic_data(), idcol = "run_id")
  # Plot logistic function  
  # output$stoch_pop_plot <- renderPlot(
  #   if (input$r > 2.45){
  #     plot(r_stochastic_data(),
  #          type = 'l', lwd = 1, col = 'red', ylab = 'Population size (N)', xlab='Time', 
  #          cex.lab = 1, cex.axis = 1)
  #   } else {
  #     plot(r_stochastic_data(),
  #          type = 'l', lwd = 1, col = 'darkblue', ylab = 'Population size (N)', xlab='Time', 
  #          cex.lab = 1, cex.axis = 1)
  #   }
  # )
  
  # Plot rate of change function  
  output$stoch_pop_plot <- renderPlot(
    ggplot(r_stochastic_data(), aes(x = time, y = N, group = run_id, colour = run_id))+
      geom_line(show.legend = FALSE)+
      ylab("Population size(N)")+
      xlab("time")+
      theme_minimal())
}

shinyApp(ui = ui, server = server)

#pagedown::chrome_print("pop_growth.Rmd", "growth_app/www/pop_growth.pdf")
