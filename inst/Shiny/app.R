# --- Install Required Packages (if needed) ---
# install.packages(c("shiny", "ggplot2", "stats4"))
# install.packages("univariateML") # Used for easy MLE estimates

library(shiny)
library(ggplot2)
library(univariateML) # Provides ml... functions for MLE

# --- Define the Shiny App UI ---
ui <- fluidPage(

  # App title
  titlePanel("Maximum Likelihood Estimation for Univariate Distributions"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      # Select Distribution
      selectInput(
        inputId = "distribution",
        label = "Choose a Distribution:",
        choices = c(
          "Normal" = "norm",
          "Exponential" = "exp",
          "Gamma" = "gamma",
          "Poisson" = "pois",
          "Binomial" = "binom"
        ),
        selected = "norm"
      ),

      # Slider for Sample Size
      sliderInput(
        inputId = "n_samples",
        label = "Sample Size (n):",
        min = 10,
        max = 500,
        value = 100,
        step = 10
      ),

      # Conditional inputs for true parameters
      uiOutput("param_inputs"),

      # Action button to regenerate data
      actionButton("regenerate", "Generate New Sample", icon = icon("refresh"))
    ),

    # Main panel for outputs
    mainPanel(

      # Tabset for plot and estimates
      tabsetPanel(
        tabPanel(
          "Data & MLE Fit Plot",
          h4("Histogram of Generated Data with Fitted Density"),
          plotOutput("data_plot")
        ),
        tabPanel(
          "Maximum Likelihood Estimates",
          h4("True vs. Estimated Parameters"),
          tableOutput("mle_table"),
          h4("Log-Likelihood Value"),
          verbatimTextOutput("log_likelihood")
        )
      )
    )
  )
)

# --- Define the Shiny App Server ---
server <- function(input, output, session) {

  # Reactive value to store the simulated data
  data_reac <- reactiveVal(NULL)

  # --- 1. Dynamic UI for Parameter Inputs ---
  output$param_inputs <- renderUI({
    req(input$distribution)
    dist <- input$distribution

    # Define the parameter inputs based on the selected distribution
    if (dist == "norm") {
      list(
        numericInput("mu", "True Mean (μ):", value = 0, step = 0.5),
        numericInput("sigma", "True SD (σ):", value = 1, min = 0.1, step = 0.1)
      )
    } else if (dist == "exp") {
      numericInput("rate", "True Rate (λ):", value = 0.5, min = 0.1, step = 0.1)
    } else if (dist == "gamma") {
      list(
        numericInput("shape", "True Shape:", value = 2, min = 0.1, step = 0.1),
        numericInput("rate_g", "True Rate:", value = 0.5, min = 0.1, step = 0.1)
      )
    } else if (dist == "pois") {
      numericInput("lambda", "True Lambda (λ):", value = 5, min = 0.1, step = 0.5)
    } else if (dist == "binom") {
      list(
        numericInput("size", "True Trials (k):", value = 10, min = 1, step = 1),
        sliderInput("prob", "True Probability (p):", min = 0.1, max = 0.9, value = 0.5, step = 0.05)
      )
    }
  })

  # --- 2. Data Simulation (Triggers on button press) ---
  observeEvent(input$regenerate, {

    dist <- input$distribution
    n <- input$n_samples

    # Use tryCatch to handle potential errors in parameter input
    sim_data <- tryCatch({

      if (dist == "norm") {
        rnorm(n, mean = input$mu, sd = input$sigma)
      } else if (dist == "exp") {
        rexp(n, rate = input$rate)
      } else if (dist == "gamma") {
        rgamma(n, shape = input$shape, rate = input$rate_g)
      } else if (dist == "pois") {
        rpois(n, lambda = input$lambda)
      } else if (dist == "binom") {
        rbinom(n, size = input$size, prob = input$prob)
      }
    }, error = function(e) {
      # Return NULL or a message if parameters are invalid
      showNotification(paste("Error generating data:", e$message), type = "error")
      return(NULL)
    })

    data_reac(sim_data)
  }, ignoreNULL = FALSE, once = TRUE) # Run once on startup, then on button press

  # --- 3. Maximum Likelihood Estimation ---
  mle_results <- reactive({
    req(data_reac())
    dist <- input$distribution
    data <- data_reac()

    tryCatch({
      if (dist == "norm") {
        # mlnorm returns a list of two parameters: mean and sd
        mle_fit <- mlnorm(data)
        est_params <- c("Mean" = unname(mle_fit[1]), "SD" = unname(mle_fit[2]))
        true_params <- c("Mean" = input$mu, "SD" = input$sigma)
        log_L <- sum(dnorm(data, mean = est_params[1], sd = est_params[2], log = TRUE))
      } else if (dist == "exp") {
        mle_fit <- mlexp(data)
        est_params <- c("Rate (λ)" = unname(mle_fit[1]))
        true_params <- c("Rate (λ)" = input$rate)
        log_L <- sum(dexp(data, rate = est_params[1], log = TRUE))
      } else if (dist == "gamma") {
        mle_fit <- mlgamma(data)
        est_params <- c("Shape" = unname(mle_fit[1]), "Rate" = unname(mle_fit[2]))
        true_params <- c("Shape" = input$shape, "Rate" = input$rate_g)
        log_L <- sum(dgamma(data, shape = est_params[1], rate = est_params[2], log = TRUE))
      } else if (dist == "pois") {
        mle_fit <- mlpois(data)
        est_params <- c("Lambda (λ)" = unname(mle_fit[1]))
        true_params <- c("Lambda (λ)" = input$lambda)
        log_L <- sum(dpois(data, lambda = est_params[1], log = TRUE))
      } else if (dist == "binom") {
        # Binomial MLE requires fixed 'size' (number of trials)
        size_val <- input$size
        mle_fit <- mlbinom(data, size = size_val)
        est_params <- c("Size (k)" = size_val, "Prob (p)" = unname(mle_fit[1]))
        true_params <- c("Size (k)" = size_val, "Prob (p)" = input$prob)
        log_L <- sum(dbinom(data, size = size_val, prob = est_params[2], log = TRUE))
      }

      # Format results into a data frame
      results <- data.frame(
        Parameter = names(true_params),
        True = as.numeric(true_params),
        MLE = as.numeric(est_params)
      )

      # Return list of results
      list(table = results, log_L = log_L, est_params = est_params)

    }, error = function(e) {
      # Handle potential MLE failures (e.g., non-convergence)
      showNotification(paste("MLE Error:", e$message), type = "warning")
      return(list(table = NULL, log_L = "Calculation Failed", est_params = NULL))
    })
  })

  # --- 4. Render Outputs ---

  # Render the estimates table
  output$mle_table <- renderTable({
    req(mle_results())
    mle_results()$table
  }, digits = 4)

  # Render the log-likelihood
  output$log_likelihood <- renderPrint({
    req(mle_results())
    mle_results()$log_L
  })

  # Render the plot
  output$data_plot <- renderPlot({
    req(data_reac(), mle_results())
    data <- data_reac()
    est_params <- mle_results()$est_params
    dist <- input$distribution

    p <- ggplot(data = data.frame(x = data), aes(x)) +
      theme_minimal() +
      labs(
        title = paste("Data Distribution with MLE Fit (n =", input$n_samples, ")"),
        x = "Value",
        y = "Density"
      )

    # Continuous Distributions: Normal, Exponential, Gamma
    if (dist %in% c("norm", "exp", "gamma")) {

      p <- p + geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "white")

      # Overlay MLE density curve
      x_range <- seq(min(data), max(data), length.out = 500)

      if (dist == "norm") {
        density_values <- dnorm(x_range, mean = est_params[1], sd = est_params[2])
      } else if (dist == "exp") {
        density_values <- dexp(x_range, rate = est_params[1])
      } else if (dist == "gamma") {
        density_values <- dgamma(x_range, shape = est_params[1], rate = est_params[2])
      }

      p <- p + geom_line(
        data = data.frame(x = x_range, y = density_values),
        aes(x, y),
        color = "red",
        linewidth = 1
      )

      # Discrete Distributions: Poisson, Binomial
    } else if (dist %in% c("pois", "binom")) {

      p <- p + geom_bar(aes(y = after_stat(prop)), fill = "skyblue", color = "white") +
        labs(y = "Relative Frequency")

      # Overlay MLE probability mass function (PMF)
      x_range <- seq(min(data), max(data), by = 1)

      if (dist == "pois") {
        pmf_values <- dpois(x_range, lambda = est_params[1])
      } else if (dist == "binom") {
        pmf_values <- dbinom(x_range, size = input$size, prob = est_params[2])
      }

      # The PMF must be scaled by the total number of observations to compare with relative frequency
      pmf_df <- data.frame(x = x_range, y = pmf_values)

      p <- p + geom_point(data = pmf_df, aes(x, y), color = "red", size = 3) +
        geom_segment(data = pmf_df, aes(x = x, xend = x, y = 0, yend = y), color = "red")
    }

    print(p)
  })
}

# --- Run the application ---
shinyApp(ui = ui, server = server)
