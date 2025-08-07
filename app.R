library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(MASS)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
      background-color: #F6F6F6;}
      .custom-title {
        font-size: 20px; 
        font-weight: bold;
        text-align: left;
        margin-bottom: 5px;
        margin-top: 5px;
      }
    "))
  ),
  div(class = "custom-title", "GrowCast: GH Effect Predictor for Idiopathic Short Stature Patients"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("agefirst", "Age at First Treatment (months):", min = 60, max = 140, value = 61),
      numericInput("daily_dose", "Daily Dose:", value = 1.5),
      sliderInput("weight", "Weight (kg):", min = 10, max = 40, value = 11),
      sliderInput("height", "Height (cm):", min = 84, max = 145, value = 86),
      sliderInput("father", "Father's Height (cm):", min = 160, max = 182, value = 168),
      sliderInput("IGFBP3", "IGFBP3 Level (ng/mL):", min = 2360, max = 7540, value = 3680),
      radioButtons("sex", "Sex:", choices = list("Male" = 0, "Female" = 1), selected = 1),
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Instruction",  
                           img(src = "instruction2.png", width = "100%")  
                  ),
                  tabPanel("Predictions",
                           plotlyOutput("H_plot"),
                           br(),
                           plotlyOutput("s100_plot")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  get_percentile_scores <- function(BMI, IGFBP3, father, sum_dose, nsim = 1000, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    A_fixed <- 17.19024
    B_fixed <-  0.63261
    K       <-  0.02181
    t       <- 54.38863
    b1      <-  0.34118
    b2      <- -0.26498
    b3      <- -0.21133
    
    A_sd <- 11.10314
    B_sd <- 0.90374
    corr <- -0.1
    
    cov_matrix <- matrix(c(
      A_sd^2, corr * A_sd * B_sd,
      corr * A_sd * B_sd, B_sd^2
    ), nrow = 2)
    
    eta <- mvrnorm(n = nsim, mu = c(0, 0), Sigma = cov_matrix)
    A_samples <- A_fixed + eta[, 1]
    B_samples <- B_fixed + eta[, 2]
    
    BMI_term    <- 1 + b1 * BMI
    IGFBP3_term <- 1 + b2 * IGFBP3
    father_term <- 1 + b3 * father
    
    scores <- B_samples + A_samples * BMI_term * IGFBP3_term * father_term *
      exp(-exp(-K * (sum_dose - t)))
    
    quantile(scores, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), names = FALSE)
  }
  
  observeEvent(input$predict, {
    updateTabsetPanel(session, "tabs", selected = "Predictions")
    
    control <- read.csv("www/control_0418.csv") %>%
      mutate(sex = ifelse(sex == 'M', 0, 1))
    
    control_filtered <- control %>% filter(age == input$agefirst, sex == input$sex)
    M_value <- control_filtered$M
    L_value <- control_filtered$L
    S_value <- control_filtered$S
    
    
    means <- list(father = 170.4681, IGFBP3 = 4137.624, BMI = 15.82319)
    sds   <- list(father = 4.80279, IGFBP3 = 955.1863, BMI = 1.48836)
    
    
    father_norm  <- (input$father - means$father) / sds$father
    IGFBP3_norm  <- (input$IGFBP3 - means$IGFBP3) / sds$IGFBP3
    BMI_input <- (input$weight / (input$height * 0.01)^2) 
    BMI_norm  <- (BMI_input - means$BMI) / sds$BMI
    
    ages <- seq(input$agefirst, input$agefirst + 20, by = 1)
    sum_doses <- (ages - input$agefirst) * input$daily_dose * 30.4
    
    score_matrix <- t(sapply(sum_doses, function(dose) {
      get_percentile_scores(BMI_norm, IGFBP3_norm, father_norm,dose, nsim = 1000)
    }))
    
    s100_10_wrnd <- score_matrix[, 1]
    s100_30_wrnd <- score_matrix[, 2]
    s100_50      <- score_matrix[, 3]
    s100_70_wrnd <- score_matrix[, 4]
    s100_90_wrnd <- score_matrix[, 5]
    
    
    
    z_input_height <- ((input$height / M_value)^L_value - 1) / (L_value * S_value)
    percentile_input_height <- pnorm(z_input_height) * 100
    percentile_shift <- percentile_input_height - first(s100_50)
    
    output$H_plot <- renderPlotly({
      control_filtered <- control %>% filter(age %in% ages, sex == input$sex)
      control_filtered$s100_1 <- s100_50
      control_filtered$s100_10_wrnd <- s100_10_wrnd
      control_filtered$s100_30_wrnd <- s100_30_wrnd
      control_filtered$s100_70_wrnd <- s100_70_wrnd
      control_filtered$s100_90_wrnd <- s100_90_wrnd
      
      M <- control_filtered$M
      L <- control_filtered$L
      S <- control_filtered$S
      
      Z1 <- qnorm(control_filtered$s100_1 / 100)
      Z10 <- qnorm(control_filtered$s100_10_wrnd / 100)
      Z30 <- qnorm(control_filtered$s100_30_wrnd / 100)
      Z70 <- qnorm(control_filtered$s100_70_wrnd / 100)
      Z90 <- qnorm(control_filtered$s100_90_wrnd / 100)
      
      H1 <- M * (L * S * Z1 + 1)^(1 / L)
      H_shift <- input$height - first(H1)
      H1 <- H1 + H_shift
      H10 <- M * (L * S * Z10 + 1)^(1 / L) + H_shift
      H30 <- M * (L * S * Z30 + 1)^(1 / L) + H_shift
      H70 <- M * (L * S * Z70 + 1)^(1 / L) + H_shift
      H90 <- M * (L * S * Z90 + 1)^(1 / L) + H_shift
      
      df <- data.frame(age = ages,
                       height_mean = round(H1, 2),
                       height_10_wrnd = round(H10, 2),
                       height_30_wrnd = round(H30, 2),
                       height_70_wrnd = round(H70, 2),
                       height_90_wrnd = round(H90, 2),
                       percentile_score = round(s100_50, 2),
                       percentile_score_10_wrnd = round(s100_10_wrnd, 2),
                       percentile_score_30_wrnd = round(s100_30_wrnd, 2),
                       percentile_score_70_wrnd = round(s100_70_wrnd, 2),
                       percentile_score_90_wrnd = round(s100_90_wrnd, 2))
      
      if (any(df$percentile_score <= 0 | df$percentile_score >=100)) {
        p1 <- ggplot() +
          annotate("text", x = 1, y = 1,
                   label = "Please check if you entered valid input values.",
                   size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
          theme_void()
        return(ggplotly(p1))
      }
      
      #p1 <- ggplot(df, aes(x = age)) +
      #  geom_line(aes(y = height_mean), color = "coral4", linetype = "dashed") +
      #  geom_ribbon(aes(ymin = height_10_wrnd, ymax = height_90_wrnd), fill = "coral1", alpha = 0.1) +
      #  geom_ribbon(aes(ymin = height_30_wrnd, ymax = height_70_wrnd), fill = "coral2", alpha = 0.2) +
      #  labs(title = "Predicted height",
      #       x = "Age (months)",
      #       y = "Height (cm)") +
      #  theme_light()
      
      p1 <- ggplot(df, aes(x = age)) +
        geom_line(aes(y = height_mean), color = "coral4", linetype = "dashed")
      
      if (all(df$percentile_score_10_wrnd >= 0 & df$percentile_score_90_wrnd <= 100)) {
        p1 <- p1 + geom_ribbon(
          aes(ymin = height_10_wrnd, ymax = height_90_wrnd),
          fill = "coral1", alpha = 0.1
        )
      }
      
      if (all(df$percentile_score_30_wrnd >= 0 & df$percentile_score_70_wrnd <= 100)) {
        p1 <- p1 + geom_ribbon(
          aes(ymin = height_30_wrnd, ymax = height_70_wrnd),
          fill = "coral1", alpha = 0.1
        )
      }
      
      p1 <- p1 +
        labs(title = "Predicted height",
             x = "Age (months)",
             y = "Height (cm)") +
        theme_light()
      
      
      ggplotly(p1)
    })
    
    output$s100_plot <- renderPlotly({
      df <- data.frame(age = ages,
                       percentile_score         = round(s100_50, 2),
                       percentile_score_10_wrnd = round(s100_10_wrnd, 2),
                       percentile_score_30_wrnd = round(s100_30_wrnd, 2),
                       percentile_score_70_wrnd = round(s100_70_wrnd, 2),
                       percentile_score_90_wrnd = round(s100_90_wrnd, 2))
      
      if (any(df$percentile_score <= 0 |df$percentile_score >=100)) {
        p2 <- ggplot() +
          annotate("text", x = 1, y = 1,
                   label = "Please check if you entered valid input values.",
                   size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
          theme_void()
        return(ggplotly(p2))
      }
      p2 <- ggplot(df, aes(x = age)) +
        geom_line(aes(y = percentile_score), color = "goldenrod3", linetype = "dashed")
      
      if (all(df$percentile_score_10_wrnd >= 0 & df$percentile_score_90_wrnd <= 100)) {
        p2 <- p2 + geom_ribbon(
          aes(ymin = percentile_score_10_wrnd, ymax = percentile_score_90_wrnd),
          fill = "goldenrod1", alpha = 0.1
        )
      }
      
      if (all(df$percentile_score_30_wrnd >= 0 & df$percentile_score_70_wrnd <= 100)) {
        p2 <- p2 + geom_ribbon(
          aes(ymin = percentile_score_30_wrnd, ymax = percentile_score_70_wrnd),
          fill = "goldenrod2", alpha = 0.2
        )
      }
      
      p2 <- p2 +
        labs(title = "Predicted percentile score",
             x = "Age (months)",
             y = "Percentile score (%)") +
        theme_light() +
        scale_y_continuous(limits = c(0, 100))
    })
  })
}

shinyApp(ui = ui, server = server)
