# Install and load necessary libraries
#install.packages(c("shiny", "tidyverse", "DT", "corrplot"))

# Load libraries
library(shiny)
library(tidyverse)
library(DT)
library(corrplot)

# Assume 'a_Theoph' dataframe is already available with the necessary columns
data(Theoph)

dm <- read.csv("/Users/misikirtesfaye/Downloads/Mixed_files/Misikir_Biometric_files/dm.csv")
attach(dm)

il13 <- read.csv("/Users/misikirtesfaye/Downloads/Mixed_files/Misikir_Biometric_files/il13.csv", skip = 3)
attach(il13)

Theoph <- Theoph %>% mutate(Subject = as.character(Subject))

il13 <- il13 %>% mutate(Subject = as.character(Subject))

dm <- dm %>%
  rename(Subject = SUBJECT) %>%
  mutate(Subject = as.character(Subject)) %>%
  mutate(SEX = ifelse(SEX == "MALE", "M", SEX))

a_Theoph <- Theoph %>%
  select(Subject, Dose, Time, conc) %>%
  left_join(dm, select(Subject, SEX, Age), by = "Subject") %>%
  left_join(il13, select(Subject, Hour.0, Hour.24), by = "Subject") %>%
  distinct()

# Define UI
# ... (previous code)

# Define UI
ui <- fluidPage(
  titlePanel("Theoph Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Checkbox for showing all subjects
      checkboxInput("show_all_subjects", "Show All Subjects", value = FALSE),
      # Slider to select individual subject
      sliderInput("subject_slider", "Select Subject",
                  min = 1, max = n_distinct(a_Theoph$Subject), value = 1, step = 1),
      # Button and Slider for Min/Max Concentration
      actionButton("filter_btn", "Filter Concentration"),
      sliderInput("conc_range", "Concentration Range",
                  min = 0, max = 20, value = c(0, 20), step = 1)
    ),
    
    mainPanel(
      tags$div(
        style = "text-align: justify;",
        "This Shiny application provides an interactive dashboard for exploring pharmacokinetic data.",
        "Pharmacokinetics (PK) is the study of how a drug is absorbed, distributed, metabolized, and excreted in an organism.",
        "The Area Under the Curve (AUC) is a key parameter in pharmacokinetics that represents the total exposure of a drug over time.",
        "Use the options in the sidebar to explore concentration summaries, AUC calculations, visualizations, and more."
      ),
      tabsetPanel(
        tabPanel("Concentration Summary", DTOutput("conc_summary")),
        tabPanel("AUC Calculation", DTOutput("auc_calculation")),
        tabPanel("AUC Visualization", plotOutput("auc_visualization")),
        tabPanel("Dose Levels Comparison", plotOutput("dose_comparison")),
        tabPanel("Descriptive Statistics", DTOutput("descriptive_stats")),
        tabPanel("Boxplot of Concentrations Over Time", plotOutput("boxplot")),
        tabPanel("Correlation Heatmap", plotOutput("correlation_heatmap")),
        tabPanel("Histogram of Age Distribution", plotOutput("age_histogram")),
        tabPanel("Scatter Plot Matrix", plotOutput("scatter_plot_matrix")),
        tabPanel("Maximum Concentration Summary", DTOutput("max_conc_summary"))
        # You can add more tabs as needed
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive element to filter data based on selected subject or all subjects
  selected_subject_data <- reactive({
    if (input$show_all_subjects) {
      a_Theoph
    } else {
      a_Theoph %>%
        filter(Subject == input$subject_slider)
    }
  })
  
  # Task 1: Concentration Summary
  output$conc_summary <- renderDT({
    filtered_data <- selected_subject_data() %>%
      filter(conc >= input$conc_range[1], conc <= input$conc_range[2])
    datatable(filtered_data)
  })
  
  # Task 2: AUC Calculation (Complete the function trapezoid)
  output$auc_calculation <- renderDT({
    trapezoid <- function(indf) {
      a_theoph <- indf %>%
        group_by(Subject) %>%
        mutate(conc2 = lag(conc),
               time2 = lag(Time)) %>%
        mutate(calc_auc = ifelse(!is.na(conc) & !is.na(conc2) & !is.na(Time) & !is.na(time2),
                                 ((conc + conc2) / 2) * (Time - time2), NA_integer_)) %>%
        summarise(AUC = sum(calc_auc, na.rm = TRUE))
      return(a_theoph)
    }
    
    # Call the trapezoid function and convert to data frame
    auc_data <- as.data.frame(trapezoid(selected_subject_data()))
    
    # Display the result in DataTable
    datatable(auc_data)
  })
  
  # Task 3: AUC Visualization
  output$auc_visualization <- renderPlot({
    if (input$show_all_subjects) {
      ggplot(a_Theoph, aes(x = Time, y = conc, color = factor(Subject))) +
        geom_line() +
        labs(title = "AUC Visualization", x = "Time (hr)", y = "Concentration (mg/L)") +
        scale_color_discrete(name = "Subject", limits = unique(a_Theoph$Subject))
    } else {
      ggplot(selected_subject_data(), aes(x = Time, y = conc, color = factor(Subject))) +
        geom_line() +
        labs(title = "AUC Visualization", x = "Time (hr)", y = "Concentration (mg/L)") +
        scale_color_discrete(name = "Subject", limits = unique(selected_subject_data()$Subject))
    }
  })
  
  # Task 4: Dose Levels Comparison
  output$dose_comparison <- renderPlot({
    plot(a_Theoph$conc ~ a_Theoph$Time)
    model1 <- lm(a_Theoph$conc ~ a_Theoph$Time)
    lines(a_Theoph$Time, fitted(model1), lwd = 3, col = 4)
    summary(model1)
  })
  
  # Task 5: Descriptive Statistics Summary Table
  output$descriptive_stats <- renderDT({
    descriptive_stats <- selected_subject_data() %>%
      select(Age, conc, Time) %>%
      summarise_all(list(mean = mean, sd = sd, median = median, min = min, max = max)) %>%
      pivot_longer(cols = everything()) %>%
      filter(!is.na(value)) %>%
      rename("Variable" = name)
    datatable(descriptive_stats, options = list(dom = 't'))
  })
  
  # Task 6: Boxplot of Concentrations Over Time by Subject
  output$boxplot <- renderPlot({
    ggplot(a_Theoph, aes(x = Time, y = conc, color = Subject)) +
      geom_boxplot() +
      labs(x = "Time (hr)", y = "Concentration (mg/L)")
  })
  
  # Task 7: Correlation Heatmap
  output$correlation_heatmap <- renderPlot({
    correlation_matrix <- cor(a_Theoph %>% select(Age, Dose, Time, conc), use = "complete.obs")
    corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black")
  })
  
  # Task 8: Histogram of Age Distribution
  output$age_histogram <- renderPlot({
    ggplot(a_Theoph, aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
      labs(x = "Age", y = "Frequency")
  })
  
  # Task 9: Scatter Plot Matrix
  output$scatter_plot_matrix <- renderPlot({
    pairs(a_Theoph %>% select(Age, Dose, Time, conc))
  })
  
  # Task 10: Maximum Concentration Summary Table
  output$max_conc_summary <- renderDT({
    max_conc_summary <- a_Theoph %>%
      group_by(Subject) %>%
      summarise(Max_Concentration = max(conc))
    datatable(max_conc_summary, options = list(dom = 't'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
