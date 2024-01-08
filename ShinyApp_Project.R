#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(dplyr)
#library(ggplot2)

libraries <- c("tidyverse", "dplyr", "shiny", "PK","ggplot2")

# Load libraries with suppression of startup messages
for (lib in libraries) {
  suppressPackageStartupMessages(library(lib, character.only = TRUE))
}



# Assume 'a_Theoph' dataframe is already available with the necessary columns

data(Theoph)

#To add a bit more complexity, we will introduce two hypothetical datasets:  

#demog.csv: demographics dataset characterizing the subjects  
#il13.csv: This dataset contains % inhibition of stimulated IL-13 release by PBMC at hour 0 and hour 24.  

#The data will need to be wrangled into an analysis ready data frame, a_Theoph, by using dplyr functions.**  

#a_Theoph will include all subjects' data in Theoph joined with demographic data (using the correct dplyr join function) from dm.csv and il13 inhibition data from il13.csv.  
#Include only the subjects from Theoph in the resulting joined dataset.  Subjects in Theoph will be included in the analysis population.  
#Create the data frame a_Theoph with the following columns.  Do not include any other columns in the data frame at this point.  
# Subject: Subject ID from Theoph  
# SEX: Sex from dm.csv  
# Age: Age from dm.csv  
# Dose: Dose from Theoph  
# Time: Time from Theoph  
# conc: conc from Theop  
# IL13PCTI: % IL-13 inhibition from il13.csv (Note that il13.csv contains a nominal time whereas Theoph contains actual time.)  

#Some basic function calls to read in the csv files are included, however, you may modify the options to the read.csv function as you see fit.  
#For example, you may not want to have strings converted to factors by default.  

#The code for generating a_Theoph will be written

##input analysis data creation code here

dm <- read.csv("/Users/misikirtesfaye/Downloads/Mixed_files/Misikir_Biometric_files/dm.csv")
attach(dm)

il13 <- read.csv("/Users/misikirtesfaye/Downloads/Mixed_files/Misikir_Biometric_files/il13.csv",skip = 3)

attach(il13)

Theoph<-Theoph%>%mutate(Subject=as.character(Subject)) 

il13<-il13 %>% mutate(Subject=as.character(Subject)) 

dm<-dm %>% rename(Subject=SUBJECT) %>% mutate(Subject=as.character(Subject)) %>% 
  mutate(SEX=ifelse(SEX=="MALE","M",SEX))

a_Theoph<-Theoph %>% select(Subject,Dose,Time,conc) %>% left_join(dm,select(Subject,SEX,AGE),by="Subject") %>% left_join(il13,select(Subject,Hour.0,Hour.24),by="Subject") %>% 
  distinct()

attach(a_Theoph)

# Define UI
ui <- fluidPage(
  titlePanel("Theoph Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Any inputs or widgets you may want to add
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Concentration Summary", tableOutput("conc_summary")),
        tabPanel("AUC Calculation", verbatimTextOutput("auc_calculation")),
        tabPanel("AUC Visualization", plotOutput("auc_visualization")),
        tabPanel("Dose Levels Comparison", plotOutput("dose_comparison")),
        tabPanel("Additional Analysis", verbatimTextOutput("additional_analysis"))
        # You can add more tabs as needed
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Task 1: Concentration Summary
  output$conc_summary <- renderTable({
    Conc_summary <- a_Theoph %>% 
      group_by(SEX) %>% 
      summarise(Peak_conc = max(conc))
    Conc_summary
  })
  
  # Task 2: AUC Calculation (Complete the function trapezoid)
  output$auc_calculation <- renderPrint({
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
    
    # Call the trapezoid function
    trapezoid(a_Theoph)
  })
  
  # Task 3: AUC Visualization
  output$auc_visualization <- renderPlot({
    a_Theoph %>%
      mutate(conc2 = lag(conc),
             time2 = lag(Time)) %>% 
      mutate(chg_con = ifelse(!is.na(conc) & !is.na(conc2), conc - conc2, conc)) %>%
      filter(!is.na(chg_con)) %>% 
      ggplot(aes(x = Time, y = chg_con, color = Subject)) + 
      geom_point() + geom_line() + 
      labs(x = "Time (hr)", y = "Change in Concentration (mg/L)")
  })
  
  # Task 4: Dose Levels Comparison
  output$dose_comparison <- renderPlot({
    plot(a_Theoph$conc ~ a_Theoph$Time)
    model1 <- lm(a_Theoph$conc ~ a_Theoph$Time)
    lines(a_Theoph$Time, fitted(model1), lwd = 3, col = 4)
    summary(model1)
  })
  
  # Task 5: Additional Analysis
  output$additional_analysis <- renderPrint({
    # Your additional analysis or code results here
    a_theoph <- a_Theoph %>%
      group_by(Subject) %>%
      mutate(conc2 = lag(conc),
             time2 = lag(Time)) %>%
      mutate(calc_auc = ifelse(!is.na(conc) & !is.na(conc2) & !is.na(Time) & !is.na(time2),
                               ((conc + conc2) / 2) * (Time - time2), NA_integer_)) %>% 
      summarise(AUC = sum(calc_auc, na.rm = TRUE))
    print(a_theoph)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


