shinyUI(fluidPage(
  titlePanel("Predicting The Success of Movie"),
  
  sidebarLayout(
    sidebarPanel(
      
      checkboxInput("lr", label = "Logistic Regression", value = FALSE),
      checkboxInput("ca", label = "CART", value = FALSE),
      checkboxInput("rf", label = "Random Forest", value = FALSE),
      checkboxInput("sv", label = "SVM", value = FALSE),
      checkboxInput("ab", label = "adaBoost", value = FALSE),
                  
      
      fileInput("file1", label = "Select Testcase"),
      actionButton("pred", label = "Predict", style = "color:red"),
      actionButton("showCART", label = "Show CART", style = "color:red"),
      actionButton("pred", label = "Predict", style = "color:red"),
      
      fluidRow(
      column(12,
      tableOutput('contents'),
      style="overflow:scroll"
      )
      )
      ),
      
    
    mainPanel(
      h2(textOutput("rowcal")),
      fluidRow(
        column(12,h2(textOutput("heading1"),style="text-align:center"),
               plotOutput("phonePlot"))
      ),
      fluidRow(
        column(4,h3("Logistics Regression"),plotOutput("cfMatlr")),
        column(4,h3("CART"),plotOutput("cfMatcart")),
        column(4,h3("Random Forest"),plotOutput("cfMatrf"))
      ),
      fluidRow(
        column(2),
        column(4,h3("adaBoost"),plotOutput("cfMatab")),
        column(4,h3("SVM"),plotOutput("cfMatSVM")),
        column(2)
      )
      
      
    )
  )
))