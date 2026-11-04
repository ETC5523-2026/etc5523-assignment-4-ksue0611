
library(shiny)

# Load data from the installed package
penguins <- penguinExplorer::penguins_clean

ui <- fluidPage(
  
  # Basic styling
  tags$head(
    tags$style("
      body {
        background-color: #f7f9fb;
      }

      .container-fluid {
        max-width: 1400px;
      }

      h2, h3, h4 {
        color: #25465c;
      }

      .well {
        background-color: #eaf2f5;
        border: 1px solid #d5e3e9;
        border-radius: 10px;
      }

      .col-sm-8 {
        background-color: white;
        padding: 20px;
        border-radius: 10px;
      }
    ")
  ),
  
  titlePanel("Penguin Species Explorer"),
  
  p("Explore how penguin body measurements differ across species and islands."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      
      checkboxGroupInput(
        "species",
        "Select penguin species:",
        choices = c("Adelie", "Chinstrap", "Gentoo"),
        selected = c("Adelie", "Chinstrap", "Gentoo")
      ),
      
      checkboxGroupInput(
        "island",
        "Select island:",
        choices = c("Biscoe", "Dream", "Torgersen"),
        selected = c("Biscoe", "Dream", "Torgersen")
      ),
      
      hr(),
      
      h4("Measurements"),
      
      selectInput(
        "x_var",
        "X-axis:",
        choices = c(
          "Bill Length (mm)" = "bill_length_mm",
          "Bill Depth (mm)" = "bill_depth_mm",
          "Flipper Length (mm)" = "flipper_length_mm",
          "Body Mass (g)" = "body_mass_g"
        ),
        selected = "bill_length_mm"
      ),
      
      selectInput(
        "y_var",
        "Y-axis:",
        choices = c(
          "Bill Length (mm)" = "bill_length_mm",
          "Bill Depth (mm)" = "bill_depth_mm",
          "Flipper Length (mm)" = "flipper_length_mm",
          "Body Mass (g)" = "body_mass_g"
        ),
        selected = "body_mass_g"
      ),
      
      p("Select species, islands and measurements to explore the data.")
    ),
    
    mainPanel(
      h3("Body Measurements"),
      textOutput("axis_message"),
      plotOutput("penguin_plot"),
      
      h3("Summary by Species"),
      tableOutput("penguin_summary"),
      
      h4("Key Finding"),
      textOutput("key_finding"),
      
      h4("Understanding the Variables"),
      p("Species refers to Adelie, Chinstrap or Gentoo penguins, while Island indicates where each penguin was observed."),
      p("Bill Length and Bill Depth describe bill measurements in millimetres."),
      p("Flipper Length is measured in millimetres, and Body Mass is measured in grams."),
      
      h4("How to Use"),
      p("Use the Species and Island filters to select the groups you want to explore."),
      p("Choose different measurements for the X-axis and Y-axis to compare their relationship."),
      
      h4("How to Interpret"),
      p("Each point represents one penguin, and the colours distinguish the three species."),
      p("The scatter plot helps identify patterns and differences in body measurements between species."),
      p("The summary table shows the number of penguins (n), mean body mass (mean_mass) and mean flipper length (mean_flipper) for each selected species."),
      p("These results describe patterns in the dataset and do not establish causal relationships.")
    )
  )
)

server <- function(input, output, session) {
  
  # Message for identical axis selections
  output$axis_message <- renderText({
    
    if (input$x_var == input$y_var) {
      return(
        "You selected the same measurement for both axes. Choose different measurements to explore their relationship."
      )
    }
    
    ""
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    
    penguins[
      penguins$species %in% input$species &
        penguins$island %in% input$island,
      ,
      drop = FALSE
    ]
  })
  
  # Scatter plot
  output$penguin_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data available. Please change your filters."
      )
    )
    
    species_names <- c("Adelie", "Chinstrap", "Gentoo")
    species_colours <- c("#377EB8", "#E69F00", "#009E73")
    
    colours <- species_colours[
      match(as.character(data$species), species_names)
    ]
    
    axis_labels <- c(
      bill_length_mm = "Bill Length (mm)",
      bill_depth_mm = "Bill Depth (mm)",
      flipper_length_mm = "Flipper Length (mm)",
      body_mass_g = "Body Mass (g)"
    )
    
    plot(
      data[[input$x_var]],
      data[[input$y_var]],
      col = colours,
      pch = 19,
      xlab = axis_labels[[input$x_var]],
      ylab = axis_labels[[input$y_var]],
      main = paste(
        axis_labels[[input$x_var]],
        "vs",
        axis_labels[[input$y_var]]
      )
    )
    
    selected_species <- species_names[
      species_names %in% as.character(unique(data$species))
    ]
    
    legend(
      "topleft",
      legend = selected_species,
      col = species_colours[
        match(selected_species, species_names)
      ],
      pch = 19,
      bty = "n"
    )
  })
  
  # Summary table using the exported package function
  output$penguin_summary <- renderTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data available. Please change your filters."
      )
    )
    
    penguinExplorer::summarise_penguins(
      data,
      input$species
    )
  }, digits = 1)
  
  # Dynamic finding based on selected data
  output$key_finding <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data available for this selection.")
    }
    
    summary <- penguinExplorer::summarise_penguins(
      data,
      input$species
    )
    
    highest <- summary[
      which.max(summary$mean_mass),
      ,
      drop = FALSE
    ]
    
    paste0(
      "Among the selected species, ",
      highest$species,
      " has the highest average body mass (",
      round(highest$mean_mass, 1),
      " g)."
    )
  })
}

shinyApp(ui, server)
