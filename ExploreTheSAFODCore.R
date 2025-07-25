library(shiny)
library(bslib)

my_theme <- bs_theme(version = 5, bootswatch = "minty")

# Application Part 1: the User Interface
ui <- page_sidebar(
  
  title ="Explore the SAFOD Core",
  theme = my_theme,
  # Sidebar panel with overview info about SAFOD
  sidebar = sidebar(
    class = "fixed-sidebar",
    img(src="safod_logo.png", width="100%"),
    br(), br(),
    p("The San Andreas Fault Observatory at Depth (SAFOD) project was established
    in 1998 with the aim to improve our understanding of the San Andreas Fault.
    It has succeeded as the first borehole to intercept regions of seismicity
    at depth along a plate-boundary fault, with near complete recovery of
    material from two active fault strands."),
    br(), br(),
    p("This application can be used to find information about each core section
    and explore available samples."),
    br(), br(),
    p(em("Last updated: August 2025"))
  ),
  # Formatting for sidebar panel
  tags$head(
    tags$style(HTML("
    .fixed-sidebar {
      width: auto;
      max-width: 350px !important
    }
    "))
  ),
  
  # Main panel that users can interact with
  accordion(
    
    accordion_panel(title='Overview',
                    div(
                      img(src="safod_holes.png", style="width:100%; height:auto;"),
                      br(), br(),
                      p("Drilling of the SAFOD core was conducted in one pilot 
                        phase and three main phases:"),
                      p(strong("1. Pilot Hole (2002):"), "A 2.2 km vertical hole drilled 
                        near the planned SAFOD drill site."),
                      p(strong("2. "), "")
                    )
    ),
    
    accordion_panel(title="Hole C", plotOutput("plot")),
    
    
    accordion_panel(title="Hole E", verbatimTextOutput("summary")),
    
    
    accordion_panel(title="Hole G", tableOutput("table"))
  )
)

# Application Part 2: the Server Logic
server <- function(input, output, session) {
  output$summary <- renderPrint({
    "This is a placeholder summary text."
  })
}

# Run the app
shinyApp(ui = ui, server = server)