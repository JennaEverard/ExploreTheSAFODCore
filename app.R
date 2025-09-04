#' EXPLORE THE CORE: SAFOD APP
#' By: Jenna Everard

#' Last Updated: August, 2025

#' Load libraries and themes
library(shiny)
library(bslib)
my_theme <- bs_theme(version = 5, bootswatch = "minty")

#' Title: Make Core Content
#'
#' Description: When a core section is selected, this function fills in
#' the text and images with content from attached speadsheets/folders
#'
#' @param selection A string. The core letter and run number (i.e. C1)
#' @param data A dataframe. The core information dataframe pre-filtered 
#' for the specific core, run, and section
#' @param inventory A dataframe. The returned materials dataframe pre-filtered 
#' for the specific core, run, and section
#'
make_core_content <- function(selection, data, inventory) {
  
  # Display nothing if nothing is selected
  if (is.null(selection) || selection == "") return(NULL)
  
  ts <- inventory[inventory$Type == "0",]
  rock <- inventory[inventory$Type == "1",]
  powder <- inventory[inventory$Type == "2",]
  billets <- inventory[inventory$Type == "3",]
  minicore <- inventory[inventory$Type == "4",]
  
  ts_header <- if (nrow(ts) > 0) {
    h6(strong("Thin Section:"))
  } else {
    NULL
  }
  
  ts_list <- if (nrow(ts) > 0) {
    tags$ul(
      lapply(seq_len(nrow(ts)), function(i) {
        dist <- ts[i, "dist_from_top"]
        display_text <- if (suppressWarnings(!is.na(as.numeric(dist)))) {
          paste0(dist, " cm")
        } else {
          paste0(dist)
        }
        tags$li(display_text)
      })
    )
  } else {
    NULL
  }
  
  rock_header <- if (nrow(rock) > 0) {
    h6(strong("Rock:"))
  } else {
    NULL
  }
  
  rock_list <- if (nrow(rock) > 0) {
    tags$ul(
      lapply(seq_len(nrow(rock)), function(i) {
        dist <- rock[i, "dist_from_top"]
        display_text <- if (suppressWarnings(!is.na(as.numeric(dist)))) {
          paste0(dist, " cm")
        } else {
          paste0(dist)
        }
        tags$li(display_text)
      })
    )
  } else {
    NULL
  }
  
  powder_header <- if (nrow(powder) > 0) {
    h6(strong("Powder:"))
  } else {
    NULL
  }
  
  powder_list <- if (nrow(powder) > 0) {
    tags$ul(
      lapply(seq_len(nrow(powder)), function(i) {
        dist <- powder[i, "dist_from_top"]
        display_text <- if (suppressWarnings(!is.na(as.numeric(dist)))) {
          paste0(dist, " cm")
        } else {
          paste0(dist)
        }
        tags$li(display_text)
      })
    )
  } else {
    NULL
  }
  
  ts_billet_header <- if (nrow(billets) > 0) {
    h6(strong("Thin Section Billet:"))
  } else {
    NULL
  }
  
  ts_billet_list <- if (nrow(billets) > 0) {
    tags$ul(
      lapply(seq_len(nrow(billets)), function(i) {
        dist <- billets[i, "dist_from_top"]
        display_text <- if (suppressWarnings(!is.na(as.numeric(dist)))) {
          paste0(dist, " cm")
        } else {
          paste0(dist)
        }
        tags$li(display_text)
      })
    )
  } else {
    NULL
  }
  
  minicore_header <- if (nrow(minicore) > 0) {
    h6(strong("Mini Core:"))
  } else {
    NULL
  }
  
  minicore_list <- if (nrow(minicore) > 0) {
    tags$ul(
      lapply(seq_len(nrow(minicore)), function(i) {
        dist <- minicore[i, "dist_from_top"]
        display_text <- if (suppressWarnings(!is.na(as.numeric(dist)))) {
          paste0(dist, " cm")
        } else {
          paste0(dist)
        }
        tags$li(display_text)
      })
    )
  } else {
    NULL
  }
  
  tagList(
    br(),
    h4(strong(data$name)), # core, run, and section title
    br(),
    p(strong('Depth (m): '), data$top_m, ' - ', data$bottom_m), # depth in meters
    p(strong('Depth (ft): '), data$top_ft, ' - ', data$bottom_ft), # depth in feet
    br(),
    p(strong('Lithology: '), data$lith), # lithology 
    br(),
    img(src = data$img_path, width='100%'), # core image
    br(),br(),
    h5(strong("Available Returned Materials")),
    # THE FOLLOWING only display when that type of material is available
    # Thin sections:
    ts_header,
    ts_list,
    # Rocks:
    rock_header,
    rock_list,
    # Powders:
    powder_header,
    powder_list,
    # Thin Section Billets:
    ts_billet_header,
    ts_billet_list,
    # Mini Core:
    minicore_header,
    minicore_list
  )
}

#' Title: Make Missing Depth Content
#'
#' Description: Same as above, except for core sections without associated depth ranges
#'
#' @param selection A string. The core letter and run number (i.e. C1)
#' @param data A dataframe. The core information dataframe pre-filtered 
#' for the specific core, run, and section
#'
make_missing_depth_content <- function(selection, data) {
  
  # Display nothing if nothing is selected
  if (is.null(selection) || selection == "") return(NULL)
  
  tagList(
    br(), 
    h4(strong(data$name)), # core, run, and section title
    br(),
    img(src = data$img_path, width='100%'), # core image
    br(),br(),
    h5(strong("Available Returned Materials")) # list of available returned materials
  )
}

#' Title: UI
#'
#' Description: The application's user interface
#'
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
                      p(strong("2. Phase 1 (2004) & Phase 2 (2005):"), "The main hole was 
                        drilled vertically for the first 1.5 km and then at an angle of approx. 
                        60 degrees from the vertical in order to intercept the full width of 
                        the San Andreas Fault near where microearthquakes were clustered (Hole C)."),
                      p(strong("3. Phase 3 (2007):"), "Three lateral sidetracks (Holes E and G) 
                        were drilled to intercept actively deforming zones of the San Andreas Fault.")
                    )
    ),
    
    # Hole C
    accordion_panel(title="Hole C", 
                    accordion(
                      accordion_panel(title="Run 1",
                                      selectInput(
                                        inputId="C1",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2"),
                                        selected=""
                                      ),
                                      uiOutput("C1_content")),
                      accordion_panel(title="Run 2",
                                      selectInput(
                                        inputId="C2",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "3", "4", "5", "6"),
                                        selected=""
                                      ),
                                      uiOutput("C2_content")),
                      accordion_panel(title="Run 3",
                                      selectInput(
                                        inputId="C3",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "7", "8", "9"),
                                        selected=""
                                      ),
                                      uiOutput("C3_content")),
                      accordion_panel(title="Core Catcher (1/2/3)",
                                      uiOutput("CCC_content")),
                      accordion_panel(title="Run 4",
                                      selectInput(
                                        inputId="C4",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "10", "11", "12",
                                                  "13", "14", "15", "16", "17",
                                                  "18", "19 and 20"),
                                        selected=""
                                      ),
                                      uiOutput("C4_content")),
                      accordion_panel(title="Run 5",
                                      selectInput(
                                        inputId="C5",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "21", "22"),
                                        selected=""
                                      ),
                                      uiOutput("C5_content")),
                      accordion_panel(title="Run 7",
                                      selectInput(
                                        inputId="C7",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "CC"),
                                        selected=""
                                      ),
                                      uiOutput("C7_content"))
                    )),
    
    # Hole E
    accordion_panel(title="Hole E", 
                    accordion(
                      accordion_panel(title="Run 1",
                                      selectInput(
                                        inputId="E1",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6", "7", "8"),
                                        selected=""
                                      ),
                                      uiOutput("E1_content")),
                      accordion_panel(title="Run 2",
                                      selectInput(
                                        inputId="E2",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5 and 6"),
                                        selected=""
                                      ),
                                      uiOutput("E2_content"))
                    )),
    
    # Hole G
    accordion_panel(title="Hole G",
                    accordion(
                      accordion_panel(title="Run 1",
                                      selectInput(
                                        inputId="G1",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6"),
                                        selected=""
                                      ),
                                      uiOutput("G1_content")),
                      accordion_panel(title="Run 2",
                                      selectInput(
                                        inputId="G2",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6", "7", "8", "9"),
                                        selected=""
                                      ),
                                      uiOutput("G2_content")),
                      accordion_panel(title="Run 3",
                                      selectInput(
                                        inputId="G3",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2"),
                                        selected=""
                                      ),
                                      uiOutput("G3_content")),
                      accordion_panel(title="Run 4",
                                      selectInput(
                                        inputId="G4",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6", "7"),
                                        selected=""
                                      ),
                                      uiOutput("G4_content")),
                      accordion_panel(title="Run 5",
                                      selectInput(
                                        inputId="G5",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6", "7"),
                                        selected=""
                                      ),
                                      uiOutput("G5_content")),
                      accordion_panel(title="Run 6",
                                      selectInput(
                                        inputId="G6",
                                        label="Choose a Section: ",
                                        choices=c("None" = "", "1", "2", "3",
                                                  "4", "5", "6"),
                                        selected=""
                                      ),
                                      uiOutput("G6_content"))
                    )),
  )
)

#' Title: server
#'
#' Description: logic for responding to user inputs
#'
server <- function(input, output, session) {
  
  # Load the information about each core section
  core_data <- read.csv("safod_core_info.csv", stringsAsFactors = FALSE)
  
  # Load the information about returned materials
  ret_materials <- read.csv("safod_returned_materials.csv", stringsAsFactors = FALSE)
  
  # Point each output area to the make_core_content function with appropriate inputs
  output$C1_content <- renderUI({
    filtered <- core_data[core_data$ID == "C1" & core_data$Section == toString(input$C1),]
    filtered_ret <- ret_materials[ret_materials$ID == "C1" & ret_materials$Section == toString(input$C1),]
    renderUI({ make_core_content(input$C1, filtered, filtered_ret) })
  })
  output$C2_content <- renderUI({
    filtered <- core_data[core_data$ID == "C2" & core_data$Section == toString(input$C2),]
    filtered_ret <- ret_materials[ret_materials$ID == "C2" & ret_materials$Section == toString(input$C2),]
    renderUI({ make_core_content(input$C2, filtered, filtered_ret) })
  })
  output$C3_content <- renderUI({
    filtered <- core_data[core_data$ID == "C3" & core_data$Section == toString(input$C3),]
    filtered_ret <- ret_materials[ret_materials$ID == "C3" & ret_materials$Section == toString(input$C3),]
    renderUI({ make_core_content(input$C3, filtered, filtered_ret) })
  })
  output$C4_content <- renderUI({
    filtered <- core_data[core_data$ID == "C4" & core_data$Section == toString(input$C4),]
    filtered_ret <- ret_materials[ret_materials$ID == "C4" & ret_materials$Section == toString(input$C4),]
    renderUI({ make_core_content(input$C4, filtered, filtered_ret) })
  })
  output$C5_content <- renderUI({
    filtered <- core_data[core_data$ID == "C5" & core_data$Section == toString(input$C5),]
    filtered_ret <- ret_materials[ret_materials$ID == "C5" & ret_materials$Section == toString(input$C5),]
    renderUI({ make_core_content(input$C5, filtered, filtered_ret) })
  })
  output$C7_content <- renderUI({
    filtered <- core_data[core_data$ID == "C7" & core_data$Section == toString(input$C7),]
    filtered_ret <- ret_materials[ret_materials$ID == "C7" & ret_materials$Section == toString(input$C7),]
    renderUI({ make_core_content(input$C7, filtered, filtered_ret) })
  })
  output$CCC_content <- renderUI({
    filtered <- core_data[core_data$ID == "CCC",]
    renderUI({ make_missing_depth_content('CCC', filtered) })
  })
  output$E1_content <- renderUI({
    filtered <- core_data[core_data$ID == "E1" & core_data$Section == toString(input$E1),]
    filtered_ret <- ret_materials[ret_materials$ID == "E1" & ret_materials$Section == toString(input$E1),]
    renderUI({ make_core_content(input$E1, filtered, filtered_ret) })
  })
  output$E2_content <- renderUI({
    filtered <- core_data[core_data$ID == "E2" & core_data$Section == toString(input$E2),]
    filtered_ret <- ret_materials[ret_materials$ID == "E2" & ret_materials$Section == toString(input$E2),]
    renderUI({ make_core_content(input$E2, filtered, filtered_ret) })
  })
  output$G1_content <- renderUI({
    filtered <- core_data[core_data$ID == "G1" & core_data$Section == toString(input$G1),]
    filtered_ret <- ret_materials[ret_materials$ID == "G1" & ret_materials$Section == toString(input$G1),]
    renderUI({ make_core_content(input$G1, filtered, filtered_ret) })
  })
  output$G2_content <- renderUI({
    filtered <- core_data[core_data$ID == "G2" & core_data$Section == toString(input$G2),]
    filtered_ret <- ret_materials[ret_materials$ID == "G2" & ret_materials$Section == toString(input$G2),]
    renderUI({ make_core_content(input$G2, filtered, filtered_ret) })
  })
  output$G3_content <- renderUI({
    filtered <- core_data[core_data$ID == "G3" & core_data$Section == toString(input$G3),]
    filtered_ret <- ret_materials[ret_materials$ID == "G3" & ret_materials$Section == toString(input$G3),]
    renderUI({ make_core_content(input$G3, filtered, filtered_ret) })
  })
  output$G4_content <- renderUI({
    filtered <- core_data[core_data$ID == "G4" & core_data$Section == toString(input$G4),]
    filtered_ret <- ret_materials[ret_materials$ID == "G4" & ret_materials$Section == toString(input$G4),]
    renderUI({ make_core_content(input$G4, filtered, filtered_ret) })
  })
  output$G5_content <- renderUI({
    filtered <- core_data[core_data$ID == "G5" & core_data$Section == toString(input$G5),]
    filtered_ret <- ret_materials[ret_materials$ID == "G5" & ret_materials$Section == toString(input$G5),]
    renderUI({ make_core_content(input$G5, filtered, filtered_ret) })
  })
  output$G6_content <- renderUI({
    filtered <- core_data[core_data$ID == "G6" & core_data$Section == toString(input$G6),]
    filtered_ret <- ret_materials[ret_materials$ID == "G6" & ret_materials$Section == toString(input$G6),]
    renderUI({ make_core_content(input$G6, filtered, filtered_ret) })
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
