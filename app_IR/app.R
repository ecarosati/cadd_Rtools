
library(shiny)
library(plotly)
library(OpenSpecy)
library(glue)
library(magrittr)
library(dplyr)
library(ggplot2)

# provare a usare due reattivi per plot che viene da file e plot che viene da database


#spectra_dir <- "C:\\Users\\Emanuele\\Documents\\GithubRepositories\\cadd_Rtools\\data\\spectra"

transparent_yellow <- "rgba(255,255,51,0.5)"    # usato per fingerprint
transparent_red <- "rgba(255,0,0,0.5)"       # usato per O-H e C=O
transparent_blue <- "rgba(26,50,235,0.5)"       # OK: usato per N-H
transparent_green <- "rgba(26,150,65,0.5)"
transparent_orange <- "rgba(255,228,181,0.5)"
transparent_violet <- "rgba(255,180,255,0.5)"

# wavenumber for regions of interest
fingerprint_wn_high <- 1300
fingerprint_wn_low <- 600

# OH_wn_high <- 3400
# OH_wn_low <- 3200
OHal_wn_high <- 3600
OHal_wn_low <- 3200

OHca_wn_high <- 3300
OHca_wn_low <- 2500

NH_wn_high <- 3500
NH_wn_low <- 3300

NHs_wn_high <- 3000
NHs_wn_low <- 2800

SH_wn_high <- 2600
SH_wn_low <- 2550

CH_wn_high <- 3100
CH_wn_low <- 2840

CHar_wn_high <- 3050
CHar_wn_low <- 3010

CHalkyne_wn_high <- 3325
CHalkyne_wn_low <- 3275

CHalkene_alkane_line <- 3000

CO_wn_high <- 1800
CO_wn_low <- 1650

CN_wn_high <- 1550
CN_wn_low <- 1450

double_bonds_high <- 1900
double_bonds_low <- 1600

triple_bonds_high <- 2260
triple_bonds_low <- 2100

SO2_wn_high <- 1370
SO2_wn_low <- 1300

NO2_wn_high <- 1570
NO2_wn_low <- 1500
# (anche 1300-1370)

CCl_wn_high <- 850
CCl_wn_low <- 600

WAT_str_high <- 3650
WAT_str_low <- 3100

WAT_ben_high <- 1710
WAT_ben_low <- 1530


link_analisi_farmaci <- "https://corsi.units.it/fa02/modulo/analisi-farmaci-046fa-2021-pds0-2019-ord-2019-comune"
link_analisi_medicinali <- "https://corsi.units.it/fa01/modulo/analisi-medicinali-037fa-2021-pds0-2015-ord-2015-comune"

# AGGIUNGERE POSSIBILITA IMPORTARE SPETTRO DA FILE



dir_spectra <- "spectra"

dir_images <- file.path("www", "images")
images_all_files <- list.files(path=dir_images)


spectra_all_data <- readRDS(file = file.path(dir_spectra, "dataIR.R"))



# Define UI for application that draws a histogram
ui <- fluidPage(

    fluidRow(
      column(2, tags$img(src="logo-corsi-units.png", width="220", height="95")),
      column(2, 
             br(),
             tags$a(href=link_analisi_farmaci, "Analisi dei farmaci"),
             br(), br(),
             tags$a(href=link_analisi_medicinali, "Analisi dei medicinali")
             ),
      column(width=8, h1("Spettroscopia IR"))
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              4,
              selectInput(
                "source",
                label="Source",
                choices = c("LAB", "NIST", "ALL"),
                selected = "LAB"
              )
            ),
            column(
              4,
              selectInput(
                "method",
                label="Method",
                choices = c("ATR", "KBR", "NUJOL", "LIQUID", "ALL"),
                selected = "ATR"
              )
            ),
            column(
              4,
              selectInput(
                "substance",
                label="Substance",
                choices = c("ALL", "INC", "STD", "BKG"),
                selected = "STD"
              )
            )
          ),
          selectInput(
            "inputdata",
            label="Data from file (.dx) or database",
            choices = c("Database", "File"),
            selected = "Database"
          ),
          
          conditionalPanel(
            condition = "input.inputdata == 'Database'",
            selectInput(
              "main_spectra",
              label="Select the molecule of your main spectra",
              choices = c(""),
              selected = ""
            )
          ),
          conditionalPanel(
            condition = "input.inputdata == 'File'",
            fileInput(
              "userfile",
              label="Upload your (.dx) file",
              buttonLabel = "Upload file",
              placeholder = "Load your .dx file",
              accept = c(".dx")
            )
          ),
          checkboxInput(
            inputId = "add_ref",
            label="Add reference",
            value=FALSE
          ),
          conditionalPanel(
            condition = "input.add_ref == true",
            selectInput(
              "ref_spectra",
              label="Select the reference molecule",
              choices = c(""),
              selected = ""
            )
          ),
          checkboxInput(
            inputId = "interpretation",
            label="Interpretation",
            value=FALSE
          ),
          conditionalPanel(
            condition = "input.interpretation == true",
            fluidRow(
              column(
                width = 6,
                checkboxGroupInput(
                  "tips1",
                  label="Regions of interest (H)",
                  choices = c("O-H(alcohol, phenol)", "O-H(carboxilic acid)", 
                              "Water(stretching)", "Water(bending)",
                              "N-H", "N-H(amine salt)", "S-H",
                              "C-H", "C-H(aromatic)", "C-H(alkene/alkane)", "C-H(alkyne)")
                )
              ),
              column(
                width = 6,
                checkboxGroupInput(
                  "tips2",
                  label="Regions of interest (other)",
                  choices = c("Fingerprint", "C=O", "C=N(conjugated)", "NO2(nitro)", 
                              "SO2", "Other double bonds", "Triple bonds", "C-Cl")
                )
              )
            )

          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(
              4,
              selectInput(
                "bg_color",
                label="Select colour for background",
                choices = c("white", "gray", "black", "yellow", "blue"),
                selected = "white"
              )
            ),
            column(
              4,
              selectInput(
                "color",
                label="Select colour for the main line",
                choices = c("black", "blue", "red", "yellow", "white"),
                selected = "black"
              )
            ),
            column(
              4,
              conditionalPanel(
                condition = "input.add_ref == true",
                selectInput(
                  "ref_color",
                  label="Select colour for reference line",
                  choices = c("black", "blue", "red", "yellow", "white"),
                  selected = "black"
                )
              )
            )
          ),
          fluidRow(
            column(8,
              plotlyOutput("MyPlot")
            ),
            column(4,
              uiOutput("MyMolname"),
              imageOutput("MyImage")
            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_IR_full <- reactive({
    spectra_all_data 
  })
  
  data_IR_selection <- reactive({
    req(data_IR_full())
    # First filter: method
    if (input$method == "ALL") {
      tmp_data_1 <- data_IR_full()
    } else {
      tmp_data_1 <- data_IR_full() %>% 
        dplyr::filter(method==input$method)
    }
    # Second filter: substance
    if (input$substance == "ALL") {
      tmp_data_2 <- tmp_data_1
    } else {
      tmp_data_2 <- tmp_data_1 %>% 
        dplyr::filter(substance==input$substance)
    }
    # Third filter: source
    if (input$source == "ALL") {
      tmp_data_3 <- tmp_data_2
    } else {
      tmp_data_3 <- tmp_data_2 %>% 
        dplyr::filter(source==input$source)
    }
    
    tmp_data_3
    
  })
  
  # observe({
  #   req(data_IR_selection())
  #   cat("data_IR_selection():\n")
  #   print(head(data_IR_selection()))
  #   print(dim(data_IR_selection()))
  #   
  #   cat("SOURCE:\n")
  #   print(data_IR_selection() %>% dplyr::select(source) %>% dplyr::distinct())
  #   
  #   cat("METHOD:\n")
  #   print(data_IR_selection() %>% dplyr::select(method) %>% dplyr::distinct())
  #   
  #   cat("SUBSTANCE:\n")
  #   print(data_IR_selection() %>% dplyr::select(substance) %>% dplyr::distinct())
  #   
  # })
  
  
  mol_selection <- reactive({
    req(data_IR_selection)
    data_IR_selection() %>% 
      dplyr::arrange(source, substance, method) %>%
      dplyr::select(molname) %>% 
      dplyr::distinct(molname) %>% 
      dplyr::arrange(molname)
  })
  
  
  observe({
    req(mol_selection)
    updateSelectInput(
      inputId = "main_spectra", 
      choices = mol_selection(), 
      selected = mol_selection()[1]
    )
    updateSelectInput(
      inputId = "ref_spectra", 
      choices = mol_selection(), 
      selected = mol_selection()[1]
    )
    
  })
  
  # # Data for the main spectra
  # spectra_data <- reactive({
  #   req(input$main_spectra, data_IR_selection())
  #   
  #   data_IR_selection() %>% 
  #     dplyr::filter(molname==input$main_spectra)
  #   
  # })
  
  spectra_molname <- reactive({
    req(spectra_data())
    spectra_data() %>% 
      dplyr::select(molname) %>% 
      dplyr::distinct(molname) %>% 
      gsub(pattern = " (.*)", replacement = "")
  })
  

  # Data from file
  # observe({
  #   if (input$inputdata == 'File') {
  #     print("BISOGNA LEGGERE IL FILE")
  #     
  #     file <- input$userfile
  # 
  #     req(file)
  #     spectra_data_user <- OpenSpecy::read_jdx(file=file$datapath)
  #     # print(head(spectra_data_user))
  #     
  #   }
  #   
  # })
  
  spectra_data <- reactive({
    
    if (input$inputdata == 'File') {

      # print("LEGGO LO SPETTRO DAL FILE DELL'UTENTE")
      
      file <- input$userfile
      
      req(file)
      spectra_data_user <- OpenSpecy::read_jdx(file=file$datapath)
      spectra_data_user %>% 
        dplyr::mutate(Tperc = 100 * intensity) %>% 
        dplyr::mutate(molname="USERDATA")
      
    } else {
      
      # print("LEGGO LO SPETTRO DA DATABASE")
      
      req(input$main_spectra, data_IR_selection())
      
      data_IR_selection() %>% 
        dplyr::filter(molname==input$main_spectra)
      
    }
    
  })
  
  # Reference
  spectra_data_ref <- reactive({
    req(input$ref_spectra, data_IR_selection())
    
    data_IR_selection() %>% 
      dplyr::filter(molname==input$ref_spectra)
    
  })
  
  # observe({
  #   req(spectra_data())
  #   cat("spectra_data():\n")
  #   print(head(spectra_data()))
  #   print(max(spectra_data()$Tperc))
  #   print(dim(spectra_data()))
  # })
  
  
  y_values <- reactive({
    req(spectra_data())
    
    for (y_value in c(0,10,20,30,40,50,60,70,80,90,100,
                      110,120,130,140,150,160,170,180,190,200)) {
      if (y_value > max(spectra_data()$Tperc)) { 
        mymax <- y_value 
        break 
      }
    }
    for (y_value in c(200,190,180,170,160,150,140,130,120,
                      110,100,90,80,70,60,50,40,30,20,10,0)) {
      if (y_value < min(spectra_data()$Tperc)) { 
        mymin <- y_value 
        break 
      }
    }
    c(mymin, mymax)
  })
  
  
  wanted_shapes <- reactive({
    
    wanted_shapes <- list()
    shapes_counter <- 1
    
    if ("Fingerprint" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_yellow,
        line = list(color = transparent_yellow, opacity = 0.3),
        x0 = fingerprint_wn_high, x1 = fingerprint_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("O-H(alcohol, phenol)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = OHal_wn_high, x1 = OHal_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("O-H(carboxilic acid)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = OHca_wn_high, x1 = OHca_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("Water(stretching)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = WAT_str_high, x1 = WAT_str_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("Water(bending)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = WAT_ben_high, x1 = WAT_ben_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("N-H" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_blue,
        line = list(color = transparent_blue, opacity = 0.3),
        x0 = NH_wn_high, x1 = NH_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("N-H(amine salt)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_blue,
        line = list(color = transparent_blue, opacity = 0.3),
        x0 = NHs_wn_high, x1 = NHs_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("S-H" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_yellow,
        line = list(color = transparent_yellow, opacity = 0.3),
        x0 = SH_wn_high, x1 = SH_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("C-H" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CH_wn_high, x1 = CH_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("C-H(aromatic)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CHar_wn_high-2, x1 = CHar_wn_low+2, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("C-H(alkene/alkane)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CHalkene_alkane_line-2, x1 = CHalkene_alkane_line+2, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    
    if ("C-H(alkyne)" %in% input$tips1) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CHalkyne_wn_high, x1 = CHalkyne_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 

    if ("C=O" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = CO_wn_high, x1 = CO_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("C=N(conjugated)" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_blue,
        line = list(color = transparent_blue, opacity = 0.3),
        x0 = CN_wn_high, x1 = CN_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    if ("Other double bonds" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = double_bonds_high, x1 = double_bonds_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("Triple bonds" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = triple_bonds_high, x1 = triple_bonds_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("NO2(nitro)" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = NO2_wn_high, x1 = NO2_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("SO2" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = SO2_wn_high, x1 = SO2_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }

    if ("C-Cl" %in% input$tips2) {
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_violet,
        line = list(color = transparent_violet, opacity = 0.3),
        x0 = CCl_wn_high, x1 = CCl_wn_low, xref = "x",
        y0 = y_values()[1], y1 = y_values()[2], yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }

    wanted_shapes
    
  })
  
  spectra_plot <- reactive({
    req(input$main_spectra, spectra_data())
    
    spectra_plot <- plot_ly(spectra_data(), type = 'scatter', mode = 'lines') %>%
      layout(
        yaxis = list(title = "T%", range = y_values()),
        xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                     autorange = "reversed"),
        plot_bgcolor = input$bg_color,
        paper_bgcolor = 'rgba(0,0,0,0.5)',
             
        shapes = wanted_shapes(),
             
        font = list(color = '#FFFFFF'),
        legend = list(orientation = 'h', y = -0.3)
      )
    
    
    if (input$add_ref == TRUE) {
      spectra_plot <- spectra_plot %>%
        add_trace(data = spectra_data(),
                  x = ~wavenumber, y = ~Tperc,
                  name = input$main_spectra,
                  line = list(color = input$color)) %>% 
        add_trace(data = spectra_data_ref(),
                  x = ~wavenumber, y = ~Tperc,
                  name = input$ref_spectra,
                  line = list(color = input$ref_color))
    } else {
      spectra_plot <- spectra_plot %>% 
        add_trace(data = spectra_data(),
                  x = ~wavenumber, y = ~Tperc,
                  name = input$main_spectra,
                  line = list(color = input$color))
    }
    
    spectra_plot 
    
  })
  
  output$MyPlot <- renderPlotly({
    spectra_plot()
  })
  
  
  output$MyMolname <- renderUI({
    req(spectra_molname())
    spectra_molname()
  })
  
  output$MyImage <- renderImage({
    req(spectra_molname())
    
    filename_png <- paste0(spectra_molname(),".png")
    
    if (filename_png %in% images_all_files) {
      imagefile <- filename_png
      image_width <- 300
      image_height <- 300
      
    } else {
      imagefile <- "Image-Not-Available.png"
      image_width <- 300
      image_height <- 400
    }
    
    # Return a list containing the filename and alt text
    expr = list(src = normalizePath(file.path('www', 'images', imagefile)), 
                width = image_width, height = image_height,
                alt = "Image")
    
    }, deleteFile = FALSE
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
