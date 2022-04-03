
library(shiny)
library(plotly)
library(OpenSpecy)
library(glue)
library(magrittr)
library(dplyr)
library(ggplot2)

#spectra_dir <- "C:\\Users\\Emanuele\\Documents\\GithubRepositories\\cadd_Rtools\\data\\spectra"

transparent_yellow <- "rgba(255,255,51,0.5)"    # usato per fingerprint
transparent_red <- "rgba(226,50,5,0.5)"       # usato per O-H e C=O
transparent_blue <- "rgba(26,50,235,0.5)"       # OK: usato per N-H
transparent_green <- "rgba(26,150,65,0.5)"
transparent_orange <- "rgba(26,150,65,0.5)"
  
# wavenumber for regions of interest
fingerprint_wn_high <- 1300
fingerprint_wn_low <- 500

OH_wn_high <- 3400
OH_wn_low <- 3200

NH_wn_high <- 3500
NH_wn_low <- 3300

CH_wn_high <- 3100
CH_wn_low <- 2850

CHalkyne_wn_high <- 3325
CHalkyne_wn_low <- 3275

CHalkene_alkane_line <- 3000

CO_wn_high <- 1800
CO_wn_low <- 1650

double_bonds_high <- 1900
double_bonds_low <- 1600

triple_bonds_high <- 2260
triple_bonds_low <- 2100


link_analisi_farmaci <- "https://corsi.units.it/fa02/modulo/analisi-farmaci-046fa-2021-pds0-2019-ord-2019-comune"
link_analisi_medicinali <- "https://corsi.units.it/fa01/modulo/analisi-medicinali-037fa-2021-pds0-2015-ord-2015-comune"

# AGGIUNGERE PULSANTE (CHECKBOX) CHE FISSA L'ASSE Y TRA 0 E 100
# AGGIUNGERE POSSIBILITA IMPORTARE SPETTRO DA FILE
# ? SPOSTARE LEGENDA SOTTO ?
# AGGIUNGERE OPZIONE PER SUGGERIMENTI (RETTANGOLI SEMITRASPARENTI A FISSI VALORI DI X)



dir_spectra <- "spectra"

dir_images <- file.path("www", "images")
images_all_files <- list.files(path=dir_images)

print(images_all_files)

spectra_all_data <- readRDS(file = file.path(dir_spectra, "dataIR.R"))

# print(head(spectra_all_data))



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
                choices = c("ATR", "KBR", "NUJOL", "GAS", "ALL"),
                selected = "ATR"
              )
            ),
            column(
              4,
              selectInput(
                "substance",
                label="Substance",
                choices = c("ALL", "INC", "STD", "BKG"),
                selected = "ALL"
              )
            )
          ),
          selectInput(
            "main_spectra",
            label="Select the molecule of your main spectra",
            choices = c(""),
            selected = ""
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
            checkboxGroupInput(
              "tips",
              label="Regions of interest",
              choices = c("fingerprint", "O-H", "N-H", 
                          "C-H", "C-H(alkane/alkene)", "C-H(alkyne)",
                          "C=O", "Other double bonds", "Triple bonds")
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
  
  observe({
    req(data_IR_selection())
    cat("data_IR_selection():\n")
    print(head(data_IR_selection()))
    print(dim(data_IR_selection()))
    
    cat("SOURCE:\n")
    print(data_IR_selection() %>% dplyr::select(source) %>% dplyr::distinct())
    
    cat("METHOD:\n")
    print(data_IR_selection() %>% dplyr::select(method) %>% dplyr::distinct())
    
    cat("SUBSTANCE:\n")
    print(data_IR_selection() %>% dplyr::select(substance) %>% dplyr::distinct())
    
  })
  
  
  mol_selection <- reactive({
    req(data_IR_selection)
    data_IR_selection() %>% 
      dplyr::arrange(source, substance, method) %>% 
      dplyr::select(molname) %>% 
      dplyr::distinct(molname)
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
  
  # Data for the main spectra
  spectra_data <- reactive({
    req(input$main_spectra, data_IR_selection())
    
    data_IR_selection() %>% 
      dplyr::filter(molname==input$main_spectra)
    
  })
  
  # Reference
  spectra_data_ref <- reactive({
    req(input$ref_spectra, data_IR_selection())
    
    data_IR_selection() %>% 
      dplyr::filter(molname==input$ref_spectra)
    
  })
  
  # y_values <- reactive({
  #   
  # })
  
  wanted_shapes <- reactive({
    
    wanted_shapes <- list()
    shapes_counter <- 1
    
    if ("fingerprint" %in% input$tips) {
      cat("ADD fingerprint!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_yellow,
        line = list(color = transparent_yellow, opacity = 0.3),
        x0 = fingerprint_wn_high, x1 = fingerprint_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("O-H" %in% input$tips) {
      cat("ADD O-H!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = OH_wn_high, x1 = OH_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("N-H" %in% input$tips) {
      cat("ADD N-H!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_blue,
        line = list(color = transparent_blue, opacity = 0.3),
        x0 = NH_wn_high, x1 = NH_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("C-H" %in% input$tips) {
      cat("ADD C-H!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = CH_wn_high, x1 = CH_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    if ("C-H(alkane/alkene)" %in% input$tips) {
      cat("ADD C-H(alkane/alkene)!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CHalkene_alkane_line-2, x1 = CHalkene_alkane_line+2, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 
    
    
    if ("C-H(alkyne)" %in% input$tips) {
      cat("ADD C-H(alkyne)!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_green,
        line = list(color = transparent_green, opacity = 0.3),
        x0 = CHalkyne_wn_high, x1 = CHalkyne_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    } 

    if ("C=O" %in% input$tips) {
      cat("ADD C=O!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_red,
        line = list(color = transparent_red, opacity = 0.3),
        x0 = CO_wn_high, x1 = CO_wn_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("Other double bonds" %in% input$tips) {
      cat("ADD Triple bonds!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = double_bonds_high, x1 = double_bonds_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    if ("Triple bonds" %in% input$tips) {
      cat("ADD Triple bonds!\n")
      wanted_shapes[[shapes_counter]] <- list(
        type = "rect",
        fillcolor = transparent_orange,
        line = list(color = transparent_orange, opacity = 0.3),
        x0 = triple_bonds_high, x1 = triple_bonds_low, xref = "x",
        y0 = 00, y1 = 100, yref = "y"
      )
      shapes_counter <- shapes_counter + 1
    }
    
    wanted_shapes
    
  })
  
  observe({
    req(wanted_shapes())
    cat("CIAO:\n")
    print(wanted_shapes())
  })
  
  spectra_plot <- reactive({
    req(input$main_spectra, spectra_data())
    
    spectra_plot <- plot_ly(spectra_data(), type = 'scatter', mode = 'lines') %>%
      layout(
        yaxis = list(title = "T%"),
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
    
    spectra_plot #%>% 
      #add_contour( z = matrix(data=c(100,120,10,29),nrow=2,ncol = 2), )
    
  })
  
  output$MyPlot <- renderPlotly({
    spectra_plot()
  })
  
  output$MyImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    # filename <- normalizePath(file.path('./images', paste('image', input$n, '.jpeg', sep='')))
    imagefile <- images_all_files[1]
    
    # png(imagefile, width=400, height=400)
    
    # Return a list containing the filename and alt text
    # list(src = filename, alt = paste("Image number", input$n))
    expr = list(src = imagefile, alt = "ImageXXX")
    
    }, deleteFile = FALSE
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
