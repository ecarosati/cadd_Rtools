
library(shiny)
library(plotly)
library(OpenSpecy)
library(glue)
library(magrittr)
library(dplyr)
library(ggplot2)

#spectra_dir <- "C:\\Users\\Emanuele\\Documents\\GithubRepositories\\cadd_Rtools\\data\\spectra"

# QUESTA VA COMMENTATA QUANDO SI LANCIA LA APP
# VA USATA SE VOGLIO LAVORARE SENZA APP
setwd("C:/Users/Emanuele/Documents/GithubRepositories/cadd_Rtools/app_IR")

# La directory spectra contiene dati già registrati
spectra_dir <- "spectra"
# print(list.files(spectra_dir))


create_df_all_spectra <- function(dir_spectra, code, nist=FALSE) {
  
  # primo step: estrarre dal nome del file le informazioni utili
  # I file devono chiamarsi IR_YYYYMMDD_MOLECOLA_CLASSE
  # Il primo campo indica il tipo di spettro
  # Il secondo campo indica la data 
  # Il terzo campo indica il nome della molecola
  # Il quarto campo indica il nome della classe
  # Tutte queste informazioni verranno inseritebin un dataframe
  
  if (nist) {
    dir_spectra <- file.path(dir_spectra, "NIST")
    idx_date <- NA
    idx_mol <- 2
    idx_method <- 3
  } else {
    dir_spectra <- file.path(dir_spectra, "LAB")
    idx_date <- 2
    idx_mol <- 3
    idx_method <- 4
  }
  
  all_files <- list.files(dir_spectra)
  f_counter <- 0
  # print(all_files)
  for (f_file in all_files) {
    f_name <- stringr::str_replace(string=f_file, pattern=".dx|.jdx", replacement = "")
    print(f_name)
    f_data <- stringr::str_split(string=f_name, pattern="_", simplify = F)[[1]] 
    
    # print(f_data)
    # print(length(f_data))
    if (f_data[1] == code) {
      f_code <- code
      # cat("CODE", f_code, "\n")
      if (nist) {
        f_date <- NA
      } else {
        f_date <- f_data[idx_date]
      }
      # cat("DATA", f_date, "\n")
      f_mol <- f_data[idx_mol]
      f_method <- f_data[idx_method]
      # f_class <- f_data[5]
      # cat("MOLECOLA", f_mol, "\n")
      # cat("CLASSE", f_class, "\n")
      # cat("f_counter", f_counter, "\n")
      spectra_data <- OpenSpecy::read_jdx(file=file.path(dir_spectra, f_file))
      # print(head(spectra_data))
      # print(colnames(spectra_data))
      if ('wavenumber' %in% colnames(spectra_data) && 
          'intensity' %in% colnames(spectra_data)) {
        
        # I dati dovrebbero essere OK e possiamo inserirli nel dataframe
        f_counter <- f_counter + 1
        
        if (f_counter == 1) {
          full_data <- spectra_data %>% 
            dplyr::mutate(code=f_code) %>% 
            dplyr::mutate(date=f_date) %>% 
            dplyr::mutate(mol=f_mol) %>% 
            dplyr::mutate(method=f_method)
            #dplyr::mutate(class=f_class)
        } else {
          single_data <- spectra_data %>% 
            dplyr::mutate(code=f_code) %>% 
            dplyr::mutate(date=f_date) %>% 
            dplyr::mutate(mol=f_mol) %>% 
            dplyr::mutate(method=f_method)
            #dplyr::mutate(class=f_class)
          full_data <- rbind(full_data, single_data)
        }
        # print(head(full_data))
      }
    }
  }
  return(full_data)
}

spectra_IR_lab <- create_df_all_spectra(dir_spectra="spectra", code="IR", nist=FALSE)
spectra_IR_nist <- create_df_all_spectra(dir_spectra="spectra", code="IR", nist=TRUE)
# spectra_UV_lab <- create_df_all_spectra(dir_spectra="spectra", code="UV", nist=FALSE)
# spectra_UV_nist <- create_df_all_spectra(dir_spectra="spectra", code="UV", nist=TRUE)




# read_spectra_from_file <- function(spectra_file, spectra_name, spectra_class, all_spectra_df) {
#   # my_IR_spectra1 <- glue::glue_collapse(c(spectra_dir, spectraname1), "\\")
#   spectra_data <- OpenSpecy::read_jdx(file=spectra_file) %>% 
#     dplyr::mutate(sostanza=spectra_name, classe=spectra_class)
#   
#   if (is.null(all_spectra_df)) {
#     all_spectra_df <- spectra_data
#   } else {
#     all_spectra_df %<>% dplyr::rbind(spectra_data)
#   }
#   return(all_spectra_df)
# }
# 
# spectraname1 <- "65-85-0-IR.jdx"
# spectraname2 <- "69-72-7-IR.jdx"



# glue::glue_collapse(c(spectra_dir, "69-72-7-IR.jdx"))

# my_IR_spectra1 <- glue::glue_collapse(c(spectra_dir, spectraname1), "\\")
# my_IR_data1 <- read_spectra_from_file(
#   spectra_file = my_IR_spectra1
# , spectra_name = "NAME_A"
# , spectra_class = "CLASS_B"
# , all_spectra_df = NULL
# )
# 
# print(head(my_IR_data1))
# 
# my_IR_spectra2 <- glue::glue_collapse(c(spectra_dir, spectraname2), "\\")
# my_IR_data12 <- read_spectra_from_file(
#   spectra_file = my_IR_spectra2
#   , spectra_name = "NAME_C"
#   , spectra_class = "CLASS_B"
#   , all_spectra_df = NULL
# )



# my_IR_data1 <- OpenSpecy::read_jdx(file=my_IR_spectra1)
# my_IR_data2 <- OpenSpecy::read_jdx(file=my_IR_spectra2)


# IR_spectra_dir <- "C:\\Users\\Emanuele\\Documents\\GithubRepositories\\didattica\\spettroscopia"
# spectraname3 <- "prova procaina.dx"
# my_IR_spectra3 <- glue::glue_collapse(c(spectra_dir, spectraname3), "\\")
# 
# 
# my_IR_data3 <- OpenSpecy::read_jdx(file=my_IR_spectra3)

# my_IR_data123 <- read_spectra_from_file(
#   spectra_file = my_IR_spectra3
#   , spectra_name = "NAME_D"
#   , spectra_class = "CLASS_F"
#   , all_spectra_df = NULL
# )




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lab Spettroscopia"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons(
            "spectroscopy",
            label="Select which type of spectra",
            choices = c("IR", "UV"),
            selected = "IR",
            inline = TRUE
          ),
          selectInput(
            "main_spectra",
            label="Select the molecule of your main spectra",
            choices = c(""),
            selected = ""
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # poi aggiungere anche colore x background
          # poi aggiungere anche colore x reference
          selectInput(
            "color",
            label="Select colour for your plot",
            choices = c("white", "yellow", "black", "blue", "red"),
            selected = "white"
          ),
          plotlyOutput("MyPlot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data1 <- reactive({
    my_IR_data1
  })
  
  data2 <- reactive({
    my_IR_data2
  })
  
  data3 <- reactive({
    my_IR_data3
  })
  
  observe({
    if (input$spectroscopy == "IR") {
      updateSelectInput(
        inputId = "main_spectra", 
        choices = c("CIAO", "IR"), 
        selected = c("CIAO")
      )
    } else if (input$spectroscopy == "UV") {
      updateSelectInput(
        inputId = "main_spectra", 
        choices = c("CIAO", "UV"), 
        selected = c("CIAO")
      )
    }
  })
  # output$main_spectra 
  
  output$MyPlot1 <- renderPlotly({
    plot_ly(data1(), type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~wavenumber, y = ~intensity, 
                name = 'Benzoic acid',
                line = list(color = 'rgba(240,236,19, 0.8)')) %>%
      layout(yaxis = list(title = "absorbance intensity [-]"),
             xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                          autorange = "reversed"),
             plot_bgcolor = 'rgb(17,0,73)',
             paper_bgcolor = 'rgba(0,0,0,0.5)',
             font = list(color = '#FFFFFF'))
  })
  
  # output$MyPlot2 <- renderPlotly({
  #   plot_ly(data3(), type = 'scatter', mode = 'lines') %>%
  #     add_trace(x = ~wavenumber, y = ~intensity, 
  #               name = 'Procaina',
  #               line = list(color = 'rgba(240,236,19, 0.8)')) %>%
  #     layout(yaxis = list(title = "absorbance intensity [-]"),
  #            xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
  #                         autorange = "reversed"),
  #            plot_bgcolor = 'rgb(17,0,73)',
  #            paper_bgcolor = 'rgba(0,0,0,0.5)',
  #            font = list(color = '#FFFFFF'))
  # })
  
  # output$MyPlot2 <- renderPlotly({
  #   plot_ly(data2(), type = 'scatter', mode = 'lines') %>%
  #     add_trace(x = ~wavenumber, y = ~intensity, 
  #               name = 'Procaina',
  #               line = list(color = 'rgba(240,236,19, 0.8)')) %>%
  #     layout(yaxis = list(title = "absorbance intensity [-]"),
  #            xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
  #                         autorange = "reversed"),
  #            plot_bgcolor = 'rgb(17,0,73)',
  #            paper_bgcolor = 'rgba(0,0,0,0.5)',
  #            font = list(color = '#FFFFFF'))
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
