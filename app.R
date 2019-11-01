# minimal example for creating a semantic librarian
# corpus: Alice and Wonderland
# author: Matt Crump

# load data #####

load(file = "sl_data.RData", envir=.GlobalEnv)

# load libraries #####

library(shiny)
library(shinyjs)
library(lsa)
library(LSAfun)
library(ggplot2)
library(V8)
library(DT)
library(dplyr)
library(Rcpp)
library(ggrepel)
library(rvest)
library(stringr)

## load functions ####

source("functions/semanticL.R")

`%then%` <- shiny:::`%OR%`

# define UI ####
ui <- fluidPage(

    # Application title
    titlePanel("Search Alice"),

    # Sidebar to choose vector spaces
    sidebarLayout(
        sidebarPanel(
            selectInput("search_vectors", label = "Search Vector Space", 
                        choices = NULL),
            selectInput("target_vectors", label = "Target Vector Space", 
                        choices = NULL),
            selectInput("search_terms", label = "Search terms", 
                        choices = NULL),
            sliderInput("num_items", label = "number of items", min = 5, 
                        max = 30, value = 10, step=1),
            sliderInput("num_clusters", label = "number of clusters", min = 1, 
                        max = 4, value = 2, step=1)
        ),

    # Main panel for plot and table
        mainPanel(
           plotOutput("sl_plot"),
           DT::dataTableOutput("sl_table")
        )
    )
)

# define server ####
server <- function(input, output, session) {
    
    ## Initialize Reactive variables ####
    values <- reactiveValues()
    
    # Update and populate vector selection options ####
    
    sl_data_names <- names(sl_data)
    vector_spaces <- sl_data_names[str_detect(names(sl_data),"_vector")]
    vector_terms <- sl_data_names[str_detect(names(sl_data),"_terms")]
    
    updateSelectizeInput(session,'search_vectors', label = "Search Vector Space",
                         choices = vector_spaces)
    
    updateSelectizeInput(session,'target_vectors', label = "Target Vector Space",
                         choices = vector_spaces)
    
    observeEvent(input$search_vectors,{
        splitter <- unlist(strsplit(input$search_vectors,split="_"))
        values$term_choice <- sl_data_names[str_detect(names(sl_data),
                                                paste(splitter[1],"_terms",sep="",collapse=''))]
        print(values$term_choice)
        
        updateSelectizeInput(session,'search_terms', label = "Search Terms",
                             choices = sl_data[values$term_choice], selected=sl_data[values$term_choice][1])
    })
    
    observeEvent(input$target_vectors,{
        splitter <- unlist(strsplit(input$target_vectors,split="_"))
        values$target_terms <- sl_data_names[str_detect(names(sl_data),
                                                       paste(splitter[1],"_terms",sep="",collapse=''))]
        print(values$target_terms)
    })
    
    # MAIN OBSERVE FUNCTION
    
    observe({
        
        validate(
            need(input$search_terms != "","none")
        )
        
        # get similarities
        sim_df <- get_similarities(input$search_terms,
                                   sl_data[[values$term_choice]],
                                   sl_data[[input$search_vectors]],
                                   sl_data[[input$target_vectors]],
                                   sl_data[[values$target_terms]])
        
        req(is.null(dim(sim_df)) == FALSE)
        
        # get MDS solution
         values$mds_SS <- get_mds_fits(num_items = input$num_items,
                                       num_clusters = input$num_clusters,
                                       input_df = sim_df, 
                                       target_vectors = sl_data[[input$target_vectors]],
                                       target_terms = sl_data[[values$target_terms]])
         
         # output table
        
        output$sl_table <- DT::renderDT({
            
            return(sim_df[1:100,])
            
        }, options = list(pageLength=10), rownames=FALSE,
        escape=FALSE, selection = 'none')
        
        # output plot
    
        output$sl_plot <- renderPlot({
            
            validate(
                need(input$search_terms != "","none"),
                need(is.null(dim(sim_df)) == FALSE,"none")
            )
            
            ggplot(values$mds_SS, aes(x= X, y= Y,
                                      color=as.factor(cluster),
                                      text=terms,
                                      label=terms))+
                geom_label_repel(size=4, color= "black",
                                 label.size=0,
                                 force=5)+
                geom_point(size = 3, alpha=.75)+
                theme_void()+
                theme(legend.position = "none")
    
        })
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
