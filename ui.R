library("shiny")
library("shinyjs")
library("shinythemes")
library("shinydashboard")
library("DT")
library("igraph")
#library("stringr")
shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    titlePanel(div("Network analysis of onset and persistent symptoms in COVID 19 patients")
    ),
    sidebarPanel(
        shinyjs::useShinyjs(),
        tabsetPanel(
            tabPanel("Demo",
                     h4("Analysis:"),
                     radioButtons('progredient_d',label="Shall progredient symptoms be considered for coloring?",
                                  choices = c("No","Yes"),selected = "Yes",inline = T),
                     uiOutput("ColorsUI_d"),
                     radioButtons('shuffle_d',label="Re-shuffle?",
                                  choices = c("No","Yes"),selected = "No",inline = T),
                     hr(),
                     actionButton('do_demo',"Start analysis",class = "btn-primary")
            ),
            tabPanel("Generate your own network",
                     h4("Input:"),
                     fileInput('inputFile',label = "Upload symptoms file"),
                     h5("Note: a 1- or 2-column txt-file is required. The initial symptoms are
                     defined in the first column (comma-separated), the progredient symptoms
                        are defined in the second column. Please make sure your file has a header."),
                     h5("Please upload anonymous data only!",style="color:red"),
                     hr(),
                     h4("Analysis:"),
                     radioButtons('progredient',label="Shall progredient symptoms be considered for coloring?",
                                  choices = c("No","Yes"),selected = "Yes",inline = T),
                     uiOutput("ColorsUI"),
                     hr(),
                     actionButton('do',"Start analysis",class = "btn-primary")
            ),
            tabPanel("Further information",
                     h4("Contact Information:"),
                     h5("Dr. Sarah Sandmann"),
                     h5("University of Muenster"),
                     h5("48149 Muenster"),
                     h5("Germany"),
                     h5("sarah.sandmann@uni-muenster.de"),
                     hr(),
                     h4("How to Cite:"),
                     h5("When using our app for your work, please cite our paper: "),
                     h5("Persistent symptoms and lab abnormalities in patients who recovered from COVID-19. doi: 10.1038/s41598-021-91270-8"),
                     hr(),
                     h4("Disclaimer:"),
                     h5("This web application is experimental and for research use only. We do not warrant 
                        that the information accessible via this website is accurate, complete or current."),
                     )
        )
        
    ),
    mainPanel(
        shinyjs::useShinyjs(),
        tabsetPanel(
            tabPanel("Network",
                     div(id = "text"),
                     imageOutput("plot1"),
            ),
            tabPanel("Analyzed data",
                     DT::dataTableOutput('table_dt')
            )
        )
    )
)
)




