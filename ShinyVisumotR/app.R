#### Shiny VisumotR ####
#______________________________________ ####

# 1: Load Libraries ####
library(shiny)
library(rlang)
library(magrittr)
library(visumotR)
library(DT)
library(shinythemes)
library(ggecho)
library(facetscales)
library(shinydashboard)
library(shinyFiles)
library(shinythemes)
library(shinyWidgets)
#______________________________________ ####

# 2: UI elements ####

# 2.1: Dashboard elements ####

# 2.1.1: Header ####
header <- dashboardHeader(title='VisumotR')

# 2.1.2: Sidebar ####
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Read In Data', tabName = 'read', icon = icon('folder-open'),
                 menuSubItem('Tracking Data', tabName = 'read_data', icon = icon('line-chart')),
                 menuSubItem('Image Files', tabName = 'read_images', icon = icon('images'))),
        menuItem('View Data', tabName = 'view', icon = icon('eye'),
                 menuSubItem('Tracking Data', tabName = 'browse_data', icon = icon('line-chart')),
                 menuSubItem('Image Files', tabName = 'browse_images', icon = icon('images'))),
        menuItem('Run VisumotR', tabName = 'visumotR', icon = icon('laptop-code'),
                 menuSubItem('Frame', tabName = 'visumot_frame', icon = icon('image')),
                 menuSubItem('Summary', tabName = 'visumot_summary', icon = icon('bar-chart-o')),
                 menuSubItem('Animation', tabName = 'visumot_all', icon = icon('film'))),
        menuItem('How to', tabName = 'how_to', icon = icon('mortar-board')),
        menuItem('About', tabName = 'about', icon = icon('question')))
    )

# 2.1.3: Tab Items ####
read_data <- tabItem(tabName = 'read_data', fluidRow(
    column(width=12,
           tabBox(width=NULL, title = "Tracking Data",
                  tabPanel('Read In',
                           br(),
                           shinyFilesButton("file", " Browse Files", "Please select file(s)", multiple = TRUE, viewtype = "detail", icon = icon('folder')),
                           br(),
                           br(),
                           div(verbatimTextOutput("filepaths"), style= "width:50%"),
                           hr(),
                           uiOutput('select_data'),
                           actionButton('load_df', label = ' Read in Dataset' ,icon = icon('download'))
                           ),
                  tabPanel('Preview Dataset', 
                           uiOutput('df_table'))
           )
        )
    )
)

read_images <- tabItem(tabName = 'read_images', fluidRow(
    column(width=12,
           tabBox(width=NULL, title = "Image Files",
                  tabPanel('Import Images',
                           shinyFilesButton("images", "Browse", "Please select file(s)", multiple = TRUE, viewtype = "detail"),
                           hr(),
                           div(verbatimTextOutput("filepaths_img"), style= "width:50%")),
                  tabPanel('Preview Images',
                           uiOutput('select_image'),
                           actionGroupButtons(
                               inputIds = c("btn1", "btn2", "btn3", 'btn4'),
                               labels = list(tags$span(icon("angle-left"),""), tags$span(icon("angle-right"),""), tags$span(icon("angle-up"), ""), tags$span(icon("angle-down"), "")),
                               status = "primary"),
                           br(),
                           br(),
                           imageOutput("img"))
                  )
           )
    )
)

browse_data <- tabItem(tabName = 'browse_data', fluidRow(
    column(width=12,
           tabBox(width=NULL, title = "Tracking Data",
                  tabPanel('Prepare Dataset',
                           numericInput('scale_time','Time scaling:',1),
                           numericInput('scale_dim', 'Dimension scaling:',1),
                           radioGroupButtons('dims', 'Dimensions:', choices = list('2D'=2,'3D'=3))),
                  tabPanel('View Dataset')
                  )
            )
    )
)

browse_images  <- tabItem(tabName = 'browse_images', fluidRow(
    column(width=3,
           box(width=NULL, title = "Prepare Images",
                           radioGroupButtons('color_space', 'Color space:', choices = list('grayscale'='gray', 'RGB'='rgb')),
                           selectInput('bit_depth','Bit Depth:', c(8,16,32)),
                           radioGroupButtons('dims_img', 'Dimensions:', choices = list('2D'=2,'3D'=3)))
    ), column(width=9, 
              box(width=NULL, title="Image Viewer",
                  actionGroupButtons(
                      inputIds = c("btn1", "btn2", "btn3", 'btn4'),
                      labels = list(tags$span(icon("angle-left"),""), tags$span(icon("angle-right"),""), tags$span(icon("angle-up"), ""), tags$span(icon("angle-down"), "")),
                      status = "primary")
                  ))
)
)

visumot_frame_tab <- tabItem(tabName = 'visumot_frame', fluidRow(
    column(width=3,
           box(width=NULL, title = "Options",
               switchInput(label = 'Mapping',
                           inputId = "mapping",
                           value = TRUE
               ),
               switchInput(label = 'Continous',
                           inputId = "continous",
                           value = TRUE
               ),
               switchInput(label = 'Discrete',
                           inputId = "discrete",
                           value = FALSE
               ),
               switchInput(label = 'Subwindow',
                   inputId = "sub_window",
                   value = FALSE
               ),
               
               switchInput(label = 'Tracks',
                           inputId = "tracks",
                           value = FALSE
               ),
               switchInput(label = 'Points',
                           inputId = "points",
                           value = FALSE
               ),
               switchInput(label = 'Scalebar',
                           inputId = "scalebar",
                           value = FALSE
               ),
               switchInput(label = 'Axis',
                           inputId = "axis",
                           value = FALSE
               ), 
               switchInput(label = '3D',
                           inputId = "dims_3d",
                           value = FALSE
               )
               
               )
    ), column(width=9, 
              box(width=NULL, title="Viewer",
                  sliderInput("frame", 'Frame:', min=1, max=120, value=1),
                  actionGroupButtons(
                      inputIds = c("btn1", "btn2", "btn3", 'btn4'),
                      labels = list(tags$span(icon("angle-left"),""), tags$span(icon("angle-right"),""), tags$span(icon("angle-up"), ""), tags$span(icon("angle-down"), "")),
                      status = "primary"),
                  br(),
                  br(),
                  br(),
                  actionButton('save_frame', 'Save Plot', icon = icon('download'))
                  
              ))
)
)


# about <- tabItem(tabName = 'about',
#             fluidPage(
#                 div(includeMarkdown('/Users/charmel/VisumotR/README.md'),style='width:900px;')
#             )
#          )

# 2.1.4: Body ####
body <- dashboardBody(
    tabItems(read_data,read_images,browse_data,browse_images, visumot_frame_tab)
    )

# 2.2: Set up UI ####
ui <- dashboardPage(header, sidebar, body)

#______________________________________ ####

# 3: Server ####
server <- function(input, output, session) {
    
    ### set up file environment
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    ## select tracking data
    shinyFileChoose(input, "file", roots = volumes, session = session)
    
    
    ## show rendered overview over selected files and their paths
    output$filepaths <- renderPrint({
        if (is.integer(input$file)) {
            cat("No files have been selected...")
        } else {
            parseFilePaths(volumes, input$file)
        }
    })
    
    ## create select data ui
    output$select_data <- renderUI(
        list(
            selectInput(
                "tracking_data", "Select dataset:",
                choices = parseFilePaths(volumes, input$file) %>% distinct(name) %>% pull()
            )
        )
    )
    
    ## read in dataframe
    df <- reactive({
        read.csv(parseFilePaths(volumes, input$file) %>% filter(name == input$tracking_data) %>% pull(datapath) )
    })
    
    ## render dataframe
    output$df_tracking <- renderDataTable({df()}, options = list(scrollX = TRUE))
    
    ## create ui
    
    output$df_table <- renderUI({
        if(is.null(input$tracking_data)){
            list('Please select and import a tracking dataset first...')
        } else {
            list(dataTableOutput("df_tracking"))
        }
        
    })
    
    
    ## select image data
    shinyFileChoose(input, "images", roots = volumes, session = session)
    
    output$filepaths_img <- renderPrint({
        if (is.integer(input$images)) {
            cat("No images have been selected...")
        } else {
            parseFilePaths(volumes, input$images)
        }
    })
    
    ## create select data ui
    output$select_image <- renderUI(
        list(
            selectInput(
                "select_image", "",
                choices = parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull()
            )
        )
    )
    
    # A plot of fixed size
    output$img <- renderImage({

        # Numeric operators
        tmpfile <- image_read(parseFilePaths(volumes, input$images) %>% filter(name == input$select_image) %>% pull(datapath)) %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        
        # Return a list
        list(src = tmpfile, contentType = "image/jpg", height=400)
    })
    
}
#______________________________________ ####

shinyApp(ui, server)
