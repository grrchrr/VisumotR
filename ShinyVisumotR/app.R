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
library(dashboardthemes)
#______________________________________ ####

# 2: UI elements ####

# 2.1: Dashboard elements ####

# 2.1.1: Header ####
header <- dashboardHeader(title='VisumotR')

# 2.1.2: Sidebar ####
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Read Data', tabName = 'read', icon = icon('folder-open'),startExpanded = TRUE,
                 menuSubItem('Tracking Data', tabName = 'read_data', icon = icon('line-chart')),
                 menuSubItem('Image Files', tabName = 'read_images', icon = icon('images'))),
        # menuItem('View Data', tabName = 'view', icon = icon('eye'),
        #          menuSubItem('Tracking Data', tabName = 'browse_data', icon = icon('line-chart')),
        #          menuSubItem('Image Files', tabName = 'browse_images', icon = icon('images'))),
        menuItem('Run VisumotR', tabName = 'visumotR', icon = icon('laptop-code'),
                 menuSubItem('Frame', tabName = 'visumot_frame', icon = icon('image')),
                 menuSubItem('Summary', tabName = 'visumot_summary', icon = icon('bar-chart-o')),
                 menuSubItem('Animation', tabName = 'visumot_all', icon = icon('film'))),
        menuItem('How to', tabName = 'how_to', icon = icon('mortar-board')),
        menuItem('About', tabName = 'about', icon = icon('question')))
    )

# 2.1.3: Tab Items ####
# 2.1.3.1: Read Data ####
read_data <- tabItem(tabName = 'read_data', fluidRow(
    column(width=12,
           tabBox(width=NULL, title = "Tracking Data",
                  tabPanel('Read In',
                           'Select .csv files which contain your tracking data. Click on Read in Dataset to import the dataset and continue with the other tabs to prepare the dataset for using it with VisumotR.',
                           hr(),
                           shinyFilesButton("file", " Browse Files", "Please select file(s)", multiple = TRUE, viewtype = "detail", icon = icon('folder')),
                           br(),
                           br(),
                           div(verbatimTextOutput("filepaths"), style= "width:50%"),
                           hr(),
                           uiOutput('select_data'),
                           actionButton('load_df', label = 'Read in', icon = icon('download')),
                           hr(),
                           uiOutput('df_table')
                           ),
                  tabPanel('Prepare Dataset',
                           'Please set the scaling factors to refer to pixel and frames as well as
                           indicate which columns in your dataset correspond to track, time and X-, Y- and Z- positions.',
                           hr(),
                           fluidRow(
                               column(6,
                                      numericInput('scale_time','Time scaling', 1),
                                      numericInput('scale_dim', 'Dimension scaling', 1),
                                      radioGroupButtons('dims_df', 'Dimensions', choices = list('2D' = 2, '3D' = 3))),
                               column(6, uiOutput('prepare_df'))),
                           actionButton('update_df', 'Update dataframe', icon = icon('refresh')),
                           hr(),
                           uiOutput('df_table_prepared')
                           )
           )
        )
    )
)


# 2.1.3.2: Read Images ####
read_images  <- tabItem(tabName = 'read_images', fluidRow(
    column(width=3,
           box(width=NULL, title = "Image Specifications",
               radioGroupButtons('color_space', 'Color Space', choices = list('Grayscale'='gray', 'RGB'='rgb')),
               radioGroupButtons('bit_depth','Bit Depth', choices = list('8-bit'=8,'16-bit'=16,'32-bit'=32)),
               radioGroupButtons('dims_img', 'Dimensions', choices = list('2D'=2,'3D'=3)),
               radioGroupButtons('stack','Timeseries', choices = list('Multiple Files'=FALSE, 'Stack'=TRUE)),
               switchInput(label = 'Normalize',
                           inputId = "normalize",
                           value = FALSE))
    ), column(width=9, 
              tabBox(width=NULL, height = '100%' ,title="Image Browser",
                     tabPanel("Import", 
                              shinyFilesButton("images", "Browse Images", "Please select file(s)", multiple = TRUE, viewtype = "detail", icon = icon('folder')),
                              hr(),
                              div(verbatimTextOutput("filepaths_img"), style= "width:100%")),
                     tabPanel("Viewer",
                              uiOutput('select_image_ui'),
                              hr(),
                              imageOutput("img"),
                              hr())
                  ))
)
)

# 2.1.3.3: Frame ####
visumot_frame_tab <- tabItem(tabName = 'visumot_frame', fluidRow(
    column(width=3,
           box(width=NULL, title = "Options",
               switchInput(label = 'Mapping',
                           inputId = "mapping",
                           value = TRUE
               ),
               uiOutput('par.map'),
               uiOutput('par.map.options'),
               hr(),
               switchInput(label = 'Subwindow',
                   inputId = "sub_window",
                   value = FALSE
               ),
               uiOutput('sub_image_opt'),
               hr(),
               switchInput(label = 'Tracks',
                           inputId = "tracks",
                           value = TRUE
               ),
               uiOutput('tracks_opt'),
               hr(),
               switchInput(label = 'Points',
                           inputId = "points",
                           value = TRUE
               ),
               uiOutput('points_opt'),
               hr(),
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
                  uiOutput('frame_select'),
                  hr(),
                  fluidRow(column(width = 1, actionGroupButtons(
                      inputIds = c("frame_b", "frame_f"),
                      labels = list(tags$span(icon("angle-left"),""), tags$span(icon("angle-right"),"")),
                      status = "primary",
                      direction = 'vertical')),
                      column(width=10,plotOutput('visumot_frame', height="800px")),
                      column(width=1,actionButton('update_frame','',icon = icon('sync'), color = 'primary'),
                             actionButton('save_frame','', icon = icon('download'), color='primary')),
                  hr()
                  )
              ))
)
)

# 2.1.3.4: Summary ####

# 2.1.3.5: Animation ####

# 2.1.3.6: How to ####

# 2.1.3.7: About ####
about <- tabItem(tabName = 'about',
            fluidPage(
                tags$video(id="video2", type = "video/mpeg4", src = 'SampleVideo_1280x720_2mb.mp4', controls = 'controls'),
                tags$img(src='test_img.png')
            )
         )

# 2.1.4: Body ####
body <- dashboardBody(
    tabItems(read_data, read_images, visumot_frame_tab, about)
    )

# 2.2: Set up UI ####
ui <- dashboardPage(header, sidebar, body)

#______________________________________ ####

# 3: Server ####
server <- function(input, output, session) {
    
    # 3.1: Read In Data ####
    # 3.1.1: Filesystem Data ####
    
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
    output$select_data <- renderUI({
        list(
            selectInput(
                "tracking_data", "Select dataset:",
                choices = parseFilePaths(volumes, input$file) %>% distinct(name) %>% pull()
            )
        )}
    )
    
    ## read in dataframe
    df_raw <- eventReactive(input$load_df,{
        read.csv(parseFilePaths(volumes, input$file) %>% filter(name == input$tracking_data) %>% pull(datapath)) %>% as_tibble()
    })
    
    ## modify column names 
    
    df <- eventReactive(input$update_df,{
        vars <- c(track=input$track, time=input$time, X=input$X, Y=input$Y)
        if (input$dims_df==3) {
            vars <- c(vars,Z=input$Z)
        }
        df_raw() %>% rename(vars)
    }) 
    
    
    
    ## UI for selecting X, Y, Z, time, and track columns
    
    output$prepare_df <- renderUI({
        if (input$tracking_data=='') {
            return(list())
        } else {
            columns <- df_raw() %>% colnames()
            column_names <- list(
                selectInput('track','Track', choices = columns, selected = columns[1]),
                selectInput('time', 'Time', choices = columns, selected = columns[2]),
                selectInput('X', 'X position', choices = columns, selected = columns[3]),
                selectInput('Y', 'Y position', choices = columns, selected = columns[4])
            )
        } 
        if (input$dims_df==3) {
             column_names <- append(column_names,list(selectInput('Z', 'Z position', choices = columns, selected = columns[5])))
        }
        return(column_names)
    })
    
    
    ## 3.1.1.1: Dataframe Browser ####
    output$df_raw <- renderDataTable({df_raw()}, options = list(scrollX = TRUE))
    
    ## create ui
    observeEvent(input$load_df,{
        output$df_table <- renderUI({
            if(input$tracking_data==''){
                list()
            } else {
                list(dataTableOutput("df_raw"))
            }
            
        })
    })
    
    
    ## 3.1.1.1: Dataframe Browser ####
    output$df_tracking <- renderDataTable({df()}, options = list(scrollX = TRUE))
    
    ## create ui
    observeEvent(input$update_df,{
    output$df_table_prepared <- renderUI({
        if(input$tracking_data==''){
            list()
        } else {
            list(dataTableOutput("df_tracking"))
        }
        
    })
    })
    
    # 3.1.2: Filesystem Images ####
    shinyFileChoose(input, "images", roots = volumes, session = session)
    
    output$filepaths_img <- renderPrint({
        if (is.integer(input$images)) {
            cat("No images have been selected...")
        } else {
            parseFilePaths(volumes, input$images)
        }
    })
    
    ## create select data ui
    output$select_image_ui <- renderUI(
        list(fluidRow(
            column(5,selectInput(
                "select_image", "",
                choices = parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull())),
            column(1,actionGroupButtons(inputIds=c('prev_img','next_img'),
                               labels = list(tags$span(icon("angle-up"),""), tags$span(icon("angle-down"),"")),
                               status = "primary",
                               direction = 'vertical'))
        )
            
        )
    )
    
    # update frames with action buttons
    observeEvent(input$prev_img, {
        
        image_list <- parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull()
        
        current_image <- which(image_list==input$select_image)
        
        updateSelectInput(session,inputId='select_image', selected = image_list[current_image-1])
    })
    
    observeEvent(input$next_img, {
        
        image_list <- parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull()
        
        current_image <- which(image_list==input$select_image)
        
        updateSelectInput(session,inputId='select_image', selected = image_list[current_image+1])
    })
    
    
    # 3.1.2.1: Image Viewer ####
    # A plot of fixed size
    output$img <- renderImage({
        if(input$select_image==''){
            tmpfile <- image_blank(400,400,color="white")
        } else {
            tmpfile <- image_read(parseFilePaths(volumes, input$images) %>%
                                      filter(name == input$select_image) %>%
                                      pull(datapath))
        }
        if(input$normalize){
            tmpfile <- tmpfile %>% image_normalize()
        }
        tmpfile <- tmpfile %>% image_write(tempfile(fileext='jpg'), format = 'jpg')
        # Return a list
        list(src = tmpfile, contentType = "image/jpg", height='100%')
    })
    
    
    # 3.2: View Data ####
    
    # 3.3: Run VisumotR ####
    # 3.3.1: Frame ####
    # 3.3.1.1: Conditional Option UI ####
    # conditional sliders
    output$par.map <- renderUI({
        if(input$mapping){
            list(
                checkboxGroupButtons(
                    inputId = "map.select",
                    choiceValues = c("continous", 
                                "discrete"),
                    choiceNames = c('Continous','Discrete'), 
                    status = 'primary', 
                    selected = 'continous'
                )
            )
        } else {
            list()
        }
    })
    
    output$frame_select <- renderUI({
        sliderInput("frame", 'Frame:', min=1, max=df()%>%distinct(time)%>%nrow(), value=1)
    })
    
    output$select_range <- renderUI({
        if(any(input$map.select=='continous')) {
            min_value <- df() %>% select_(input$par.map.cont) %>% pull() %>% min()
            max_value <- df() %>% select_(input$par.map.cont) %>% pull() %>% max()
            sliderInput('par.map.cont.range','',value = c(min_value,max_value), min = min_value, max = max_value)
        } else {
            list()
        }
        
    })
    
    
    
    # conditional options for mapping 
    output$par.map.options <- renderUI({
        ui_cont <- list()
        ui_disc <- list()
        if(input$mapping){
            if(any(input$map.select=='continous')) {
                ui_cont <- list(fluidRow(column(1,
                                                radioGroupButtons('par.map.cont.val',
                                                                  choiceNames = c('Color','Size'),
                                                                  choiceValues = c('color','size'),
                                                                  status = 'primary',
                                                                  direction = 'vertical')),
                                         column(2),
                                         column(7,
                                                selectInput('par.map.cont',
                                                            'Parameter:',
                                                            choices = df() %>% select_if(is.numeric) %>% colnames())
                                                )
                                         ),
                                uiOutput('select_range')
                                )
            }
            if(any(input$map.select=='discrete')){
                ui_disc <- list(fluidRow(column(1,
                                                radioGroupButtons('par.map.disc.val',
                                                                  choiceNames = c('Color','Shape'),
                                                                  choiceValues = c('color','shape'),
                                                                  selected = 'shape',
                                                                  direction = 'vertical', 
                                                                  status = 'primary')),
                                         column(2),
                                         column(7,
                                                selectInput('par.map.disc',
                                                            'Parameter:',
                                                            choices = df() %>% select_if(negate(is.numeric)) %>% colnames())
                                                )
                                         )
                )
            }
        }
        append(ui_cont, ui_disc)
    })
    
    # conditional options for subwindow
    output$sub_image_opt <- renderUI({
        if (input$sub_window) {
            list(sliderInput('sub_n_col','Columns', value = 3, min=1, max=15, step = 1), 
                sliderInput('sub_window_size', 'Windowsize', value = 50, min=5, max = 250, step = 5)
                )
        } else {
            list()
        }
    })
    # conditional options for points 
    output$points_opt <- renderUI({
        if(input$points){
            list(
                sliderInput('points.size','Size', value = 1, min = 1, max=20, step = 1),
                sliderInput('points.alpha','Alpha', value = 0.75, min=0, max = 1, step = 0.05),
                radioGroupButtons('points.stat','Blur',choices = list('On'='echo', 'Off'='identity'))
            )
        } else {
            list()
        }
    })
    
    
    
    # conditional options for tracks
    output$tracks_opt <- renderUI({
        max_frame <- df()%>%distinct(time)%>%nrow()
        if(input$tracks){
            list(
                sliderInput('tracks.length', 'Length', value=max_frame, min = 0, max=max_frame, step = 1),
                sliderInput('tracks.size', 'Size', value = 1, min = 1, max = 20, step = 1),
                sliderInput('tracks.alpha', 'Alpha', value = 0.5, min=0, max = 1, step = 0.05),
                selectizeInput('tracks_select','Filter tracks',choices=df()%>%distinct(track)%>%pull(), selected=NULL, multiple=TRUE)
            )
        } else {
            list()
        }
    })
    
    # update frames with action buttons
    observeEvent(input$frame_b, {
        current_frame <- input$frame
        updateSliderInput(session,inputId='frame', value = current_frame-1)
    })
    observeEvent(input$frame_f, {
        current_frame <- input$frame
        updateSliderInput(session,inputId='frame', value = current_frame+1)
    })
    
    
    
    # 3.3.1.2: Run vismot_frame() ####
    frame_gg <- eventReactive(list(input$update_frame,input$frame_b,input$frame_f),{
        visumot_frame(df(),
                      frame = input$frame,
                      image = parseFilePaths(volumes, input$images) %>% slice(input$frame) %>% pull(datapath),
                      points.size = input$points.size,
                      points.alpha = input$points.alpha,
                      points.stat = input$points.stat,
                      tracks = input$tracks_select,
                      tracks.size = input$tracks.size,
                      tracks.length = input$tracks.length,
                      tracks.alpha = input$tracks.alpha,
                      sub.img = input$sub_window,
                      sub.window = input$sub_window_size,
                      sub.col = input$sub_n_col, 
                      par.min = input$par.map.cont.range[1],
                      par.max = input$par.map.cont.range[2],
                      par.map = input$par.map.cont,
                      image.normalize = input$normalize)
    }
    )
    
    output$visumot_frame <- renderPlot(frame_gg())
    
    
    # 3.3.2: Summary ####
    # 3.3.3: Animation ####
    
    
    # 3.4: Save files ####
    
    
    
}
#______________________________________ ####

shinyApp(ui, server)
