library(shiny)
library(shinyjs)
library(RSQLite)
library(heat3d)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rayshader)
library(rgl)

##### Setup UI #####
ui.setup <- fluidPage(
  #Not working???
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  h2('Welcome!'),
  actionButton('beginExp', 'Begin Experiment')
)



##### Experiment UI #####
ui.experiment <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2('Experiment'),
      radioButtons('guessExpSmaller', 'Which value is smaller?', c('Letter 1','Letter 2')),
      sliderInput('guessExpRatio', 'If the larger value that you identified is 100 units, how many units is the smaller value?',
                  min = 0, max = 100, value = 50, step = 0.01, ticks = F),
      actionButton('expNext', 'Next'),
      uiOutput('expAttentionCheck'),
      checkboxInput('expShowLocations', 'Check this box if you need help finding the values to compare'),
      uiOutput('expShowLocationsPlot')
    ),
    mainPanel(
      h2('Data'),
      plotOutput('expCurrentPlot'),
      uiOutput('expCurrentPlotUI'),
      tableOutput('expCurrentTrial')
    )
  )
)


##### End #####
ui.end <- fluidPage(
  h2('Thank you for participating!')
)



##### UI Logic #####
ui <- navbarPage(
  title = 'Experiment',
  id = 'navbar',
  tabPanel('Setup', ui.setup),
  tabPanel('Experiment', ui.experiment),
  tabPanel('End', ui.end)
)



##### Server Logic #####
server <- function(input, output, session) {

  ##### Experiment Setup #####
  observeEvent(input$beginExp, {

    # Collect user information and update subject number for block
    conn <- dbConnect(SQLite(), 'test.db')
    if('user' %in% dbListTables(conn)){
      user <- dbReadTable(conn, 'user')
      trial_data$subjectN <- 1 + max(user$subjectN)
    } else {
      trial_data$subjectN <- 0
    }
    user <- data.frame(
      subjectID = session$token,
      subjectN = trial_data$subjectN,
      timeStartApp = trial_data$timeStartApp
      # Demographic information too
    )
    dbWriteTable(conn, 'user', user, append = TRUE)
    dbDisconnect(conn)


    # Create lineup order
    trial_data$expLineup <- design %>%
      filter(block == (trial_data$subjectN %% max(design$block+1))) %>%
      arrange(plot, trt.ratio) %>%
      group_by(plot) %>%
      mutate(plotOrder = 1:n()) %>%
      ungroup() %>%
      slice_sample(prop=1) %>%
      mutate(trialNum = 1:n())


    # Sample data for each plot
    # Process: sample 5 values for plotID for each plot type
    trial_data$expRatios <- replicate(
      length(unique(trial_data$expLineup$plot)),
      sample.int(
        length(unique(design$trt.ratio)),
        size = length(unique(as.character(trial_data$expLineup$trt.ratio))))
      ) %>% as.data.frame() %>%
      pivot_longer(everything(), names_to = 'plot', values_to = 'plotID') %>%
      arrange(plot, plotID) %>%
      group_by(plot) %>%
      mutate(plotOrder = 1:n()) %>%
      mutate(plot = case_when(
        plot == 'V1' ~ '2d',
        plot == 'V2' ~ '3ddc',
        plot == 'V3' ~ '3dds',
        plot == 'V4' ~ '3dp'
      )) %>% ungroup()
    # names(trial_data$expRatios) <- unique(trial_data$expLineup$plot)


    # Randomize plotID and inner join with plot data
    trial_data$expData <- trial_data$expRatios %>%
      # pivot_longer(everything(), names_to = 'plot', values_to = 'plotID') %>%
      # slice_sample(prop = 1) %>%
      # mutate(trialNum = 1:n()) %>%
      inner_join(design_data, by = c('plotID', 'plot')) %>%
      inner_join(trial_data$expLineup, by = c('plot', 'plotOrder')) %>%
      arrange(trialNum)

    expInitialLetters <- trial_data$expData %>%
      filter(trialNum == 1, ratio == trt.ratio)


    updateNavbarPage(session, 'navbar', selected = 'Experiment')
    updateRadioButtons(session, 'guessExpSmaller', choices = sort(expInitialLetters$label),
                       selected = '')
    updateSliderInput(session, 'guessExpRatio', value = 50)
  })






  #Update to next trial
  observeEvent(input$expNext, {
    if(trial_data$expCurrentTrial < nrow(trial_data$expRatios)){
      trial_data$expCurrentTrial <- trial_data$expCurrentTrial + 1
      # updateRadioButtons(session, 'guessExpSmaller', choices = sort(trial_data$expCurrentRatio$label),
      #                    selected = '')
      updateSliderInput(session, 'guessExpRatio', value = 50)

    } else {
      updateNavbarPage(session, 'navbar', selected = 'End')
      trial_data$expCurrentTrial <- 1
    }


  })


  trial_data <- reactiveValues(
    subjectID = session$token,
    subjectN = NULL,
    timeStartApp = Sys.time(),
    timeStartTrial = NULL,
    expLineup = NULL,
    expRatios = NULL,
    expData = NULL,
    expCurrentTrial = 1,
    expCurrentRatio = NULL
  )

  output$expCurrentTrial <- renderTable({
    filter(trial_data$expData, trialNum == trial_data$expCurrentTrial)
    # trial_data$expLineup
  })


  output$expShowLetters <- renderUI({
    sort(trial_data$expCurrentRatio$label)


  })




  output$expCurrentPlot <- renderPlot({

    trial_data$expCurrentRatio <- filter(trial_data$expData,
                                         trialNum == trial_data$expCurrentTrial,
                                         ratio == trt.ratio)
    updateRadioButtons(session, 'guessExpSmaller', choices = sort(trial_data$expCurrentRatio$label),
                       selected = '')

    filter(trial_data$expData, trialNum == trial_data$expCurrentTrial) %>%
      ggplot(aes(x = x, y = y, fill = z)) +
      geom_tile(color = 'black') +
      geom_text(aes(label = label), na.rm = T) +
      scale_fill_gradient(low = 'white', high = 'darkblue',
                          limits = c(0,100)) +
      scale_x_continuous(breaks = 1:10) +
      scale_y_continuous(breaks = 1:10) +
      theme_minimal() +
      theme(aspect.ratio = 1, legend.position = 'none')
  })






  output$expAttentionCheck <- renderUI({
    if(F){
      list(h4('Attention Check'),
        numericInput('expAttentionCheckX', 'What is the x coordinate of the smaller bar you identified?', value = NULL),
      numericInput('expAttentionCheckY', 'What is the y coordinate of the smaller bar you identified?', value = NULL))
    } else {NULL}
  })



  output$expShowLocationsPlot <- renderUI({
    if(input$expShowLocations){
      locationData <- filter(trial_data$expData, trialNum == trial_data$expCurrentTrial) %>%
        # inner_join(trial_data$expLineup, by = c('plot', 'plotOrder')) %>%
        mutate(mark = ifelse(trt.ratio == ratio & !is.na(ratio), T, F))

      output$locationPlot <- renderPlot({
        locationData %>%
        ggplot(aes(x = x, y = y, fill = mark)) +
        geom_tile(color = 'black') +
        scale_fill_manual(values = c('white', 'red')) +
        scale_x_continuous(breaks = 1:10) +
        scale_y_continuous(breaks = 1:10) +
        theme_minimal() +
        theme(aspect.ratio = 1, legend.position = 'none')})
      output$test <- renderTable({
        # trial_data$expRatios
        # filter(trial_data$expData, trialNum == trial_data$expCurrentTrial)
        # locationData
        # trial_data$expRatios
        # trial_data$expCurrentRatio
        # trial_data$expLetters
        })
      list(
        plotOutput('locationPlot'),
        tableOutput('test'))
    }
  })



  output$expCurrentPlotUI <- renderUI({

    trial_data$expCurrentRatio <- filter(trial_data$expData,
                                         trialNum == trial_data$expCurrentTrial,
                                         ratio == trt.ratio)
    updateRadioButtons(session, 'guessExpSmaller', choices = sort(trial_data$expCurrentRatio$label),
                       selected = '')


    plot.type <- unique(trial_data$expCurrentRatio[['plot']])
    plot.num <- unique(trial_data$expCurrentRatio[['plotID']])

    #Color plot (2d, 3ddc)
    plot.color <- filter(trial_data$expData, trialNum == trial_data$expCurrentTrial) %>%
      ggplot(aes(x = x, y = y, fill = z)) +
      geom_tile(color = 'black') +
      geom_text(aes(label = label), na.rm = T, color = 'black') +
      scale_fill_gradient(low = 'white', high = 'darkblue',
                          limits = c(0,100)) +
      scale_x_continuous(breaks = 1:10) +
      scale_y_continuous(breaks = 1:10) +
      labs(x = 'x-axis', y = 'y-axis', caption = paste0('Plot ID: ', plot.type, '-', plot.num))+
      theme_minimal() +
      theme(aspect.ratio = 1, panel.background = element_rect(fill = 'white', color = 'white'),
            panel.grid = element_blank(), legend.position = 'none',
            plot.background = element_rect(fill = 'white', color = 'white'),
            axis.ticks = element_line(color = 'black'))

    #Solid plot (3dds)
    plot.solid <- filter(trial_data$expData, trialNum == trial_data$expCurrentTrial) %>%
      ggplot(aes(x = x, y = y, fill = z)) +
      geom_tile(color = 'black') +
      geom_text(aes(label = label), na.rm = T, color = 'black') +
      scale_fill_gradient(low = 'darkblue', high = 'darkblue',
                          limits = c(0,100)) +
      scale_x_continuous(breaks = 1:10) +
      scale_y_continuous(breaks = 1:10) +
      labs(x = 'x-axis', y = 'y-axis', caption = paste0('Plot ID: ', plot.type, '-', plot.num))+
      theme_minimal() +
      theme(aspect.ratio = 1, legend.position = 'none',
            panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            panel.grid = element_blank())


    output$plot2d <- renderPlot({
      plot.color
    })

    output$plot3ddc <- renderRglwidget({
      plot_gg(plot.color, raytrace = F)
      rglwidget()
    })

    output$plot3dds <- renderRglwidget({
      plot_gg(plot.solid, raytrace = F)
      rglwidget()
    })


    switch(unique(filter(trial_data$expData, trialNum == trial_data$expCurrentTrial)[['plot']]),
           "2d" = plotOutput("plot2d"),
           "3ddc" = rglwidgetOutput("plot3ddc"),
           "3dds" = rglwidgetOutput("plot3dds"))


  })






}


shinyApp(ui, server)
