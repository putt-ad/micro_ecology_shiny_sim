# geochem correlation viewer
# A.putt more at https://github.com/putt-ad/micro_ecology_shiny_sim
# April 2020

# check if pkgs are installed already, if not, this should install them automatically:
# (http://stackoverflow.com/a/4090208/1036500)
list.of.packages <- c("ggplot2",
                      "readr",
                      "shiny",
                      "DT",
                      "curl",
                      "RCurl",
                      "GGally",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "tabplot",
                      "shinycssloaders",
                      "shinythemes",
                      "leaflet",
                      "RColorBrewer",
                      "tidyverse"
                      )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all these
lapply(list.of.packages, require, character.only = TRUE)


ui <- fluidPage(theme = shinytheme("sandstone"),
                mainPanel(
                  titlePanel("Geochem Correlation Viewer"),
                  p("more project available at", a("Andrew's GitHub", href = "https://github.com/putt-ad")),

                  tabsetPanel(
                    # opening tab - loading the csv
                    tabPanel("Data input",
                             p("Before uploading your data, check that the the numeric variables contain only the digits 0-9 and uniform nomenclature to indicate missing data."),
                             p("Heads up! Since missing data can skew a PCA, rows with one or more NAs will be excluded from the PCA."),
                             p("To see the app's function it will run automatically with ", a("example geochem data", href = "https://github.com/putt-ad/micro_ecology_shiny_sim/blob/master/test_corr.csv"),  " but you can upload your own data using the upload data option below."),
                             tags$hr(),

                             # select and read file (tab 1)
                             fileInput('file1', 'Choose a data file to upload:',
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv'
                                       )),

                             tags$hr(),
                             p("Select the options that match your file format."),
                             p("headers are the titles of the columns in the uploaded file, normally this will remain selected."),
                             #  header selection

                             #  header selection
                             radioButtons(inputId = 'header',
                                          label = 'Header',
                                          choices = c('Columns have headers'='Yes',
                                                      'Columns do not have headers'='No'),
                                          selected = 'Yes'),

                             #  delimiter selection

                             p("seperators are what seperate the columns from each other."),

                             radioButtons('sep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          ','),
                             #  quaotation of data selection

                             radioButtons('quote', 'Quote',
                                          c(None='',
                                            'Double Quote'='"',
                                            'Single Quote'="'"),
                                          '"'),

                             #  missing data selection
                             radioButtons('na', 'Missing Data',
                                          c(None='',
                                            'NA'='NA',
                                            'na' = 'na',
                                            'n.a.'='n.a.',
                                            'N/A' = 'N/A'),
                                          'NA'),

                             tags$hr(),
                             p("Here is the raw data from the CSV file"),
                             DT::dataTableOutput('contents')

                    ), # end file  tab
                    # data viewer tab table plot *LETS ADD A PLOT OF OUTPUTS HERE TOO?? (tab2)
                    tabPanel("Data viewer",

                             p("The tableplot below (it will take a few seconds to appear)
                     may be useful to explore the relationships between the variables,
                     to discover strange data patterns, and to check the occurrence and selectivity of missing values."),
                             plotOutput("tableplot")%>% withSpinner(color="#ff8700"),
                             tags$hr(),
                             downloadButton("SummaryDownload", "Download"),
                             p("Here is a summary of the data"),
                             tableOutput('summary'),
                             tags$hr()
                    ), # end  tab

                    # show data relationships
                    tabPanel("Map",
                             #                   uiOutput("choose_columns_biplot"),
                             tags$hr(),
                             p("We're still a work in progress."),
                             p("Map becomes active before data is available and crashes app"),
                             #                    leafletOutput("map"),
                             #                    uiOutput("pH_slider"),

                             selectInput("colors", "Map Marker Color",
                                         rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                             ),
                             checkboxInput("legend", "Show legend", TRUE),


                             #                   plotOutput("corr_plot"),
                             tags$hr(),

                             #                   p("Summary of correlations"),
                             #                  tableOutput("corr_tables")
                    ),
                    #
                    tabPanel("Correlation Plots",
                             uiOutput("choose_columns_biplot"),
                             tags$hr(),
                             p("This plot may take a few moments to appear when analysing large datasets. You may want to exclude highly correlated variables from the PCA."),

                             plotOutput("corr_plot")%>% withSpinner(color="#ff8700"),
                             tags$hr(),
                             h2("Summary of correlations"),
                             p("Sorted from most positive to most negative"),
                             tableOutput("corr_tables"),
                             #          ), # end  tab

                             #          tabPanel("Diagnostics",

                             p("Find out more about the KMO and Bartlett Tests", a("here", href = "https://www.ibm.com/support/knowledgecenter/SSLVMB_26.0.0/statistics_casestudies_project_ddita/spss/tutorials/fac_telco_kmo_01.html"),
                               "Also note that since missing data messes up principal components analysis. Variables with zero variance are excluded."),
                             tags$hr(),
                             p("Here is the output of Bartlett's sphericity test. Bartlett's test of sphericity tests whether the data comes from multivariate normal distribution with zero covariances. If p > 0.05 then PCA may not be very informative"),
                             verbatimTextOutput("bartlett"),
                             tags$hr(),
                             p("Here is the output of the Kaiser-Meyer-Olkin (KMO) index test. The overall measure varies between 0 and 1, and values closer to 1 are better. A value of 0.6 is a suggested minimum. "),
                             verbatimTextOutput("kmo")



                    ), # end  tab

                    tabPanel("PC Plots",

                             p("Choose the columns of your data to include in the PCA."),
                             p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
                             p("The PCA is automatically re-computed each time you change your selection."),
                             p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                             p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
                             uiOutput("choose_columns_pca"),
                             tags$hr(),
                             p("Select options for the PCA computation (using the prcomp function)"),
                             radioButtons(inputId = 'center',
                                          label = 'Center',
                                          choices = c('Shift variables to be zero centered'='Yes',
                                                      'Do not shift variables'='No'),
                                          selected = 'Yes'),

                             radioButtons('scale.', 'Scale',
                                          choices = c('Scale variables to have unit variance'='Yes',
                                                      'Do not scale variables'='No'),
                                          selected = 'Yes'),
                             h2("Scree plot"),
                             p("The scree plot shows the variances of each PC, and the cumulative variance explained by each Principal Component (out of total possible variation) "),
                             plotOutput("plot2", height = "300px"),
                             tags$hr(),
                             h2("PC plot: zoom and select points"),
                             p("Select the grouping variable."),
                             uiOutput("the_grouping_variable"),
                             tags$hr(),
                             p("Select the PCs to plot"),
                             uiOutput("the_pcs_to_plot_x"),
                             uiOutput("the_pcs_to_plot_y"),
                             tags$hr(),

                             h2("Interactive Pricipal Components Plot"),
                             p("here's how you use this plot:"),
                             p(" 1. Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                             p(" 2. Then select points on zoomed plot below to get exportable sample information (you can select and copy the data as text)."),
                             p(" 3. select or deselect data above or apply a different grouping variable and the plots will automatically update."),
                             plotOutput ("z_plot1", height = 400,
                                         brush = brushOpts(
                                           id = "z_plot1Brush",
                                           resetOnNew = TRUE)),
                             tags$hr(),

                             p("Click and drag on the plot below to select points, and inspect the table of selected points below, you can use the column headers to further sort your output"),

                             plotOutput("z_plot2", height = 400,
                                        brush = brushOpts(
                                          id = "plot_brush_after_zoom",
                                          resetOnNew = TRUE)),
                             tags$hr(),
                             p("Details of the brushed points"),
                             DT::dataTableOutput("brush_info_after_zoom")
                    ), # end  tab



                    tabPanel("PCA data summary",
                             verbatimTextOutput("pca_details"),
                             helpText("The code for this Shiny app is ", a("available on github", href = "https://github.com/putt-ad/micro_ecology_shiny_sim"), ". As you test and use this software please send any feedback, question, etc. to ", a("putt-ad/github", href = "https://github.com/putt-ad/micro_ecology_shiny_sim"), "."
                             )

                    ), # end  tab

                    tabPanel("ANOVA
                   ",
                             p("More like No-NOVA... This element is still under construction"),
                             downloadButton("downloadData", "Download")
                    ) # end  tab


                  )))

server <- function(input, output) {

  # default_file
  basedata <- getURL("https://raw.githubusercontent.com/putt-ad/micro_ecology_shiny_sim/master/test_corr.csv")
  file0 <- read.csv(text = basedata)


  #read in the CSV (tab1)
  file_data_fn <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(file0)
    file_data <-   read.csv(inFile$datapath, header = (input$header == "Yes"),
                            sep = input$sep, quote = input$quote, na.strings = input$na, stringsAsFactors=FALSE)
    return(file_data)
  })



  # display a table of the CSV contents (tab1)
  output$contents <-  DT::renderDataTable({
    #
    file_data_fn()
  })

  # tableplot (data viewier tab)
  output$tableplot <- renderPlot({
    if(is.null(file_data_fn())) return()
    file_data <- file_data_fn()
    tabplot::tableplot(file_data, title = "data table plot")

  })


  # gives a summary table of data (data viewer tab)
  output$summary <-  renderTable({
    file_data <- file_data_fn()
    psych::describe(file_data)
  })

  # plot summary output NEED TO UPDATE THIS!!!
  #  output$summary <- ggplot()


  # Check boxes to choose columns
  output$choose_columns_biplot <- renderUI({

    file_data <- file_data_fn()

    colnames <- names(file_data)

    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns_biplot", "Choose up to five columns to display on the scatterplot matrix",
                       choices  = colnames,
                       selected = colnames[1:5])
  })

  # corr plot
  output$corr_plot <- renderPlot({
    file_data <- file_data_fn()
    # Keep the selected columns
    columns_biplot <-    input$columns_biplot
    file_data_subset_biplot <- file_data[, columns_biplot, drop = FALSE]
    ggpairs(file_data_subset_biplot)
  })

  # correlation summary tables (WOULD BE NICE TO SELECt AUTOMATICALLY FROM THIS TO MAKE A PCA...)
  output$corr_tables <- renderTable({
    file_data <- file_data_fn()
    # filter out columns of non-numeric data
    file_data_num <- file_data[,sapply(file_data,is.numeric)]
    # exclude cols with zero variance
    file_data_num <- file_data_num[,!apply(file_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]

    #find more details and support for the following code, https://cran.r-project.org/web/packages/MASS/MASS.pdf.


    res <- Hmisc::rcorr(as.matrix(file_data_num))
    corr_matrix <- res$r
    pmat <- res$P
    ut <- upper.tri(corr_matrix)
    df <- data.frame(
      row = rownames(corr_matrix)[row(corr_matrix)[ut]],
      column = rownames(corr_matrix)[col(corr_matrix)[ut]],
      cor  = (corr_matrix)[ut],
      p = pmat[ut]
    )
    with(df, df[order(-cor), ])

  })

  output$bartlett <- renderPrint({
    file_data <- file_data_fn()
    file_data_num <- na.omit(file_data[,sapply(file_data,is.numeric)])
    # exclude cols with zero variance
    file_data_num <- file_data_num[,!apply(file_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]

    cortest.bartlett(cor(file_data_num), n = nrow(file_data_num))
  })

  output$kmo <- renderPrint({
    file_data <- file_data_fn()
    file_data_num <- file_data[,sapply(file_data,is.numeric)]
    # exclude cols with zero variance
    file_data_num <- file_data_num[,!apply(file_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]

    # R <- cor(file_data_num)
    # KMO(R)

    # http://www.opensubscriber.com/message/r-help@stat.math.ethz.ch/7315408.html
    # KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
    kmo = function( data ){

      library(MASS)
      X <- cor(as.matrix(data))
      iX <- ginv(X)
      S2 <- diag(diag((iX^-1)))
      AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
      IS <- X+AIS-2*S2                         # image covariance matrix
      Dai <- sqrt(diag(diag(AIS)))
      IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
      AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
      a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
      AA <- sum(a)
      b <- apply((X - diag(nrow(X)))^2, 2, sum)
      BB <- sum(b)
      MSA <- b/(b+a)                        # indiv. measures of sampling adequacy

      AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
      # correlation matrix. That is the
      # negative of the partial correlations,
      # partialling out all other variables.

      kmo <- BB/(AA+BB)                     # overall KMO statistic

      # Kaiser-Meyer-Olkin Factor Analysis Test
      if (kmo >= 0.00 && kmo < 0.50){
        test <- 'not suitable for Factor Analysis, KMO is less than 0.5. Test a different relationship.'
      } else if (kmo >= 0.50 && kmo < 0.60){
        test <- 'poor degree of common variance you should probably select different parameters.'
      } else if (kmo >= 0.60 && kmo < 0.70){
        test <- 'weak degree of common variance these are probably not ideal parameters.'
      } else if (kmo >= 0.70 && kmo < 0.80){
        test <- 'Not bad! There is a Moderate degree of common variance among these selected parameters.'
      } else if (kmo >= 0.80 && kmo < 0.90){
        test <- 'Acceptable degree of common variance among selected parameters.'
      } else {
        test <- 'Excellent degree of common variance among selected parameters!!'
      }

      ans <- list(  overall = kmo,
                    report = test,
                    individual = MSA,
                    AIS = AIS,
                    AIR = AIR )
      return(ans)

    }    # end of kmo()
    kmo(na.omit(file_data_num))

  })


  #this is a set of data that the pca will use, these are what make up the final PC selections, and must be numeric.
  output$choose_columns_pca <- renderUI({

    file_data <- file_data_fn()

    # Get the data set with the appropriate name

    # we only want to show numeric cols
    file_data_num <- na.omit(file_data[,sapply(file_data,is.numeric)])
    # exclude cols with zero variance
    file_data_num <- file_data_num[,!apply(file_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]


    colnames <- names(file_data_num)

    # Create the checkboxes and select them all by default

    checkboxGroupInput("columns", "Choose columns",
                       choices  = colnames,
                       selected = colnames)
  })


  # add groups to map that color the variables and ovals
  output$pH_slider <- renderUI({
    file_data <- file_data_fn()
    sliderInput("range", "pH Range Selector", min(file_data$pH), max(file_data$pH),
                value = range(file_data$pH), step = 0.1)
  })

  output$the_grouping_variable <- renderUI({
    file_data <- file_data_fn()

    # since some data sets have small amounts of variation due to earlier filtering of missing data, etc.
    # this will limit it that not all groups are applicable becasue a group with lots of options is confusing.
    grouping_cols <- sapply(seq(1, ncol(file_data)), function(i) length(unique(file_data[,i])) < nrow(file_data)/3 )

    file_data_group_cols <- file_data[, grouping_cols, drop = FALSE]
    # drop down selection
    selectInput(inputId = "the_grouping_variable",
                label = "Group your data:",
                choices=c("None", names(file_data_group_cols)))

  })
  #this is the map option selector output

  # Subset pH values of the data
  #    filteredData <- reactive({
  #      file_data <- file_data_fn()
  #      file_data[file_data$pH >= input$range[1] & file_data$pH <= input$range[2],]
  #    })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  #    colorpal <- reactive({
  #      file_data <- file_data_fn()

  #    file_data <- file_data_fn()
  #      colorNumeric(input$colors, file_data$pH)
  #    })
  #
  #    output$map <- renderLeaflet({
  #      file_data <- file_data_fn()

  # set map to location of geochem data
  #      leaflet(file_data) %>% addTiles() %>%
  #        fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  #    })

  # observe user changes to make new circles
  #    observe({
  #      pal <- colorpal()

  #      leafletProxy("map", data = filteredData()) %>%
  #        clearShapes() %>%
  #        addCircles(radius = ~10^pH/10, weight = 1, color = "gray",
  #                   fillColor = ~pal(pH), fillOpacity = 0.7, popup = ~paste(pH)
  #        )
  #    })

  # Use a separate observer to recreate the legend as needed.
  #    observe({
  #      file_data <- file_data_fn()
  #      proxy <- leafletProxy("map", data = file_data)
  #
  # Remove any existing legend, and only if the legend is
  # enabled, create a new one.
  #      proxy %>% clearControls()
  #      if (input$legend) {
  #        pal <- colorpal()
  #        proxy %>% addLegend(position = "bottomright",
  #                            pal = pal, values = ~pH
  #        )
  #      }
  #    })




  pca_objects <- reactive({
    # Keep the selected columns but make sure no na values will interfere with the analysis, as NAs skew PCA
    columns <-    input$columns
    file_data <- na.omit(file_data_fn())
    file_data_subset <- na.omit(file_data[, columns, drop = FALSE])

    # from http://rpubs.com/sinhrks/plot_pca
    # this option allows the user to decide if the variables focus around zero or not
    pca_output <- prcomp(na.omit(file_data_subset),
                         center = (input$center == 'Yes'),
                         scale. = (input$scale. == 'Yes'))
    # principal coordinate
    pcs_df <- cbind(file_data, pca_output$x)

    return(list(file_data = file_data,
                file_data_subset = file_data_subset,
                pca_output = pca_output,
                pcs_df = pcs_df))

  })

  # the principal coordinates from the
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x

    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x",
                label = "X axis:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })

  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x

    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y",
                label = "Y axis:",
                choices= colnames(pca_output),
                selected = 'PC2')
  })



  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "gray", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variance explained") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })


  # PC plot
  output$choose_label <- renderUI({

    file_data <- file_data_fn()

    # Get the data set with the appropriate name

    labelcols <- names(file_data)

    # Create the checkboxes and select them all by default

    selectInput(inputId = "choose_label",
                label = "Choose Feature color",
                choices  = labelcols,
                selected = labelcols)
  })

  pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    file_data <- file_data_fn()

    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable

    if(grouping == 'None'){
      # plot without grouping variable
      pc_plot_no_groups  <- ggplot(pcs_df,
                                   aes_string(input$the_pcs_to_plot_x,
                                              input$the_pcs_to_plot_y
                                   )) +


        geom_point (colour = "gray") + geom_text(aes(label = labels),  size = 5, hjust=0, vjust=0) +
        theme_bw(base_size = 14) +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)"))
      # the plot
      pc_plot_no_groups


    } else {
      # plot with grouping variable

      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x,
                                                   input$the_pcs_to_plot_y,
                                                   fill = 'fill_',
                                                   colour = 'fill_'
      )) +
        stat_ellipse(geom = "polygon", alpha = 0.1) + geom_point(colour = "gray") +

        geom_text(aes(label = labels),  size = 5, hjust=0, vjust=0) +
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% variance explained)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% variance explained)"))
      # the plot
      pc_plot_groups
    }


  })

  output$brush_info <- renderTable({
    # brushed point selection makes a different PC plot
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })


  # for zooming
  output$z_plot1 <- renderPlot({

    pca_biplot()

  })

  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$z_plot1Brush
    if (!is.null(brush)) {
      zooming$x <- c(brush$xmin, brush$xmax)
      zooming$y <- c(brush$ymin, brush$ymax)
    }
    else {
      zooming$x <- NULL
      zooming$y <- NULL
    }
  })


  # for zooming
  output$z_plot2 <- renderPlot({

    pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y)


  })

  #  output$brush_info_after_zoom <- renderTable({
  # the brushing function
  #    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  #  })

  output$pca_details <- renderPrint({
    #
    print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)

  })
  output$brush_info_after_zoom <-  DT::renderDataTable({
    #
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })

  output$downloadData <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$brushed_info_after_zoom, input$file1, sep = ".")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$brushed_info_after_zoom, "csv" = ",", "tsv" = "\t")

      # Write to a file specified by the 'file' argument
      write.table(pca_objects(), file, sep = sep,
                  row.names = FALSE)
    }
  )


}
shinyApp(ui,server)




