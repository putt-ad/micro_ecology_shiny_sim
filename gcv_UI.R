

ui <- fluidPage(theme = shinytheme("sandstone"),
  mainPanel(
    titlePanel("Geochem Viewer"),

        tabsetPanel(
          # opening tab - loading the csv
          tabPanel("Data input",
                   p("Before uploading your data, check that it is clean, especially ensure that the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
                   p("Since missing data can skew a PCA, rows with one or more NAs will be excluded from the PCA."),
                   p("Be sure that all missing data is labeled as NA, becasue columns that contain a mixture of numbers and text may be missed all together."),
                   p("Have a look at the ", a("earthquakes-23k.csv", href = "https://github.com/plotly/datasets/blob/master/earthquakes-23k.csv"),  " file included with this app to see what a clean CSV file looks like."),
                   tags$hr(),

                # select and read file (tab 1)
                   fileInput('file1', 'Choose a CSV file to upload:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),

                   tags$hr(),
                   p("Select the options that match your .CSV file."),
                p("headers are the titles of the columns in the uploaded file, normally this will remain selected."),

                   radioButtons(inputId = 'header',
                                label = 'Header',
                                choices = c('Columns have headers'='Yes',
                                            'Columns do not have headers'='No'),
                                selected = 'Yes'),

                p("seperators are what seperate the columns from each other."),

                   radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ','),

                   radioButtons('quote', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"'),

                   tags$hr(),
                   p("Here is the raw data from the CSV file"),
                   DT::dataTableOutput('contents')

                 ), # end file  tab
          # data viewer tab table plot *LETS ADD A PLOT OF OUTPUTS HERE TOO?? (tab2)
          tabPanel("Data viewer",

                   p("The tableplot below (it will take a few seconds to appear)
                     may be useful to explore the relationships between the variables,
                     to discover strange data patterns, and to check the occurrence and selectivity of missing values."),
                   plotOutput("tableplot")%>% withSpinner(color="#6fcb9f80"),
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
                    h2("Map becomes active before data is available and crashes app"),
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

                   plotOutput("corr_plot")%>% withSpinner(color="#6fcb9f80"),
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

                   p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                   p("Then select points on zoomed plot below to get more information about the points."),
                   p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
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
                   helpText("The code for this Shiny app is ", a("available on github", href = "https://github.com/putt-ad/micro_ecology_shiny_sim"), ". As you test and use this software please send any feedback, question, etc. to ", a("putt-ad/github", href = "https://github.com/putt-ad/micro_ecology_shiny_sim"), ".",
                            "This code is shared under a", a("CC-BY", href = "http://creativecommons.org/licenses/by/4.0/"), " license with mathematics code accessed from programs and packages shared under the open source licenses of ", a(href = "https://opensource.org/licenses/MIT", "MIT"), "and", a(href = "http://www.opensubscriber.com/message/r-help@stat.math.ethz.ch/7315408.html", "open subscriber"), "."
                   )

          ), # end  tab

          tabPanel("ANOVA
                   ",
                   p("More like No-NOVA... This element is still under construction"),
                   downloadButton("downloadData", "Download")
          ) # end  tab


          )))


