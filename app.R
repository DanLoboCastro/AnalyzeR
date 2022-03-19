##______________________________________________________##
##                                                      ##
## Medical Statistics Dissertation Project (Shiny code) ##    
##______________________________________________________##

## Daniel Castro ## 

# last modification: 29/10/2021


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
#                                                          |
#     *    *     *    *    *  *     * ***** ***** *****    |
#    * *   * *   *   * *   *    * *     *   ***   *****    |
#   * - *  *   * *  * - *  *     *     *    *     *  *     |
#  *     * *     * *     * ***** *    ***** ***** *   *    |
#                                                          |                                                         
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# libraries to load # ----


suppressPackageStartupMessages(  

c(
library(caret), 
library(tidyr),
library(NbClust),  
library(xlsx), 
library(shiny),
library(shinydashboard),
library(shinydashboardPlus),
library(dashboardthemes),
library(mice),
library(DT),
library(VIM),
library(readxl),
library(epitools),
library(survival),
library(naniar),
library(dplyr),
library(ggplot2),
library(data.table),
library(dlookr),
library(markdown),
library(plotly),
library(ggfortify),
library(survminer),
library(psych),
library(GGally),
library(factoextra),
library(FactoMineR),
library(dlookr),
library(summarytools),
library(purrr),
library(openxlsx),
library(BSDA),
library(hrbrthemes),
library(performance),
library(gt),
library(broom),
library(gtsummary),
library(fontawesome),
library(aod),
library(DescTools),
library(tidyverse),
library(ggeffects),
library(effects),
library(pscl),
library(MASS),
library(shinyalert),
library(see),
library(shinycustomloader),
library(periscope),
library(viridis),
library(heatmaply),
library(corrr),
library(sjPlot),
library(sjmisc),
library(sjlabelled),
library(parameters),
library(ggforce),
library(DataExplorer),
library(knitr),
library(fresh),
library(vov)

)
)


## THEME customization ----

customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(18,18,18)"
  ,primaryFontColor = "rgb(18,18,18)"
  ,infoFontColor = "rgb(18,18,18)"
  ,successFontColor = "rgb(18,18,18)"
  ,warningFontColor = "rgb(18,18,18)"
  ,dangerFontColor = "rgb(18,18,18)"
  ,bodyBackColor =	"rgb(255,255,255)"#"rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "#303030"
  
  ,headerButtonBackColor =  "#303030"#"rgb(238,238,238)"
  ,headerButtonIconColor = "#303030" #"rgb(75,75,75)"
  ,headerButtonBackColorHover =  "#303030" #"rgb(210,210,210)"
  ,headerButtonIconColorHover =  "#303030" #"rgb(0,0,0)"
  
  ,headerBackColor =  "#303030"#rgb(24,24,24)"
  ,headerBoxShadowColor = "#303030"
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor =  "#303030"
  
  #   cssGradientThreeColors(
  #   direction = "down"
  #   ,colorStart = "rgb(20,97,117)"
  #   ,colorMiddle = "rgb(56,161,187)"
  #   ,colorEnd = "rgb(3,22,56)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 50
  #   ,colorEndPos = 100
  # )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "transparent"
  
  ,sidebarUserTextColor = "rgb(179,179,179)"
  
  ,sidebarSearchBackColor = "rgb(40,40,40)"
  ,sidebarSearchIconColor = "rgb(64,64,64)"
  ,sidebarSearchBorderColor = "rgb(40,40,40)"
  
  ,sidebarTabTextColor = "rgb(179,179,179)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(64,64,64)"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(23, 127, 255)"
  
  #   cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = "rbg(45,45,45)"
  
  #   cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "transparent"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "transparent"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(23, 127, 255)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(23, 127, 255)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(23, 127, 255)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(23, 127, 255)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



##__________________________
##
##   User Interface         ----
##__________________________




ui <- dashboardPage(
  #footer = dashboardFooter(),
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(title = span(
    tags$img(
      src = "sumR.png",
      height = '35',
      width = '35'
    )
  )),
  #, disable = TRUE),
  #title = span(tags$img(src="hexA.png", height = '35', width ='35'), "AnalyzeR")),
  sidebar = dashboardSidebar(
    minified = TRUE,
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Welcome", tabName = "home", icon = icon("home")),
      menuItem(
        "Where to start ?",
        tabName = "wstart",
        icon = icon("question-circle")
      ),
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Explore", tabName = "explore", icon = icon("th")),
      menuItem("Missings", tabName = "missings", icon = icon("asterisk")),
      menuItem(
        "Inference",
        tabName = "infer",
        icon = icon("square-root-alt")
      ),
      menuItem(
        "Regression",
        tabName = "Regression",
        icon = icon("chart-line")
      ),
      menuItem(
        "Survival Analysis",
        tabName = "survival",
        icon = icon("clock")
      ),
      menuItem(
        "Dimensionality Reduction",
        tabName = "pca",
        icon = icon("arrows-alt")
      )
    )
  ),
  
  dashboardBody(
    #shinyDashboardThemes(theme = "blue_gradient"),
    customTheme,
    tabItems(
      tabItem(
        tabName = "home",
        
        mainPanel(
          width = 12,
          
          
          
          use_vov(),
          fade_in_down(
            duration = "fast",
            delay = 0,
            div(img(
              src = "hexA.png",
              height = '300',
              width = '300'
            ), style = "text-align: center;")
          ),
          use_vov(),
          fade_in_down(
            duration = "fast",
            delay = 0,
            div(h1("Welcome to AnalyzeR!"), style = "text-align: center;")
          ),
          use_vov(),
          fade_in_down(
            duration = "fast",
            delay = 1,
            div(h2("Perform advanced statistical analysis"),
                style = "text-align: center;")
          ),
          use_vov(),
          fade_in_down(
            duration = "fast",
            delay = 3,
            div(h2("Without code!"),
                style = "text-align: center;")
          ),
          use_vov(),
          fade_in_down(
            duration = "fast",
            delay = 5,
            div(h1("Enjoy ;)"),
                style = "text-align: center;")
          ),
        )
        
      ),
      
      tabItem(
        tabName = "dataset",
        
        
        sidebarPanel(
          width = 3,
          h3("Import a dataset"),
          radioButtons(
            "formt",
            " File format",
            choices = c(CSV = "csv",
                        Excel = "excel"),
            selected = "csv"
          ),
          tags$hr(),
          fileInput("file1", "Choose a file", accept = c(".csv", ".txt", "xls")),
          tags$style("

             .progress-bar {
             background-color: rgb(23, 127, 255);
             }

             "),
          
          tags$hr(),
          conditionalPanel(
            condition = "input.formt == 'csv'",
            
            checkboxInput("header", "Header", TRUE),
            radioButtons(
              "sep",
              "Separator",
              choices = c(
                Comma = ",",
                Semicolon = ";",
                Tab = "/t"
              ),
              selected = ","
            ),
            radioButtons(
              "quote",
              "Quote",
              choices = c(
                None = "",
                "Double Quote" = '"',
                "Single Quote" = "'"
              ),
              selected = '"'
            )
          ),
          # tags$hr(),
          # uiOutput("c.numeric"),
          #
          tags$hr(),
          actionButton("report", label = "Show instant report")
        ),
        
        mainPanel(fluidRow(
          withLoader(
            DT::dataTableOutput("data1", width = "90%"),
            type = "html",
            loader = "dnaspin"
          ),
          
          verbatimTextOutput("c.show")
          
          
        ))
      ),
      
      tabItem(tabName = "wstart",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    h3("Where to start?"),
                    uiOutput("resptype"),
                    uiOutput("respsubtype"),
                    uiOutput("mtype"),
                    h5("You should use:"),
                    verbatimTextOutput("recomendd"),
                    selectInput(
                      "wtc",
                      "I want to check...",
                      choices = list(
                        "linear regr." = 1,
                        "logistic regr." = 2,
                        "Poisson regr." = 3,
                        "t-test" = 4,
                        "Shapiro-W test" = 5,
                        "Chi-sqared test" = 6
                      )
                    )
                  ),
                  
                  
                  mainPanel(
                    width = 7,
                    
                    
                    conditionalPanel(
                      "input.wtc == 1",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("il_reg"))
                    ),
                    conditionalPanel(
                      "input.wtc == 2",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("ilogi_reg"))
                    ),
                    conditionalPanel(
                      "input.wtc == 3",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("il_pois"))
                    ),
                    conditionalPanel(
                      "input.wtc == 4",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("it_test"))
                    ),
                    conditionalPanel(
                      "input.wtc == 5",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("ishapirot"))
                    ),
                    conditionalPanel(
                      "input.wtc == 6",
                      use_vov(),
                      fade_in_up(duration = "slow", uiOutput("ichisq"))
                    )
                    
                    
                  )
                )
              )),
      
      tabItem(tabName = "explore",
              fluidRow(
                tags$head(tags$style(".rightAlign{float:center;}")),
                column(width = 12,
                       tabsetPanel(
                         navlistPanel(
                           HTML("<b>Summaries</b>"),
                           widths = c(2, 10),
                           
                           tabPanel("General Summary",
                                    
                                    mainPanel(htmlOutput("sumariz1"))),
                           
                           tabPanel("Detailed Summary",
                                    
                                    mainPanel(
                                      uiOutput("tabcl"),
                                      htmlOutput("sumarizB")
                                    )),
                           
                           HTML("<b>Visualizations</b>"),
                           
                           #tabPanel("Categories",
                           
                           # sidebarLayout(
                           #         sidebarPanel(
                           #           uiOutput("TableA"),
                           #           uiOutput("TableB"),
                           #           uiOutput("Cco")
                           #
                           #           ),
                           #         mainPanel(
                           #
                           #           withLoader(plotlyOutput("cat_plot", width = '90%', height = "80%"), type = "html", loader = "dnaspin")
                           #
                           #
                           #
                           #         )
                           #
                           #       )),
                           
                           
                           
                           tabPanel(
                             "2D plots",
                             
                             sidebarLayout(
                               sidebarPanel(
                                 width = 3,
                                 h4("Slelect variables to Histograms and Boxplots"),
                                 uiOutput("select"),
                                 uiOutput("selg"),
                                 radioButtons(
                                   inputId = "gbuttons",
                                   "Add Groups?",
                                   c("Yes", "No"),
                                   selected = "No",
                                   inline = TRUE
                                 )
                               ),
                               
                               
                               
                               mainPanel(
                                 #width = 10,
                                 
                                 conditionalPanel(
                                   condition = "input.gbuttons == 'Yes'",
                                   withLoader(
                                     plotlyOutput("plot", width = '90%', height = "80%"),
                                     type = "html",
                                     loader = "dnaspin"
                                   )
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.gbuttons == 'No'",
                                   withLoader(
                                     plotlyOutput("plot0", width  = '90%', height = "80%"),
                                     type = "html",
                                     loader = "dnaspin"
                                   )
                                 )
                                 
                               )
                             ),
                             
                             sidebarLayout(
                               sidebarPanel(
                                 width = 3,
                                 h4("Select variables to 2D plot"),
                                 uiOutput("select2d"),
                                 uiOutput("select2d1"),
                                 radioButtons(
                                   inputId = "gbuttons2",
                                   "Add Groups?",
                                   c("Yes", "No"),
                                   selected = "No",
                                   inline = TRUE
                                 ),
                                 
                                 uiOutput("select2dGroup")
                               ),
                               
                               
                               
                               mainPanel(
                                 #width = 10,
                                 
                                 
                                 conditionalPanel(
                                   condition = "input.gbuttons2 == 'Yes'",
                                   withLoader(
                                     plotlyOutput("plot2de", width = '90%', height = "80%"),
                                     type = "html",
                                     loader = "dnaspin"
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.gbuttons2 == 'No'",
                                   withLoader(
                                     plotlyOutput("plot2de0", width = '90%', height = "80%"),
                                     type = "html",
                                     loader = "dnaspin"
                                   )
                                 )
                               )
                             )
                           ),
                           
                           tabPanel("3D plots",
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 3,
                                        h4("Select variables to 3D plot"),
                                        uiOutput("select3d"),
                                        uiOutput("select3d1"),
                                        uiOutput("select3d2"),
                                        uiOutput("select3dGroup"),
                                        uiOutput("select3dGroupColors")
                                        
                                      ),
                                      mainPanel(box(
                                        withLoader(plotlyOutput("plot3d"), type = "html", loader = "dnaspin"),
                                        width = 12,
                                        height = 20
                                      ))
                                    )),
                           
                           tabPanel(
                             "Grid plot",
                             sidebarPanel(width = 3,
                                          h4("Visualize Grid Plot"),
                                          uiOutput("choose_columns")
                                          #downloadButton("foo","Download Plot")),
                                          mainPanel(withLoader(
                                            plotOutput("plotggp", width = "80%"),
                                            type = "html",
                                            loader = "dnaspin"
                                          ))
                             ),
                             
                             
                             tabPanel("Correlation heatmap",
                                      mainPanel(
                                        h3("Interactive Correlation Heatmap"),
                                        plotlyOutput("heatmap", width = "100%", height =
                                                       "600px")
                                      )),
                             
                             
                             
                             
                             tabPanel("Tendencies",
                                      sidebarLayout(
                                        sidebarPanel(
                                          width = 3,
                                          h4("Select variables"),
                                          uiOutput("xlin"),
                                          uiOutput("ylin"),
                                          #uiOutput("gr.col"),
                                          uiOutput("lin.m"),
                                          
                                          conditionalPanel("input.lincm == 'glm'",
                                                           uiOutput("lin.lof")),
                                          conditionalPanel("input.lincm == 'loess'",
                                                           uiOutput("l.span"))
                                          
                                        ),
                                        
                                        mainPanel(
                                          conditionalPanel("input.lincm == 'glm'", plotlyOutput("lin.glm")),
                                          conditionalPanel("input.lincm == 'loess'", plotlyOutput("lin.loess")),
                                          conditionalPanel("input.lincm == 'lm'", plotlyOutput("lin.lm"))
                                        )
                                        
                                        
                                      )),
                             
                             
                             tabPanel("Clustering",
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          width = 3,
                                          h4("Clustering"),
                                          uiOutput("clu1"),
                                          # X variable
                                          uiOutput("clu2"),
                                          # Y variable,
                                          uiOutput("clum"),
                                          useShinyalert(),
                                          actionButton("helpCLU", "Need help?")
                                        ),
                                        # Choose number of clusters
                                        mainPanel(box(
                                          withLoader(plotlyOutput("clux"), type = "html", loader = "dnaspin"),
                                          width = 10
                                        ))  #plot
                                        
                                      ))
                             
                           )
                         ))
                )),
              
              tabItem(tabName = "missings",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h3("Missing Values Imputation"),
                          tags$br(),
                          uiOutput("mivsx"),
                          tags$br(),
                          actionButton("impute", "Impute!"),
                          tags$br(),
                          tags$br(),
                          uiOutput('downloadData'),
                          tags$br(),
                          tags$br(),
                          #uiOutput("foo2"),
                          useShinyalert(),
                          actionButton("helpMISS", "Need help?")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Patterns", fluidRow(column(
                            plotOutput("missmap", width = "100%"), width = 12
                          ))),
                          #tabPanel("Inspect", fluidRow(column(uiOutput("misd"), plotOutput("missplott", width = "100%"), width = 12))),
                          tabPanel("Diagnostics", fluidRow(column(
                            uiOutput("mivs"), plotOutput("mi.s", width = "100%"), width = 12
                          )))#,
                          #tabPanel("Diagnostics", fluidRow(column(plotOutput("mips", width = "100%" ), width = 12)))
                          
                          
                        ))
                        
                      )),
              
              tabItem(tabName = "infer",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Hypothesis Testing"),
                          width = 3,
                          selectizeInput(
                            inputId = "Xtest",
                            label = "Choose statistical test",
                            choices = list(
                              "Shapiro-Wilk (normality)" = 1,
                              "Correlation test" = 2,
                              "t-test" = 3,
                              #"ANOVA" = 4,
                              "Chi-Square test" = 5,
                              #"Wilcoxon test" = 6,
                              "variance (F) test" = 8,
                              "vairnace (levene) test" = 9
                            ),
                            multiple = F
                          ),
                          
                          conditionalPanel("input.Xtest == 1", uiOutput("xshp")),
                          conditionalPanel(
                            "input.Xtest == 2",
                            uiOutput("c1"),
                            uiOutput("c2"),
                            uiOutput("copt"),
                            uiOutput("copt2"),
                            uiOutput("Ccfl")
                          ),
                          conditionalPanel(
                            "input.Xtest == 3",
                            uiOutput("tt1"),
                            uiOutput("tt2"),
                            uiOutput("tcfl")
                          ),
                          #conditionalPanel("input.Xtest == 4", uiOutput("anY"), uiOutput("anX")),
                          conditionalPanel("input.Xtest == 5", uiOutput("chiX1"), uiOutput("chiX2")),
                          #conditionalPanel("input.Xtest == 6", uiOutput("wilcox1"), uiOutput("wilcox2"), uiOutput("wopt"), uiOutput("wcfl"), uiOutput("wpair")),
                          #conditionalPanel("input.Xtest == 7", uiOutput("ztest1"), uiOutput("ztest2"), uiOutput("zopt"), uiOutput("zcfl")),
                          #conditionalPanel("input.Xtest == 8", uiOutput("proptest1"), uiOutput("proptest2")),
                          #conditionalPanel("input.Xtest == 9", uiOutput("binomtest1"), uiOutput("binomtest2")),
                          conditionalPanel(
                            "input.Xtest == 8",
                            uiOutput("vartest1"),
                            uiOutput("vartest2"),
                            uiOutput("varopt"),
                            uiOutput("varcfl")
                          ),
                          conditionalPanel(
                            "input.Xtest == 9",
                            uiOutput("bvartest1"),
                            uiOutput("bvartest2")
                          ),
                          
                          #conditionalPanel("input.Xtest == 9", uiOutput("bartlet1"), uiOutput("bartlet2"))
                          
                          useShinyalert(),
                          actionButton("helpINFER", "Need help?")
                        ),
                        
                        mainPanel(
                          conditionalPanel(
                            "input.Xtest == 1",
                            withLoader(
                              DT::dataTableOutput("shpN", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          ),
                          conditionalPanel(
                            "input.Xtest == 2",
                            withLoader(
                              DT::dataTableOutput("corr", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          ),
                          conditionalPanel(
                            "input.Xtest == 3",
                            withLoader(
                              DT::dataTableOutput("tt", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          ),
                          #conditionalPanel("input.Xtest == 4", withLoader(DT::dataTableOutput("anov"), type = "verbatimText", loader = "dnaspin")),
                          conditionalPanel(
                            "input.Xtest == 5",
                            withLoader(
                              DT::dataTableOutput("chisq", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          ),
                          #conditionalPanel("input.Xtest == 6", withLoader(DT::dataTableOutput("wilcoxt"), type = "html", loader = "dnaspin")),
                          #conditionalPanel("input.Xtest == 7", DT::dataTableOutput("zt")),
                          #conditionalPanel("input.Xtest == 8", DT::dataTableOutput("prt")),
                          #conditionalPanel("input.Xtest == 9", DT::dataTableOutput("bnt")),
                          conditionalPanel(
                            "input.Xtest == 8",
                            withLoader(
                              DT::dataTableOutput("vrt", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          ),
                          conditionalPanel(
                            "input.Xtest == 9",
                            withLoader(
                              DT::dataTableOutput("vrt2", width = "60%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          )
                          #conditionalPanel("input.Xtest == 9", DT::dataTableOutput("brt"))
                        )
                      )),
              
              
              tabItem(tabName = "Regression",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          radioButtons(
                            "radio1",
                            label = h3("Choose regression type"),
                            choices = list(
                              "Linear" = 1,
                              "Logistic Binary" = 2,
                              #"Logistic Ordinal" = 3, "Logistic Multinomial" = 4,
                              "Poisson" = 5
                            ),
                            selected = F
                          ),
                          uiOutput("select2"),
                          uiOutput("select3"),
                          actionButton("analysis", "Analyze!"),
                          useShinyalert(),
                          actionButton("helpREGR", "Need help?")
                          #downloadButton("foo3", "Download Plot")
                        ),
                        mainPanel(
                          conditionalPanel(
                            "input.radio1 == 1",
                            tabsetPanel(
                              tabPanel("Model summary", fluidRow(column(
                                width = 7,
                                withLoader(htmlOutput("testq"), type = "html", loader = "dnaspin")
                              ))),
                              tabPanel("Coefficients plot", fluidRow(column(
                                width = 7,
                                withLoader(plotOutput("lcoef"), type = "html", loader = "dnaspin")
                              ))),
                              tabPanel("Diagnostics", fluidRow(column(
                                width = 12,
                                withLoader(plotOutput("diag"), type = "html", loader = "dnaspin")
                              )))
                            )
                          ),
                          
                          conditionalPanel(
                            "input.radio1 == 2",
                            tabsetPanel(
                              tabPanel("Model summary", fluidRow(column(
                                width = 7,
                                withLoader(htmlOutput("testq2"), type = "html", loader = "dnaspin")
                              ))),
                              tabPanel("Coefficients plot", fluidRow(column(
                                width = 7, plotOutput("logiOdds")
                              ))),
                              tabPanel("GOF metrics", fluidRow(column(
                                width = 7, verbatimTextOutput("logi_gof")
                              )))
                              
                            )
                          ),
                          
                          
                          conditionalPanel(
                            "input.radio1 == 5",
                            tabsetPanel(
                              tabPanel("Model summary", fluidRow(column(
                                width = 7, uiOutput("testq3")
                              ))),
                              tabPanel("Coeficients plot", fluidRow(column(
                                width = 7, plotOutput("plotq3")
                              ))),
                              tabPanel("GOF metrics", fluidRow(column(
                                width = 7, verbatimTextOutput("logi_gof2")
                              )))
                              
                            )
                          )
                        )
                      )),
              
              
              
              
              
              tabItem(tabName = "survival",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Survival Analysis"),
                          width = 3,
                          uiOutput("selectTIME"),
                          uiOutput("selectSTATUS"),
                          uiOutput("selectVARS"),
                          uiOutput("selectTREAT"),
                          actionButton("analysis2", "Analyze!"),
                          # downloadButton("foo4", "Download Plot"),
                          useShinyalert(),
                          actionButton("helpSURV", "Need help?")
                          
                        ),
                        
                        mainPanel(tabsetPanel(
                          tabPanel("Model summary", fluidRow(column(
                            width = 10,
                            withLoader(htmlOutput("COXsum"), type = "html", loader = "dnaspin")
                          ))),
                          #tabPanel("Diagnosis",fluidRow(column(width=10, plotOutput("COXdiag")))),
                          tabPanel("KM plot", fluidRow(column(
                            width = 10,
                            withLoader(
                              plotOutput("KMplot", width = "100%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                          )))
                        ))
                      )),
              
              tabItem(tabName = "pca",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h4("Principal Component Analysis"),
                          uiOutput("pdd"),
                          
                          actionButton("analysisPCA", "Analyze!"),
                          useShinyalert(),
                          actionButton("helpPCA", "Need help?"),
                          
                          uiOutput("dimA"),
                          uiOutput("dimB"),
                          uiOutput("clumPCA"),
                          uiOutput("Coll"),
                          uiOutput("Colls")
                          
                        ),
                        
                        mainPanel(fluidRow(
                          column(
                            width = 11,
                            
                            withLoader(
                              plotlyOutput("plotlyPCA", width = "65%"),
                              type = "html",
                              loader = "dnaspin"
                            )
                            
                            ,
                            conditionalPanel(
                              condition = "output.plotlyPCA"
                              ,
                              h4("Summary")
                              ,
                              h5("Importance")
                              ,
                              verbatimTextOutput("PC.info")
                              ,
                              h5("Rotations")
                              ,
                              verbatimTextOutput("PC.info2")
                              
                            )
                          )
                        ))
                      ))
              
              
      )
    )
  )
  
  
  
  ##______________________________________
  ##
  ##            SERVER function          ----
  ##______________________________________
  
  
  
  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 30 * 1024 ^ 2) #increasing allowed file size
    
    options(shiny.trace = FALSE)
    
    
    output$il_reg <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./l_reg.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$il_pois <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./l_pois.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$ilogi_reg <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./logi_reg.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$ichisq <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./chisq.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$ishapirot <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./shapirot.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$ichisq <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./chisq.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    output$it_test <- renderUI({
      withMathJax(HTML(markdown::markdownToHTML(
        knit("./t_test.Rmd", quiet = TRUE), fragment.only = TRUE
      )))
      
    })
    
    
    
    
    
    # Help messages
    
    
    observeEvent(input$helpCLU, {
      shinyalert(
        title = "Clustering",
        text = "Start by choosing two distinct numeric variables and then select the clustering method (by default, K-means is selected).
                   Then an optimal partition (clusters) will be calculated and presented in the plot.",
        
        size = "s",
        type = "info",
        html = TRUE,
        confirmButtonText = "OK"
      )
      
    })
    
    observeEvent(input$helpMISS, {
      shinyalert(
        title = "Handling missing values",
        
        text = "Impute missing values with MICE (Multiple Chained Equations), check the imputation and download imputed data in a .xlsx file.",
        
        size = "m",
        type = "info",
        html = TRUE,
        confirmButtonText = "Got it!"
      )
      
      
    })
    
    observeEvent(input$helpINFER, {
      shinyalert(
        title = "Statistical Tests",
        
        text = "Here you can choose from a variety of statistical tests whith selected variables
                   in order to evaluate an hypothesis. If you are unsure about which test to use, check the
                   'Where to start?' section.",
        
        size = "s",
        type = "info",
        html = TRUE,
        confirmButtonText = "OK"
      )
      
    })
    
    
    observeEvent(input$helpREGR, {
      shinyalert(
        title = "Regression",
        
        text = "Choose between the various models according to your response variable. First, you should select the dependent and independent variables, then click run!",
        
        size = "s",
        type = "info",
        html = TRUE,
        confirmButtonText = "Thanks!"
      )
      
    })
    
    observeEvent(input$helpSURV, {
      shinyalert(
        title = "Survival Analysis",
        text = "In this section you can perform Survival Analysis via Cox-Regression, and Kaplan-Meyer plot.",
        
        size = "m",
        type = "info",
        html = TRUE,
        confirmButtonText = "Got it!"
      )
      
    })
    
    observeEvent(input$helpPCA, {
      shinyalert(
        title = "Principal Components Analysis",
        
        text = "This is a dimensionality reduction (DR) technique for numerical variables.
                   DR methodology aims to retain as much information as possible with significantly
                   less dimensions that the original data. This becomes usefull when analysing high
                   dimensionality datasets.  Run PCA and explore interactive
                   visualizations of results.",
        
        size = "m",
        type = "info",
        html = TRUE,
        confirmButtonText = "Great info!"
      )
      
    })
    
    
    
    ## Where to start
    
    output$resptype <-
      renderUI({
        selectizeInput(
          inputId = "rtype",
          label = "Select response class",
          choices = list("Numeric" = 1, "Categorical" = 2),
          multiple = F,
          selected = F
        )
      })
    output$respsubtype <-
      renderUI({
        selectizeInput(
          inputId = "rstype",
          label = "Select response subclass",
          choices = list(
            "Binary {0/1}" = 1,
            ">=2 categories" = 2,
            "Counts" = 3,
            "Continuous" = 4,
            "Time to event" = 5
          ),
          multiple = F,
          selected = F
        )
      })
    output$mtype <-
      renderUI({
        selectizeInput(
          inputId = "mstype",
          label = "Select method type",
          choices = list(
            "Regression" = 1,
            "Hypothesis-test" = 2,
            "Dimensionlality Reduction" = 3
          ),
          multiple = F,
          selected = F
        )
      })
    
    output$recomendd <-  renderText({
      if (input$rtype == 2 &
          input$rstype == 1 &
          input$mstype == 1) {
        "Logistic{Binary} Regression"
      } else {
        if (input$rtype == 1 &
            input$rstype == 3 &
            input$mstype == 1) {
          "Poisson Regression"
        } else {
          if (input$rtype == 1 &
              input$rstype == 4 &
              input$mstype == 1) {
            "Linear Regression"
          } else {
            if (input$rtype == 1 &
                input$rstype == 5 &
                input$mstype == 1) {
              "Survival Analysis"
            } else {
              if (input$rtype == 1 &
                  input$rstype == 4 &
                  input$mstype == 3) {
                "Principal Components Analysis (PCA)"
              } else {
                if (input$rtype == 1 &
                    input$rstype == 4 &
                    input$mstype == 2) {
                  "Check Hypothesis tests below"
                } else {
                  if (input$rtype == 2 &
                      input$rstype == 2 &
                      input$mstype == 2) {
                    "Check Hypothesis tests below"
                  } else {
                    if (input$rtype == 2 &
                        input$rstype == 2 &
                        input$mstype == 1) {
                      "Logistic Multinomial Regression"
                    } else {
                      if (input$rtype == 1 &
                          input$rstype == 3 &
                          input$mstype == 2) {
                        "Check Hypothesis tests below"
                      } else {
                        if (input$rtype == 2 &
                            input$rstype == 2 &
                            input$mstype == 3) {
                          "Multiple Correspondence Analysis (MCA)"
                        } else {
                          "Ups! No compatible method"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
    })
    
    
    output$home.rmd <- renderUI({
      HTML(markdown::markdownToHTML(knit('home_page.rmd', quiet = TRUE), fragment.only =
                                      TRUE))
    })
    
    
    output$wts.rmd <- renderUI({
      HTML(markdown::markdownToHTML(knit('wts_page.rmd', quiet = TRUE), fragment.only =
                                      TRUE))
    })
    
    
    
    #### IMPORT DATASET ####
    
    
    
    dados <- reactive({
      if (input$formt == "excel") {
        file <- input$file1
        req(file)
        
        df <- data.frame(read.xlsx(file$datapath, 1, startRow = 1))
        #, sep=c(";",",",".","/t")), header=F)
        return(df)
        
      } else {
        if (input$formt == "csv") {
          file <- input$file1
          req(file)
          df <- data.frame(
            read.csv(
              file$datapath,
              header = input$header,
              sep = input$sep,
              quote = input$quote,
              stringsAsFactors = TRUE
            )
          ) #csv
          #, sep=c(";",",",".","/t")), header=F)
          return(df)
        }
      }
      
    })
    
    
    # additional read_excel with converted variables
    
    
    
    
    
    
    # display 10 rows initially
    output$data1 <- DT::renderDataTable({
      DT::datatable(dados(), options = list(scrollX = TRUE))
    })
    
    output$select <- renderUI({
      dattA <- data.frame(dados())
      num <- dattA %>% select_if(is.numeric)
      
      selectizeInput(
        inputId = "variable1",
        label = "Choose variable to plot",
        choices = names(num),
        multiple = F
      )
      
    })
    
    
    observeEvent(input$report, {
      withProgress(message = "Writing report...", value = 0, {
        dd.r <- data.frame(dados())
        
        create_report(dd.r)
        
      })
    })
    
    
    
    
    #
    #     output$TableA <- renderUI({
    #
    #         dattA<-data.frame(dados())
    #
    #         selectizeInput(inputId = "CA", label = "Categorical variable", choices = names(dattA), multiple = F)
    #
    #     })
    #     output$TableB <- renderUI({
    #
    #         dattA<-data.frame(dados())
    #
    #         selectizeInput(inputId = "CB", label = "Numberic variable", choices = names(dattA), multiple = F)
    #
    #     })
    #
    #
    #     output$Cco<- renderUI({
    #
    #       dattA<-data.frame(dados())
    #       numD <- dattA
    #
    #       selectizeInput(inputId = "CO", label = "Fill", choices=names(numD), multiple = F)
    #
    #     })
    #
    
    output$sumariz1 <- renderUI({
      print(summarytools::dfSummary(dados(), graph.magnif = 0.75, headings = FALSE),
            method = 'render')
    })
    
    
    output$tabcl <- renderUI({
      dattA <-
        data.frame(dados()) %>% select_if(purrr::negate(is.numeric))
      
      
      selectizeInput(
        inputId = "tabclass",
        label = "Categorical variable",
        choices = names(dattA),
        multiple = F
      )
      
    })
    
    
    output$sumarizB <- renderUI({
      print(data.frame(dados()) %>%
              tbl_summary(by = input$tabclass) %>%
              add_p(),
            method = 'render')
    })
    
    
    
    
    # output$sumariz2 <- renderUI({
    #
    #     dats <- data.frame(dados())
    #
    #     var_1 <- req(dats[[input$variableTA]])
    #     var_2 <- req(dats[[input$variableTB]])
    #
    #     print(summarytools::ctable(var_1, var_2,
    #                                chisq = TRUE,
    #                                OR = TRUE,
    #                                RR = TRUE,
    #                                headings = FALSE,
    #                                dnn = c(names(dats[input$variableTA]),
    #                                        names(dats[input$variableTB]))
    #                                ), method = 'render')
    #
    # })
    
    #
    #
    # output$cat_plot <- renderPlotly({
    #
    #   datsC <- data.frame(dados())# %>% select_if(is.numeric)
    #
    # ggplotly(
    #
    #    ggplot(datsC, aes(fill = factor(.data[[as.name(input$CB)]]), y = factor(.data[[as.name(input$CO)]]), x = .data[[as.name(input$CB)]])) +
    #     geom_bar(position="fill", stat= "identity") +
    #     scale_fill_viridis(discrete = T) +
    #     ggtitle("") +
    #     theme_ipsum() +
    #     xlab("") + ylab("")
    #
    # )
    #
    #
    # })
    #
    
    
    
    
    
    
    
    output$plot <- renderPlotly({
      datsd <- data.frame(dados())
      
      if (is.null(input$variable1)) {
        return()
      }
      
      
      
      g7 <-  ggplot(datsd, aes(x = .data[[as.name(input$variable1)]],
                               fill = .data[[as.name(input$varselg)]])) +
        geom_histogram(
          color = "#e9ecef",
          alpha = 0.6,
          position = 'identity',
          bins = 20
        ) +
        theme_ipsum() +
        xlab(names(.data[input$variable1])) +
        labs(fill = "")
      
      ppl <- ggplotly(g7, width = 400, height = 400)
      
      g6 <- ggplot(datsd, aes(x = .data[[as.name(input$varselg)]],
                              y = .data[[as.name(input$variable1)]],
                              fill = .data[[as.name(input$varselg)]])) + geom_boxplot() +
        coord_flip() +
        scale_fill_discrete(name = names(.data[input$varselg])) +
        ylab(names(.data[input$variable1])) +
        xlab(names(.data[input$varselg])) +
        theme_ipsum()
      
      ppx <- ggplotly(g6, width = 700, height = 500)
      
      subplot(
        ppl,
        ppx,
        nrows = 2,
        margin = 0.04,
        heights = c(0.6, 0.4)
      )
      
      
      
      
    })
    
    output$plot0 <- renderPlotly({
      dats <- data.frame(dados())
      
      if (is.null(input$variable1)) {
        return()
      }
      
      
      g5 <- ggplot(dats, aes(x = .data[[as.name(input$variable1)]])) +
        geom_histogram(
          color = "black",
          fill = "cadetblue4",
          alpha = 0.6,
          position = 'identity',
          bins = 20
        ) +
        theme_ipsum() +
        xlab(names(.data[input$variable1])) +
        labs(fill = "")
      
      pl <- ggplotly(g5, width = 400, height = 400)
      
      
      
      g4 <- ggplot(dats, aes(x = "",
                             y = .data[[as.name(input$variable1)]],)) + geom_boxplot(color = "black", fill = "cadetblue4",) +
        coord_flip() +
        ylab(names(.data[input$variable1])) +
        theme_ipsum()
      
      pbx <- ggplotly(g4, width = 700, height = 500)
      
      subplot(
        pl,
        pbx,
        nrows = 2,
        margin = 0.04,
        heights = c(0.6, 0.4)
      )
      
      
    })
    
    
    
    
    output$selg <- renderUI({
      dats <- data.frame(dados())
      
      numD <- dats %>% select_if(purrr::negate(is.numeric))
      
      selectizeInput(
        inputId = "varselg",
        label = "Group",
        choices = names(numD),
        multiple = F
      )
      
      
    })
    
    
    # 2D plot #
    
    output$select2d <- renderUI({
      dattA <- data.frame(dados())
      numD <- dattA %>% select_if(is.numeric)
      
      selectizeInput(
        inputId = "v2d1",
        label = "Variable X",
        choices = names(numD),
        multiple = F
      )
    })
    
    output$select2d1 <- renderUI({
      dattA <- data.frame(dados())
      numD <- dattA %>% select_if(is.numeric)
      
      selectizeInput(
        inputId = "v2d2",
        label = "Variable Y",
        choices = names(numD),
        multiple = F
      )
    })
    
    output$select2dGroup <- renderUI({
      dattA <- data.frame(dados())
      numDs <- dattA %>% select_if(purrr::negate(is.numeric))
      
      selectizeInput(
        inputId = "v2dG",
        label = "Group",
        choices = names(numDs),
        multiple = F
      )
      
    })
    
    output$plot2de <- renderPlotly({
      dats <- data.frame(dados())
      
      g3 <- ggplot(dats,
                   aes(
                     x = .data[[as.name(input$v2d1)]],
                     y = .data[[as.name(input$v2d2)]],
                     color = .data[[as.name(input$v2dG)]]
                   )) +
        geom_point() +
        ylab(names(.data[input$v2d2])) +
        xlab(names(.data[input$v2d1])) +
        scale_color_discrete(names(.data[input$v2dG])) +
        theme_ipsum()
      
      
      ggplotly(g3, width = 700, height = 500)
      
      
    })
    
    
    output$plot2de0 <- renderPlotly({
      dats <- data.frame(dados())
      
      g2 <- ggplot(dats,
                   aes(x = .data[[as.name(input$v2d1)]],
                       y = .data[[as.name(input$v2d2)]])) +
        geom_point(color = "cadetblue4") +
        ylab(names(.data[input$v2d2])) +
        xlab(names(.data[input$v2d1])) +
        theme_ipsum()
      
      ggplotly(g2 , width = 700, height = 500)
      
    })
    
    
    
    
    # 3d plot #
    
    output$select3d <- renderUI({
      selectizeInput(
        inputId = "v3DA",
        label = "Variable 1",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$select3d1 <- renderUI({
      selectizeInput(
        inputId = "v3DB",
        label = "Variable 2",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$select3d2 <- renderUI({
      selectizeInput(
        inputId = "v3DC",
        label = "Variable 3",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$select3dGroup <- renderUI({
      selectizeInput(
        inputId = "v3DG",
        label = "Color",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$select3dGroupColors <- renderUI({
      selectizeInput(
        inputId = "v3DGC",
        label = "Group palette",
        choices = c("Set1", "Set2", "Dark2", "Accent"),
        multiple = F
      )
      
    })
    
    
    output$plot3d <- renderPlotly({
      dats <- data.frame(dados())
      
      
      fig <- if (is.numeric(dats[[as.name(input$v3DG)]]) == TRUE) {
        plot_ly(
          dats,
          
          x = ~ dats[[as.name(input$v3DA)]],
          y = ~ dats[[as.name(input$v3DB)]],
          z = ~ dats[[as.name(input$v3DC)]],
          
          type = "scatter3d",
          mode = "markers",
          
          marker = list(
            color = ~ dats[[as.name(input$v3DG)]],
            colorbar = list(title = names(dats[input$v3DG])),
            colorscale = 'Viridis'
          )
        ) %>%
          layout(scene = list(
            xaxis = list(title = names(dats[input$v3DA])),
            yaxis = list(title = names(dats[input$v3DB])),
            zaxis = list(title = names(dats[input$v3DC]))
            
          ))
        
      } else {
        plot_ly(
          dats,
          
          x = ~ dats[[as.name(input$v3DA)]],
          y = ~ dats[[as.name(input$v3DB)]],
          z = ~ dats[[as.name(input$v3DC)]],
          
          color = ~ dats[[as.name(input$v3DG)]],
          colors = input$v3DGC
          
        ) %>%
          add_markers() %>%
          layout(scene = list(
            xaxis = list(title = names(dats[input$v3DA])),
            yaxis = list(title = names(dats[input$v3DB])),
            zaxis = list(title = names(dats[input$v3DC]))
            
          ))
        
      }
      
      fig
      
      
    })
    
    
    output$choose_columns <- renderUI({
      the_data <- dados()
      
      colnames <- names(the_data)
      
      # Create the checkboxes and select them all by default
      selectizeInput(
        inputId = "columns_",
        label = "Choose variables",
        choices  = colnames,
        multiple = TRUE
      )
    })
    
    
    output$plotggp <- renderPlot({
      the_data <- req(dados())
      # Keep the selected columns
      columns_toplot <- req(input$columns_)
      the_data_subset <- the_data[, columns_toplot, drop = FALSE]
      ggpairs(req(the_data_subset))
    })
    
    # plotInput <- function() {
    #
    #   the_data <- req(dados())
    #   # Keep the selected columns
    #   columns_toplot <-req(input$columns_)
    #   the_data_subset<- the_data[, columns_toplot, drop = FALSE]
    #   ggpairs(req(the_data_subset))
    #
    # }
    #
    # output$foo = downloadHandler(
    #   filename = 'ggpairs.png',
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = width, height = height,
    #                      res = 300, units = "in")
    #     }
    #     ggsave(file, plot = plotInput(), device = device)
    #   })
    
    
    # FACETS ----
    
    output$xlin <- renderUI({
      dat.lin <- data.frame(dados())
      
      selectizeInput(
        "x.lin",
        "x axis",
        choices = names(dat.lin[, sapply(dat.lin, is.numeric)]),
        selected = T,
        multiple = F
      )
      
    })
    
    output$ylin <- renderUI({
      dat.lin <- data.frame(dados())
      
      selectizeInput(
        "y.lin",
        "y axis",
        choices = names(dat.lin[, sapply(dat.lin, is.numeric)]),
        selected = T,
        multiple = F
      )
      
    })
    
    output$gr.col <- renderUI({
      dat.lin <- data.frame(dados())
      
      selectizeInput(
        "gr.lin",
        "Group color",
        choices = names(dat.lin),
        selected = T,
        multiple = F
      )
      
    })
    
    output$lin.m <- renderUI({
      dat.lin <- data.frame(dados())
      
      selectizeInput(
        "lincm",
        "Linear method",
        choices = c("loess", "lm", "glm"),
        selected = T,
        multiple = F
      )
      
    })
    
    
    
    output$lin.lof <- renderUI({
      dat.lin <- data.frame(dados())
      
      selectizeInput(
        "lin.lofs",
        "GLM family",
        choices = c("poisson", "binomial"),
        selected = T,
        multiple = F
      )
      
      
      
    })
    
    
    
    output$l.span <- renderUI({
      sliderInput(
        "s.span",
        "Smoothing",
        min = 0,
        max = 1,
        animate = F,
        value = 0.5
      )
    })
    
    
    output$lin.loess <- renderPlotly({
      dat.lins <- data.frame(dados())
      dat.lin <- na.exclude(dat.lins)
      
      
      
      d <-
        ggplot(dat.lin, aes_string(x = input$x.lin, y = input$y.lin)) + #group=serviceInstanceName
        geom_point(color = "navy", size = 2) +
        geom_smooth(
          method = "loess",
          span = input$s.span,
          aes(colour = "green", fill = "lightblue"),
          se = T
        ) + theme_light() +
        scale_fill_manual(values = "lightblue")  +
        scale_colour_manual(values = "green") +
        theme(legend.position = "none")
      
      
      
      #facet_grid(. ~ Species, margins=TRUE)
      
      
      ggplotly(d)
      
    })
    
    output$lin.lm <- renderPlotly({
      dat.lins <- data.frame(dados())
      dat.lin <- na.exclude(dat.lins)
      
      
      
      d <-
        ggplot(dat.lin, aes_string(x = input$x.lin, y = input$y.lin)) + #group=serviceInstanceName
        geom_point(color = "navy", size = 2) +
        geom_smooth(method = "lm",
                    aes(colour = "green", fill = "lightblue"),
                    se = T) + theme_light() +
        scale_fill_manual(values = "lightblue")  +
        scale_colour_manual(values = "green") +
        theme(legend.position = "none")
      
      
      
      #facet_grid(. ~ Species, margins=TRUE)
      
      
      ggplotly(d)
      
    })
    
    
    
    output$lin.glm <- renderPlotly({
      dat.lins <- data.frame(dados())
      dat.lin <- na.exclude(dat.lins)
      
      
      
      d <-
        ggplot(dat.lin, aes_string(x = input$x.lin, y = input$y.lin)) + #group=serviceInstanceName
        geom_point(color = "navy", size = 2) +
        geom_smooth(
          method = "glm",
          method.args = list(family = input$lin.lofs),
          aes(colour = "green", fill = "lightblue"),
          se = T
        ) + theme_light() +
        scale_fill_manual(values = "lightblue")  +
        scale_colour_manual(values = "green") +
        theme(legend.position = "none")
      
      
      
      #facet_grid(. ~ Species, margins=TRUE)
      
      
      ggplotly(d)
      
    })
    
    
    
    
    
    
    
    ## CLUSTERING ##
    
    
    output$clu1 <- renderUI({
      selectizeInput(
        inputId = "clu1x",
        label = "Choose variable X axis",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$clu2 <- renderUI({
      selectizeInput(
        inputId = "clu2x",
        label = "Choose variable Y axis",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$clum <- renderUI({
      selectizeInput(
        inputId = "clu_m",
        label = "Choose clustering method",
        choices = c(
          "ward.D",
          "ward.D2",
          "single",
          "complete",
          "average",
          "mcquitty",
          "median",
          "centroid",
          "kmeans"
        ) ,
        selected = "kmeans",
        multiple = F
      )
      
    })
    
    
    output$clux <- renderPlotly({
      daa <- data.frame(dados()[, c(input$clu1x, input$clu2x)])
      da1 <- data.frame(na.exclude(daa))
      bn <-
        NbClust(da1, method = input$clu_m, distance = "euclidean")
      Cluster <- factor(bn$Best.partition)
      
      ggplotly(
        ggplot(da1,
               aes(
                 x = .data[[as.name(input$clu1x)]],
                 y = .data[[as.name(input$clu2x)]],
                 color = Cluster
               )) +
          geom_point() +
          ylab(names(da1[input$clu1x])) +
          xlab(names(da1[input$clu2x])) +
          scale_color_brewer(palette = "Set1") +
          theme_ipsum()
      )
      
    })
    
    
    
    ### MISSINGS ###
    
    output$missmap <- renderPlot({
      dfa <- data.frame(dados())
      
      missing.values <- dfa %>%
        gather(key = "key", value = "val") %>%
        mutate(isna = is.na(val)) %>%
        group_by(key) %>%
        mutate(total = n()) %>%
        group_by(key, total, isna) %>%
        summarise(num.isna = n()) %>%
        mutate(pct = num.isna / total * 100)
      
      
      levels <-
        (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
      
      
      dfa %>%
        mutate(id = row_number()) %>%
        gather(-id, key = "key", value = "val") %>%
        mutate(isna = factor(is.na(val))) %>%
        ggplot(aes(key, id, fill = isna)) +
        geom_raster(alpha = 0.8) +
        scale_fill_manual(
          name = "",
          values = c("blue4", "grey50"),
          labels = c("Present", "Missing")
        ) +
        scale_x_discrete(limits = levels) +
        labs(x = "Variable",
             y = "Row Number", title = "Missing values by row") +
        coord_flip() + theme_light() + theme(text = element_text(size = 17))
      
      
      
      
      #vis_miss(dados(), cluster = FALSE, sort_miss = FALSE)
      
      
    }, height = 500, width = 750)
    
    # output$misd <- renderUI({ selectizeInput("misf", "Factor to inspect", choices = names(dados() %>% select_if(purrr::negate(is.numeric))), multiple = F) })
    #
    # output$missplott <- renderPlotly({
    #
    #   datsF <- data.frame(dados()%>% select_if(purrr::negate(is.numeric)))
    #
    #
    #
    #   # fp.x <- input$pc.vars
    #   # fp.i <- NULL
    #   #
    #   # for(i in seq_along(fp.x)){ fp.i[i] <- which(names(datsF) == fp.x[i]) }
    #   #
    #   #
    #   # data_F <- na.exclude(dats.F[, c(p.i)])
    #
    #
    #
    #
    #   ggplotly(
    #   gg_miss_fct(x = datsF, fct = as.factor(.data[[as.name(get(input$misf))]]))
    #   )
    #
    #
    # })
    #
    
    
    
    
    
    
    
    
    # plotInput2 <- function() { vis_miss(dados(), cluster = FALSE, sort_miss = FALSE) }
    #
    # output$foo2 <- renderUI({
    #
    #   if(!is.null(dat_impos())) {
    #     downloadButton('Outputplot', 'Download missings pattern Plot')
    #
    #   }
    # })
    #
    
    # output$Outputplot <- downloadHandler(
    #
    #   filename = 'missmap.png',
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = width, height = height,
    #                      res = 300, units = "in")
    #     }
    #     ggsave(file, plot = plotInput2(), device = device)
    #
    #     })
    
    
    
    # imputation
    
    output$mivsx <- renderUI({
      numericInput(
        inputId = "mivx",
        label = "Max iterations",
        min = 1 ,
        max = 100,
        value = 50
      )
      
    })
    
    
    dat_impos <- eventReactive(req(input$impute), {
      withProgress(message = "Computing...(this may take several minutes)", value =
                     0, {
                       dat_miss <- data.frame(dados())
                       
                       dat_impo <-
                         mice(dat_miss, printFlag = F, maxit = input$mivx)
                       
                       
                       #data.frame(dat_impo$ximp)
                       
                       return(dat_impo)
                       
                     })
    })
    
    
    
    output$mivs <- renderUI({
      selectizeInput(
        inputId = "miv",
        label = "Choose variable",
        choices =  names(dados() %>% select_if(is.numeric)),
        multiple = F
      )
      
    })
    
    
    # output$mips <- renderPlot({
    #
    #   if(!is.null(dat_impos())) {
    #
    #   mim <- as.vector(colnames(data.frame(dados()) %>% select_if(is.numeric)), mode = "character")
    #   plot(dat_impos(), mim)
    #   }
    #
    # }, height = 700, width = 800)
    #
    output$mi.s <- renderPlot({
      prod <- data.frame(dados())
      impL <- complete(dat_impos())
      
      Imp <- NULL
      for (i in 1:nrow(prod)) {
        if (sum(is.na(prod[i, ])) > 0) {
          Imp[i] <- "Imputed"
        } else {
          Imp[i] <- "observed"
        }
        
      }
      
      
      impL$Imputation <- factor(Imp)
      
      g <- NULL
      for (i in 1:ncol(impL)) {
        g[i] <- list(
          ggplot(
            data = impL,
            aes_string(
              x = colnames(impL)[i],
              group = "Imputation",
              colour = "Imputation"
            )
          ) +
            geom_density(
              adjust = 1.5,
              alpha = .4,
              size = 1
            ) +
            theme_ipsum() +
            scale_colour_manual(values = c("cyan3", "blue4")) + ggtitle(paste(colnames(impL)[i])) +
            theme(legend.title = element_blank())
        )
        
      }
      
      #impL.num <- as.numeric(which(sapply(impL,is.numeric)))
      
      #do.call(grid.arrange, c(g[impL.num], list(ncol=2)))
      
      g[which(names(prod) == input$miv)]
      
      
    }, height = 500, width = 700)
    
    
    complete_imputed_data <- reactive({
      complete(dat_impos(), 2)
    })
    
    
    
    
    output$downloadData <- renderUI({
      if (!is.null(dat_impos())) {
        downloadButton('OutputFile', 'Download Imputed Data')
        
      }
      
    })
    
    
    output$OutputFile <- downloadHandler(
      filename = function() {
        paste("imputed-", Sys.Date(), ".xlsx", sep = "")
      },
      
      content = function(file) {
        write.xlsx(complete_imputed_data(), file)
      }
      
    )
    
    
    #output$imp.error <- renderPrint({
    
    
    # datsC <- data.frame(na.exclude(data.frame(dados()))) # complete for test
    #
    # datsCni <- datsC %>% dplyr::select(where(is.numeric))
    #
    # amost <- 50
    # datsCn <- NULL
    # datsCn.i <- NULL
    # datsCn.na <- NULL
    # for(i in 1:amost){
    #
    # datsCn[i] <- list(data.frame(datsCni[sample(c(1:nrow(datsCni)), 0.5*nrow(datsCni)),]))
    # datsCn.na[i] <- list(data.frame(prodNA(data.frame(datsCn[[i]]), 0.2)))
    # datsCn.i[i] <- list(complete(mice(datsCn.na, printFlag = FALSE),2))
    #
    #
    # }
    #
    # # fiqei aqui  IMPUTAtion ----
    #
    # i.error <- NULL
    #
    # for(i in 1:ncol(datsCn)){
    #
    #   i.error[i] <- sqrt(mean((datsCn[,i] - datsCn.i[,i])^2))
    # }
    #
    # ine <- data.frame(Variable = names(datsCn), RMSE = round(i.error,3))
    #
    # ine
    
    
    #})
    
    
    #
    # output$imp.cat <- renderPrint({
    #
    #
    #   datsC <- data.frame(na.exclude(data.frame(dados()))) # complete for test
    #
    #   datsCc <- datsC %>% dplyr::select(where(!is.numeric))
    #
    #   datsCc.na <- data.frame(prodNA(datsCc, 0.2))
    #   datsCc.i <- complete(mice(datsCc.na, printFlag = FALSE),2)
    #
    #
    #   i.accuracy <- NULL
    #   xtab <- NULL
    #
    #   for(i in 1:ncol(datsCc)){
    #
    #
    #     xtab[i] <- list(table(datsCc.na[,i], datsCc.i[,i]))
    #
    #
    #     i.accuracy[i] <- confusionMatrix(xtab[[i]])
    #
    #   }
    #
    #   i.accuracy[[1]]
    #
    #
    # })
    
    
    
    
    
    
    
    
    
    
    
    ### INFERENCE ###
    
    
    
    # shapiro-wilk normality test OK
    
    output$xshp <- renderUI({
      selectizeInput(
        inputId = "xshpx",
        label = "Choose variable to test",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$shpN <- DT::renderDataTable({
      dattt <- data.frame(dados())
      
      xx <- as.numeric(unlist(dattt[req(input$xshpx)]))
      
      sk <- shapiro.test(xx)
      
      DT::datatable(noquote(t(as.data.frame(
        broom::tidy(sk)
      ))))
      
    })
    
    
    
    # correlation test OK
    
    output$c1 <- renderUI({
      selectizeInput(
        inputId = "xc1",
        label = "Choose variable 1 to test",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$c2 <- renderUI({
      selectizeInput(
        inputId = "xc2",
        label = "Choose variable 2 to test",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$copt <- renderUI({
      selectizeInput(
        inputId = "Copt",
        label = "Choose alternative hypotheis type",
        choices = c("two.sided", "less", "greater"),
        multiple = F
      )
      
    })
    
    output$copt2 <- renderUI({
      selectizeInput(
        inputId = "Copt2",
        label = "Choose correlation method",
        choices = c("pearson", "kendall", "spearman"),
        multiple = F
      )
      
    })
    
    
    
    output$Ccfl <- renderUI({
      numericInput("Ccfl",
                   "Choose confidence level",
                   value = 0.95,
                   max = 1)
      
    })
    
    
    
    output$corr <- DT::renderDataTable({
      dattt <- data.frame(dados())
      attach(dattt)
      
      xx1 <- as.numeric(unlist(dattt[req(input$xc1)]))
      xx2 <- as.numeric(unlist(dattt[req(input$xc2)]))
      
      shi <-
        cor.test(
          xx1,
          xx2,
          alternative = req(input$Copt),
          conf.level = req(input$Ccfl),
          method = req(input$Copt2)
        )
      DT::datatable(noquote(t(as.data.frame(
        broom::tidy(shi)
      ))))
    })
    
    
    # t-test OK
    
    output$tt1 <- renderUI({
      selectizeInput(
        inputId = "xtt1",
        label = "Choose variable 1 to test",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$tt2 <- renderUI({
      selectizeInput(
        inputId = "xtt2",
        label = "Choose variable 2 to test",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$topt <- renderUI({
      selectizeInput(
        inputId = "Topt",
        label = "Choose alternative hypotheis type",
        choices = c("two.sided", "less", "greater"),
        multiple = F
      )
      
    })
    
    
    output$tcfl <- renderUI({
      numericInput(
        "Tcfl",
        "Choose confidence level",
        value = 0.95,
        min = 0,
        max = 1
      )
      
    })
    
    
    
    output$tt <- DT::renderDataTable({
      dattt <- data.frame(dados())
      attach(dattt)
      
      
      xx1 <- as.numeric(unlist(dattt[req(input$xtt1)]))
      xx2 <- as.numeric(unlist(dattt[req(input$xtt2)]))
      
      tyus <-
        t.test(xx1,
               xx2,
               alternative = input$Topt,
               conf.level = input$Tcfl)
      DT::datatable(noquote(t(as.data.frame(
        broom::tidy(tyus)
      ))))
    })
    
    
    
    # Chi-square test OK
    
    output$chiX1 <- renderUI({
      selectizeInput(
        inputId = "chi1",
        label = "Choose variable 1",
        choices = names(dados()),
        multiple = F
      )
    })
    
    output$chiX2 <- renderUI({
      selectizeInput(
        inputId = "chi2",
        label = "Choose variable 2",
        choices = names(dados()),
        multiple = F
      )
    })
    
    
    output$chisq <- DT::renderDataTable({
      dattt <- data.frame(dados())
      attach(dattt)
      
      x1 <- unlist(dattt[req(input$chi1)])
      x2 <- unlist(dattt[req(input$chi2)])
      
      tabChi <- table(x1, x2)
      
      chiu <- chisq.test(tabChi)
      DT::datatable(noquote(t(as.data.frame(
        broom::tidy(chiu)
      ))))
    })
    
    
    
    
    # Wilcoxon- test
    
    # output$wilcox1<-renderUI({
    #
    #     selectizeInput(inputId = "wix1", label="Choose variable 1", choices= names(dados()), multiple = F)
    #
    # })
    #
    # output$wilcox2<-renderUI({
    #
    #     selectizeInput(inputId = "wix2", label="Choose variable 2", choices= names(dados()), multiple = F)
    #
    # })
    #
    # output$wopt<-renderUI({
    #
    #     selectizeInput(inputId = "Wopt", label="Choose alternative hypotheis type", choices= c("two.sided", "less", "greater"), multiple = F)
    #
    # })
    #
    #
    # output$wcfl<-renderUI({
    #
    #     numericInput("wcfl", "Choose confidence level", value = 0.95, max = 1)
    #
    # })
    #
    # output$wpair<-renderUI({
    #
    #     selectizeInput(inputId = "Wopt2", label="Paired test?", choices= c(TRUE, FALSE), multiple = F)
    #
    # })
    #
    #
    #
    # output$wilcoxt<-renderPrint({
    #
    #     dattt<-data.frame(dados())
    #     attach(dattt)
    #
    #     xx1<-as.numeric(unlist(dattt[req(input$wix1)]))
    #     xx2<-as.numeric(unlist(dattt[req(input$wix2)]))
    #
    #
    #     wilcox.test(xx1, xx2, alternative = input$Wopt, conf.level = input$wcfl, paired = input$Wopt2)
    #
    # })
    
    
    
    
    # z-test
    
    # output$ztest1<-renderUI({
    
    #   selectizeInput(inputId = "wix1", label="Choose variable 1", choices= names(dados()), multiple = F)
    
    # })
    
    # output$ztest2<-renderUI({
    
    #  selectizeInput(inputId = "wix2", label="Choose variable 2", choices= names(dados()), multiple = F)
    
    # })
    
    # output$zopt<-renderUI({
    
    #   selectizeInput(inputId = "Zopt", label="Choose alternative hypotheis type", choices= c("two.sided", "less", "greater"), multiple = F)
    
    # })
    
    
    # output$zcfl<-renderUI({
    
    #   numericInput("zcfl", "Choose confidence level", value = 0.95, min = 0, max = 1)
    
    # })
    
    
    
    # output$zt<-renderPrint({
    
    #   dattt<-data.frame(dados())
    #   attach(dattt)
    
    #  xx1<-as.numeric(unlist(dattt[req(input$wix1)]))
    #  xx2<-as.numeric(unlist(dattt[req(input$wix2)]))
    
    
    #  z.test(xx1, xx2, alternative = input$Zopt, conf.level = input$zcfl)
    
    # })
    
    
    # var test F-test
    
    output$vartest1 <- renderUI({
      selectizeInput(
        inputId = "wi1",
        label = "Choose variable 1",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$vartest2 <- renderUI({
      selectizeInput(
        inputId = "wi2",
        label = "Choose variable 2",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$varopt <- renderUI({
      selectizeInput(
        inputId = "Vopt",
        label = "Choose alternative hypotheis type",
        choices = c("two.sided", "less", "greater"),
        multiple = F
      )
      
    })
    
    
    output$varcfl <- renderUI({
      numericInput(
        "cfl",
        "Choose confidence level",
        value = 0.95,
        min = 0,
        max = 1
      )
      
    })
    
    
    
    output$vrt <- DT::renderDataTable({
      dattt <- data.frame(dados())
      attach(dattt)
      
      xx1 <- as.numeric(unlist(dattt[req(input$wi1)]))
      xx2 <- as.numeric(unlist(dattt[req(input$wi2)]))
      
      
      vii <-
        var.test(xx1,
                 xx2,
                 alternative = input$Vopt,
                 conf.level = input$cfl)
      DT::datatable(noquote(t(as.data.frame(
        broom::tidy(vii)
      ))))
    })
    
    
    # var test BRTLET
    
    output$bvartest1 <- renderUI({
      selectizeInput(
        inputId = "bwi1",
        label = "Choose variable",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    output$bvartest2 <- renderUI({
      selectizeInput(
        inputId = "bwi2",
        label = "Choose group",
        choices = names(dados()),
        multiple = F
      )
      
    })
    
    
    output$vrt2 <- DT::renderDataTable({
      dattt <- data.frame(dados())
      attach(dattt)
      
      bxx1 <- as.numeric(unlist(dattt[req(input$bwi1)]))
      bxx2 <- factor(unlist(dattt[req(input$bwi2)]))
      
      
      bvii <- car::leveneTest(bxx1 ~ bxx2, data = dattt)
      DT::datatable(noquote(t(data.frame(
        round(as.data.frame(broom::tidy(bvii)), 3)
      ))))
    })
    
    
    
    
    
    ### REGRESSION ----
    
    output$select2 <- renderUI({
      varSelectInput(
        inputId = "DepVar",
        label = "Dependent Variable:",
        dados(),
        selected = F,
        multiple = FALSE
      )
    })
    output$select3 <- renderUI({
      varSelectInput(
        inputId = "IndVar",
        label = "Independent Variables:",
        dados(),
        selected = F,
        multiple = TRUE
      )
    })
    
    # FORMULA for REGRESSION ----
    
    myformula <- reactive({
      expln <- paste(req(input$IndVar), collapse = "+")
      as.formula(paste(req(input$DepVar), " ~ ", expln))
    })
    #yy <- reactive({ req(input$DepVar) })
    
    
    # LINEAR REGRESSION ----
    
    
    mod <- eventReactive(req(input$analysis), {
      lm(myformula(), data = dados())
    })
    
    output$testq <- renderUI({
      t.lm <- tab_model(mod())
      
      HTML(t.lm$knitr)
      
    })
    #renderPrint({ parameters(mod()) })
    
    # output$testqA <- renderPrint({
    #
    #   ri <- data.frame(r.squared = summary(mod())[8],            # r.squared
    #                    adjusted.r.squared = summary(mod())[9])   # adjusted r.squared
    #
    #   rownames(ri) <- c("")
    #
    #   return(ri)
    #
    # })
    
    
    output$lcoef <- renderPlot({
      plot(parameters(mod()))
    })
    
    
    
    output$diag <- renderPlot({
      performance::check_model(mod())
    }, height = 650, width = 750)
    #
    # plotInput3 <- function() { check_model(mod())  }
    #
    # output$foo3 = downloadHandler(
    #   filename = 'diag_linear.png',
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = width, height = height,
    #                      res = 300, units = "in")
    #     }
    #     ggsave(file, plot = plotInput3(), device = device)
    #   })
    #
    
    
    # LOGISTIC REGRESSION ----
    
    # binary
    
    mod2 <- eventReactive(req(input$analysis), {
      glm(myformula(),
          data = dados(),
          family = binomial(link = "logit"))
    })
    output$testq2 <- renderUI({
      t.lm2 <- tab_model(mod2())
      
      HTML(t.lm2$knitr)
      
    })
    
    
    
    
    #renderPrint({ parameters(mod2()) })
    
    
    # output$testq2A <- renderPrint({
    #
    #
    #   ril <- data.frame(r.squared = summary(mod2())[8],
    #                    adjusted.r.squared = summary(mod2())[9])
    #
    # rownames(ril) <- c("")
    #
    # return(ril)
    #
    #
    # })
    
    output$logiOdds <- renderPlot({
      plot(parameters(mod2()))
    })
    
    output$logi_gof <- renderPrint({
      anova(mod2(),
            update(mod2(), ~ 1),    # update here produces null model for comparison
            test = "Chisq")
      
      
    })
    
    
    
    output$diag2 <-  renderPlot({
      performance::check_model(mod2())
    }, height = 650, width = 750)
    
    
    
    
    
    
    # ordinal
    
    # expln.ord <- reactive({ paste(req(input$IndVar), collapse = "+") })
    #
    #    yy.ord <- reactive({
    #
    #     dados2 <- as.data.frame(dados())
    #     dpvs <- input$DepVar
    #
    #     dpv.fs <- factor(dados2[dpvs], levels = unique(dados2[dpvs]), ordered = TRUE)
    #
    #     return(dpv.fs)
    #
    #     })
    #
    # dados.ord <- reactive({
    #
    #   dados2 <- as.data.frame(dados())
    #   dpv <- input$DepVar
    #
    #   dpv.f <- factor(dados2[dpv], levels = unique(dados2[dpv]), ordered = TRUE)
    #
    #   numc <- which(colnames(dados2) == names(dados2[dpv]))
    #
    #   dados2[,numc] <- dpv.f
    #
    # return(dados2)
    #
    # })
    #
    #
    #   mod2a.null <- eventReactive(req(input$analysis), {
    #
    #     mod0.ord <- polr(yy.ord() ~ 1, data = dados.ord(), Hess = TRUE)
    #
    #     return(mod0.ord)
    #
    #     })
    #
    #   mod2a <- eventReactive(req(input$analysis), {
    #
    #     mod1.ord <- polr( yy.ord() ~ 1 , data = dados.ord(), Hess = TRUE)
    #
    #     return(mod1.ord)
    #
    #
    #     })
    #
    #   output$testq2a <- renderPrint({
    #       summary(mod2a())
    #   })
    #
    #   output$diagm1 <- renderPrint({
    #     anova(mod2a(), mod2a.null())
    #   })
    #
    #   output$diagm2 <- renderPrint({
    #     PseudoR2(mod2a(), which = c("CoxSnell", "Nagelkerke", "McFadden"))
    #   })
    #
    #   output$diagm3 <- renderPrint({
    #     sa <- coef(summary(mod2a))
    #       pva <- pnorm(abs(sa[,"t value"]), lower.tail = FALSE)*2
    #       spa <- cbind(s, "p-value" = pva)
    #       spa
    #   })
    #
    #   # multinomial
    #
    #   mod2b.null <- eventReactive(req(input$analysis), { multinom(yy() ~ 1 , data = as.matrix(dados()), model =TRUE) })
    #   mod2b <- eventReactive(req(input$analysis), { multinom( myformula() , data = as.matrix(dados()), model = TRUE) })
    #
    #   output$testq2b <- renderPrint({
    #       summary(mod2b())
    #   })
    #
    #   output$diaml1 <- renderPrint({ anova(mod2b(), mod2b.null()) })
    #   output$diaml2 <- renderPrint({
    #
    #     zb <- summary(mod2b)$coefficients/summary(mod2b)$standard.errors # Wald Z value
    #     pb <- (1-pnorm(abs(zb), 0, 1))*2 # p-value
    #     pb
    #
    #   })
    #   output$diaml3 <- renderPrint({ PseudoR2(mod2b(), which = c("CoxSnell", "Nagelkerke", "McFadden")) })
    #
    #
    # POISSON REGRESSEION
    
    mod3 <-
      eventReactive(req(input$analysis), {
        glm(myformula(), data = dados(), family = "poisson")
      })
    
    ovd <-
      reactive({
        as.numeric(performance::check_overdispersion(mod3()))[4]
      })
    zi <-
      reactive({
        as.numeric(performance::check_zeroinflation(mod3())$ratio)
      })
    
    mod3.nb <-
      eventReactive(req(input$analysis), {
        zeroinfl(myformula() , data = dados(), dist = "negbin")
      })
    mod3a <-
      eventReactive(req(input$analysis), {
        zeroinfl(myformula(), data = dados())
      })
    mod3b <-
      eventReactive(req(input$analysis), {
        glm.nb(Deaths ~ Rel.Hum, data = snails)
      })
    
    output$testq3 <- renderUI({
      if (isTRUE(ovd() < 0.05 & zi() < 0.5)) {
        du <- tab_model(mod3.nb())
        HTML(du$knitr)
        
      } else {
        if (isTRUE(ovd() > 0.05 & zi() < 0.5)) {
          du2 <- tab_model(mod3a())
          HTML(du2$knitr)
        } else {
          if (isTRUE(ovd() < 0.05 & zi() > 0.5)) {
            du3 <- tab_model(mod3b())
            HTML(du3$knitr)
          } else {
            #if(isTRUE(ovd()>0.05 & zi()>0.5)){
            du4 <- tab_model(mod3())
            HTML(du4$knitr)
            #}
          }
        }
      }
    })
    
    
    
    output$plotq3 <- renderPlot({
      if (isTRUE(ovd() < 0.05 & zi() < 0.5)) {
        plot(parameters(mod3.nb()))
        
      } else {
        if (isTRUE(ovd() > 0.05 & zi() < 0.5)) {
          plot(parameters(mod3a()))
          
        } else {
          if (isTRUE(ovd() < 0.05 & zi() > 0.5)) {
            plot(parameters(mod3b()))
            
          } else {
            #if(isTRUE(ovd()>0.05 & zi()>0.5)){
            
            plot(parameters(mod3()))
            
            #}
          }
        }
      }
    })
    
    output$logi_gof2 <- renderPrint({
      if (isTRUE(ovd() < 0.05 & zi() < 0.5)) {
        anova(mod3.nb(),
              update(mod3.nb(), ~ 1),
              test = "Chisq")
        
        
      } else {
        if (isTRUE(ovd() > 0.05 & zi() < 0.5)) {
          anova(mod3a(),
                update(mod3a(), ~ 1),
                test = "Chisq")
          
          
        } else {
          if (isTRUE(ovd() < 0.05 & zi() > 0.5)) {
            anova(mod3b(),
                  update(mod3b(), ~ 1),    # update here produces null model for comparison
                  test = "Chisq")
            
          } else {
            #if(isTRUE(ovd()>0.05 & zi()>0.5)){
            
            anova(mod3(),
                  update(mod3(), ~ 1),    # update here produces null model for comparison
                  test = "Chisq")
            
            
            #}
          }
        }
      }
    })
    
    
    
    
    
    
    
    #
    # output$diag3b <- renderPrint({
    #
    #     if(isTRUE(ovd()<0.05 & zi()>0.5)){ PseudoR2(mod3b(), which = c("CoxSnell", "Nagelkerke", "McFadden")) } else {
    #                if(isTRUE(ovd()>0.05 & zi()>0.5)){ PseudoR2(mod3(), which = c("CoxSnell", "Nagelkerke", "McFadden")) }
    #             }
    # })
    #
    #
    # output$diag3c <- renderPrint({
    #
    #     if(isTRUE(ovd()<0.05 & zi()<0.5)){ vuong(mod3(), mod3.nb()) } else {
    #         if(isTRUE(ovd()>0.05 & zi()<0.5)){ vuong(mod3(), mod3a()) } else {
    #             if(isTRUE(ovd()<0.05 & zi()>0.5)){ vuong(mod3(), mod3b()) }
    #                 }}
    # })
    
    
    # output$diag3d <- renderPrint({
    #
    #     if(isTRUE(ovd()<0.05 & zi()>0.5)){ anova(mod3b(), mod3b.null()) } else {
    #         if(isTRUE(ovd()>0.05 & zi()>0.5)){ anova(mod3(), mod3.null()) }
    #         }
    # })
    
    
    
    
    
    
    ## SURVIVAL ANALYSIS
    
    output$selectTIME <- renderUI({
      varSelectInput(
        inputId = "TIME",
        label = "Select Time variable",
        dados(),
        selected = F,
        multiple = FALSE
      )
    })
    
    output$selectSTATUS <- renderUI({
      varSelectInput(
        inputId = "STATUS",
        label = "Select Outcome variable to Cox-regressionn",
        dados(),
        selected = F,
        multiple = FALSE
      )
    })
    
    output$selectTREAT <- renderUI({
      varSelectInput(
        inputId = "TREAT",
        label = "Select Treatment variable",
        dados(),
        selected = F,
        multiple = FALSE
      )
    })
    
    output$selectVARS <- renderUI({
      varSelectInput(
        inputId = "VARS",
        label = "Select Variables to Cox-regression",
        dados(),
        selected = F,
        multiple = TRUE
      )
    })
    
    # COX REGRESSION (SURVIVAL)
    
    myformula2 <- reactive({
      expln <- paste(req(input$VARS), collapse = "+")
      as.formula(paste(
        "Surv(",
        req(input$TIME),
        ",",
        req(input$STATUS),
        ")",
        " ~ ",
        expln
      ))
    })
    
    
    mod4 <- eventReactive(req(input$analysis2), {
      coxph(myformula2(), data = dados())
    })
    
    output$COXsum <- renderUI({
      t.sm <- tab_model(mod4())
      
      HTML(t.sm$knitr)
      
    })
    
    #summary(mod4())
    
    #output$COXdiag <- renderPlot({
    #    check_model(mod4(), height = 650, width = 750)
    #})
    
    myformula3 <- reactive({
      as.formula(paste(
        "Surv(",
        req(input$TIME),
        ",",
        req(input$STATUS),
        ")",
        " ~ ",
        req(input$TREAT)
      ))
    })
    
    surv_data <- reactive({
      raw_surv <- dados()
      data.frame(Time = raw_surv[[input$TIME]],
                 Status    = raw_surv[[input$STATUS]],
                 Treat  = raw_surv[[input$TREAT]])
    })
    
    output$KMplot <- renderPlot({
      new_fit <-
        reactive({
          survfit(Surv(Time, Status) ~ Treat, data = surv_data())
        })
      ggsurvplot(
        new_fit(),
        risk.table = TRUE,
        pval = TRUE,
        data = surv_data()
      )
    }, height = 500, width = 600)
    
    # plotInput4 <- function() {
    #
    # new_fit<-reactive({survfit(Surv(Time, Status)~Treat, data=surv_data())})
    # ggsurvplot(new_fit(), risk.table = TRUE, pval = TRUE, data = surv_data())
    # }
    
    # output$foo4 = downloadHandler(
    #   filename = 'km_plot.png',
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = width, height = height,
    #                      res = 300, units = "in")
    #     }
    #     ggsave(file, plot = plotInput4(), device = device)
    #   })
    
    
    ### PCA ----
    
    
    output$pdd <- renderUI({
      ddaa <- data.frame(dados())
      
      selectizeInput(
        inputId = "pc.vars",
        label = "Select variables to include in PCA",
        choices = names(ddaa[, sapply(ddaa, is.numeric)]),
        selected = T,
        multiple = T
      )
      
    })
    
    res.pca <- eventReactive(req(input$analysisPCA), {
      df <- data.frame(na.exclude(data.frame(dados())))
      
      p.x <- input$pc.vars
      p.i <- NULL
      
      for (i in seq_along(p.x)) {
        p.i[i] <- which(names(df) == p.x[i])
      }
      
      
      data_num <- na.exclude(df[, c(p.i)])
      
      pca <- prcomp(req(data_num), scale. = T, center = F)
      
      return(pca)
    })
    
    
    observeEvent(!is.null(res.pca()) , {
      output$PC.info <- renderPrint({
        (summary(res.pca()))
        
      })
      
      output$PC.info2 <- renderPrint({
        (res.pca()$rotation)
        
        
      })
      
    })
    
    
    
    output$PCA <- renderPrint({
      PC <- res.pca()
      summary(PC)
      
    })
    
    
    output$dimA <- renderUI({
      pc <- data.frame(res.pca()$x)
      selectizeInput(
        inputId = "dimA",
        label = "Dimension X-axis",
        choices = names(pc),
        multiple = F
      )
      
    })
    
    output$dimB <- renderUI({
      pc <- data.frame(res.pca()$x)
      selectizeInput(
        inputId = "dimB",
        label = "Dimension Y-axis",
        choices = names(pc),
        multiple = F
      )
      
    })
    
    
    output$clumPCA <- renderUI({
      selectizeInput(
        inputId = "clu_mPCA",
        label = "Choose clustering method",
        choices = c(
          "ward.D",
          "ward.D2",
          "single",
          "complete",
          "average",
          "mcquitty",
          "median",
          "centroid",
          "kmeans"
        ) ,
        selected = "kmeans",
        multiple = F
      )
      
    })
    
    
    output$Colls <- renderUI({
      selectizeInput(
        inputId = "Colls",
        label = "Group colors",
        choices = c("Set1", "Set2", "Paired", "Dark2"),
        multiple = F
      )
      
    })
    
    
    
    output$plotlyPCA <- renderPlotly({
      pcc <- data.frame(res.pca()$x)
      
      
      p.xA <- input$dimA
      #p.xB <- input$dimB
      
      p.ixP <- NULL
      
      for (i in seq_along(p.xA)) {
        p.ixP[i] <- which(names(pcc) == p.xA[i])
        #p.ixP[i] <- which(names(pcc) == p.xB[i])
      }
      
      
      data_PCs <- na.exclude(pcc[, c(p.xA)])
      
      
      #daaPCA <- data.frame(pcc[[c(as.name(input$dimA),as.name(input$dimB))]])
      #da1PCA <- data.frame(na.exclude(daaPCA))
      
      bnPCA <-
        NbClust(data_PCs,
                method = input$clu_mPCA,
                distance = "euclidean")
      Cluster <- factor(bnPCA$Best.partition)
      
      
      
      
      theme <-
        theme(
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"),
          plot.margin = unit(c(1, 1, 1, 1), "line")
        )
      
      ggplotly(
        ggplot(data = pcc, aes(
          x = .data[[as.name(input$dimA)]], y = .data[[as.name(input$dimB)]], color = Cluster
        )) +
          geom_point() +
          ggtitle("Biplot") + theme + xlab(noquote(input$dimA)) +
          ylab(noquote(input$dimB)) + scale_color_brewer(palette = input$Colls)
      )
      
      
    })
    
    
    
    outputOptions(output, "plotlyPCA", suspendWhenHidden = FALSE)
    
    
    
    
    
    
    # Correlation heatmap ----
    
    corr_data <- reactive({
      num_data <- dados() %>% dplyr::select(where(is.numeric))
      
      cor(num_data, use = "pairwise.complete.obs")
      
    })
    
    
    output$heatmap <- renderPlotly({
      plot_ly(colors = "RdBu") %>%
        add_heatmap(x = rownames(corr_data()),
                    y = colnames(corr_data()),
                    z = corr_data()) %>%
        colorbar(limits = c(-1, 1))
      
    })
    
    
    
  }
  
  shinyApp(ui, server)
  
  #runApp(list(ui = ui, server = server), launch.browser =T)













