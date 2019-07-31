library(shiny)

navbarPage("HIV",
  tabPanel("unsupervisedLearn,cluster",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("var", "cluster var", selected = "meanCase", choices = c("meanCase","meanPop")
        )
      ),
      mainPanel(
        plotOutput("plotCluster")
        #h5("(find the model parameter.)"),
        #verbatimTextOutput("treemodelresult")
      )
    )
  ),



  tabPanel("unsupervisedLearn,PCA",
    #
    column(6,
           box(width=12,title="PC result",
               background="red",solidHeader=TRUE,
               h5("(find the model parameter.)"),
               verbatimTextOutput("PCAresult"))


   # verbatimTextOutput("summary")
     ),

   column(width=6,

          box(width=12,
              plotOutput("plotPC"),
              br(),
              h4("The plot PC ")
          )
   )

),



tabPanel("supervisedLearn,trees",
         #
         column(3,
                box(width=12,title="trees choice",
                    background="red",solidHeader=TRUE,
                    h5("(choose a supervised learning model.)"),
                    selectizeInput("treemodelmethod", "treemodelmethod", selected = "rpart", choices = c("rpart", "treebag","rf", "gbm"))

                )
         ),
         column(width=9,
                fluidRow(
                  box(width=12,
                      verbatimTextOutput("trainresult")),
                  br(),
                  h4(" Tree training results")

                )
         )

),

tabPanel("scroll,status bar and click",
         #
         column(6,
                box( title = "Case Analyses Details", status = "primary", height =
                       "595",width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("trace_table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                     ))
         ),
         column(width=6,
                fluidRow(
                  box(width=12,

                      plotOutput("plot5", click = "plot_click"),
                      br(),
                      br(),
                      verbatimTextOutput("info5"),

                      h4("The plot above displays the relation of   population with cases of HIV in USA")


                  )
                )


       #  column(width=3,
       #         fluidRow(
       #           box(width=12,
       #               verbatimTextOutput("summary4"),
       #               br(),
      #              h4("The first column provides year,The second column is the HIV cases for that year"),

      #                h4("The other columns provide mean, median and  SD ")


                )
            )




  #navbarMenu("More",
  #  tabPanel("Table",
   #   DT::dataTableOutput("table")
   # ),
   # tabPanel("About",
   #   fluidRow(
   #     column(6,
    #     includeMarkdown("about.md")
    #   ),
    #    column(3,
    #    img(class="img-polaroid",
     #      src=paste0("http://upload.wikimedia.org/",
     #       "wikipedia/commons/9/92/",
     #   "1919_Ford_Model_T_Highboy_Coupe.jpg"))
         # tags$small(
         #   "Source: Photographed at the Bay State Antique ",
          #  "Automobile Club's July 10, 2005 show at the ",
          #  "Endicott Estate in Dedham, MA by ",
          #  a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
           #   "User:Sfoskett")
          )

