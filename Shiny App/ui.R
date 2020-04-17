ui <- dashboardPage(skin = "yellow", useShinyjs(),
  header = dashboardHeader(
    title = "Suicide Awareness"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "High Level Stats",
        tabName = "highLevelStats",
        icon = icon("bezier-curve")
      ),
      menuItem(
        text = "Demographics",
        tabName = "demographics",
        icon = icon("users")
      ),
      # menuItem(
      #   text = "Related Factors",
      #   tabName = "relatedFactors",
      #   icon = icon("layer-group")
      # ),
      menuItem(
        text = "Data Explorer I: States",
        tabName = "statesMap",
        icon = icon("search-location")
      ),
      menuItem(
        text = "Data Explorer II: Global",
        tabName = "worldMap",
        icon = icon("search-location")
      ),
      menuItem(
        text = "Summary",
        tabName = "summary",
        icon = icon("hands-helping")
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "highLevelStats",
        fluidRow(
          column(width = 4,
            uiOutput(outputId = "statsOverall"),
            column(width = 8,
              sliderInput(inputId = "overallProgress", label = "", min = 1, max = 8, value = 1,
                   animate = animationOptions(interval = 5000), width = "200px")
            ),
            column(width = 1,
              br(), br(),
              actionButton(inputId = "previousOverall", label = "", icon = icon("angle-left"),
                style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")
            ),
            column(width = 1, offset = 1,
              br(), br(),
              actionButton(inputId = "nextOverall", label = "", icon = icon("angle-right"),
                style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")
            )
          ),
          column(width = 4,
                 uiOutput(outputId = "statsMethod"),
                 column(width = 8, sliderInput(inputId = "methodProgress", label = "", min = 1, max = 2, value = 1,
                             step = 1, animate = animationOptions(interval = 5000))),
                 column(width = 1,
                        br(), br(), actionButton(inputId = "previousMethod", label = "", icon = icon("angle-left"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")),
                 column(width = 1, offset = 1,
                        br(), br(), actionButton(inputId = "nextMethod", label = "", icon = icon("angle-right"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;"))
                 
          ),
          column(width = 4,
                 uiOutput(outputId = "statsGender"),
                 column(width = 8, sliderInput(inputId = "genderProgress", label = "", min = 1, max = 3, value = 1,
                             animate = animationOptions(interval = 5000))),
                 column(width = 1,
                        br(), br(), actionButton(inputId = "previousGender", label = "", icon = icon("angle-left"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")),
                 column(width = 1, offset = 1,
                        br(), br(), actionButton(inputId = "nextGender", label = "", icon = icon("angle-right"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;"))
          )
        ),
        fluidRow(
          column(width = 4,
                 uiOutput(outputId = "statsAge"),
                 column(width = 8, sliderInput(inputId = "ageProgress", label = "", min = 1, max = 5, value = 1,
                             animate = animationOptions(interval = 5000))),
                 column(width = 1,
                        br(), br(), actionButton(inputId = "previousAge", label = "", icon = icon("angle-left"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")),
                 column(width = 1, offset = 1,
                        br(), br(), actionButton(inputId = "nextAge", label = "", icon = icon("angle-right"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;"))
          ),
          column(width = 4,
                 uiOutput(outputId = "statsRace"),
                 column(width = 8, sliderInput(inputId = "raceProgress", label = "", min = 1, max = 4, value = 1,
                             animate = animationOptions(interval = 5000))),
                 column(width = 1,
                        br(), br(), actionButton(inputId = "previousRace", label = "", icon = icon("angle-left"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")),
                 column(width = 1, offset = 1,
                        br(), br(), actionButton(inputId = "nextRace", label = "", icon = icon("angle-right"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;"))
          ),
          column(width = 4,
                 uiOutput(outputId = "statsOrientation"),
                 column(width = 8, sliderInput(inputId = "orientationProgress", label = "", min = 1, max = 2, value = 1,
                             step = 1, animate = animationOptions(interval = 5000))),
                 column(width = 1,
                        br(), br(), actionButton(inputId = "previousOrientation", label = "", icon = icon("angle-left"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;")),
                 column(width = 1, offset = 1,
                        br(), br(), actionButton(inputId = "nextOrientation", label = "", icon = icon("angle-right"),
                                                 style = "background-color: #AED6F1; border-color: #000; height: 35px; width: 35px; border-radius: 50%;"))
          )
        )
      ),
      tabItem(tabName = "demographics",
        fluidRow(
          selectInput(inputId = "demographic", label = "Demographic Type",
                      choices = c("gender","age","race","interaction"), width = "150px")
        ),
        jqui_sortable(
          ui = div(id = "genderPlots",
            column(width = 6,
              plotlyOutput(outputId = "gender2017", width = "500px")
            ),
            column(width = 6,
              dygraphOutput(outputId = "genderByYear", width = "500px")
            )
          )
        ),
        # jqui_sortable(
        #   ui = 
            div(id = "agePlots",
            column(width = 6,
              plotlyOutput(outputId = "age2017", width = "500px")
            ),
            column(width = 6,
              dygraphOutput(outputId = "ageByYear", width = "500px")
            )
          )
        # )
        ,
        jqui_sortable(
          ui = div(id = "racePlots",
            column(width = 6,
              plotlyOutput(outputId = "race2017", width = "500px")
            ),
            column(width = 6,
              dygraphOutput(outputId = "raceByYear", width = "500px")
            )
          )
        ),
        # jqui_sortable(
        #   ui = 
            div(id = "interactionPlots",
            column(width = 6,
              plotlyOutput(outputId = "maleHeatmap", width = "500px")
            ),
            column(width = 6,
              plotlyOutput(outputId = "femaleHeatmap", width = "500px")
            )
          )
        # )
      ),
      tabItem(tabName = "statesMap",
        column(width = 8,
          leafletOutput(outputId = "statesMap"),
          sliderInput(
            inputId = "stateYear",
            label = "Year",
            min = 1999,
            max = 2017,
            value = 2017,
            sep = "",
            animate = animationOptions(interval = 500)
          )
        ),
        column(
          width = 4,
          plotOutput(outputId = "stateRatesLineChart"), br(),
          actionButton(inputId = "clearStates", label = "Clear States")
        )
      ),
      tabItem(tabName = "worldMap",
        column(width = 8,
          leafletOutput(outputId = "nationsMap"),
          selectInput(
            inputId = "nationYear",
            label = "Year",
            choices = c(2000,2005,2010,2015,2016),
            selected = 2016
          )
        ),
        column(width = 4,
          plotOutput(outputId = "nationRatesLineChart"), br(),
          actionButton(inputId = "clearNations", label = "Clear Nations")
        )
      ),
      tabItem(tabName = "summary",
      column(width = 6, offset = 3,
        br(),br(),br(),
        box(width = 12,
            h3("I. Female victims are reported to attempt suicide more often, although more male victims die from suicide.
            The interaction of firearms is very important here."), br(), br(),
            h3("II. Suicide deaths are highest for the 45-54 and 75+ age groups. However, for the 15-24 age group, suicide is the
            3rd leading cause of death."), br(), br(),
            h3("III. Suicide deaths are becoming a serious issue with Whites, especially those living in the Mountain states.")  
          )
      )
      )
    )
  )
)