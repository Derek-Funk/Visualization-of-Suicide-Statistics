server <- function(input, output, session) {
  clicked <- reactiveValues(
    states = vector(),
    nations = vector()
  )
  
  #high level stats
  output$statsOverall <- renderUI(expr = {
    box(title = "Overall", width = 12, height = "170px",
        if(input$overallProgress==1) {
          h4("Suicide is the 10th leading cause of death in the United States.")
        } else if(input$overallProgress==2) {
          h4("Approximately 50,000 lives are lost every year.")
        } else if(input$overallProgress==3) {
          h4("This is equivalent to 123 deaths every day...")
        } else if(input$overallProgress==4) {
          h4("...and 1 suicide death every 12 minutes.")
        } else if(input$overallProgress==5) {
          h4("In 2017, there were an estimated 1,400,000 suicide attempts in the nation...")
        } else if(input$overallProgress==6) {
          h4("...this means a death occurs for every 25 suicide attempts.")
        } else if(input$overallProgress==7) {
          h4("Globally, 800,000 victims die from suicide.")
        } else {
          h4("This is equivalent to a life being lost every 40 seconds.")
        }
    )
  })
  
  observeEvent(eventExpr = input$previousOverall, handlerExpr = {
    x <- finiteDecrementer(value = input[["overallProgress"]], limit = 8)
    updateSliderInput(session = session, inputId = "overallProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextOverall, handlerExpr = {
    x <- finiteIncrementer(value = input[["overallProgress"]], limit = 8)
    updateSliderInput(session = session, inputId = "overallProgress", value = x)
  })
  
  output$statsMethod <- renderUI(expr = {
    box(title = "Method of Attempt", width = 12, height = "170px",
        if(input$methodProgress==1) {
          h4("Firearms are the most common method of death from suicide, accounting
               for half of the cases.")
        } else {
          h4("The next most common methods of death from suicide are suffocation
               (including hangings) and poisoning.")
        }
      )
  })
  
  observeEvent(eventExpr = input$previousMethod, handlerExpr = {
    x <- finiteDecrementer(value = input[["methodProgress"]], limit = 2)
    updateSliderInput(session = session, inputId = "methodProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextMethod, handlerExpr = {
    x <- finiteIncrementer(value = input[["methodProgress"]], limit = 2)
    updateSliderInput(session = session, inputId = "methodProgress", value = x)
  })
  
  output$statsGender <- renderUI(expr = {
    box(title = "Gender", width = 12, height = "170px",
        if(input$genderProgress==1) {
          h4("80% of deaths from suicide are male victims...")
        } else if(input$genderProgress==2) {
          h4("in large part due to male victims selecting firearms as the method.")
        } else {
          h4("At the same time, females report to attempt suicide 3 times more than men.")
        }
    )
  })
  
  observeEvent(eventExpr = input$previousGender, handlerExpr = {
    x <- finiteDecrementer(value = input[["genderProgress"]], limit = 3)
    updateSliderInput(session = session, inputId = "genderProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextGender, handlerExpr = {
    x <- finiteIncrementer(value = input[["genderProgress"]], limit = 3)
    updateSliderInput(session = session, inputId = "genderProgress", value = x)
  })
  
  output$statsAge <- renderUI(expr = {
    box(title = "Age", width = 12, height = "230px",
        if(input$ageProgress==1) {
          h4("In the United States, suicide is the 3rd leading cause of death for 15- to 24-year olds.")
        } else if(input$ageProgress==2) {
          h4("For the same age group globally, it is the 2nd leading cause.")
        } else if(input$ageProgress==3) {
          h4("Suicide rates for females are highest in the age group 45-54.")
        } else if(input$ageProgress==4) {
          h4("Suicide rates for males are highest in the age group 75+.")
        } else {
          h4("Suicide rates are especially high among the elderly if they are divorced or widowed.")
        }
    )
  })
  
  observeEvent(eventExpr = input$previousAge, handlerExpr = {
    x <- finiteDecrementer(value = input[["ageProgress"]], limit = 5)
    updateSliderInput(session = session, inputId = "ageProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextAge, handlerExpr = {
    x <- finiteIncrementer(value = input[["ageProgress"]], limit = 5)
    updateSliderInput(session = session, inputId = "ageProgress", value = x)
  })
  
  output$statsRace <- renderUI(expr = {
    box(title = "Race", width = 12, height = "230px",
        if(input$raceProgress==1) {
          h4("Suicide is highest among Whites and Native Americans, at rates 3 times higher than other races.")
        } else if(input$raceProgress==2) {
          h4("The White and Native American suicide rates have been steadily increasing in the 21st century.")
        } else if(input$raceProgress==3) {
          h4("Suicide rates for other races have seen an uptick in the last few years.")
        } else {
          h4("In high school, suicide attempts are highest among Black students.")
        }
    )
  })
  
  observeEvent(eventExpr = input$previousRace, handlerExpr = {
    x <- finiteDecrementer(value = input[["raceProgress"]], limit = 4)
    updateSliderInput(session = session, inputId = "raceProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextRace, handlerExpr = {
    x <- finiteIncrementer(value = input[["raceProgress"]], limit = 4)
    updateSliderInput(session = session, inputId = "raceProgress", value = x)
  })
  
  output$statsOrientation <- renderUI(expr = {
    box(title = "Sexual Orientation", width = 12, height = "230px",
        if(input$orientationProgress==1) {
          h4("Lesbian, gay and bisexual adolescents attempt suicide 3 times more than others.")
        } else {
          h4("Lesbian, gay and bisexual adolescents who come from families that do not accept them are
               times more likely to attempt suicide than those whose families do accept them.")
        }
    )
  })
  
  observeEvent(eventExpr = input$previousOrientation, handlerExpr = {
    x <- finiteDecrementer(value = input[["orientationProgress"]], limit = 2)
    updateSliderInput(session = session, inputId = "orientationProgress", value = x)
  })
  
  observeEvent(eventExpr = input$nextOrientation, handlerExpr = {
    x <- finiteIncrementer(value = input[["orientationProgress"]], limit = 2)
    updateSliderInput(session = session, inputId = "orientationProgress", value = x)
  })
  
  #demographics
  observeEvent(eventExpr = input$demographic, handlerExpr = {
    if(input$demographic=="gender") {
      jqui_hide("#age2017", effect = "drop")
      jqui_hide("#ageByYear", effect = "drop")
      jqui_hide("#race2017", effect = "drop")
      jqui_hide("#raceByYear", effect = "drop")
      jqui_hide("#maleHeatmap", effect = "drop")
      jqui_hide("#femaleHeatmap", effect = "drop")
      delay(500, jqui_show("#gender2017", effect = "drop"))
      delay(500, jqui_show("#genderByYear", effect = "drop"))
    } else if(input$demographic=="age") {
      jqui_hide("#gender2017", effect = "drop")
      jqui_hide("#genderByYear", effect = "drop")
      jqui_hide("#race2017", effect = "drop")
      jqui_hide("#raceByYear", effect = "drop")
      jqui_hide("#maleHeatmap", effect = "drop")
      jqui_hide("#femaleHeatmap", effect = "drop")
      delay(500, jqui_show("#age2017", effect = "drop"))
      delay(500, jqui_show("#ageByYear", effect = "drop"))
    } else if(input$demographic=="race") {
      jqui_hide("#gender2017", effect = "drop")
      jqui_hide("#genderByYear", effect = "drop")
      jqui_hide("#age2017", effect = "drop")
      jqui_hide("#ageByYear", effect = "drop")
      jqui_hide("#maleHeatmap", effect = "drop")
      jqui_hide("#femaleHeatmap", effect = "drop")
      delay(500, jqui_show("#race2017", effect = "drop"))
      delay(500, jqui_show("#raceByYear", effect = "drop"))
    } else {
      jqui_hide("#gender2017", effect = "drop")
      jqui_hide("#genderByYear", effect = "drop")
      jqui_hide("#age2017", effect = "drop")
      jqui_hide("#ageByYear", effect = "drop")
      jqui_hide("#race2017", effect = "drop")
      jqui_hide("#raceByYear", effect = "drop")
      delay(500, jqui_show("#maleHeatmap", effect = "drop"))
      delay(500, jqui_show("#femaleHeatmap", effect = "drop"))
    }
  })
  
  output$gender2017 <- renderPlotly(expr = {
    plot_ly(usa2017, labels = ~Sex, values = ~Deaths, type = "pie") %>%
      layout(title = "2017 Suicide Deaths by Gender")
  })
  
  output$genderByYear <- renderDygraph(expr = {
    maleCrude <- usa %>% filter(Sex=="Males") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    femaleCrude <- usa %>% filter(Sex=="Females") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    sexCrude <- bind_cols(maleCrude, femaleCrude)[,-3]
    names(sexCrude)[2:3] <- c("Male", "Female")
    dygraph(data = sexCrude, main = "Suicide Deaths by Gender (1999-2017)", xlab = "Year",
            ylab = "Suicide Deaths (per 100,000 population)")
  })
  
  output$age2017 <- renderPlotly(expr = {
    dataFrame <- usa2017 %>% group_by(Age.Group2) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    dataFrame <- dataFrame[-1,]
    plot_ly(x = dataFrame$Age.Group2, y = dataFrame$Crude, type = "bar") %>%
      layout(
        title = "2017 Suicide Deaths by Age Group",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Suicide Deaths (per 100,000 population)")
      )
  })
  
  output$ageByYear <- renderDygraph(expr = {
    y1 <- usa %>% filter(Age.Group2=="15-24") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y2 <- usa %>% filter(Age.Group2=="25-34") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y3 <- usa %>% filter(Age.Group2=="35-44") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y4 <- usa %>% filter(Age.Group2=="45-54") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y5 <- usa %>% filter(Age.Group2=="55-64") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y6 <- usa %>% filter(Age.Group2=="65-74") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y7 <- usa %>% filter(Age.Group2=="75+") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    ageCrude <- bind_cols(y1, y2, y3, y4, y5, y6, y7)[,-c(3,5,7,9,11,13)]
    names(ageCrude)[2:8] <- c("15-24","25-34","35-44","45-54","55-64","65-74","75+")
    dygraph(data = ageCrude, main = "Suicide Deaths by Age Group (1999-2017)",
            xlab = "Year",
            ylab = "Suicide Deaths (per 100,000 population)") %>%
      dyLegend(labelsSeparateLines = TRUE, show = "follow") %>%
      dySeries("15-24", label = "Age 15-24") %>%
      dySeries("25-34", label = "Age 25-34") %>%
      dySeries("35-44", label = "Age 35-44") %>%
      dySeries("45-54", label = "Age 45-54") %>%
      dySeries("55-64", label = "Age 55-64") %>%
      dySeries("65-74", label = "Age 65-74") %>%
      dySeries("75+", label = "Age 75+")
  })
  
  output$race2017 <- renderPlotly(expr = {
    dataFrame <- usa2017 %>% group_by(Race) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    dataFrame <- data.frame(Race = dataFrame$Race, Crude = dataFrame$Crude, stringsAsFactors = FALSE)
    dataFrame$Race <- factor(dataFrame$Race, levels = unique(dataFrame$Race)[order(dataFrame$Crude, decreasing = TRUE)])
    plot_ly(x = dataFrame$Race, y = dataFrame$Crude, type = "bar") %>%
      layout(
        title = "2017 Suicide Deaths by Race",
        xaxis = list(title = "Race"),
        yaxis = list(title = "Suicide Deaths (per 100,000 population)")
      )
  })
  
  output$raceByYear <- renderDygraph(expr = {
    y1 <- usa %>% filter(Race=="White") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y2 <- usa %>% filter(Race=="Native American") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y3 <- usa %>% filter(Race=="Asian") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    y4 <- usa %>% filter(Race=="Black") %>% group_by(Year) %>%
      summarize(Crude = sum(Deaths) / sum(Population) * 100000)
    raceCrude <- bind_cols(y1, y2, y3, y4)[,-c(3,5,7)]
    names(raceCrude)[2:5] <- c("White", "Native American", "Asian", "Black")
    dygraph(data = raceCrude, main = "Suicide Deaths by Race (1999-2017)", xlab = "Year",
            ylab = "Suicide Deaths (per 100,000 population)") %>%
      dyLegend(width = "200px")
  })
  
  output$maleHeatmap <- renderPlotly(expr = {
    males <- usa %>% filter(Sex=="Males")
    males$Race <- factor(males$Race, levels = unique(males$Race)[c(1,3,4,2)])
    maleGroups <- males %>% group_by(Age.Group2, Race) %>%
      summarize(Deaths = sum(Deaths), Population = sum(Population)) %>%
      mutate(Crude = Deaths / Population * 100000) %>%
      arrange(desc(Age.Group2))
    plot_ly(x = unique(maleGroups$Race), y = unique(maleGroups$Age.Group2),
            z = matrix(maleGroups$Crude, ncol = 4, byrow = TRUE), type = "heatmap") %>%
      layout(
        title = "Male Deaths per 100,000 Population (1999-2017)",
        xaxis = list(title = "Race"),
        yaxis = list(title = "Age Group")
      )
  })
  
  output$femaleHeatmap <- renderPlotly(expr = {
    females <- usa %>% filter(Sex=="Females")
    females$Race <- factor(females$Race, levels = unique(females$Race)[c(1,3,4,2)])
    femaleGroups <- females %>% group_by(Age.Group2, Race) %>%
      summarize(Deaths = sum(Deaths), Population = sum(Population)) %>%
      mutate(Crude = Deaths / Population * 100000) %>%
      arrange(desc(Age.Group2))
    plot_ly(x = unique(femaleGroups$Race), y = unique(femaleGroups$Age.Group2),
            z = matrix(femaleGroups$Crude, ncol = 4, byrow = TRUE), type = "heatmap") %>%
      layout(
        title = "Female Deaths per 100,000 Population (1999-2017)",
        xaxis = list(title = "Race"),
        yaxis = list(title = "Age Group")
      )
  })
  
  #states explorer
  statesPalette <- reactive(
    x = colorBin(
      palette = "Blues",
      domain = statePolygons[[as.character(input$stateYear)]],
      bins = suicideRateBins
    )
  )
  
  stateLabels <- reactive(
    x = sprintf(
      "<strong>%s</strong><br/>%g",
      statePolygons$name, statePolygons[[as.character(input$stateYear)]]
    ) %>% lapply(htmltools::HTML)
  ) 
  
  statesMap <- reactive(
    leaflet(statePolygons) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(
        "MapBox",
        options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv(mapboxToken)
        )
      ) %>%
      addPolygons(
        fillColor = ~statesPalette()(statePolygons[[as.character(input$stateYear)]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = stateLabels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = statePolygons$name
        ,group = "click.list"
      ) %>%
      addLegend(
        pal = statesPalette(),
        values = ~input$stateYear,
        opacity = 0.7,
        title = HTML("Suicide Deaths<br>per 100,000<br>population"),
        position = "bottomright"
      ) %>%
      addControl(
        html = HTML("<b>Suicide Rates by State in ", input$stateYear, "</b>"),
        position = "topright"
      )
  )
  
  output$statesMap <- renderLeaflet(
    expr = {
      statesMap()
    }
  )
  
  observeEvent(
    eventExpr = input$statesMap_shape_click, 
    handlerExpr = {
      newClickedState <- input$statesMap_shape_click
      if(newClickedState$id %in% statesVector) {
        if(length(clicked$states)==5) {
          showNotification(ui = "Can only select up to 5 states at a time", duration = 3)
        } else {
          print(clicked$states)
          clicked$states <- c(clicked$states, newClickedState$id)  
          print(clicked$states)
          clickedStateLines <- statePolygons[which(statePolygons$name %in% clicked$states),]
          if(is.null(newClickedState$id)){
            req(newClickedState$id)
          } else if(!newClickedState$id %in% clickedStateLines@data$id ){
            leafletProxy(mapId = "statesMap") %>%
              addPolygons(
                data = clickedStateLines,
                layerId = clickedStateLines@data$id,
                color = "#f6be00"
              )
          }
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = input$clearStates, 
    handlerExpr = {
      output$statesMap <- renderLeaflet(
        expr = {
          clicked$states <- NULL
          statesMap()
        }
      )
    }
  )
  
  output$stateRatesLineChart <- renderPlot(
    expr = {
      if(length(clicked$states)>=1) {
        createStateRatesLineChart()
      } else {
        lineChartBlankMessage()
      }
    }
  )
  
  createStateRatesLineChart <- reactive(
    x = {
      filteredStates <- states %>% filter(State %in% clicked$states)
      ggplot(
        data = filteredStates,
        mapping=aes(x=Year, y=Crude.Rate, color=State)
      ) +
        geom_line() +
        geom_point() +
        ylab("Suicide Deaths (per 100,000 population)")
    }
  )
  
  lineChartBlankMessage <- reactive(
    ggplot() +
      ggtitle("Click on states")
  )
  
  #global explorer
  nationsPalette <- reactive(
    x = colorBin(
      palette = "Blues",
      domain = nationPolygons[[as.character(input$nationYear)]],
      bins = suicideRateBinsNations
    )
  )

  nationLabels <- reactive(
    x = sprintf(
      "<strong>%s</strong><br/>%g",
      nationPolygons$name, nationPolygons[[as.character(input$nationYear)]]
    ) %>% lapply(htmltools::HTML)
  )

  nationsMap <- reactive(
    leaflet(nationPolygons) %>%
      setView(40, 37.8, 1) %>%
      addProviderTiles(
        "MapBox",
        options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv(mapboxToken)
        )
      ) %>%
      addPolygons(
        fillColor = ~nationsPalette()(nationPolygons[[as.character(input$nationYear)]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = nationLabels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = nationPolygons$name,
        group = "click.list"
      ) %>%
      addLegend(
        pal = nationsPalette(),
        values = ~input$nationYear,
        opacity = 0.7,
        title = HTML("Suicide Deaths<br>per 100,000<br>population"),
        position = "bottomright"
      ) %>%
      addControl(
        html = HTML("<b>Suicide Rates by Nation in ", input$nationYear, "</b>"),
        position = "topright"
      )
  )

  output$nationsMap <- renderLeaflet(
    expr = {
      nationsMap()
    }
  )

  observeEvent(
    eventExpr = input$nationsMap_shape_click,
    handlerExpr = {
      newClickedNation <- input$nationsMap_shape_click
      if(newClickedNation$id %in% nationsVector) {
        if(length(clicked$nations)==5) {
          showNotification(ui = "Can only select up to 5 nations at a time", duration = 3)
        } else {
          print(newClickedNation$id)
          clicked$nations <- c(clicked$nations, newClickedNation$id)
          clickedNationLines <- nationPolygons[which(nationPolygons$name %in% clicked$nations),]
          if(is.null(newClickedNation$id)){
            req(newClickedNation$id)
          } else if(!newClickedNation$id %in% clickedNationLines@data$id ){
            leafletProxy(mapId = "nationsMap") %>%
              addPolygons(
                data = clickedNationLines,
                layerId = clickedNationLines@data$id,
                color = "#f6be00"
              )
          }
        }
      }
    }
  )

  observeEvent(
    eventExpr = input$clearNations,
    handlerExpr = {
      output$nationsMap <- renderLeaflet(
        expr = {
          clicked$nations <- NULL
          nationsMap()
        }
      )
    }
  )

  output$nationRatesLineChart <- renderPlot(
    expr = {
      if(length(clicked$nations)>=1) {
        createNationRatesLineChart()
      } else {
        lineChartBlankMessageNations()
      }
    }
  )

  createNationRatesLineChart <- reactive(
    x = {
      filteredNations <- nations %>% filter(Country %in% clicked$nations)
      ggplot(
        data = filteredNations,
        mapping=aes(x=Year, y=Crude.Rate, color=Country)
      ) +
        geom_line() +
        geom_point() +
        ylab("Suicide Deaths (per 100,000 population)")
    }
  )

  lineChartBlankMessageNations <- reactive(
    ggplot() +
      ggtitle("Click on nations")
  )
}
