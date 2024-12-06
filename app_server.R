# Server section
app_server <- function(input, output, session) {
  MART.Dash_original <<- MART.Dash # backup original data
  
  # Observe the button click to show the modal dialog
  observeEvent(input$showDescription, {
    showModal(modalDialog(
      title = "Application Description",
      "This app allows for analysis of budget plan data. A debt client is a client who has went through the budget plan process and as such we have collected demographic, income, expenditure, asset, and debt data from them. This has allowed allowed us to create many more variables on top. One example of this is Surplus, which is what is left over at the end of the month once essential expenditure has been taken away from income. When Surplus is less than 0 that client is in a negative budget. The app makes calculations using the full dataset. However, if you wish to filter to e.g., only private renters, or those with energy debts greater than 10,000, do so here in the front page, making sure to click confirm when you have made your selection. To revert back to the whole dataset, simply click Reset.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$variablePicker <- renderUI({
    # Filter out the columns "BP.ID" and "Client.Ref"
    valid_choices <- names(MART.Dash)[sapply(MART.Dash, is.factor) & !(names(MART.Dash) %in% c("BP_ID", "Client_Ref"))]
    
    selectInput("group_var", "Choose a grouping variable:", choices = valid_choices)
  })
  
  output$levelPicker <- renderUI({
    req(input$group_var)
    pickerInput("group_levels", "Select levels:", choices = levels(MART.Dash[, input$group_var]), multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  
  output$numericVariablePicker <- renderUI({
    selectInput("numeric_var", "Choose a numeric variable:", choices = c("No filter needed", names(MART.Dash[sapply(MART.Dash, is.numeric)])))
  })
  
  output$numericFilterTypePicker <- renderUI({
    selectInput("numeric_filter_type", "Choose a filter type:", choices = c("Greater than", "Less than", "Equal to", "Between"))
  })
  
  output$numericValuePicker <- renderUI({
    if (input$numeric_var != "No filter needed") {
      req(input$numeric_var)
      req(input$numeric_filter_type)

      if (input$numeric_filter_type == "Between") {
        fluidRow(
          column(6, numericInput("numeric_lower", "Set lower bound:",
                                 value = min(MART.Dash[, input$numeric_var], na.rm = TRUE)
          )),
          column(6, numericInput("numeric_upper", "Set upper bound:",
                                 value = max(MART.Dash[, input$numeric_var], na.rm = TRUE)
          ))
        )
      } else {
        numericInput("numeric_value", "Set value:", value = mean(MART.Dash[, input$numeric_var], na.rm = TRUE))
      }
    }
  })

########################################  
  
  
  observeEvent(input$confirm, {
    if (!is.null(input$group_var) && !is.null(input$group_levels)) {
      MART.Dash <<- MART.Dash %>% filter(get(input$group_var) %in% input$group_levels)
      if (nrow(MART.Dash) == 0) {
        output$filterStatus <- renderText("Warning: No data matches the filter criteria for the selected group.")
      } else {
        output$filterStatus <- renderText("By jove you've only went and set a filter!")
      }
    }
    
    if (input$numeric_var != "No filter needed") {
      if (input$numeric_filter_type == "Greater than") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) > input$numeric_value)
        if (nrow(MART.Dash) == 0) {
          output$filterStatus <- renderText("Warning: No data matches the 'Greater than' filter criteria.")
        } else {
          output$filterStatus <- renderText("By jove you've only went and set a filter!")
        }
      } else if (input$numeric_filter_type == "Less than") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) < input$numeric_value)
        if (nrow(MART.Dash) == 0) {
          output$filterStatus <- renderText("Warning: No data matches the 'Less than' filter criteria.")
        } else {
          output$filterStatus <- renderText("By jove you've only went and set a filter!")
        }
      } else if (input$numeric_filter_type == "Equal to") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) == input$numeric_value)
        if (nrow(MART.Dash) == 0) {
          output$filterStatus <- renderText("Warning: No data matches the 'Equal to' filter criteria.")
        } else {
          output$filterStatus <- renderText("By jove you've only went and set a filter!")
        }
      } else if (input$numeric_filter_type == "Between") {
        # Use the correct input IDs for numeric lower and upper bounds
        req(input$numeric_lower, input$numeric_upper)
        MART.Dash <<- MART.Dash %>% filter(
          get(input$numeric_var) >= input$numeric_lower &
            get(input$numeric_var) <= input$numeric_upper
        )
        if (nrow(MART.Dash) == 0) {
          output$filterStatus <- renderText("Warning: No data matches the 'Between' filter criteria.")
        } else {
          output$filterStatus <- renderText("By jove you've only went and set a filter!")
        }
      }
    }
  })
  
  observeEvent(input$reset, {
    MART.Dash <<- MART.Dash_original # Restore original data
    output$filterStatus <- renderText("OK, phew, back to normal!")
  })
  
  #########################################
  
  # Make tab 1 table
  table_data <- reactive({
    # MART.Counts(input$Variable1, input$Variable2)
    # MART.Dash <- readRDS("MART.Dash.rds")
    Time <- sym(input$Variable1)
    Variable <- sym(input$Variable2)
    
    data <- table(MART.Dash[[Time]], MART.Dash[[Variable]])
    data2 <- as.data.frame(data)
    
    data2 <- data2 %>%
      rename(!!input$Variable1 := Var1, !!input$Variable2 := Var2)
    
    data2
  })
  
  
  output$table1 <- DT::renderDataTable({
    dt <- DT::datatable(table_data(), options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting
    dt <- dt %>%
      formatStyle(
        "Freq", # name of the column
        backgroundColor = styleInterval(
          c(100, 300),
          c("lightcoral", "lightgoldenrodyellow", "lightgreen")
        )
      )
    
    dt
  })
  
  
  # Download tab 1 data
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Variable2, ".csv")
    },
    content = function(file) {
      write.csv(table_data(), file)
    }
  )
  
  # Make tab 2 data
  tab2_data <- reactive({
    # Proportion.Calculator(input$Tab2Variable1, input$Tab2Variable2)
    # MART.Dash <- readRDS("MART.Dash.rds")
    Time <- sym(input$Tab2Variable1)
    Variable <- sym(input$Tab2Variable2)
    Data <- MART.Dash %>%
      #filter(!is.na({{ Variable }})) %>% 
      count({{ Time }}, {{ Variable }}) %>%
      spread({{ Variable }}, n) %>%
      select(-1) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      setNames(paste0(names(.), ".Percent")) %>%
      cbind(data.frame(MART.Dash %>%
                         count({{ Time }}, {{ Variable }}) %>%
                         spread({{ Variable }}, n))) %>%
      relocate(where(is.factor), .before = where(is.numeric)) %>%
      mutate_if(is.numeric, round, 2)
    
    Data <- Data %>%
      select({{ Time }}, everything())
    
    Data
  })
  
  output$table2 <- DT::renderDataTable({
    dt <- DT::datatable(tab2_data(), options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Get column names
    colnames <- colnames(tab2_data())
    
    # Filter out the 'Year' column and any column containing 'Percent'
    target_columns <- colnames[!colnames %in% "Year" & !grepl("Percent", colnames)]
    
    # Apply conditional formatting to the target columns
    for (col in target_columns) {
      dt <- dt %>%
        formatStyle(
          col,
          backgroundColor = styleInterval(
            c(100, 300),
            c("lightcoral", "lightgoldenrodyellow", "lightgreen")
          )
        )
    }
    
    dt
  })
  
  
  # Download tab 2 data
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(input$Tab2Variable2, ".csv")
    },
    content = function(file) {
      write.csv(tab2_data(), file)
    }
  )
  
  # Make tab 3 data
  # tab3_data <- reactive({
  #   # Proportion.Calculator.2(input$Tab3Variable1, input$Tab3Variable2, input$Tab3Variable3)
  #   # MART.Dash <- readRDS("MART.Dash.rds")
  #   Time <- sym(input$Tab3Variable1)
  #   Var1 <- sym(input$Tab3Variable2)
  #   Var2 <- sym(input$Tab3Variable3)
  #   Data <- MART.Dash %>%
  #     #mutate(across(c({{Var1}}, {{Var2}}), ~replace_na(.x, "Missing"))) %>%
  #     count({{ Time }}, {{ Var1 }}, {{ Var2 }}) %>%
  #     spread({{ Var2 }}, n) %>%
  #     select(-1, -2) %>%
  #     data.matrix() %>%
  #     prop.table(margin = 1) %>%
  #     data.frame() %>%
  #     mutate_if(is.numeric, ~ . * 100) %>%
  #     setNames(paste0(names(.), ".Percent")) %>%
  #     cbind(data.frame(MART.Dash %>%
  #                        count({{ Time }}, {{ Var1 }}, {{ Var2 }}) %>%
  #                        spread({{ Var2 }}, n))) %>%
  #     relocate(where(is.factor), .before = where(is.numeric)) %>%
  #     mutate_if(is.numeric, round, 2)
  #   
  #   Data <- Data %>%
  #     select({{ Time }}, everything())
  #   
  #   Data
  # })
  
  # # return tab 3 data
  # output$table3 <- DT::renderDataTable({
  #
  #   DT::datatable(tab3_data(),options = list(scrollY = "200px"),
  #                 class = "cell-border")
  # })
  
  tab3_data <- reactive({
    Time <- sym(input$Tab3Variable1)
    Var1 <- sym(input$Tab3Variable2)
    Var2 <- sym(input$Tab3Variable3)
    
    # Get unique levels for Var2 dynamically
    unique_levels_var2 <- unique(MART.Dash[[as_string(Var2)]])
    
    Data <- MART.Dash %>%
      # Group and count occurrences of each combination of Time, Var1, and Var2
      count(!!Time, !!Var1, !!Var2) %>%
      # Ensure all combinations of Time, Var1, and Var2 are represented, filling missing counts with 0
      complete(!!Time, !!Var1, !!Var2 := unique_levels_var2, fill = list(n = 0)) %>%
      # Spread Var2 categories into separate columns
      spread(!!Var2, n) %>%
      # Convert to matrix, calculate proportions row-wise, and then back to a data frame
      select(-1, -2) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      # Multiply proportions by 100 to get percentages
      mutate_if(is.numeric, ~ . * 100) %>%
      # Set column names with ".Percent" suffix for percentages
      setNames(paste0(names(.), ".Percent")) %>%
      # Re-bind original count data and position factor columns before numeric ones
      cbind(data.frame(MART.Dash %>%
                         count(!!Time, !!Var1, !!Var2) %>%
                         complete(!!Time, !!Var1, !!Var2 := unique_levels_var2, fill = list(n = 0)) %>%
                         spread(!!Var2, n))) %>%
      relocate(where(is.factor), .before = where(is.numeric)) %>%
      # Round numeric columns to 2 decimal places
      mutate_if(is.numeric, round, 2)
    
    # Ensure the Time column is in the first position
    Data <- Data %>%
      select(!!Time, everything())
    
    Data
  })
  
  
  output$table3 <- DT::renderDataTable({
    dt <- DT::datatable(tab3_data(), options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Get column names
    colnames <- colnames(tab3_data())
    
    # Filter out the 'Year' column and any column containing 'Percent'
    target_columns <- colnames[!colnames %in% "Year" & !grepl("Percent", colnames)]
    
    # Apply conditional formatting to the target columns
    for (col in target_columns) {
      dt <- dt %>%
        formatStyle(
          col,
          backgroundColor = styleInterval(
            c(100, 300),
            c("lightcoral", "lightgoldenrodyellow", "lightgreen")
          )
        )
    }
    
    dt
  })
  
  
  # Download tab 3 data
  output$download3 <- downloadHandler(
    filename = function() {
      paste0(input$Tab3Variable2, ".csv")
    },
    content = function(file) {
      write.csv(tab3_data(), file)
    }
  )
  
  # Make tab 4 data
  tab4_data <- reactive({
    # Summary.Stats(input$Tab4Variable1, input$Tab4Variable2)
    # MART.Dash <- readRDS("MART.Dash.rds")
    Time <- sym(input$Tab4Variable1)
    Variable <- sym(input$Tab4Variable2)
    
    if (Variable == "Surplus") {
      Data <- MART.Dash %>%
        group_by({{ Time }}) %>%
        summarise(
          Mean = mean({{ Variable }}, na.rm = TRUE), Median = median({{ Variable }}),
          Trimmed.Mean.20 = mean({{ Variable }}, trim = 0.2, na.rm = TRUE), n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      Data
    } else {
      Data <- MART.Dash %>%
        filter({{ Variable }} != 0) %>%
        group_by({{ Time }}) %>%
        summarise(
          Mean = mean({{ Variable }}, na.rm = TRUE), Median = median({{ Variable }}),
          Trimmed.Mean.20 = mean({{ Variable }}, trim = 0.2, na.rm = TRUE), n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      Data
    }
  })
  
  # Return tab 4 data
  output$table4 <- DT::renderDataTable({
    dt <- DT::datatable(tab4_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the 'n()' column
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  # Download tab 4 data
  output$download4 <- downloadHandler(
    filename = function() {
      paste0(input$Tab4Variable2, ".csv")
    },
    content = function(file) {
      write.csv(tab4_data(), file)
    }
  )
  
  
  # Make tab 5 data
  tab5_data <- reactive({
    # Summary.Stats.v2(input$Tab5Variable1, input$Tab5Variable2, input$Tab5Variable3)
    # MART.Dash <- readRDS("MART.Dash.rds")
    Time <- sym(input$Tab5Variable1)
    Var.Group <- sym(input$Tab5Variable2)
    Var.Number <- sym(input$Tab5Variable3)
    
    if (Var.Number == "Surplus") {
      Data <- MART.Dash %>%
        group_by({{ Time }}, {{ Var.Group }}) %>%
        summarise(
          Mean = mean({{ Var.Number }}, na.rm = TRUE), Median = median({{ Var.Number }}),
          Trimmed.Mean.20 = mean({{ Var.Number }}, trim = 0.2, na.rm = TRUE), n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      Data
    } else {
      Data <- MART.Dash %>%
        filter({{ Var.Number }} != 0) %>%
        group_by({{ Time }}, {{ Var.Group }}) %>%
        summarise(
          Mean = mean({{ Var.Number }}, na.rm = TRUE), Median = median({{ Var.Number }}),
          Trimmed.Mean.20 = mean({{ Var.Number }}, trim = 0.2, na.rm = TRUE), n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      Data
    }
  })
  
  # Return tab 5 data
  output$table5 <- DT::renderDataTable({
    dt <- DT::datatable(tab5_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the columns
    # Replace 'ColumnName' with the name of the column you want to format
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Download tab 5 data
  output$download5 <- downloadHandler(
    filename = function() {
      paste0(input$Tab5Variable3, ".csv")
    },
    content = function(file) {
      write.csv(tab5_data(), file)
    }
  )
  
  # Make tab 6 data
  tab6_data <- reactive({
    # Month.Comparison(input$Tab6Variable1, input$Tab6Variable2, input$Tab6Variable3,input$Tab6Variable4)
    # MART.Dash <- readRDS("MART.Dash.rds")
    
    Month1 <- as.character(input$Tab6Variable1)
    Variable <- sym(input$Tab6Variable4)
    Year1 <- as.character(input$Tab6Variable2)
    Year2 <- as.character(input$Tab6Variable3)
    
    Data <- MART.Dash %>%
      group_by(Month, Year) %>%
      filter({{ Variable }} > 0) %>%
      summarise(Mean = mean({{ Variable }}), Median = median({{ Variable }}), Trimmed.Mean = mean({{ Variable }}, trim = 0.2)) %>%
      filter(Year == {{ Year1 }} | Year == {{ Year2 }}) %>%
      filter(Month == {{ Month1 }}) %>%
      arrange(desc(Year))
    
    Data.2 <- data.frame(
      "Month" = "%", "Year" = "Change", "Mean" = ((Data$Mean[1] - Data$Mean[2]) / Data$Mean[2] * 100),
      "Median" = ((Data$Median[1] - Data$Median[2]) / Data$Median[2] * 100),
      "Trimmed.Mean" = ((Data$Trimmed.Mean[1] - Data$Trimmed.Mean[2]) / Data$Trimmed.Mean[2] * 100)
    )
    
    Data.Combined <- rbind(Data, Data.2)
    Data.Combined <- as_tibble(Data.Combined) %>% mutate_if(is.numeric, round, 2)
    
    Data.Combined
  })
  
  # return tab 6 data
  output$table6 <- DT::renderDataTable({
    DT::datatable(tab6_data(),
                  options = list(scrollY = "200px"), 
                  class = "hover row-border stripe",
                  filter = "bottom")
  })
  
  # Download tab 6 data
  output$download6 <- downloadHandler(
    filename = function() {
      paste0(input$Tab6Variable4, ".csv")
    },
    content = function(file) {
      write.csv(tab6_data(), file)
    }
  )
  
  # Make tab 7 data
  tab7_data <- reactive({
    # Projection.Expenditure.MEGA(input$number7a, input$number7b, input$number7c, input$number7d,
    #                               input$number7e, input$number7f, input$number7g, input$number7h)
    
    # MART.Dash <- readRDS("MART.Dash.rds")
    
    Water <- input$number7a
    Communications <- input$number7b
    Health <- input$number7c
    Insurance <- input$number7d
    Transport <- input$number7e
    Food <- input$number7f
    Energy <- input$number7g
    Childcare <- input$number7h
    
    if (Childcare > 0) {
      dat <- MART.Dash %>%
        dplyr::filter(Expenditure.Childcare > 0)
    } else {
      dat <- MART.Dash
    }
    Prop1 <- (100 - Water) / 100
    Prop2 <- (100 - Communications) / 100
    Prop3 <- (100 - Health) / 100
    Prop4 <- (100 - Insurance) / 100
    Prop5 <- (100 - Transport) / 100
    Prop6 <- (100 - Food) / 100
    Prop7 <- (100 - Energy) / 100
    Prop8 <- (100 - Childcare) / 100
    dat <- dat %>%
      dplyr::mutate(
        New.Water = Expenditure.Water * (Prop1),
        New.Communications = Expenditure.Communications.Total * (Prop2),
        New.Health = Expenditure.Care.and.Health.Total * (Prop3),
        New.Insurance = Expenditure.Insurance.Total * (Prop4),
        New.Transport = Expenditure.Transport.Total * (Prop5),
        New.Food = Expenditure.Groceries * (Prop6),
        New.Energy = Expenditure.Utilities.Total * (Prop7),
        New.Childcare = Expenditure.Childcare * (Prop8),
        New.Expenditure.Total = Expenditure.Total - ((Expenditure.Water - New.Water) + (Expenditure.Communications.Total - New.Communications) +
                                                       (Expenditure.Care.and.Health.Total - New.Health) + (Expenditure.Insurance.Total - New.Insurance) +
                                                       (Expenditure.Transport.Total - New.Transport) + (Expenditure.Groceries - New.Food) +
                                                       (Expenditure.Utilities.Total - New.Energy) + (Expenditure.Childcare - New.Childcare)),
        New.Surplus = Income.Total - New.Expenditure.Total,
        New.Negative.Budget = case_when(
          New.Surplus <= 0 ~ "Negative.Budget",
          TRUE ~ "Positive.Budget"
        ),
        Budget.Change = case_when(
          Demographic.Negbud == "Negative.Budget" &
            New.Negative.Budget == "Positive.Budget" ~ "Changed",
          TRUE ~ "Unchanged"
        )
      )
    
    dat.subset <- subset(dat, Quarter == levels(dat$Quarter)[length(levels(dat$Quarter))])
    
    Prop.Table <- prop.table(table(dat.subset$New.Negative.Budget))
    Prop.Table <- as.data.frame(Prop.Table) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "Negative.Budget")
    
    old.dat <- prop.table(table(dat.subset$Demographic.Negbud))
    old.dat <- as.data.frame(old.dat) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "neg")
    
    New.Surplus <- dat %>%
      dplyr::filter(Expenditure.Water > 0 & Expenditure.Communications.Total > 0 &
                      Expenditure.Care.and.Health.Total > 0 & Expenditure.Insurance.Total > 0) %>%
      group_by(Quarter) %>%
      summarise(Median = median(New.Surplus)) %>%
      filter(Quarter == levels(dat$Quarter)[length(levels(dat$Quarter))])
    
    Old.Surplus <- MART.Dash %>%
      dplyr::filter(Expenditure.Water > 0 & Expenditure.Communications.Total > 0 &
                      Expenditure.Care.and.Health.Total > 0 & Expenditure.Insurance.Total > 0) %>%
      group_by(Quarter) %>%
      summarise(Median = median(Surplus)) %>%
      filter(Quarter == levels(dat$Quarter)[length(levels(dat$Quarter))])
    
    Final.Output <- rbind(Prop.Table, old.dat) %>%
      mutate(Most.Recent.Quarter = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
      mutate(Surplus = c(New.Surplus$Median[1], Old.Surplus$Median[1])) %>%
      mutate(Sample = nrow(dat.subset)) %>%
      mutate(
        Total.Negative =
          case_when(
            Most.Recent.Quarter == "New.Negative.Budget" ~
              (sum(dat$Quarter == levels(dat$Quarter)[length(levels(dat$Quarter))]) * (Prop.Table$Percentage / 100)),
            TRUE ~ (sum(dat$Quarter == levels(dat$Quarter)[length(levels(dat$Quarter))]) * (old.dat$Percentage / 100))
          )
      ) %>%
      mutate(Total.Negative = round(Total.Negative)) %>%
      mutate(Total.Saved = c((Total.Negative[2] - Total.Negative[1]), NA_real_)) %>%
      mutate(Cost.Quarter = case_when(
        Most.Recent.Quarter == "New.Negative.Budget" ~ (sum(dat.subset$Expenditure.Total)) - (sum(dat.subset$New.Expenditure.Total)),
        TRUE ~ NA_real_
      )) %>%
      mutate(Parameters = paste("W", Water, "C", Communications, "H", Health, "I", Insurance,
                                "T", Transport, "F", Food,
                                "E", Energy, "C", Childcare,
                                sep = "."
      )) %>%
      mutate(Cost.Person.Quarter = Cost.Quarter / Total.Saved) %>%
      mutate_if(is.numeric, round, 2) %>%
      select(Parameters, everything())
    
    Final.Output
  })
  
  # return tab 7 data
  output$table7 <- DT::renderDataTable({
    DT::datatable(tab7_data(),
                  options = list(scrollY = "200px"), 
                  class = "hover row-border stripe",
                  filter = "bottom")
  })
  
  # Download tab 7 data
  output$download7 <- downloadHandler(
    filename = function() {
      paste0("Tariff", ".csv")
    },
    content = function(file) {
      write.csv(tab7_data(), file)
    }
  )
  
  # Make tab 8 data
  tab8_data <- reactive({
    # Projection.Expenditure.2(input$number8a, input$number8b,input$Tab8Variable1,input$Tab8Variable2)
    # MART.Dash <- readRDS("MART_dash.rds")
    
    Per1 <- input$number8a
    Per2 <- input$number8b
    
    Exp1 <- sym(input$Tab8Variable1)
    Exp2 <- sym(input$Tab8Variable2)
    dat <- MART.Dash
    Prop1 <- (100 - Per1) / 100
    Prop2 <- (100 - Per2) / 100
    dat <- dat %>%
      dplyr::filter({{ Exp1 }} > 0 & {{ Exp2 }} > 0) %>%
      dplyr::mutate(
        New.Exp1 = {{ Exp1 }} * (Prop1),
        New.Exp2 = {{ Exp2 }} * (Prop2),
        New.Expenditure.Total = Expenditure.Total - (({{ Exp1 }} - New.Exp1) + ({{ Exp2 }} - New.Exp2)),
        New.Surplus = Income.Total - New.Expenditure.Total,
        New.Negative.Budget = case_when(
          New.Surplus <= 0 ~ "Negative.Budget",
          TRUE ~ "Positive.Budget"
        )
      )
    
    dat.subset <- subset(dat, Quarter == levels(Quarter)[length(levels(Quarter))])
    
    Prop.Table <- prop.table(table(dat.subset$New.Negative.Budget))
    Prop.Table <- as.data.frame(Prop.Table) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "Negative.Budget")
    
    old.dat <- prop.table(table(dat.subset$Demographic.Negbud))
    old.dat <- as.data.frame(old.dat) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "neg")
    
    Final.Output <- rbind(Prop.Table, old.dat) %>%
      mutate(Most.Recent.Quarter = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
      mutate_if(is.numeric, round, 2)
    
    Final.Output
  })
  
  # return tab 8 data
  output$table8 <- DT::renderDataTable({
    DT::datatable(tab8_data(),
                  options = list(scrollY = "200px"), 
                  class = "hover row-border stripe",
                  filter = "bottom")
  })
  
  # Download tab 8 data
  output$download8 <- downloadHandler(
    filename = function() {
      paste0(input$Tab8Variable1, ".csv")
    },
    content = function(file) {
      write.csv(tab8_data(), file)
    }
  )
  
  # Make tab 9 data
  tab9_data <- reactive({
    # Projection.Income(input$number9, input$Tab9Variable1)
    # MART.Dash <- readRDS("MART_dash.rds")
    
    Percentage <- input$number9
    Income <- sym(input$Tab9Variable1)
    dat <- MART.Dash
    Proportion <- (100 + Percentage) / 100
    dat <- dat %>%
      dplyr::filter({{ Income }} > 0) %>%
      dplyr::mutate(
        New.Income = {{ Income }} * (Proportion),
        New.Income.Total = Income.Total + (New.Income - {{ Income }}),
        New.Surplus = New.Income.Total - Expenditure.Total,
        New.Negative.Budget = case_when(
          New.Surplus <= 0 ~ "Negative.Budget",
          TRUE ~ "Positive.Budget"
        )
      )
    
    dat.subset <- subset(dat, Quarter == levels(Quarter)[length(levels(Quarter))])
    
    Prop.Table <- prop.table(table(dat.subset$New.Negative.Budget))
    Prop.Table <- as.data.frame(Prop.Table) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "Negative.Budget")
    
    old.dat <- prop.table(table(dat.subset$Demographic.Negbud))
    old.dat <- as.data.frame(old.dat) %>%
      rename("Most.Recent.Quarter" = "Var1") %>%
      mutate(Percentage = Freq * 100) %>%
      select(-Freq) %>%
      filter(Most.Recent.Quarter == "neg")
    
    Final.Output <- rbind(Prop.Table, old.dat) %>%
      mutate(Most.Recent.Quarter = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
      mutate_if(is.numeric, round, 2)
    
    Final.Output
  })
  
  # return tab 9 data
  output$table9 <- DT::renderDataTable({
    DT::datatable(tab9_data(),
                  options = list(scrollY = "200px"), 
                  class = "hover row-border stripe",
                  filter = "bottom")
  })
  
  # Download tab 9 data
  output$download9 <- downloadHandler(
    filename = function() {
      paste0(input$Tab9Variable1, ".csv")
    },
    content = function(file) {
      write.csv(tab9_data(), file)
    }
  )
  
  ##########################################################
  # tab 20 data
  table20_data <- reactive({
    # x-axis label
    Var1 <- sym(input$Tab20Variable1)
    
    # Time variable
    Time <- sym(input$TimeVariableNeg)
    
    # data for plot
    Data <- MART.Dash %>%
      count({{ Time }}, {{ Var1 }}, Demographic.Negbud) %>%
      spread(Demographic.Negbud, n) %>%
      select(-1, -2) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      setNames(paste0(names(.), ".Percent")) %>%
      cbind(data.frame(MART.Dash %>%
                         count({{ Time }}, {{ Var1 }}, Demographic.Negbud) %>%
                         spread(Demographic.Negbud, n))) %>%
      relocate(where(is.factor), .before = where(is.numeric)) %>%
      mutate_if(is.numeric, round, 2)
    
    Data <- Data %>%
      select({{ Time }}, everything()) %>% 
      mutate(n = neg + pos)
    
    Data
  })
  
  
  # tab 20 plot input
  table20_plot <- reactive({
    # Extended colour palette
    Cab.Colours <- c(
      "#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba",
      "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#d9bf77", "#66b2b2",
      "#8c4356", "#f7ea39", "#394240", "#f26c4f", "#3b3e42", "#d27d67", "#6b9362",
      "#f9cb9c", "#1e5090", "#e58d7d", "#2f615e", "#c49bcd", "#e2cf56",
      "#6f83a5", "#cf6a88", "#55805b", "#e9a1cf", "#768d64"
    )
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab20Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariableNeg)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table20_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    plot <- ggplot(table20_data(), aes(x = {{ TimeVar }}, y = neg.Percent, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(size = 0.75) +
      # geom_point() +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1)
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    plot
  })
  
  # tab 20 plot output
  output$plot20 <- renderPlotly({
    # Extended colour palette
    Cab.Colours <- c(
      "#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba",
      "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#d9bf77", "#66b2b2",
      "#8c4356", "#f7ea39", "#394240", "#f26c4f", "#3b3e42", "#d27d67", "#6b9362",
      "#f9cb9c", "#1e5090", "#e58d7d", "#2f615e", "#c49bcd", "#e2cf56",
      "#6f83a5", "#cf6a88", "#55805b", "#e9a1cf", "#768d64"
    )
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab20Variable1)
    
    TimeVar <- sym(input$TimeVariableNeg)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table20_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    p <- ggplot(table20_data(), aes(x = {{ TimeVar }}, y = neg.Percent, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(size = 1) +
      #geom_point(size = 3) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_blank(), # This line removes the legend background
        legend.key = element_blank(), # This line removes the box around the legend
        legend.text = element_text(colour = "#004b88", size = 8)
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1) +
      labs(colour = NULL) # This line removes the legend title
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  # tab 20 data output
  output$table20 <- DT::renderDataTable({
    dt <- DT::datatable(table20_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n"
    dt <- dt %>% formatStyle(
      "n",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  # download tab 20 data
  output$download20 <- downloadHandler(
    filename = function() {
      paste0(input$Tab20Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table20_data(), file)
    }
  )
  # download tab 20 plot
  output$download20b <- downloadHandler(
    filename = function() {
      paste0(input$Tab20Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table20_plot(), device = device, height = 5.5)
    }
  )
  #########################################################
  
  # data for plot 10
  table10_data <- reactive({
    # x-axis label
    Var.Number <- sym(input$Tab10Variable1)
    
    # Time Variable
    TimeVar <- sym(input$TimeVariable)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    # data for plot
    dat <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    # create similar table using MART.Dash_original dataframe
    dat2 <- MART.Dash_original %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    # Check if the number of rows for MART.Dash is the same as MART.Dash_original
    if (nrow(MART.Dash) != nrow(MART.Dash_original)) {
      dat <- dat %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": Filtered"))
      dat2 <- dat2 %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": All"))
      dat <- bind_rows(dat, dat2)
      dat <- dat %>%
        select(Category, everything())
    }
    
    dat
  })
  
  
  # plot 10
  table10_plot <- reactive({
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "005743", "#c2bdde", "#9a1d4e", "#d4e5ef")
    
    # Time variable
    TimeVar <- sym(input$TimeVariable)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table10_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    # If row counts of the dataframes differ, make a grouped line chart
    if (nrow(MART.Dash) != nrow(MART.Dash_original)) {
      plot <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean, group = Category, color = Category)) +
        geom_line(size = 0.75) +
        #geom_point() +
        scale_colour_manual(values = Cab.Colours[1:2])
    }
    # If row counts of the dataframes are the same, make the original line chart
    else {
      plot <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean)) +
        geom_line(group = 1, size = 0.75, colour = "#004b88") +
        #geom_point() +
        scale_colour_manual(values = Cab.Colours)
    }
    
    # Common plot aesthetics
    plot <- plot + theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      ylab(input$Tab10Variable1)
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    
    plot
  })
  
  
  
  # Return tab 10 data
  output$table10 <- DT::renderDataTable({
    dt <- DT::datatable(table10_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Return plot10 as Plotly
  output$plot10 <- renderPlotly({
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "005743", "#c2bdde", "#9a1d4e", "#d4e5ef")
    
    # Time variable
    TimeVar <- sym(input$TimeVariable)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table10_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    # If row counts of the dataframes differ, make a grouped line chart
    if (nrow(MART.Dash) != nrow(MART.Dash_original)) {
      p <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean, group = Category, color = Category)) +
        geom_line(size = 1) +
        #geom_point(size = 3) +
        scale_colour_manual(values = Cab.Colours[1:2])
    }
    # If row counts of the dataframes are the same, make the original line chart
    else {
      p <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean, group = 1)) +
        geom_line(size = 1, colour = "#004b88") +
        #geom_point(size = 3) +
        scale_colour_manual(values = Cab.Colours)
    }
    
    # Common plot aesthetics
    p <- p + theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_blank(), # This line removes the legend background
        legend.key = element_blank(), # This line removes the box around the legend
        legend.text = element_text(colour = "#004b88", size = 8)
      ) +
      ylab(input$Tab10Variable1) +
      # scale_x_discrete(breaks=c("2019-1","2020-1","2021-1", "2022-1", "2023-1"),
      #                  labels=c("2019.Q1","2020.Q1","2021.Q1", "2022.Q1", "2023.Q1")) +
      labs(colour = NULL)
    # scale_y_continuous(limits = c(min(table10_data()$Mean) * 0.9, max(table10_data()$Mean) * 1.1))
    
    if (!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Download tab 10 data
  output$download10 <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table10_data(), file)
    }
  )
  
  # Download tab 10 plot
  output$download10b <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table10_plot(), device = device, height = 5.5)
    }
  )
  
  
  table11_data <- reactive({
    # x-axis label
    Var.Group <- sym(input$Tab11Variable1)
    Var.Number <- sym(input$Tab11Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    # data for plot
    dat <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(!is.na({{ Var.Group }}), !grepl("NA|Other", as.character({{ Var.Group }}), ignore.case = TRUE))
    
    # overall statistics (group not applied)
    overall <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    # Convert Var.Group to a string and add a new column to overall
    Var.Group.String <- rlang::as_string(Var.Group)
    overall <- overall %>%
      dplyr::mutate(!!Var.Group.String := "All")
    
    # combine the grouped data and the overall data
    combined_dat <- rbind(dat, overall)
    
    combined_dat
  })
  
  
  # plot 11 input
  table11_plot <- reactive({
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "005743", "#c2bdde", "#9a1d4e", "#d4e5ef")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    plot <- ggplot(table11_data(), aes(x = {{ TimeVar }}, y = Mean, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(size = 0.75) +
      #geom_point() +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1)
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    plot
  })
  
  # Return tab 11 data
  output$table11 <- DT::renderDataTable({
    dt <- DT::datatable(table11_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Return plot11
  output$plot11 <- renderPlotly({
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "005743", "#c2bdde", "#9a1d4e", "#d4e5ef")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    p <- ggplot(table11_data(), aes(x = {{ TimeVar }}, y = Mean, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(size = 1) +
      #geom_point(size = 3) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_blank(), # This line removes the legend background
        legend.key = element_blank(), # This line removes the box around the legend
        legend.text = element_text(colour = "#004b88", size = 8)
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1) +
      labs(colour = NULL) # This line removes the legend title
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  
  
  # Download tab 11 data
  output$download11 <- downloadHandler(
    filename = function() {
      paste0(input$Tab11Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table11_data(), file)
    }
  )
  
  # Download tab 11 plot
  output$download11b <- downloadHandler(
    filename = function() {
      paste0(input$Tab11Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table11_plot(), device = device, height = 5.5)
    }
  )
  
  # data for plot 12
  table12_data <- reactive({
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable3)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!is.na({{ Var.Group }}), !grepl("NA|Other", as.character({{ Var.Group }}), ignore.case = TRUE)) %>% # This line filters NA and Other values
      mutate_if(is.numeric, round, 2)
    dat.plot
  })
  
  
  # plot 12 input
  table12_plot <- reactive({
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    # Plot
    ggplot(table12_data(), aes(x = {{ Var.Group }}, y = Mean, fill = {{ Var.Group }})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      # scale_fill_manual(values= Cab.Colours.Modified) +
      ylab(input$Tab12Variable2)
  })
  
  
  # Return tab 12 data
  output$table12 <- DT::renderDataTable({
    dt <- DT::datatable(table12_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  
  # Return plot12
  output$plot12 <- renderPlotly({
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    # Plot
    p <- ggplot(table12_data()) +
      geom_bar(aes(x = {{ Var.Group }}, y = Mean, fill = {{ Var.Group }}), stat = "identity", color = "#004b88", size = 0.75, show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88")
      ) +
      ylab(input$Tab12Variable2)
    
    # Check condition and remove x axis labels if true
    if (input$Tab12Variable1 == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#004b88"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 12 data
  output$download12 <- downloadHandler(
    filename = function() {
      paste0(input$Tab12Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table12_data(), file)
    }
  )
  
  # Download tab 12 plot
  output$download12b <- downloadHandler(
    filename = function() {
      paste0(input$Tab12Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table12_plot(), device = device, height = 5.5)
    }
  )
  
  # data for plot 13
  table13_data <- reactive({
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable4)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group1 }}, {{ Var.Group2 }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group1 }}), ignore.case = TRUE), !is.na({{ Var.Group1 }})) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group2 }}), ignore.case = TRUE), !is.na({{ Var.Group2 }})) %>%
      mutate_if(is.numeric, round, 2)
    dat.plot
  })
  
  
  
  
  # plot 13 input
  table13_plot <- reactive({
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Plot
    ggplot(table13_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      # scale_fill_manual(values= Cab.Colours.Modified) +
      ylab(input$Tab13Variable3)
  })
  
  
  # Return tab 13 data
  output$table13 <- DT::renderDataTable({
    dt <- DT::datatable(table13_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # plot 13 input
  output$plot13 <- renderPlotly({
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Plot
    p <- ggplot(table13_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88")
      ) +
      ylab(input$Tab13Variable3)
    
    # Check condition and remove x axis labels if true
    if (input$Tab13Variable1 == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#004b88"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 13 data
  output$download13 <- downloadHandler(
    filename = function() {
      paste0(input$Tab13Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table13_data(), file)
    }
  )
  
  # Download tab 13 plot
  output$download13b <- downloadHandler(
    filename = function() {
      paste0(input$Tab13Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table13_plot(), device = device, height = 5.5)
    }
  )
  
  # data for plot 14
  table14_data <- reactive({
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable5)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- MART.Dash %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group1 }}, {{ Var.Group2 }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group1 }}), ignore.case = TRUE), !is.na({{ Var.Group1 }})) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group2 }}), ignore.case = TRUE), !is.na({{ Var.Group2 }}))
    
    dat.plot
  })
  
  
  
  table14_plot <- reactive({
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    
    # Plot
    ggplot(table14_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, position = "dodge") +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      # scale_fill_manual(values= Cab.Colours.Modified) +
      ylab(input$Tab14Variable3)
  })
  
  
  # return tab 14 data
  output$table14 <- DT::renderDataTable({
    dt <- DT::datatable(table14_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Return plot14
  output$plot14 <- renderPlotly({
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    # Plot
    p <- ggplot(table14_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, position = "dodge", show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88")
      ) +
      ylab(input$Tab14Variable3)
    
    # Check if Var.Group1 equals to 'Demographic.Income.Band' to remove X axis labels
    if (rlang::as_string(Var.Group1) == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"), showlegend = FALSE)
  })
  
  
  # Download tab 14 data
  output$download14 <- downloadHandler(
    filename = function() {
      paste0(input$Tab14Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table14_data(), file)
    }
  )
  
  # Download tab 14 plot
  output$download14b <- downloadHandler(
    filename = function() {
      paste0(input$Tab14Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table14_plot(), device = device, height = 5.5)
    }
  )
  
  # data for plot 15
  table15_data <- reactive({
    Var.Group <- input$Tab15Variable1
    Var.Number <- input$Tab15Variable2
    TimeVar <- sym(input$TimeVariable6)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- MART.Dash %>%
      conditional_filter(Var.Number)
    
    # If TimeVar is "Year", transform it to a specific format (e.g. append quarter information)
    if (as.character(TimeVar) == "Year") {
      dat.plot <- dat.plot %>%
        mutate(!!TimeVar := as.integer(Year) + 2018)
    }
    
    dat.plot <- dat.plot %>%
      dplyr::group_by(!!TimeVar, (!!sym(Var.Group))) %>%
      dplyr::summarise(
        Mean = mean((!!sym(Var.Number)), na.rm = TRUE, trim = 0.2),
        Median = median((!!sym(Var.Number))),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(!grepl("Other|NA", as.character((!!sym(Var.Group))), ignore.case = TRUE), !is.na((!!sym(Var.Group))))
    
    dat.plot
  })
  
  # plot 15 input
  table15_plot <- reactive({
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable6)
    
    # # Copy colour palette over from Flourish
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#e37222", "#7192be")
    
    # modify colour palette to contain as many items as levels of inputted factor variable
    Cab.Colours.Modified <- Cab.Colours[1:nlevels(table15_data()[[Var.Group]])]
    if ("Other/prefer not to say/unknown" %in% levels(table15_data()[[Var.Group]])) {
      Cab.Colours.Modified <- head(Cab.Colours.Modified, -1)
    }
    print(Cab.Colours.Modified)
    
    
    # Plot
    ggplot(table15_data(), aes(x = {{ TimeVar }}, y = Mean, fill = {{ Var.Group }})) +
      geom_area(colour = "#004b88", size = 1, show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      scale_fill_manual(values = Cab.Colours) +
      ylab(input$Tab15Variable2)
  })
  
  
  # return tab 15 data
  output$table15 <- DT::renderDataTable({
    dt <- DT::datatable(table15_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Return plot15
  output$plot15 <- renderPlotly({
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable6)
    
    # # Copy colour palette over from Flourish
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#e37222", "#7192be")
    
    
    # Plot
    p <- ggplot(table15_data(), aes(x = {{ TimeVar }}, y = Mean, fill = {{ Var.Group }})) +
      geom_area(colour = "#004b88", size = 1, show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_blank(),
        legend.text = element_text(colour = "#004b88", size = 8),
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = Cab.Colours) +
      ylab(input$Tab15Variable2) +
      labs(fill = NULL) # This line removes the legend title
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 15 data
  output$download15 <- downloadHandler(
    filename = function() {
      paste0(input$Tab15Variable2, ".csv")
    },
    content = function(file) {
      write.csv(table15_data(), file)
    }
  )
  
  # Download tab 15 plot
  output$download15b <- downloadHandler(
    filename = function() {
      paste0(input$Tab15Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table15_plot(), device = device, height = 5.5)
    }
  )
  
  # Table 16 data
  table16_data <- reactive({
    Var.Group <- sym(input$Tab16Variable1)
    TimeVar <- if (as.character(input$TimeVariable7) == "Year") "Year" else "Year.Quarter"
    
    # Convert Year to integer if necessary
    dat.plot <- MART.Dash
    if (TimeVar == "Year") {
      dat.plot <- dat.plot %>%
        mutate(Year = as.integer(Year) + 2018)
    }
    
    # data for plot
    data <- dat.plot %>%
      filter(!grepl("Other|NA", as.character(!!Var.Group), ignore.case = TRUE), !is.na(!!Var.Group)) %>%
      group_by(!!sym(TimeVar), !!Var.Group) %>%
      summarize(n = n()) %>%
      spread(!!Var.Group, n) %>%
      ungroup()
    
    data.plot <- data %>%
      select(-1) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      mutate(Time = data[[TimeVar]], n = rowSums(data[, -1])) %>%
      melt(id.vars = c("Time", "n")) %>%
      rename(Condition = "variable", Percentage = "value") %>%
      mutate_if(is.numeric, round, 2) %>%
      select(Time, Condition, Percentage, n)
    
    data.plot
  })
  
  
  # plot 16 input
  table16_plot <- reactive({
    Var.Group <- sym(input$Tab16Variable1)
    
    # # Copy colour palette over from Flourish
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#e37222", "#7192be")
    
    
    # modify colour palette to contain as many items as levels of inputted factor variable
    Cab.Colours.Modified <- Cab.Colours[1:nlevels(table16_data()[[Var.Group]])]
    if ("Other/prefer not to say/unknown" %in% levels(table16_data()[[Var.Group]])) {
      Cab.Colours.Modified <- head(Cab.Colours.Modified, -1)
    }
    print(Cab.Colours.Modified)
    
    
    # Plot
    ggplot(table16_data(), aes(x = Time, y = Percentage, fill = Condition)) +
      geom_area(alpha = 0.6, size = 1, colour = "#004b88") +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88"),
        legend.title = element_text(colour = "#004b88")
      ) +
      scale_fill_manual(values = Cab.Colours)
  })
  
  # return tab 16 data
  output$table16 <- DT::renderDataTable({
    dt <- DT::datatable(table16_data(),
                        options = list(scrollY = "200px"), 
                        class = "hover row-border stripe",
                        filter = "bottom")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  
  # Return plot16
  output$plot16 <- renderPlotly({
    Var.Group <- sym(input$Tab16Variable1)
    
    # # Copy colour palette over from Flourish
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#e37222", "#7192be")
    
    
    # Plot
    p <- ggplot(table16_data(), aes(x = Time, y = Percentage, fill = Condition)) +
      geom_area(alpha = 0.6, size = 1, colour = "#004b88", show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        strip.background = element_rect(
          color = "#004b88",
          fill = "#fcbb69", size = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_blank(),
        legend.text = element_text(colour = "#004b88", size = 8),
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = Cab.Colours) +
      labs(fill = NULL) # This line removes the legend title
    
    # Convert to Plotly and remove legend
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 16 data
  output$download16 <- downloadHandler(
    filename = function() {
      paste0(input$Tab16Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table16_data(), file)
    }
  )
  
  # Download tab 16 plot
  output$download16b <- downloadHandler(
    filename = function() {
      paste0(input$Tab16Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table16_plot(), device = device, height = 5.5)
    }
  )
  
  table18_data <- reactive({
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    
    # MART.Dash <- readRDS("MART_dash.rds")
    
    # Load shape data
    shape_data <- readRDS("shape_data.rds")
    shapes_la <- readRDS("shapes_la.rds")
    shapes_bmra <- readRDS("Shape.BMRA.rds")
    shapes_icb <- readRDS("shapes_icb.rds")
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # Create data map
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- MART.Dash %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- MART.Dash %>%
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    
    joined_data
  })
  
  table18_plot <- reactive({
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    Colour <- as.character(input$Tab18Variable4)
    
    # Load shape data
    # shape_data <- readRDS("shape_data.rds")
    # shapes_la <- readRDS("shapes_la.rds")
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # Create data map
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- MART.Dash %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- MART.Dash %>%
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    
    # Calculate global min and max
    if (input$Tab18Variable1 == "Surplus") {
      global_min <- MART.Dash %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = "drop") %>%
        summarise(Global.Min = min(T.Mean, na.rm = TRUE))
      
      global_max <- MART.Dash %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = "drop") %>%
        summarise(Global.Max = max(T.Mean, na.rm = TRUE))
    } else {
      global_min <- MART.Dash %>%
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = "drop") %>%
        summarise(Global.Min = min(T.Mean, na.rm = TRUE))
      
      global_max <- MART.Dash %>%
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = "drop") %>%
        summarise(Global.Max = max(T.Mean, na.rm = TRUE))
    }
    
    # Define colour scheme
    if (Colour == "Red") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#800000" # dark red
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#800000" # dark red
      }
    } else if (Colour == "Green") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#006400" # dark green
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#006400" # dark green
      }
    }
    
    # Plot the choropleth map
    plot.map <- ggplot() +
      geom_sf(data = joined_data, aes(
        geometry = geometry, fill = T.Mean,
        text = paste0(group_var, ": ", !!group_var_sym, "<br>", input$Tab18Variable1, ": ", round(T.Mean, 2))
      )) +
      scale_fill_gradient(
        low = low_colour, high = high_colour,
        limits = c(global_min$Global.Min, global_max$Global.Max),
        name = paste0(input$Tab18Variable1)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    plot.map
  })
  
  # return tab 18 data
  output$table18 <- DT::renderDataTable({
    dt <- DT::datatable(table18_data(), options = list(scrollY = "200px"), class = "cell-border")
    
    # Apply conditional formatting to the column "n"
    dt <- dt %>% formatStyle(
      "n",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  ## Return plot18
  output$plot18 <- renderPlotly({
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    Colour <- as.character(input$Tab18Variable4)
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # Create data map
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- MART.Dash %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- MART.Dash %>%
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>%
        summarise(
          T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
          n = n(), .groups = "drop"
        ) %>%
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    
    # Define colour scheme
    if (Colour == "Red") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#800000" # dark red
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#800000" # dark red
      }
    } else if (Colour == "Green") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#006400" # dark green
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#006400" # dark green
      }
    }
    
    
    # Plot the choropleth map
    plot.map <- ggplot() +
      geom_sf(data = joined_data, aes(
        geometry = geometry, fill = T.Mean,
        text = paste0(group_var, ": ", !!group_var_sym, "<br>", input$Tab18Variable1, ": ", round(T.Mean, 2))
      )) +
      scale_fill_gradient(
        low = low_colour, high = high_colour,
        name = paste0(input$Tab18Variable1)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    # Convert to plotly
    plotly_map <- ggplotly(plot.map, tooltip = "text")
    
    return(plotly_map)
  })
  
  
  # Download tab 18 data
  output$download18 <- downloadHandler(
    filename = function() {
      paste0(input$Tab18Variable1, ".csv")
    },
    content = function(file) {
      # Get the data
      data_to_write <- table18_data()
      
      # Remove the geometry column
      data_to_write <- data_to_write %>%
        select(0:3)
      
      # Write the modified data to a CSV file
      write.csv(data_to_write, file)
    }
  )
  
  # Download tab 18 plot
  output$download18b <- downloadHandler(
    filename = function() {
      paste0(input$Tab18Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table18_plot(), device = device, height = 5.5)
    }
  )
  
  
  # Table 17 data
  table17_data <- reactive({
    # # Copy colour palette over from Flourish
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef")
    
    # MART.Dash <- readRDS("MART_Dash.rds")
    
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    Financial.Year.Input <- as.character(input$Tab17Variable3)
    
    # Filter data based on input Financial.Year
    filtered_data <- MART.Dash %>% filter(Financial.Year == Financial.Year.Input)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    # Extract the necessary columns for the two selected variables
    comparison_data <- filtered_data %>%
      select(!!variable1, !!variable2, Financial.Year) %>%
      filter(!is.na(!!variable1) | !is.na(!!variable2)) %>%
      conditional_filter(variable2)
    
    # Trim data based on percentiles for variable2 only, because variable1 is categorical
    lower_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .05, na.rm = TRUE)
    upper_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .95, na.rm = TRUE)
    
    trimmed_data <- comparison_data %>%
      filter(!!variable2 >= lower_quantile2 & !!variable2 <= upper_quantile2)
    
    return(trimmed_data)
  })
  
  
  
  # plot 17 input
  table17_plot <- reactive({
    # Extended colour palette
    Cab.Colours <- c(
      "#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba",
      "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#d9bf77", "#66b2b2",
      "#8c4356", "#f7ea39", "#394240", "#f26c4f", "#3b3e42", "#d27d67", "#6b9362", "#f9cb9c"
    )
    
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    Financial.Year.Input <- as.character(input$Tab17Variable3)
    
    plot <- ggplot(table17_data(), aes(x = !!variable2, fill = !!variable1)) +
      geom_density(alpha = 0.5) +
      ylab("Density") +
      xlab(input$Tab17Variable1) + # Replace "Variable Name" with the actual variable name
      theme_classic() +
      scale_fill_manual(values = Cab.Colours) +
      theme(
        text = element_text(size = 15),
        axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88", size = 8),
        legend.title = element_blank()
      ) +
      labs(fill = NULL)
    
    plot
  })
  
  
  # Return tab 17 plot
  output$plot17 <- renderPlotly({
    # Extended colour palette
    Cab.Colours <- c(
      "#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba",
      "#005743", "#c2bdde", "#9a1d4e", "#d4e5ef", "#d9bf77", "#66b2b2",
      "#8c4356", "#f7ea39", "#394240", "#f26c4f", "#3b3e42", "#d27d67", "#6b9362", "#f9cb9c"
    )
    
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    Financial.Year.Input <- as.character(input$Tab17Variable3)
    
    
    plot <- ggplot(table17_data(), aes(x = !!variable2, fill = !!variable1)) +
      geom_density(alpha = 0.5) +
      ylab("Density") +
      xlab(input$Tab17Variable1) + # Replace "Variable Name" with the actual variable name
      theme_classic() +
      scale_fill_manual(values = Cab.Colours) +
      theme(
        text = element_text(size = 15),
        axis.text.x = element_text(color = "#004b88"),
        axis.text.y = element_text(color = "#004b88"),
        axis.line.x.bottom = element_line(colour = "#004b88"),
        axis.line.y.left = element_line(colour = "#004b88"),
        axis.title.x = element_text(colour = "#004b88"),
        axis.title.y = element_text(colour = "#004b88"),
        legend.background = element_rect(colour = "#004b88", fill = "white"),
        legend.text = element_text(colour = "#004b88", size = 8),
        legend.title = element_blank()
      ) +
      labs(fill = NULL)
    
    
    # Convert to Plotly
    plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "fill"))
    
    return(plotly_plot)
  })
  
  
  
  # Download tab 17 plot
  output$download17b <- downloadHandler(
    filename = function() {
      paste0(input$Tab17Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table17_plot(), device = device, height = 5.5)
    }
  )

#### thats a wrap
}
