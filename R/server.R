#' Server function for StepReg Shiny Application
#'
#' Defines the server logic for the StepReg Shiny application.
#' This function handles data processing, stepwise regression analysis,
#' and result visualization.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @importFrom shiny reactiveValues observeEvent req renderUI selectInput updateSelectInput showModal modalDialog renderPrint renderPlot renderText downloadHandler eventReactive observe
#' @importFrom DT datatable renderDT
#' @importFrom summarytools dfSummary
#' @importFrom cowplot plot_grid
#' @importFrom rmarkdown render
#' @importFrom grDevices pdf
#' @importFrom stats as.formula cor na.omit terms
#' @importFrom utils data read.table
#' @importFrom dplyr %>% select all_of mutate
#' @importFrom stringr str_to_title str_split
#' @importFrom StepReg stepwise performance
#' @importFrom ggplot2 ggsave
#' @importFrom shiny HTML icon onSessionEnded stopApp reactive
#' @importFrom graphics text
#' @importFrom stats setNames
#' @importFrom flextable qflextable
#'
server <- function(input, output, session) {
  # Function to read uploaded dataset
  df = reactiveValues(path = NULL)
  
  # Initialize reactive values for button states
  button_states <- reactiveValues(
    download_enabled = FALSE,
    downloadPlot_enabled = FALSE,
    download_process_plot_enabled = FALSE
  )
  
  observeEvent(input$example_dataset, {
    req(input$example_dataset)
    # Read the selected example dataset using data() function
    tryCatch({
      if (input$example_dataset == "mtcars") {
        df$data <- get("mtcars", envir = asNamespace("datasets"))
      } else {
        # Load data from StepReg package
        data(list = input$example_dataset, package = "StepReg", envir = environment())
        dataset_name <- input$example_dataset
        
        # Special processing for lung data
        if (dataset_name == "lung") {
          lung_data <- get(dataset_name, envir = environment())
          lung_data <- lung_data %>%
            mutate(sex = factor(.data$sex, levels = c(1, 2))) %>% 
            na.omit() # get rid of incomplete records
          df$data <- lung_data
        } else {
          df$data <- get(dataset_name, envir = environment())
        }
      }
    }, error = function(e) {
      # Fallback to get() method if data() fails
      if (input$example_dataset == "mtcars") {
        df$data <- get("mtcars", envir = asNamespace("datasets"))
      } else {
        df$data <- get(input$example_dataset, envir = asNamespace("StepReg"))
        if (input$example_dataset == "lung") {
          df$data <- df$data %>%
            mutate(sex = factor(.data$sex, levels = c(1, 2))) %>% 
            na.omit()
        }
      }
    })
  })
  
  # Function to upload user custom dataset:
  observeEvent(c(input$upload_file, input$header, input$sep, input$quote), {
    req(input$upload_file)
    # Read the uploaded file
    tryCatch(
      df$data <- read.table(input$upload_file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote),
      error = function(e) {
        warning("An error occurred uploading dataset:", e$message)
        df$data <- NULL
      })
  })
  
  output_list <- c("numeric_var", "factor_var", "character_var", "integer_var")
  
  lapply(output_list, function(i) {
    output_name <- unlist(str_split(i, "_"))[1]
    output[[paste0("colname_in_", output_name)]] <- renderUI({
      req(df$data)
      var_names <- names(df$data)[sapply(df$data, class) == output_name]
      selectInput(inputId = i,
                  label = paste0(str_to_title(output_name), " variables"),
                  choices = names(df$data),
                  selected = var_names,
                  multiple = TRUE)
    })
  })
  
  observeEvent(input$numeric_var, {
    updateSelectInput(session, "character_var", selected = setdiff(input$character_var, input$numeric_var))
    updateSelectInput(session, "factor_var", selected = setdiff(input$factor_var, input$numeric_var))
    updateSelectInput(session, "integer_var", selected = setdiff(input$integer_var, input$numeric_var))
  })

  observeEvent(input$factor_var, {
    updateSelectInput(session, "numeric_var", selected = setdiff(input$numeric_var, input$factor_var))
    updateSelectInput(session, "character_var", selected = setdiff(input$character_var, input$factor_var))
    updateSelectInput(session, "integer_var", selected = setdiff(input$integer_var, input$factor_var))
  })

  observeEvent(input$character_var, {
    updateSelectInput(session, "numeric_var", selected = setdiff(input$numeric_var, input$character_var))
    updateSelectInput(session, "factor_var", selected = setdiff(input$factor_var, input$character_var))
    updateSelectInput(session, "integer_var", selected = setdiff(input$integer_var, input$character_var))
  })

  observeEvent(input$integer_var, {
    updateSelectInput(session, "numeric_var", selected = setdiff(input$numeric_var, input$integer_var))
    updateSelectInput(session, "factor_var", selected = setdiff(input$factor_var, input$integer_var))
    updateSelectInput(session, "character_var", selected = setdiff(input$character_var, input$integer_var))
  })
  
  observeEvent(input$change_class, {
    #dont use req() below, otherwise cannot do eval function.
    #req(input$numeric_var, input$factor_var, input$integer_var, input$character_var)
    req(df$data)
    var_forget <- colnames(df$data)[!colnames(df$data) %in% c(input$numeric_var, input$factor_var, input$integer_var, input$character_var)]
    if(length(var_forget) > 0) {
      showModal(modalDialog(
        title = "Missing Variables",
        paste0("Please specify variable type for ",paste0(var_forget,collapse=", "),'.'),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    mutate_variable <- function(var_names, var_type) {
      lapply(var_names, function(i) {
        df$data <- eval(parse(text = paste0('df$data %>% mutate(',
                                             i,
                                             ' = as.',
                                             var_type,
                                             '(',
                                             i,
                                             '))')))
      })
    }

    mutate_variable(input$numeric_var, "numeric")
    mutate_variable(input$factor_var, "factor")
    mutate_variable(input$integer_var, "integer")
    mutate_variable(input$character_var, "character")
  })

  # Enable run button if all required fields are specified by user:
  run_analysis_enabled <- reactive({
    # Check if formula is provided
    if (is.null(input$formula_input) || input$formula_input == "") {
      return(FALSE)
    }
    # Check if strategy is selected
    if (is.null(input$strategy) || length(input$strategy) == 0) {
      return(FALSE)
    }
    # Check if metric is selected based on detected type
    if (is.null(input$type)) {
      return(FALSE)
    } 
    if (input$type == "linear") {
      # Check if it's multivariate by looking for cbind in the formula
      if (grepl("cbind\\(", input$formula_input)) {
        if (is.null(input$metric_multivariate_linear) || length(input$metric_multivariate_linear) == 0) {
          return(FALSE)
        }
      } else {
        if (is.null(input$metric_univariate_linear) || length(input$metric_univariate_linear) == 0) {
          return(FALSE)
        }
      }
    } else if (input$type %in% c("logit", "cox", "poisson", "gamma", "negbin")) {
      if (is.null(input$metric_glm_cox) || length(input$metric_glm_cox) == 0) {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
    return(TRUE)
  })
  
  exploratory_plot_enabled <- reactive({
    tryCatch({
      if (is.null(input$var_plot) || length(input$var_plot) == 0){
        return(FALSE)
      } else {
        return(TRUE)
      }
    }, error = function(e) {
      return(FALSE)
    })
  })
  
  # Button states are now controlled by UI logic instead of shinyjs
  # This removes the dependency on shinyjs and avoids session issues
  
  rv <- reactiveValues()
  rv$nmetric <- 1
  rv$nvar <- 1
  
  # Output for formula examples
  output$formula_placeholder_help <- renderUI({
    req(df$data)
    
    # Get examples based on selected regression type
    examples_list <- switch(
      input$type,
      "linear" = {
        list(
          "specified variables: y ~ x1 + x2",
          "all variables: y ~ .",
          "all variables except x1: y ~ . - x1",
          "main effects and interaction: y ~ x1*x2",
          "continuous-nested-within-class effects: y ~ x1 + x1:x2",
          "multiple response: cbind(y1, y2) ~ .",
          "no intercept: y ~ . + 0 or y ~ . - 1"
        )
      },
      "logit" = {
        list(
          "specified variables: y ~ x1 + x2",
          "all variables: y ~ .",
          "all variables except x1: y ~ . - x1",
          "main effects and interaction: y ~ x1*x2",
          "continuous-nested-within-class effects: y ~ x1 + x1:x2",
          "no intercept: y ~ . + 0 or y ~ . - 1"
        )
      },
      "poisson" = {
        list(
          "specified variables: y ~ x1 + x2",
          "all variables: y ~ .",
          "all variables except x1: y ~ . - x1",
          "main effects and interaction: y ~ x1*x2",
          "continuous-nested-within-class effects: y ~ x1 + x1:x2",
          "no intercept: y ~ . + 0 or y ~ . - 1"
        )
      },
      "gamma" = {
        list(
          "specified variables: y ~ x1 + x2",
          "all variables: y ~ .",
          "all variables except x1: y ~ . - x1",
          "main effects and interaction: y ~ x1*x2",
          "continuous-nested-within-class effects: y ~ x1 + x1:x2",
          "no intercept: y ~ . + 0 or y ~ . - 1"
        )
      },
      "negbin" = {
        list(
          "specified variables: y ~ x1 + x2",
          "all variables: y ~ .",
          "all variables except x1: y ~ . - x1",
          "main effects and interaction: y ~ x1*x2",
          "continuous-nested-within-class effects: y ~ x1 + x1:x2",
          "no intercept: y ~ . + 0 or y ~ . - 1"
        )
      },
      "cox" = {
        list(
          "specified variables: Surv(time, status) ~ x1 + x2",
          "all variables: Surv(time, status) ~ .",
          "all variables except x1: Surv(time, status) ~ . - x1",
          "main effects and interaction: Surv(time, status) ~ x1*x2",
          "continuous-nested-within-class effects: Surv(time, status) ~ x1 + x1:x2",
          "stratified Cox regression: Surv(time, status) ~ x1 + strata(x2)"
        )
      },
      list("Enter your formula here...")
    )
    
    tags$div(
      style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #3498db;",
      tags$h6(style = "margin: 0 0 8px 0; color: #2c3e50;", paste0("Formula Examples for ", input$type, " regression:")),
      tags$ul(
        style = "margin: 0; padding-left: 20px;",
        lapply(examples_list, function(example) {
          tags$li(
            style = "font-family: monospace; font-size: 11px; color: #495057; margin-bottom: 2px;",
            example
          )
        })
      )
    )
  })
  
  # Generate dynamic placeholder for include input based on formula and data
  output$include_input_ui <- renderUI({
    req(df$data)
    
    # Default choices when no formula is entered
    default_choices <- c("None" = NULL)
    
    if (is.null(input$formula_input) || input$formula_input == "" || trimws(input$formula_input) == "") {
      return(
        selectInput(
          "include_input",
          "Include:",
          choices = default_choices,
          selected = NULL
        )
      )
    }
    
    tryCatch({
      formula <- as.formula(input$formula_input)
      term_form <- terms(formula, data = df$data)
      x_name <- attr(term_form, "term.labels")
      var_choices <- setNames(x_name, x_name)
      
      selectInput(
        "include_input",
        "Include:",
        choices = var_choices,
        selected = NULL,
        multiple = TRUE
      )
    }, error = function(e) {
      selectInput(
        "include_input",
        "Include:",
        choices = default_choices,
        selected = NULL,
        multiple = TRUE
      )
    })
  })
  
  # Perform stepwise regression based on uploaded dataset
  stepwiseModel <- eventReactive(input$run_analysis, {
    # Button states are now controlled by UI logic instead of shinyjs
    req(df$data)
    
    # Validate formula input
    if (is.null(input$formula_input) || input$formula_input == "" || trimws(input$formula_input) == "") {
      stop("Please enter a valid formula")
    }
    
    # Try to parse the formula with error handling
    tryCatch({
      formula <- as.formula(input$formula_input)
    }, error = function(e) {
      stop(paste("Invalid formula format:", e$message, "\nPlease check your formula syntax."))
    })
    
    metric <- switch(
      input$type,
      "linear" = {
        # Check if it's multivariate by looking for cbind in the formula
        if (grepl("cbind\\(", as.character(formula)[3])) {
          input$metric_multivariate_linear
        } else {
          input$metric_univariate_linear
        }
      },
      "cox" = input$metric_glm_cox,
      "logit" = input$metric_glm_cox,
      "poisson" = input$metric_glm_cox,
      "gamma" = input$metric_glm_cox,
      "negbin" = input$metric_glm_cox
    )
    rv$nmetric <- length(metric)
    rv$nvar <- ncol(df$data)/10
    # if round() = 2, then run make plot twice, so dont update input.
    #updateSelectInput(session, "relative_height", selected = round(rv$nmetric*rv$nvar))
    
    res <- stepwise(
        formula = formula,
        data = df$data,
        type = input$type,
        strategy = input$strategy,
        metric = metric,
        sle = input$sle,
        sls = input$sls,
        include = input$include_input,
        test_method_linear = input$Approx_F,
        test_method_glm = input$glm_test,
        test_method_cox = input$cox_test,
        test_ratio = input$test_ratio,
        feature_ratio = ifelse(any(input$strategy %in% c('forward','bidirection')) && !is.null(input$feature_ratio), input$feature_ratio, 1)
    )
    
    summary_list <- setNames(
      lapply(attr(res, "nonhidden"), function(i) {
        lapply(res[[i]], summary)
      }),
      attr(res, "nonhidden")
    )
    
    
    process_plot <- setNames(
      lapply(attr(res,"nonhidden"),function(i){
        setNames(
          lapply(c("detail","overview"),function(j){
            plot(res,strategy=i,process=j)
          }),
          c("detail","overview")
        )
      }),
      attr(res,"nonhidden")
    )

    if(all(input$strategy %in% 'subset') & all(metric %in% 'SL')) {
      model_performance <- NULL
    } else {
      model_performance <- performance(res)
    }
    results <- list(summary_list, process_plot, model_performance,res$argument,res$variable)
    
    # Button states are now controlled by UI logic instead of shinyjs
    results
  })

  observeEvent(input$strategy, {
    updateSelectInput(
      session, 
      "strategy_plot", 
      choices = input$strategy
    )
  })
  
  # Generate output and enable download button:
  output$modelSelection <- renderPrint({
    tryCatch({
      stepwiseModel()[[1]]
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Please run the analysis first", col = "blue", cex = 1.2)
    })
  })
  
  output$detail_plot <- renderPlot({
    tryCatch({
      req(stepwiseModel(), input$strategy_plot)
      selected_plot <- plot_grid(plotlist = rev(stepwiseModel()[[2]][[input$strategy_plot]]), 
                                 ncol = 1, 
                                 labels = "AUTO", 
                                 rel_heights = c(1, as.numeric(input$relative_height)))
      rv$all_plot <- selected_plot
      selected_plot
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Please run the analysis first", col = "blue", cex = 1.2)
    })
  }, res =96, 
  width = function() { (320 * 2) }, 
  height = function() { (320 * 4 * (rv$nmetric/(rv$nmetric + 1)) * rv$nvar) })
  
  output$selectionPlotText <- renderUI({
    HTML("<b>Visualization of Variable Selection:\n</b>")
  })
  output$selectionStatText <- renderText({
    HTML("<b>Results of Variable Selection:\n</b>")
  })
  
  output$modelParametersText <- renderText({
    HTML("<b>Model Parameters and Configuration:\n</b>")
  })
  
  output$modelVariablesText <- renderText({
    HTML("<b>Variable Selection Summary:\n</b>")
  })
  output$modelPerformanceText <- renderText({
    tryCatch({
      req(stepwiseModel())
      # Get the metric from the stepwiseModel results
      metric <- stepwiseModel()[[4]]$metric
      if(all(input$strategy %in% 'subset') & all(metric %in% 'SL')) {
        HTML("<b>Model performance evaluation isn't available for selection strategy 'subset':\n</b>")
      } else {
        HTML("<b>Model Performance Evaluation Across All Combinations of Strategy and Metric:\n</b>")
      }
    }, error = function(e) {
      HTML("<b>Model Performance Evaluation Across All Combinations of Strategy and Metric:\n</b>")
    })
  })
  
  output$modelPerformance <- renderDT({ 
    tryCatch({
      req(stepwiseModel())
      # Get the metric from the stepwiseModel results
      metric <- stepwiseModel()[[4]]$metric
      if(!(all(input$strategy %in% 'subset') & all(metric %in% 'SL'))) {
        DT::datatable(stepwiseModel()[[3]], options = list(scrollX = TRUE))
      }
    }, error = function(e) {
      DT::datatable(data.frame(Message = "No model performance results available"), options = list(scrollX = TRUE))
    })
  })
  # Output Data
  output$tbl <- renderDT({
    req(df$data)
    DT::datatable(df$data, options = list(scrollX = TRUE))
  })
  
  # Output Model Parameters
  output$modelParameters <- renderDT({
    req(stepwiseModel())
    DT::datatable(stepwiseModel()[[4]], options = list(scrollX = TRUE))
  })
  
  # Output Model Variables
  output$modelVariables <- renderDT({
    req(stepwiseModel())
    DT::datatable(stepwiseModel()[[5]], options = list(scrollX = TRUE))
  })

  # Render the appropriate summary based on the selected type
  observe({
    output$summary <- renderPrint({
      req(df$data)
      pdf(file = NULL)
      summarytools::dfSummary(df$data,graph.col = FALSE)
    })
  })
  
  observe({
    req(df$data)
    updateSelectInput(session, "var_plot", choices = colnames(df$data))
  })
  
  plot_data <- eventReactive(input$make_plot, {
    # Button states are now controlled by UI logic instead of shinyjs
    req(input$plot_type, input$var_plot)
    plot_type <- createPlot(input$plot_type, input$var_plot, df$data)
    # if (input$plot_type == "Pairs plot") {
    #   plot_result <- plot_type
    # } else {
      #grid.arrange(grobs = plot_type)
      plot_result <- plot_grid(plotlist = plot_type)
    # }
    # Button states are now controlled by UI logic instead of shinyjs
    return(plot_result)
  })
  
  output$Plot <- renderPlot({
   plot_data()
  })
  
  # Render the error message in the main panel
  output$error_message <- renderText({
    "No errors"  # Display the stored error message
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$plot_type, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_data(), device = "png")
    }
  )
  
  output$download_process_plot <- downloadHandler(
    filename = function() { paste(input$strategy_plot, '_selection_process.png', sep='') },
    content = function(file) {
      ggsave(file, plot = rv$all_plot, device = "png")
    }
  )
  
  output$download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("StepReg_report_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file('shiny/report.Rmd', package='StepRegShiny'), tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(modelParameters = stepwiseModel()[[4]],
                     modelVariables = stepwiseModel()[[5]],
                     modelSelection = stepwiseModel()[[1]], 
                     selectionPlot = stepwiseModel()[[2]],
                     modelPerformance = stepwiseModel()[[3]],
                     relValue = input$relative_height)
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  session$onSessionEnded(function() { stopApp() }) 
}
