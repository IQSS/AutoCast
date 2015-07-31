##### weightcast function that takes 'gridcast' or 'groupcast' object and launches Shiny-based
##### interface to select optimal weights
##### Author: Konstantin Kashin Date: July 18, 2015

#' @title Weight selection for AutoCast
#'
#' @description
#' \code{weightcast} launches an interactive, Shiny-based application that allows the user to select multiple weight combinations that fit her preference for fit versus smoothness of the forecasts.
#'
#'
#' @param x Object of class \code{"gridcast"} or a list of \code{"gridcast"} objects. 
#' @param out.file Character. Gives full filename (and path) of where to save selected weight combinations (if requested). Default: NULL.
#' @param out.object Character. Gives name of object in \code{parent.frame()} to which to assign selected weight combinations (if requested). Default: NULL.
#' @param center.diags Logical. If \code{TRUE}, each of the four diagnostic variables is centered (de-meaned). Default: \code{TRUE}.
#' @param scale.diags Logical. If \code{TRUE}, each of the four diagnostic variables is scaled after centering, if applicable (divided by standard deviation).
#'   Default: \code{TRUE}.
#' @param options Options passed to \code{options} argument of \code{ShinyApp}.
#'
#' @return \code{NULL}.

#' @export
weightcast <- function(x, out.file = NULL, out.object = NULL, center.diags = T, scale.diags = T, options=list()) {
    # set up selectedWeights object that will hold selected weights
    selectedWeightsTemplate <- list()
    class(selectedWeightsTemplate) <- "weightcast"
    # get parent frame
    env <- parent.frame()
    
    # stop if x is NULL
    if(is.null(x)){
    	stop("'x' must be a 'gridcast' object or a list of 'gridcast' objects.", call. = F)
    }
    
    # check if class of x is gridcast
    if (class(x) == "gridcast") {
        isList <- FALSE
        
        # rescale diags, if needed
        diags <- scale(x$validation$diags, center = center.diags, scale = scale.diags)
        if (center.diags) {
            attr(selectedWeightsTemplate, "diag:center") <- mean.diags <- attr(diags, "scaled:center")
        } else {
            mean.diags <- NULL
        }
        if (scale.diags) {
            attr(selectedWeightsTemplate, "diag:scale") <- sd.diags <- attr(diags, "scaled:scale")
        } else {
            sd.diags <- NULL
        }
        x$validation$diags <- diags
        x <- list(x)
    } else if (is.list(x) & all(sapply(x, function(e) class(e) == "gridcast"))) {
    	# check to make sure that each element of the list is a 'gridcast' object
        isList <- TRUE
        forecastNames <- names(x)
        
        # rescale diags, if needed
        if (center.diags | scale.diags) {
        	# create array of all diagnostics
    		alldiags <- do.call(rbind, lapply(x, function(e) e$validation$diags))
        	diag.mean <- apply(alldiags, 2, mean)
    		diag.sd <- apply(alldiags, 2, sd)
            
            if (center.diags) {
                attr(selectedWeightsTemplate, "diag:center") <- mean.diags <- diag.mean
            } else {
                mean.diags <- NULL
            }
            if (scale.diags) {
                attr(selectedWeightsTemplate, "diag:scale") <- sd.diags <- diag.sd
            } else {
                sd.diags <- NULL
            }
            for (e in 1:length(x)) {
                if (center.diags) {
                  x[[e]]$validation$diags <- sweep(x[[e]]$validation$diags, MARGIN = 2, 
                    mean.diags)
                }
                if (scale.diags) {
                  x[[e]]$validation$diags <- sweep(x[[e]]$validation$diags, MARGIN = 2, 
                    sd.diags, FUN = "/")
                }
            }
        } else {
            mean.diags <- NULL
            sd.diags <- NULL
        }
    } else {
        # if neither gridcast or groupcast object
        stop("'x' must be a 'gridcast' object or a list of 'gridcast' objects.", call. = F)
    }
    
    # check if have out name
    if (is.null(out.file) & is.null(out.object)) {
        out.object <- sub("/", "", tempfile(pattern = "weightcast_", tmpdir = ""))
        out.file <- paste(getwd(), "/", out.object, ".rds", sep = "")
    } else if (is.null(out.file)) {
        out.file <- tempfile(pattern = "weightcast_", tmpdir = getwd(), fileext = ".rds")
    } else if (is.null(out.object)) {
        out.object <- sub("/", "", tempfile(pattern = "weightcast_", tmpdir = ""))
    }
    if (!is.character(out.file)) {
        stop("'out.file' must be either NULL or 'character' class.", call. = F)
    }
    if (!is.character(out.object)) {
        stop("'out.object' must be either NULL or 'character' class.", call. = F)
    }
    
    # store ages & times of first forecast (will initialize selectedAges and selectedTimes
    # with it)
    ages <- x[[1]]$aux$ages
    times <- x[[1]]$aux$times
    startAges <- ages[c(1, length(ages))]
    startTimes <- times[c(1, length(times))]
    
    ############################# SHINY APP ###########
    shinyApp(ui = fluidPage(tags$head(includeCSS(system.file("css/bootstrap.css", package = "ShinyAutoCast")), 
        includeScript("http://code.highcharts.com/highcharts.js"), includeScript("http://code.highcharts.com/modules/no-data-to-display.js"), 
        includeScript("http://code.highcharts.com/modules/data.js"), includeScript("http://code.highcharts.com/modules/exporting.js"), 
        includeScript(system.file("js/autocast.js", package = "ShinyAutoCast"))), title = "AutoCast", 
        tags$h2("AutoCast: time-series cross-sectional demographic forecasting"), fluidRow(column(2, 
            align = "center", uiOutput("navPrev")), column(8, align = "center", uiOutput("selectForecast")), 
            column(2, align = "center", uiOutput("navNext"))), wellPanel(fluidRow(conditionalPanel(condition = "input.toggleMore == null | input.toggleMore % 2 == 0", 
            column(8, align = "left", sliderInput("tradeoff", NULL, min = 0, max = 100, value = 50, 
                step = 1))), conditionalPanel(condition = "input.toggleMore != null & input.toggleMore % 2 == 1", 
            column(2, align = "center", div(class = "pushDown", numericInput("w_mse", "Weight on fit:", 
                value = 70, min = 0, max = 100, step = 1))), column(2, align = "center", numericInput("w_age", 
                "Weight on age smoothness:", value = 10, min = 0, max = 100, step = 1)), column(2, 
                align = "center", numericInput("w_time", "Weight on time smoothness:", value = 10, 
                  min = 0, max = 100, step = 1)), column(2, align = "center", numericInput("w_agetime", 
                "Weight on age/time smoothness:", value = 10, min = 0, max = 100, step = 1))), 
            column(4, align = "left", actionButton("toggleMore", label = "More control", icon = icon("level-down")))), 
            fluidRow(column(4, align = "left", uiOutput("dynamicButton")), column(2, align = "left", 
                checkboxInput("showHistograms", label = "Show diagnostics", value = FALSE)))), 
        hr(), fluidRow(column(6, align = "center", plotOutput("timePlot", height = "400px")), 
            column(6, align = "center", plotOutput("agePlot", height = "400px"))), conditionalPanel(condition = "input.showHistograms", 
            fluidRow(column(12, align = "center", plotOutput("histograms", height = "300px")))), 
        hr(), wellPanel(fluidRow(column(12, align = "left", tags$h4("Selected weights:"))), 
            fluidRow(class = "selectedWeights", column(10, selectizeInput("selectedWeights", 
                label = NULL, choices = c(), multiple = TRUE, options = list(create = TRUE))), 
                column(1, align = "center", actionButton("downloadButton", label = "Save", 
                  icon = icon("download"))), column(1, align = "center", actionButton("assignButton", 
                  label = "Assign", icon = icon("arrow-left"))))), fluidRow(column(6, align = "center", 
            tags$h4("Time Profile of Selected Forecasts")), column(6, align = "center", tags$h4("Age Profile of Selected Forecasts"))), 
        fluidRow(column(6, align = "left", selectizeInput("selectedAges", "Select ages:", 
            choices = ages, multiple = TRUE, options = list(create = FALSE), selected = startAges)), 
            column(6, align = "left", selectizeInput("selectedTimes", "Select times:", choices = times, 
                multiple = TRUE, options = list(create = FALSE), selected = startTimes))), 
        fluidRow(column(6, align = "center", tags$div(id = "timeplot", style = "width: 100%; margin: 0 auto")), 
            column(6, align = "center", tags$div(id = "ageplot", style = "width: 100%; margin: 0 auto"))), 
        hr(), fluidRow(column(12, align = "left", tags$p("\u00A9 2015 Konstantin Kashin, Gary King, and Samir Soneji. This work is licensed under a", 
            tags$a(href = "http://creativecommons.org/licenses/by-nc-sa/4.0/", "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License"), 
            ".")))), server = function(input, output, session) {
        selectedWeights <- selectedWeightsTemplate
        rvalues <- reactiveValues(priorWeight = NULL)
        reactivePosition <- reactiveValues(i = 1)
        
        ################################################ NAVIGATING FORECASTS #################
        
        ### render UI for prev / next buttons
        output$navPrev <- renderUI({
            if (isList) {
                return(actionButton("prevButton", label = "Previous", icon = icon("arrow-left")))
            } else {
                return(invisible())
            }
        })
        
        output$navNext <- renderUI({
            if (isList) {
                return(actionButton("nextButton", label = "Next", icon = icon("arrow-right")))
            } else {
                return(invisible())
            }
        })
        
        observe({
            if (is.null(input$prevButton)) {
                return()
            }
            if (input$prevButton == 0) {
                return()
            }
            isolate({
                reactivePosition$i <- reactivePosition$i - 1
                updateSelectInput(session, "selectForecast", selected = forecastNames[reactivePosition$i])
            })
            
        })
        
        observe({
            if (is.null(input$nextButton)) {
                return()
            }
            if (input$nextButton == 0) {
                return()
            }
            isolate({
                reactivePosition$i <- reactivePosition$i + 1
                updateSelectInput(session, "selectForecast", selected = forecastNames[reactivePosition$i])
            })
            
        })
        
        ### render UI for select
        output$selectForecast <- renderUI({
            if (isList) {
                return(selectInput("selectForecast", label = NULL, choices = forecastNames, 
                  selected = forecastNames[1]))
            } else {
                return(invisible())
            }
        })
        
        ### when change forecast from dropdown menu, change i
        observe({
            if (is.null(input$selectForecast)) {
                return()
            } else {
                current.i <- which(forecastNames == input$selectForecast)
                isolate({
                  if (reactivePosition$i == current.i) {
                    return()
                  } else {
                    reactivePosition$i <- current.i
                  }
                })
            }
        })
        
        ### if i changes, send message to enable or disable buttons
        observe({
            # add reactivity to prevButton so that it is disabled at startup; otherwise message is
            # sent before button is rendered
            input$prevButton
            
            if (reactivePosition$i == 1) {
                prevDisable <- TRUE
                nextDisable <- FALSE
            } else if (reactivePosition$i == length(forecastNames)) {
                prevDisable <- FALSE
                nextDisable <- TRUE
            } else {
                prevDisable <- FALSE
                nextDisable <- FALSE
            }
            session$sendCustomMessage(type = "disableNav", message = list(prevDisable = prevDisable, 
                nextDisable = nextDisable))
        })
        
        
        
        ############## ON FLUSH (STORE WEIGHTS) ################
        
        ### onFlush fxn is run right before Shiny flushes (stores previous value of weights) need
        ### this in order to properly update weight input widgets when toggle between slider and
        ### input boxes (needs to remember last entered weight)
        session$onFlush(once = FALSE, function() {
            isolate({
                rvalues$priorWeight <- getOptim()$weights
            })
        })
        
        
        ############################################# INPUTING WEIGHTS #################
        
        ### send message to javascript to toggle between more / fewer details button responsive to
        ### toggleMore button
        observe({
            session$sendCustomMessage(type = "updateDetailToggle", message = list(name = "toggleMore", 
                val = as.numeric(input$toggleMore))  # need as.numeric otherwise formatC throws warning
)
        })
        
        observe({
            if (!is.null(input$tradeoff)) {
                session$sendCustomMessage(type = "updateSliderLabels", message = list(val = TRUE))
            }
        })
        
        ### on toggleMore, set other input (slider or numeric) to the one that was previously
        ### selected
        observe({
            if (is.null(input$toggleMore) | input$toggleMore == 0) {
                return()
            }
            
            if (input$toggleMore%%2 == 0) {
                isolate({
                  prevWeights <- rvalues$priorWeight
                })
                updateSliderInput(session, "tradeoff", value = (100 - prevWeights[1]))
            } else {
                isolate({
                  prevWeights <- rvalues$priorWeight
                  if (!is.null(prevWeights)) {
                    # round to nearest tenth
                    prevWeights <- round(prevWeights, 1)
                    updateNumericInput(session, "w_mse", value = prevWeights[1])
                    updateNumericInput(session, "w_age", value = prevWeights[2])
                    updateNumericInput(session, "w_time", value = prevWeights[3])
                    updateNumericInput(session, "w_agetime", value = prevWeights[4])
                  }
                })
            }
        })
        
        ############################################# SELECTING WEIGHTS #################
        
        
        ### toggle between select and remove button (based on whether current weight combination is
        ### already selected) reactive to changing weights, toggling input, and selectedWeights
        ### note: determine if weight combination is already selected using weights rounded to
        ### nearest tenth
        output$dynamicButton <- renderUI({
            weight.values.slider <- c(100 - input$tradeoff, input$tradeoff/3, input$tradeoff/3, 
                input$tradeoff/3)
            weight.values.detail <- c(input$w_mse, input$w_age, input$w_time, input$w_agetime)
            weight.values.detail[is.na(weight.values.detail)] <- 0
            weight.values.detail <- weight.values.detail/sum(weight.values.detail) * 100
            
            # isolate toggleMore because it's enough to fire if input changes
            if (isolate({
                input$toggleMore
            })%%2 == 0) {
                weight.values <- weight.values.slider
            } else {
                weight.values <- weight.values.detail
            }
            weight.value.text <- paste(round(weight.values, 1), collapse = "-")
            
            if (any(input$selectedWeights == weight.value.text)) {
                return(actionButton("removeButton", label = paste("Remove weights (", weight.value.text, 
                  ")", sep = ""), icon("minus")))
            } else {
                return(actionButton("selectButton", label = paste("Save weights (", weight.value.text, 
                  ")", sep = ""), icon("plus")))
            }
        })
        
        
        ### define select button once click on select button, take current weights and send to
        ### selectize (responsive to button)
        observe({
            if (is.null(input$selectButton)) {
                return()
            }
            if (input$selectButton == 0) {
                return()
            }
            w <- isolate(getOptim()$weights)
            selectedWeights[[paste(round(w, 1), collapse = "-")]] <<- w
            
            updateSelectInput(session, "selectedWeights", choices = names(selectedWeights), 
                selected = names(selectedWeights))
        })
        
        ### define remove button once click on select button, take current weights and remove from
        ### selectize (responsive to button)
        observe({
            if (is.null(input$removeButton)) {
                return()
            }
            if (input$removeButton == 0) {
                return()
            }
            # round to nearest tenth
            w <- paste(round(isolate(getOptim()$weights), 1), collapse = "-")
            selectedWeights[[w]] <<- NULL
            
            updateSelectInput(session, "selectedWeights", choices = names(selectedWeights), 
                selected = names(selectedWeights))
        })
        
        ### once click on selectize item, update weight combination (responsive to clickedWeight
        ### from js)
        observe({
            if (!is.null(input$clickedWeight)) {
                clickedWeights <- as.numeric(strsplit(input$clickedWeight$weight, "-")[[1]])
                
                if (isolate(input$toggleMore)%%2 == 0) {
                  updateSliderInput(session, "tradeoff", value = (100 - clickedWeights[1]))
                } else {
                  updateNumericInput(session, "w_mse", value = clickedWeights[1])
                  updateNumericInput(session, "w_age", value = clickedWeights[2])
                  updateNumericInput(session, "w_time", value = clickedWeights[3])
                  updateNumericInput(session, "w_agetime", value = clickedWeights[4])
                }
            }
        })
        
        ########################################################### SAVING / OUTPUTTING SELECTED WEIGHTS ################
        
        ############## DOWNLOAD BUTTON ################
        
        ### define download button responsive to download button
        observe({
            if (input$downloadButton == 0) {
                return()
            }
            isolate({
                saveRDS(selectedWeights, file = out.file)
                message("Saved weights as ", out.file)
            })
        })
        
        
        ############## ASSIGN BUTTON ################
        
        ### define assign button responsive to assign button
        observe({
            if (input$assignButton == 0) {
                return()
            }
            isolate({
                assign(out.object, selectedWeights, envir = env)
                message("Assigned weights to ", out.object, " to ", environmentName(env),  " environment.")
            })
        })
        
        ####################################################### OBJECTIVE FUNCTION CALCULATION ################
        
        getOptim <- reactive({
            weight.values.slider <- c(100 - input$tradeoff, input$tradeoff/3, input$tradeoff/3, 
                input$tradeoff/3)
            weight.values.detail <- c(input$w_mse, input$w_age, input$w_time, input$w_agetime)
            weight.values.detail[is.na(weight.values.detail)] <- 0
            weight.values.detail <- weight.values.detail/sum(weight.values.detail) * 100
            
            # isolate toggleMore so that graphs aren't replotted when click 'more control'
            if (isolate(input$toggleMore)%%2 == 0) {
                weight.values <- weight.values.slider
            } else {
                weight.values <- weight.values.detail
            }
            gridObject <- x[[reactivePosition$i]]
            
            d <- gridObject$validation$diags
            obj.fxn <- rowSums(d * rep(weight.values, each = nrow(d)))
            opt <- which.min(obj.fxn)
            sigma.opt <- gridObject$validation$sigma[opt, ]
            
            a <- array(data = NA, dim = c(length(gridObject$aux$ages), length(gridObject$aux$times), 
                2), dimnames = list(age = gridObject$aux$ages, time = gridObject$aux$times, 
                variable = c("y", "yhat")))
            
            a[rownames(gridObject$validation$yhat.list[[opt]]), colnames(gridObject$validation$yhat.list[[opt]]), 
                "yhat"] <- gridObject$validation$yhat.list[[opt]]
            a[rownames(gridObject$y), colnames(gridObject$y), "y"] <- gridObject$y
            a <- dcast(melt(a), age + time ~ variable)
            attr(a, "sample.frame") <- gridObject$aux$args.yourcast$sample.frame
            
            ### dat for plots
            dat <- list()
            dat$weights <- weight.values
            dat$opt <- opt
            dat$diags <- gridObject$validation$diags
            dat$a <- a
            dat$holdout.times <- as.numeric(gridObject$validation$holdout.years)
            return(dat)
        })
        
        ################################### PLOTS ################
        
        ##### PLOT time profile #####
        output$timePlot <- renderPlot({
            dat <- getOptim()
            print(timeplot(d = dat$a, uncertainty = F, holdout.times = dat$holdout.times, 
                insample.forecast = T))
        })
        
        ##### PLOT age profile #####
        output$agePlot <- renderPlot({
            dat <- getOptim()
            print(ageplot(d = dat$a, uncertainty = F))
        })
        
        ##### PLOT histograms of diagnostics #####
        output$histograms <- renderPlot({
            dat <- getOptim()
            colnames(dat$diags) <- c("MSE", "Age Arc", "Time Arc", "Age/Time Arc")
            rownames(dat$diags) <- NULL
            dat$diags <- as.data.frame(dat$diags)
            diags.opt <- dat$diags[dat$opt, , drop = F]
            names(diags.opt) <- c("MSE", "Age Arc", "Time Arc", "Age/Time Arc")
            diags.melt <- melt(dat$diags, id.vars = NULL)
            diags.opt.melt <- melt(diags.opt, id.vars = NULL)
            suppressMessages(print(ggplot(data = diags.melt, aes_string(x = 'value')) + geom_histogram(position = "identity") + 
                facet_grid(~variable, scales = "free_x") + geom_vline(data = diags.opt.melt, 
                aes_string(xintercept = 'value'), color = "red", size = 2, alpha = 0.5) + scale_x_continuous("Value of Diagnostic") + 
                scale_y_continuous("Number of Forecasts") + theme_bw()))
        })
        
        
        ##### PLOT age and time profiles for selected weight combinations 
        ##### Triggers: reactivePosition$i, input$selectedAges, input$selectedTimes, input$SelectedWeights
        ##### Output: custom message w/ age and time data read by JavaScript & used as input to
        ##### HighCharts
        observe({
            # pull correct forecast, ages, times, and scales
            gridObject <- x[[reactivePosition$i]]
            selectedAges <- input$selectedAges
            selectedTimes <- input$selectedTimes
            
            
            isolate({
                ages <- gridObject$aux$ages
                times <- gridObject$aux$times
                holdout.years <- as.numeric(gridObject$validation$holdout.years)
                
                selectedAges <- selectedAges[selectedAges %in% ages]
                if (is.null(selectedAges)) {
                  selectedAges <- ages[c(1, length(ages))]
                }
                selectedTimes <- selectedTimes[selectedTimes %in% times]
                if (is.null(selectedTimes)) {
                  selectedTimes <- times[c(1, length(times))]
                }
                
                updateSelectInput(session, "selectedAges", choices = ages, selected = selectedAges)
                updateSelectInput(session, "selectedTimes", choices = times, selected = selectedTimes)
                
                # reorder selected ages and times
                selectedAges <- sort(selectedAges)
                selectedTimes <- sort(selectedTimes)
            })  # end of isolate 
            
            
            ageScale <- scales::gradient_n_pal(colours = rainbow(7), values = ages)
            timeScale <- scales::gradient_n_pal(colours = rainbow(7), values = times)
            
            if (!is.null(input$selectedWeights)) 
                { 
                  ### get list of selected forecasts (based on optimizing objective fxn)
                  selectList <- lapply(input$selectedWeights, function(w) {
                    weight.values <- as.numeric(strsplit(w, "-")[[1]])
                    d <- gridObject$validation$diags
                    obj.fxn <- rowSums(d * rep(weight.values, each = nrow(d)))
                    opt <- which.min(obj.fxn)
                    sigma.opt <- gridObject$validation$sigma[opt, ]
                    yhat <- gridObject$validation$yhat.list[[opt]]
                    return(list(weight = w, opt = opt, sigma = sigma.opt, yhat = yhat))
                  })
                  
                  
                  ### get data for time profile (for selected ages)
                  outTime <- lapply(selectedAges, function(a) {
                    # observed data
                    ytemp <- gridObject$y[as.character(a), ]
                    yvalid <- ytemp[as.character(holdout.years)]
                    yobs <- ytemp[!(names(ytemp) %in% as.character(holdout.years))]
                    
                    yvalidVals <- lapply(1:length(yvalid), function(i) list(x = as.numeric(names(yvalid)[i]), 
                      y = as.numeric(yvalid[i])))
                    names(yvalidVals) <- NULL
                    
                    yobsVals <- lapply(1:length(yobs), function(i) list(x = as.numeric(names(yobs)[i]), 
                      y = as.numeric(yobs[i])))
                    names(yobsVals) <- NULL
                    
                    yobsList <- list(data = yobsVals, name = a, color = ageScale(as.numeric(a)), 
                      datatype = "Observed", age = a, type = "scatter", marker = list(radius = 2, 
                        symbol = "circle"))
                    yvalidList <- list(data = yvalidVals, name = paste(a, "valid", sep = "-"), 
                      linkedTo = ":previous", color = ageScale(as.numeric(a)), age = a, datatype = "Holdout", 
                      type = "scatter", marker = list(symbol = "cross", lineWidth = 1, lineColor = NULL))
                    
                    out <- list(yobsList, yvalidList)
                    
                    # get list of forecast data
                    fcasts <- lapply(selectList, function(f) {
                      fa <- f$yhat[a, ]
                      vals <- lapply(1:length(fa), function(i) list(x = as.integer(names(fa[i])), 
                        y = as.numeric(fa[i])))
                      return(list(data = vals, type = "line", name = paste("Age75", f$weight, 
                        sep = "-"), weight = f$weight, sigmaHa = round(as.numeric(f$sigma[1]), 
                        2), sigmaHt = round(as.numeric(f$sigma[2]), 2), sigmaHat = round(as.numeric(f$sigma[3]), 
                        2), age = a, datatype = "Forecast", linkedTo = ":previous", color = ageScale(as.numeric(a)), 
                        marker = list(enabled = FALSE, symbol = "circle"), enableMouseTracking = TRUE, 
                        states = list(hover = list(lineWidth = 0))))
                    })
                    names(fcasts) <- NULL
                    
                    out <- append(out, fcasts)
                    return(out)
                  })
                  outTime <- unlist(outTime, recursive = F)
                  
                  # get data for age profile (for selected times)
                  outAge <- lapply(selectedTimes, function(t) {
                    # get list of forecast data
                    fcasts <- lapply(selectList, function(f) {
                      fa <- f$yhat[, t]
                      vals <- lapply(1:length(fa), function(i) list(as.integer(names(fa[i])), 
                        as.numeric(fa[i])))
                      return(list(weight = f$weight, sigmaHa = round(as.numeric(f$sigma[1]), 
                        2), sigmaHt = round(as.numeric(f$sigma[2]), 2), sigmaHat = round(as.numeric(f$sigma[3]), 
                        2), data = vals, type = "line", name = paste(t, f$weight, sep = "-"), 
                        time = t, linkedTo = ":previous", color = timeScale(as.numeric(t)), 
                        marker = list(enabled = FALSE, symbol = "circle"), enableMouseTracking = TRUE, 
                        states = list(hover = list(lineWidth = 2))))
                    })
                    # fix to have unlinked legend & clean label for first weight combo in time
                    names(fcasts) <- NULL
                    fcasts[[1]]$linkedTo <- NULL
                    fcasts[[1]]$name <- t
                    
                    return(fcasts)
                  })
                  outAge <- unlist(outAge, recursive = F)
                  
                  
                  session$sendCustomMessage(type = "selectedWeightPlots", message = list(dataTime = outTime, 
                    dataAge = outAge)  # need as.numeric otherwise formatC throws warning
)
                }  # end of !is.null(selectedWeights)
        })  # end of observe for age and time profiles
        
        
    }, options=options)
} 
