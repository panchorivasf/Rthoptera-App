Rthoptera <- function() {
  
  # Function to check if a package is installed and install if necessary
  check_and_install <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      if (pkg == "brochure") {
        if (!require("remotes", character.only = TRUE)) {
          install.packages("remotes", dependencies = TRUE)
          library("remotes", character.only = TRUE)
        }
        remotes::install_github("ColinFay/brochure")     
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
      library(pkg, character.only = TRUE)
    }
  }
  
  
  # List of packages to be used
  packages <- c(
    "shiny", "shinyWidgets", "shinycssloaders",
    "bslib", "brochure", "cli", "openxlsx",
    "signal", "bioacoustics", "tuneR", "seewave", # "warbleR",
    "ggplot2", "plotly", "cowplot","patchwork",
    "dplyr",   "tidyr", "magrittr","DT", "openxlsx" 
  )
  
  # Check and install each package
  invisible(lapply(packages, check_and_install))
  
  
  log_where <- function(req) {
    cli::cat_rule(sprintf("%s - %s", Sys.time(), req$PATH_INFO))
    req
  }
  
  nav_links <- function() {
    tags$head(tags$style(
      HTML(
        "
      .nav-links ul {
        list-style-type: none;
        margin: 5;
        padding: 5;
        display: flex;
        background-color: #212121;
      }
      .nav-links li {
        margin-right: 20px; /* Spacing between buttons */
      }
      .nav-links li a {
        display: block;
        color: white;
        padding: 10px 15px; /* Adjust padding to control button size */
        text-decoration: none;
        background-color: #555;
        font-size: 12px; /* Adjust font size for the text */
        border-radius: 5px; /* Optional: Adds rounded corners to the buttons */
      }
      .nav-links li a:hover {
        background-color: #777;
      }
    "
      )
    ), div(class = "nav-links", tags$ul(
      tags$li(tags$a(href = "/", "Import")), 
      tags$li(tags$a(href = "/page2", "Downsample")),
      tags$li(tags$a(href = "/page3", "High-pass Filter")), 
      tags$li(tags$a(href = "/page4", "Trim")),
      tags$li(tags$a(href = "/page5", "Spectral Statistics")),
      tags$li(tags$a(href = "/page6", "Multi-Power Spectra")),
      tags$li(tags$a(href = "/page7", "Temporal Statistics")),
      tags$li(tags$a(href = "/page8", "Spectrogram")),
      tags$li(tags$a(href = "/page9", "Oscillogram")),
      tags$li(tags$a(href = "/page10", "Multiplot")),
      tags$li(tags$a(href = "/page11", "Multi-oscillogram"))
      
      
    )))
  }
  
  # Page 1 Import
  page_1 <- function() {
    brochure::page(
      href = "/",
      ui = function(request) {
        tagList(
          h1("Import", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
                 body {
                margin: 5px; /* Adds margin around the entire page */
              }
              #audioPlot {
                height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 5px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 3px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }
              "
              )
            )),
            fluidRow(
              column(3, fileInput(
                "audioFile",
                "Choose an audio file",
                accept = c("audio/wav", ".wav", ".mp3", ".wac")
              )),
              column(2, verticalLayout(
                textInput("newName", "Name for new wave:", value = ""),
                actionButton("saveEditedWave", "Save")
              ))
            )
          )
        )
      },
      server = function(input, output, session) {
        # Increase maximum file size to 100 MB
        options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
        
        # Reactive value to store wave object
        waveObject <- reactiveVal(NULL)
        
        # Function to read and process audio file
        read_and_process_audio <- function(filepath) {
          wave <- read_audio(filepath)
          wave <- rmoffset(wave, output = "Wave")
          wave
        }
        
        # Observe file input and read the audio file
        observeEvent(input$audioFile, {
          req(input$audioFile)
          tryCatch({
            wave <- read_and_process_audio(input$audioFile$datapath)
            waveObject(wave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to read the audio file. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        # Observe save button and save the wave object
        observeEvent(input$saveEditedWave, {
          req(waveObject(), input$newName)
          tryCatch({
            assign(input$newName, waveObject(), envir = .GlobalEnv)
            showModal(modalDialog(
              title = "Saved!",
              paste0("Available as '", input$newName, "' in the R environment."),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to save the wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })

        
        # Save state before switching pages
        session$onSessionEnded(function() {
          # Code to save the current state of the page
          # Example: save waveObject() and other inputs to a session-specific storage
        })
      }
    )
  }
  
  # Page 2 Downsample
  page_2 <- function() {
    brochure::page(
      href = "/page2",
      ui = function(request) {
        tagList(
          h1("Downsample", 
             style = "font-size: 28px; margin-left: 15px; margin-top: 0px; 
             margin-bottom: 2px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
              body {
                margin: 5px; /* Adds margin around the entire page */
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 10px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 10px; /* Increases horizontal space between inputs */
              }
              
              "
              )
            )),
            
          tags$script(
            HTML(
              "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
              "
            )
          ),
          
          fluidRow(
            column(3, 
                   div(class = 'inline', selectInput("selectedWave", "Select a wave object:", choices = NULL, width = '90%')),
                   div(style = "margin-top: 5px;", actionButton("refresh", "Refresh", width='70%'))
            ),
            column(1, 
                   div(style = "margin-top: 20px;", actionButton("plotMeanSpectrum", "Plot"))
            ),
            column(2, 
                   div(class = 'inline', textInput("newName", "Name for new wave:", value = "")),
                   div(style = "margin-top: 5px;", actionButton("saveEditedWave", "Save"))
            ),
            column(2, 
                   div(class = 'inline', selectInput('maxfreq', 'New Max. Freq. (kHz):', choices = c(48, 96, 125))),
                   div(style = "margin-top: 5px;", actionButton("downsample", "Downsample"))
            )

          ),
          
          fluidRow(
            column(12, 
                   div(style = "margin-top: 15px;",
                       withSpinner(plotlyOutput("audioPlot", height = "500px", width = "1480px"), type = 6)))
          )
        )
        )
      },
      
      server = function(input, output, session) {
        waveObject <- reactiveVal(NULL)
        plotly_obj <- reactiveVal()
        
        
        meanspectrum_plotly <- function(wave,
                                        background = '#274C77',
                                        foreground = "white",
                                        hover_bgcolor = "white",
                                        hover_fontcolor = "black") {
          mean_spectrum <- seewave::meanspec(wave, 
                                             f = wave@samp.rate,
                                             wl = 2048, 
                                             ovlp = 50, 
                                             plot = FALSE)
          mean_spectrum_df <- data.frame(
            freq = mean_spectrum[, 1], 
            mean_amp = mean_spectrum[, 2]  
          )
          
          plot_ly(data = mean_spectrum_df, x = ~freq, y = ~mean_amp, type = 'scatter', mode = 'lines', line = list(color = 'white')) %>%
            add_ribbons(ymin = 0, ymax = ~mean_amp, fillcolor = foreground, line = list(color = foreground)) %>%
            layout(
              title = "",
              xaxis = list(
                title = "Frequency (kHz)",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                ticks = "outside",
                tickcolor = foreground,
                tickwidth = 1,
                linecolor = foreground,
                ticklen = 5,
                automargin = TRUE,
                zeroline = FALSE,
                showline = TRUE
              ),
              yaxis = list(
                title = "Mean Amplitude",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                ticks = "outside",
                tickvals = pretty(mean_spectrum_df$mean_amp, n = 3),
                tickcolor = foreground,
                tickwidth = 1,
                ticklen = 5,
                rangemode= 'tozero',
                linecolor = foreground,
                zeroline = FALSE,
                showline = TRUE
              ),
              paper_bgcolor = background,
              plot_bgcolor = background,
              shapes = list(
                list(
                  type = "line",
                  x0 = 0,
                  x1 = max(mean_spectrum_df$freq),
                  xref = "x",
                  y0 = 0.1,
                  y1 = 0.1,
                  yref = "y",
                  line = list(
                    color = foreground,
                    dash = "dot"
                  )
                )
              ),
              margin = list(
                l = 50,  
                r = 10, 
                b = 60, 
                t = 50   
              ),
              showlegend = FALSE
            ) %>%
            config(displayModeBar = TRUE) %>% 
            style(
              hovertemplate = paste0(
                "Frequency: %{x:.1f} kHz<br>", 
                "<extra></extra>"  
              )
            )
        }
        
        # Function to update wave object choices
        update_wave_choices <- function() {
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "selectedWave", choices = waveObjects)
        }
        
        # Observe to update wave object choices initially and on refresh
        observe({
          update_wave_choices()
        })
        
        observeEvent(input$refresh, {
          update_wave_choices()
        })
        
        # Update the reactive waveObject whenever the selection changes
        observeEvent(input$selectedWave, {
          req(input$selectedWave)
          tryCatch({
            newWave <- get(input$selectedWave, envir = .GlobalEnv)
            waveObject(newWave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to load the selected wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        
        output$audioPlot <- renderPlotly({
          req(waveObject())
          req(input$plotMeanSpectrum)
          p <- meanspectrum_plotly(waveObject())
          
          plotly_obj(p)
          p
          
        })
        
        
        
        
        observeEvent(input$downsample, {
          req(waveObject())
          tryCatch({
            resampled_wave <- resamp(waveObject(),
                                     g = (as.numeric(input$maxfreq) * 1000) * 2, # from kHz to Hz , from Nyquist to sampling rate
                                     output = "Wave"
            )
            waveObject(resampled_wave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to downsample the wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        observeEvent(input$saveEditedWave, {
          req(waveObject(), input$newName)
          tryCatch({
            assign(input$newName, waveObject(), envir = .GlobalEnv)
            showModal(modalDialog(
              title = "Saved!",
              paste0("Available as '", input$newName, "' in the R environment."),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to save the wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
      }
    )
  }
  
  # Page 3 HPF
  page_3 <- function() {
    brochure::page(
      href = "/page3",
      
      ui = function(request) {
        tagList(
          h1("High-pass Filter", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
                 body {
                margin: 5px; /* Adds margin around the entire page */
              }
 
              .btn-group-vertical > .btn {
                margin-bottom: 5px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 3px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }
              "
              )
            )),
            
            tags$script(
              HTML(
                "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
              "
              )
            ),
            
            fluidRow(
              column(3,
                     selectInput("selectedWave", "Select a wave object:", 
                                 choices = NULL, width = '100%'),
                     actionButton("refresh", "Refresh List")
              ),
              column(3,
                     actionButton("plotMeanSpectrum", "Mean Spectrum"),
                     actionButton("plotSpectro", "Spectrogram")
              ),
              
              column(
                1,
                numericInput(
                  "filterValue",
                  "HPF (kHz)",
                  value = 0,
                  min = 0
                )
              ),
              column(
                2, actionButton("applyFilter", "Apply Filter")
              )
              ,
              
              column(2, verticalLayout(
                textInput("newName", "Name for new wave:", value = ""),
                actionButton("saveEditedWave", "Save")
              )),
            ),
           

            fluidRow(
              column(12,
                     div(style = "margin-top: 15px;",
                         withSpinner(plotlyOutput("audioPlot", height = "500px", width = "1480px"), type = 6)))
            )
          )
        )
      },
      
      server = function(input, output, session) {
        
        waveObject <- reactiveVal(NULL)
        # plotly_obj <- reactiveVal()
        
        spectrogram_plotly <- function(wave, 
                                       floor = -50,
                                       background = '#274C77',
                                       foreground = "white",
                                       hover_bgcolor = "white",
                                       hover_fontcolor = "black") {
          win_len <- 0.005 * wave@samp.rate
          hop_len <- 0.002 * wave@samp.rate
          overlap <- ((win_len - hop_len) / win_len) * 150
          
          spect <- wave |>
            seewave::spectro(
              wl = win_len,
              ovlp = overlap,
              plot = FALSE
            )
          
          colnames(spect$amp) <- spect$time
          rownames(spect$amp) <- spect$freq
          
          spect_df <- spect$amp |>
            as_tibble(rownames = "freq") |>
            pivot_longer(
              -freq, 
              names_to = "time", 
              values_to = "amp"
            ) |>
            mutate(
              freq = as.numeric(freq),
              time = as.numeric(time)
            )
          
          spect_df_floor <- spect_df |> 
            mutate(
              amp_floor = ifelse(amp < floor, floor, amp)
            )
          
          spect_plot <- plot_ly(
            data = spect_df_floor,
            x = ~time,
            y = ~freq,
            z = ~amp_floor,
            type = "heatmap",
            colorscale = list(c(0, background), c(1, foreground)),
            zmin = floor,
            zmax = max(spect_df$amp),
            hoverinfo = "x+y", 
            hovertemplate = paste(
              "Time: %{x:.3f} s<br>",
              "Freq: %{y:.1f} kHz<extra></extra>"
            ),
            showscale = FALSE
          ) %>%
            layout(
              xaxis = list(
                title = "Time (s)",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                tickcolor = foreground,
                linecolor = foreground, 
                mirror = TRUE
              ),
              yaxis = list(
                title = "Frequency (kHz)",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                tickcolor = foreground,
                linecolor = foreground, 
                mirror = TRUE
              ),
              paper_bgcolor = background,
              plot_bgcolor = background,
              margin = list(t = 25, r = 15, b = 55, l = 25),
              title = "",
              showlegend = FALSE 
            ) %>%
            style(
              hoverlabel = list(
                bgcolor = hover_bgcolor,
                font = list(color = hover_fontcolor)
              )
            )
          
          return(spect_plot)
        }
        
        meanspectrum_plotly <- function(wave,
                                        background = '#274C77',
                                        foreground = "white",
                                        hover_bgcolor = "white",
                                        hover_fontcolor = "black") {
          mean_spectrum <- seewave::meanspec(wave, 
                                             f = wave@samp.rate,
                                             wl = 2048, 
                                             ovlp = 50, 
                                             plot = FALSE)
          mean_spectrum_df <- data.frame(
            freq = mean_spectrum[, 1], 
            mean_amp = mean_spectrum[, 2]  
          )
          
          plot_ly(data = mean_spectrum_df, x = ~freq, y = ~mean_amp, type = 'scatter', mode = 'lines', line = list(color = 'white')) %>%
            add_ribbons(ymin = 0, ymax = ~mean_amp, fillcolor = foreground, line = list(color = foreground)) %>%
            layout(
              title = "",
              xaxis = list(
                title = "Frequency (kHz)",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                ticks = "outside",
                tickcolor = foreground,
                tickwidth = 1,
                linecolor = foreground,
                ticklen = 5,
                automargin = TRUE,
                zeroline = FALSE,
                showline = TRUE
              ),
              yaxis = list(
                title = "Mean Amplitude",
                titlefont = list(size = 10, color = foreground),
                tickfont = list(size = 10, color = foreground),
                ticks = "outside",
                tickvals = pretty(mean_spectrum_df$mean_amp, n = 3),
                tickcolor = foreground,
                tickwidth = 1,
                ticklen = 5,
                rangemode= 'tozero',
                linecolor = foreground,
                zeroline = FALSE,
                showline = TRUE
              ),
              paper_bgcolor = background,
              plot_bgcolor = background,
              shapes = list(
                list(
                  type = "line",
                  x0 = 0,
                  x1 = max(mean_spectrum_df$freq),
                  xref = "x",
                  y0 = 0.1,
                  y1 = 0.1,
                  yref = "y",
                  line = list(
                    color = foreground,
                    dash = "dot"
                  )
                )
              ),
              margin = list(
                l = 50,  
                r = 10, 
                b = 60, 
                t = 50   
              ),
              showlegend = FALSE
            ) %>%
            config(displayModeBar = TRUE) %>% 
            style(
              hovertemplate = paste0(
                "Frequency: %{x:.1f} kHz<br>", 
                "<extra></extra>"  
              )
            )
        }
        
        
        # Function to update wave object choices
        update_wave_choices <- function() {
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "selectedWave", choices = waveObjects)
        }
        
        # Observe to update wave object choices initially and on refresh
        observe({
          update_wave_choices()
        })
        
        observeEvent(input$refresh, {
          update_wave_choices()
        })
        
        # Update the reactive waveObject whenever the selection changes
        observeEvent(input$selectedWave, {
          req(input$selectedWave)
          tryCatch({
            newWave <- get(input$selectedWave, envir = .GlobalEnv)
            waveObject(newWave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to load the selected wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        output$audioPlot <- renderPlotly({
          req(waveObject())
          req(input$plotMeanSpectrum)
          meanspectrum_plotly(waveObject())
        })
        
        
        observeEvent(input$plotMeanSpectrum, {
          req(waveObject())
          output$audioPlot <- renderPlotly({
            meanspectrum_plotly(waveObject())
          })
        })
        
        output$audioPlot <- renderPlotly({
          req(waveObject())
          req(input$plotSpectro)
          spectrogram_plotly(waveObject())
        })
        
        
        observeEvent(input$plotSpectro, {
          req(waveObject())
          output$audioPlot <- renderPlotly({
            spectrogram_plotly(waveObject())
          })
        })
        
        observeEvent(input$applyFilter, {
          req(waveObject())
          tryCatch({
            filtered_wave <- ffilter(
              waveObject(),
              from = (input$filterValue) * 1000,
              output = "Wave"
            )
            waveObject(filtered_wave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to apply the high-pass filter. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        observeEvent(input$saveEditedWave, {
          req(waveObject(), input$newName)
          tryCatch({
            assign(input$newName, waveObject(), envir = .GlobalEnv)
            showModal(modalDialog(
              title = "Saved!",
              paste0("Available as '", input$newName, "' in the R environment."),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to save the wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        # Save state before switching pages
        session$onSessionEnded(function() {
          # Code to save the current state of the page
          # Example: save waveObject() and other inputs to a session-specific storage
        })
        
        
      }
      
    )
  }
  
  # Page 4 Trim 
  page_4 <- function() {
    brochure::page(
      href = "/page4",
      
      ui =  function(request) {
        tagList(
          h1("Trim", style = "font-size: 28px; margin-left: 15px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
               body {
              margin: 5px; /* Adds margin around the entire page */
            }
            
            .btn-group-vertical > .btn {
              margin-bottom: 10px; /* Adds space between vertical buttons */
            }
            .row {
              margin-bottom: 10px; /* Adds vertical space between rows */
            }
            .shiny-input-container {
              margin-right: 2px; /* Reduces horizontal space between inputs */
            }
            "
              )
            )),
            tags$script(
              HTML(
                "
            Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
              if (message) {
                document.getElementById('spinner').style.display = 'block';
              } else {
                document.getElementById('spinner').style.display = 'none';
              }
            });
            "
              )
            ),
            
            fluidRow(
              column(
                3,
                selectInput("selectedWave", "Select a wave object:", 
                            choices = NULL, width = '100%'),
                actionButton("refresh", "Refresh List"),
                actionButton("plot", "Plot Waveform")
              ),
              column(3, verticalLayout(
                actionButton("zoomIn", "Zoom In"),
                actionButton("zoomOut", "Zoom Out")
              )),
              column(3, 
                     actionButton("resetView", "Reset View")
              ),
              column(
                3,
                verticalLayout(
                  textInput("selectionName", "Name for the selection:"),
                  actionButton("saveSelection", "Save Selection")
                )
              )
            ),
            
            fluidRow(
              column(11, 
                     div(style = "margin-top: 15px;",
                         withSpinner(plotOutput("wavePlot", 
                                                brush = brushOpts(id = "waveBrush", direction = "x"),
                                                height = "500px", width = "1480px"), type =6))) 
            )
          )
        )
      },
      
      server = function(input, output, session) {
        
        waveObject <- reactiveVal(NULL)
        plotVisible <- reactiveVal(TRUE)
        selectedRegion <- reactiveVal(NULL)
        zoomedRegion <- reactiveVal(NULL)
        
        oscillo3 <- function(wave) {
          oscillo_data <- seewave::oscillo(wave, plot = FALSE)
          time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
          amplitude <- oscillo_data / max(abs(oscillo_data))
          oscillo_df <- data.frame(time = time, amplitude = amplitude)
          
          ggplot(oscillo_df, aes(x = time, y = amplitude)) +
            geom_line(color = "white") +
            theme_minimal(base_size = 15) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(n.breaks = 3, expand = c(0.1, 0.1)) +
            theme(
              plot.margin = margin(t = 15, r = 10, b = 15, l = 10, unit = 'pt'),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "#274C77", color = "#274C77"),
              plot.background = element_rect(fill = "#274C77", color = "#274C77"),
              axis.line.y = element_line(colour = "white"),
              axis.line.x = element_line(colour = "white"), 
              axis.ticks.x = element_line(colour = "white"),
              axis.ticks.y = element_line(colour = "white"),
              axis.title.x = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white"),
              axis.title = element_text(size = 10, colour = "white"),
              axis.text = element_text(size = 10, colour = "white"),
              legend.position = "none"
            ) +
            labs(y = "Relative Amplitude", x = "Time (s)")
        }
        
        # Function to update wave object choices
        update_wave_choices <- function() {
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "selectedWave", choices = waveObjects)
        }
        
        # Observe to update wave object choices initially and on refresh
        observe({
          update_wave_choices()
        })
        
        observeEvent(input$refresh, {
          update_wave_choices()
        })
        
        # Update the reactive waveObject whenever the selection changes
        observeEvent(input$selectedWave, {
          req(input$selectedWave)
          tryCatch({
            newWave <- get(input$selectedWave, envir = .GlobalEnv)
            waveObject(newWave)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Failed to load the selected wave object. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          })
        })
        
        output$wavePlot <- renderPlot({
          req(input$plot)
          req(plotVisible())
          wave <- get(input$selectedWave, envir = .GlobalEnv)
          if (!is.null(wave)) {
            if (!is.null(zoomedRegion())) {
              extract <- extractWave(wave, from = zoomedRegion()[1], to = zoomedRegion()[2], xunit = "time")
              oscillo3(extract)
            } else {
              oscillo3(wave)
            }
          }
        }, height = 520) # Only specifying height to avoid conflict with CSS width
        
        observeEvent(input$waveBrush, {
          currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
          if (!is.null(zoomedRegion())) {
            currentRegion <- c(zoomedRegion()[1] + currentRegion[1], zoomedRegion()[1] + currentRegion[2])
          }
          selectedRegion(currentRegion)
        })
        
        observeEvent(input$zoomIn, {
          req(selectedRegion())
          zoomedRegion(selectedRegion())
          selectedRegion(NULL)
        })
        
        observeEvent(input$resetView, {
          zoomedRegion(NULL)
        })
        
        observeEvent(input$zoomOut, {
          if (!is.null(zoomedRegion())) {
            zoomRange <- zoomedRegion()[2] - zoomedRegion()[1]
            newStart <- max(0, zoomedRegion()[1] - zoomRange * 0.1)
            newEnd <- min(get(input$selectedWave, envir = .GlobalEnv)@samp.rate, zoomedRegion()[2] + zoomRange * 0.1)
            if (newEnd - newStart >= get(input$selectedWave, envir = .GlobalEnv)@samp.rate) {
              zoomedRegion(NULL)
            } else {
              zoomedRegion(c(newStart, newEnd))
            }
          }
        })
        
        observeEvent(input$saveSelection, {
          req(selectedRegion(), input$selectedWave, input$selectionName)
          wave <- get(input$selectedWave, envir = .GlobalEnv)
          selection <- extractWave(wave, from = selectedRegion()[1], to = selectedRegion()[2], xunit = "time")
          assign(input$selectionName, selection, envir = .GlobalEnv)
          showModal(modalDialog(
            title = "Saved",
            paste0("Available as '", input$selectionName, "' in the R environment."),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        })
        
        observeEvent(input$plot, {
          plotVisible(TRUE)
          selectedRegion(NULL)
          zoomedRegion(NULL)
        })
        
        # Save state before switching pages
        session$onSessionEnded(function() {
          # Code to save the current state of the page
          # Example: save selectedWave() and other inputs to a session-specific storage
        })
      }
    )
  }
  
  # Page 5 Spectral Statistics
  page_5 <- function() {
    brochure::page(
      href = "/page5",
      ui = function(request) {
        tagList(
          h1("Spectral Statistics", 
             style = "font-size: 28px; margin-left: 15px; margin-top: 0px; 
           margin-bottom: 2px; margin-right: 15px;"),
          nav_links(),
          
          fluidPage(
            
            theme = bslib::bs_theme(bootswatch = "darkly"),
            
            tags$head(tags$style(
              
              HTML(
                "
              body {
                margin: 5px; /* Adds margin around the entire page */
              }
              .btn-group-vertical > .btn {
                margin-bottom: 6px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 6px; /* Adds vertical space between rows */
              }
              .plotly {
                width: 95%; /* Adjust plot width */
                margin: auto; /* Center the plot */
              }
              
              .dataTables_wrapper .caption-top {
              caption-side: top !important;
              font-weight: bold;
              color: white; 
              }
                "
              )
              
            )),
            
            tags$script(
              
              HTML(
                
                "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
                "
                
              )
            ),
            
            sidebarLayout(
              sidebarPanel(
                width = 2,
                selectInput("selectedWave", "Select a Wave Object:", 
                            choices = NULL, width = '100%'),
                textInput("sp.name", "Species Name", value = ""),
                textInput("sound.type", "Sound Type", value = "Calling song"),
                numericInput("temp", "Temp (°C)", value = NA, min = 0, max = 60, step = 0.1),
                numericInput("hpf", "HPF (kHz)", value = NA, min = 0, max = 15, step = 1),
                selectInput("dbth", "dB Bandwidth Threshold", choices = c(-3, -10, -20), selected = -20),
                actionButton("submit", "Run Analysis")
                # checkboxInput("total", "Total Bandwidth", value = FALSE),
                # checkboxInput("robust", "Robust Calculation", value = FALSE),
                # selectInput("ampMax", "Amplitude Scale", choices = list("dB (max0)" = 0, "Linear" = 1), selected = 1),
                # checkboxInput("lines", "Show Lines", value = TRUE)
              ),
              mainPanel(
                fluidRow(
                  column(2, verticalLayout(
                    checkboxInput("total", "Total Bandwidth", value = FALSE)),
                    checkboxInput("robust", "Robust Calculation", value = FALSE)
                    ),
                  column(2, selectInput("ampMax", "Amplitude Scale", choices = list("dB (max0)" = 0, "Linear" = 1), selected = 1)),
                  column(1,checkboxInput("lines", "Show Lines", value = TRUE)),
                  column(3, textInput("dataName", "Name for data frame", value = "")),
                  column(2, verticalLayout(
                    actionButton("saveDataEnv", "Save to R"),
                    downloadButton("downloadData", "Save CSV")
                  )),
                  column(2,
                    downloadButton("savePlot", "Save Plot")
                  )
          
                ),
                
                
                div(style = "margin-top: 10px; margin-left: 10px; margin-right: 10px;",
                    withSpinner(uiOutput("plotOutput"), 
                                type = 6)),
                withSpinner(DTOutput("dataOutput"), type = 6)
          
              )
            )
          )
        )
      },
      server = function(input, output, session) {
        # Function definition
        specStats <- function(wave, sp.name = "Species name", sound.type = "Call 1", temp = NULL,
                              hpf = NULL, dbth = -20, total.range = FALSE, robust = FALSE, ampMax = 1, lines = FALSE) {
          require(tuneR)
          require(seewave)
          require(dplyr)
          require(plotly)
          
          ampMax <- as.numeric(ampMax)
          
          freq_per_bin <- if (robust) {
            244.1406
          } else {
            30.51758
          }
          
          sampling_rate <- wave@samp.rate
          wlen <- sampling_rate / freq_per_bin
          
          spec <- meanspec(wave, wl = wlen, dB = if (ampMax == 0) "max0" else NULL, plot = FALSE)
          spEnt <- sh(spec)
          
          spec_df <- as.data.frame(spec)
          names(spec_df) <- c("Frequency", "Amplitude")
          
          max_amp_index <- which.max(spec_df$Amplitude)
          peak_frequency <- spec_df$Frequency[max_amp_index]
          A_peak <- spec_df$Amplitude[max_amp_index]
          
          A_ref <- if (ampMax == 1) {
            A_peak * 10^(dbth / 20)
          } else {
            A_peak - abs(dbth)
          }
          
          minfreq_index <- if (total.range) {
            which(spec_df$Amplitude[1:max_amp_index] >= A_ref)[1]
          } else {
            max(which(spec_df$Amplitude[1:max_amp_index] <= A_ref))
          }
          
          maxfreq_index <- if (total.range) {
            max_amp_index + which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref)[length(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref))]
          } else {
            max_amp_index + min(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] <= A_ref)) - 1
          }
          
          minfreq <- spec_df$Frequency[minfreq_index]
          maxfreq <- spec_df$Frequency[maxfreq_index]
          
          p <- plot_ly(
            spec_df,
            x = ~ Frequency,
            y = ~ Amplitude,
            type = 'scatter',
            mode = 'lines',
            line = list(color = 'black', width = 0.5),
            hovertemplate = paste("Frequency: %{x:.2f} kHz<br>Amplitude: %{y:.2f}<extra></extra>"),
            showlegend = FALSE
          )
          
          text_label <- paste(
            "<b> Summary Statistics</b>",
            "<br> Low Freq:", round(minfreq, 1), "kHz",
            "<br> Peak Freq:", round(peak_frequency, 1), "kHz",
            "<br> High Freq:", round(maxfreq, 1), "kHz",
            "<br> Bandwidth:", round(maxfreq - minfreq, 1), "kHz",
            "<br> Temp:", temp, "°C",
            "<br> HPF:", hpf, "kHz"
          )
          
          p <- p %>%
            layout(
              yaxis = list(range = ifelse(ampMax == 0, c(-50, 0), c(0, 1))),
              margin = list(l = 50, r = 50, t = 100, b = 50),
              title = list(text = sprintf("<i>%s</i>", sp.name), x = 0, y = 1.1, xref = "paper", yref = "paper", xanchor = 'left', yanchor = 'top'),
              xaxis = list(title = "Frequency (kHz)"),
              yaxis = list(title = ifelse(ampMax == 1, "Relative Amplitude", "Amplitude (dB)")),
              annotations = list(
                x = 1, y = .95, text = text_label, showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'right', yanchor = 'top',
                font = list(size = 12), bgcolor = 'rgba(255,255,255,1)', bordercolor = '#404040', align = 'left'
              )
            )
          
          if (lines) {
            p <- p %>%
              add_trace(
                x = c(minfreq, minfreq), y = c(min(spec_df$Amplitude), A_ref),
                type = 'scatter', mode = 'lines', line = list(color = "#1E90FF", width = 1, dash = 'solid'),
                name = "Min Frequency", hovertemplate = paste("MinFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
              ) %>%
              add_trace(
                x = c(peak_frequency, peak_frequency), y = c(min(spec_df$Amplitude), max(spec_df$Amplitude)),
                type = 'scatter', mode = 'lines', line = list(color = "#EE0000", width = 1, dash = 'solid'),
                name = "Peak Frequency", hovertemplate = paste("PeakFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
              ) %>%
              add_trace(
                x = c(maxfreq, maxfreq), y = c(min(spec_df$Amplitude), A_ref),
                type = 'scatter', mode = 'lines', line = list(color = "#FF7F00", width = 1, dash = 'solid'),
                name = "Max Frequency", hovertemplate = paste("MaxFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
              ) %>%
              add_trace(
                x = spec_df$Frequency, y = rep(A_ref, nrow(spec_df)),
                type = 'scatter', mode = 'lines', line = list(color = 'forestgreen', width = 1, dash = 'dash'),
                name = "dB Threshold", hovertemplate = paste("-20 dB Threshold <extra></extra>"), showlegend = TRUE
              )
          }
          
          p <- p %>% add_markers(
            x = peak_frequency, y = max(spec_df$Amplitude), type = 'scatter', mode = 'markers',
            marker = list(symbol = 'triangle-down', color = "#EE0000", size = 10), name = "Peak", showlegend = TRUE, hoverinfo = 'none', inherit = FALSE
          )
          
          scaling <- ifelse(ampMax == 1, "max1", "max0")
          q_factor <- peak_frequency / (maxfreq - minfreq)
          
          df <- tibble(
            species = sp.name, sound.type = sound.type, low.f = round(minfreq, 1), high.f = round(maxfreq, 1),
            bandw = round(maxfreq - minfreq, 1), peak.f = round(peak_frequency, 1), q = round(q_factor, 1),
            spec.ent = round(spEnt, 1), temp = temp, par.hpf = hpf, par.dbth = dbth,
            par.samprate = sampling_rate / 1000, par.wlen = round(wlen), par.freq.res = round(freq_per_bin, 1),
            par.robust = robust, par.scale = scaling
          )
          
          list(data = df, plot = p)
        }
        
        # Store reactive values
        values <- reactiveValues(speciesName = "", callType = "")
        
        # Update the title whenever the input changes
        observe({
          values$speciesName <- input$sp.name
          values$callType <- input$sound.type
        })
        
        # Observer to update available wave objects in the environment
        observe({
          wave_names <- ls(envir = .GlobalEnv)
          wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "selectedWave", choices = wave_names)
        })
        
        # This reactive expression will re-run only when the "Plot" button is clicked
        result <- eventReactive(input$submit, {
          req(input$selectedWave)
            wave <- get(input$selectedWave, envir = .GlobalEnv)
            specStats(
              wave = wave,
              sp.name = input$sp.name,
              sound.type = input$sound.type,
              temp = input$temp,
              hpf = input$hpf,
              dbth = as.numeric(input$dbth),
              total.range = input$total,
              lines = input$lines,
              ampMax = as.numeric(input$ampMax),
              robust = input$robust
            )
        })
        
        output$plotOutput <- renderUI({
          req(result())
          plotlyOutput("plotlyPlot", width = '1150px') 
        })
        
        output$plotlyPlot <- renderPlotly({
          req(result())
          result()$plot %>% 
            layout(margin = list(
              l = 80, r = 0, t = 80, b = 80
            ),
            annotations = list(
              list(
                text = input$sound.type,
                font = list(size = 13, color = 'black'),
                showarrow = FALSE, align = 'right',
                x = 0, y = 1.1, xref = 'x', yref = 'paper'
              )
            ))
        })
        
        output$dataOutput <- renderDT({
          req(result())
          datatable(result()$data, 
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: center;",
                      class = "caption-top",
                      "Spectral Data"
                    ),
                    options = list(
            pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = "_all"))
          ))
        })
        
  
        # Download handler for downloading data
        output$downloadData <- downloadHandler(
          filename = function() {
            paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_stats_", "saved_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
          },
          content = function(file) {
            write.csv(result()$data, file, row.names = FALSE)
          }
        )
        
        # Save data frame in the R environment
        observeEvent(input$saveDataEnv, {
          req(result(), input$dataName)
          assign(input$dataName, result()$data, envir = .GlobalEnv)
          showModal(modalDialog(
            title = "Saved",
            paste0("Available as '", input$dataName, "' in the R environment."),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        })
        
        # Save plot
        output$savePlot <- downloadHandler(
          filename = function() {
            paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_analysis_", "saved_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
          },
          content = function(file) {
            htmlwidgets::saveWidget(result()$plot, file, selfcontained = TRUE)
          }
        )
        
      }
    )
  }
  
  # Page 6 Multi Power Spectra
  page_6 <- function() {
    brochure::page(
      href = "/page6",
      ui <- function(request) {
        tagList(
          h1("Multi Power Spectra", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
              body {
                margin: 5px; /* Adds margin around the entire page */
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 5px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 10px; /* Increases horizontal space between inputs */
              }
              "
              )
            ))),
          tags$script(
            HTML(
              "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
              "
            )
          ),
          
          fluidRow(
            column(12,
                   div(style = "display: flex; justify-content: space-around; align-items: center; flex-wrap: wrap;",
                       div(style = "margin-right: 5px;", selectInput("wave_select", "Select a Wave Object:", choices = NULL, width = '100%')),
                       div(style = "margin-right: 5px;", selectInput("wl", "Window Length: ", selected = 4096, choices = c(512,1024,2048,4096,8192), width='80%')),
                       div(style = "margin-right: 5px;", actionButton("plot_button", "Plot", class = "btn-small")),
                       div(style = "margin-right: 5px;", actionButton("add_selection", "Add Selection", class = "btn-small")),
                       div(style = "margin-right: 5px;", numericInput("alpha", "Fill Opacity", value = 0.8, min = 0.1, max = 1, step = 0.1), width='60%'),
                       div(style = "margin-right: 5px;", downloadButton("download_oscillogram", "Download Oscillogram", class = "btn-small")),
                       div(downloadButton("download_power_spectra", "Download Power Spectra", class = "btn-small"))
                   )
            )
          ),
          
          fluidRow(
            div(style = "margin-top: 10px; margin-left: 20px; margin-right: 20px;",
                column(12, withSpinner(plotOutput("oscillogram", height = "150px", width = "1460px",
                                              brush = brushOpts(id = "wave_brush", direction = "x")), type = 6)
            ))
          ),
          
          fluidRow(
            div(style = "margin-top: 2px; margin-left: 20px; margin-right: 20px;",
                column(12, withSpinner(plotlyOutput("mean_spectrum", height = "350px", width = "1460px"), type = 6))
          ))
        )
      },
      
    server <- function(input, output, session) {
      
      wave_df <- function(wave){
        srate <- wave@samp.rate
        amplitude <- wave@left
        tbl <- tibble(amplitude = amplitude)
        tbl <- tbl %>%
          mutate(index = row_number(),
                 time = (index - 1) / srate) %>%
          select(c(amplitude, time)) %>%
          mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
        return(tbl)
      }
      
      createOscillogram <- function(wave, brush_data = list(), colors = NULL) {
        tbl <- wave_df(wave)
        
        p <- ggplot(tbl, aes(x = time, y = amplitude)) +
          geom_line(color = "black", size = 0.5) +
          theme_minimal() +
          theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(color = "black")
          ) +
          expand_limits(y = c(-1.2, 1.2)) +
          labs(x = "Time (s)", y = "Amplitude")
        
        if (!is.null(colors) && length(brush_data) > 0) {
          for (i in seq_along(brush_data)) {
            range <- brush_data[[i]]
            selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
            p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude),
                               color = colors[i], size = 0.5)
          }
        }
        
        return(p)
      }
      
      extract.meanspec <- function(wave, from = NULL, to = NULL, wl = as.numeric(input$wl)) {
        full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE, 
                                       wl = wl, fftw = TRUE)
        full_spec_df <- tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
        return(full_spec_df)
      }
      
      plot.meanspec <- function(wave, wl = as.numeric(input$wl)) {
        full_spec <- extract.meanspec(wave, wl = as.numeric(input$wl))
        p <- plot_ly(full_spec, x = ~frequency, y = ~amplitude, type = 'scatter', 
                     mode = 'lines', line = list(color = 'black'), name = 'Mean') %>%
          config(displayModeBar = TRUE) %>%
          layout(hovermode = 'x') %>%
          layout(hovertemplate = 'Amplitude: %{y:.2f}')
        
        p <- p %>%
          layout(
            xaxis = list(
              title = list(text = "Frequency (kHz)", standoff = 10),
              ticklen = 5,
              automargin = TRUE,
              zeroline = FALSE, 
              showline = TRUE   
            ),
            yaxis = list(title = "Amplitude",
                         rangemode = "tozero",
                         ticklen = 5,
                         showline = TRUE),  
            legend = list(
              orientation = "h",
              x = 0.5,
              y = 1.1,
              xanchor = "center"
            ),
            margin = list(
              l = 50,  
              r = 10, 
              b = 60, 
              t = 50   
            )
          )
        
        return(p)
      }
      
      selected_wave <- reactiveVal()
      brushed_ranges <- reactiveVal(list())
      brush_colors <- reactiveVal(c("#0072B2","#E69F00","#009E73", "#CC79A7",
                                    
                                    "#F0E442", "#56B4E9", "#999999","#D55E00" ))
      
      plotly_obj <- reactiveVal()
      
      observeEvent(input$wave_select, {
        if (input$wave_select != "") {
          wave_obj <- get(input$wave_select, envir = .GlobalEnv)
          selected_wave(wave_obj)
          brushed_ranges(list())
        }
      })
      
      observe({
        wave_names <- ls(envir = .GlobalEnv)
        wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
        updateSelectInput(session, "wave_select", choices = wave_names)
      })
      
      output$oscillogram <- renderPlot({
        req(input$plot_button)
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- brush_colors()
        p <- createOscillogram(wave, brush_data, colors)
        p
      })
      
      output$mean_spectrum <- renderPlotly({
        req(input$plot_button)
        req(selected_wave())
        wave <- selected_wave()
        p <- plot.meanspec(wave, wl = as.numeric(input$wl))
        plotly_obj(p)
        p
      })
      
      observeEvent(input$add_selection, {
        req(input$wave_brush)
        brush <- input$wave_brush
        brush_data <- brushed_ranges()
        brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
        brushed_ranges(brush_data)
        
        wave <- selected_wave()
        range <- c(brush$xmin, brush$xmax)
        spec <- extract.meanspec(wave, from = range[1], to = range[2], wl = as.numeric(input$wl))
        spec$amplitude <- spec$amplitude * max(wave_df(wave) %>% dplyr::filter(time >= range[1] & time <= range[2]) %>% pull(amplitude))
        colors <- brush_colors()
        
        selection_number <- length(brush_data)
        selection_name <- paste("Selection", selection_number)
        
        plotlyProxy("mean_spectrum", session) %>%
          plotlyProxyInvoke("addTraces", list(
            x = spec$frequency, y = spec$amplitude, type = 'scatter', 
            mode = 'none', fill = 'tozeroy', 
            fillcolor = toRGB(colors[selection_number], alpha = input$alpha), name = selection_name,
            hovertemplate = 'Amplitude: %{y:.2f}'
          ))
        
        # Update plotly object with new trace
        p <- plotly_obj()
        p <- p %>%
          add_trace(x = spec$frequency, y = spec$amplitude, type = 'scatter', mode = 'none', 
                    fill = 'tozeroy', fillcolor = toRGB(colors[selection_number], alpha = input$alpha), 
                    name = selection_name,
                    hovertemplate = 'Amplitude: %{y:.2f}', line = list(color = 'rgba(0,0,0,0)'))
        plotly_obj(p)
      })
      
      output$download_oscillogram <- downloadHandler(
        filename = function() {
          paste0("oscillogram_", Sys.Date(), ".png")
        },
        content = function(file) {
          req(selected_wave())
          wave <- selected_wave()
          brush_data <- brushed_ranges()
          colors <- brush_colors()
          p <- createOscillogram(wave, brush_data, colors)
          
          ggsave(filename = file, plot = p, device = "png", 
                 width = 20, height = 4, 
                 units = "in", dpi = 300, bg="white")
        }
      )
      
      output$download_power_spectra <- downloadHandler(
        filename = function() {
          paste0("meanpowerspectra_", Sys.Date(), ".html")
        },
        content = function(file) {
          req(plotly_obj())
          saveWidget(plotly_obj(), file, selfcontained = TRUE)
        }
      )
      
    }
    )
  }
  
  # Page 7 Temporal Statistics
  page_7 <- function(){
    brochure::page(
      href = "/page7",
      
      ui <- function(request){
        tagList(
          h1("Temporal Statistics", style = "font-size: 28px; margin-left: 15px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(tags$style(
              HTML(
                "
               body {
                margin: 5px; /* Adds margin around the entire page */
              }
            .btn-group-vertical > .btn {
              margin-bottom: 10px; /* Adds space between vertical buttons */
            }
            .row {
              margin-bottom: 10px; /* Adds vertical space between rows */
            }
            .shiny-input-container {
              margin-right: 2px; /* Reduces horizontal space between inputs */
            }
            
            .dataTables_wrapper .caption-top {
            caption-side: top !important;
            font-weight: bold;
            color: white; 
            }
          
            "
              )
            ))),
          tags$script(
            HTML(
              "
            Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
              if (message) {
                document.getElementById('spinner').style.display = 'block';
              } else {
                document.getElementById('spinner').style.display = 'none';
              }
            });
            "
            )
          ), 

          fluidRow(
            column(
              width = 2,
              div(
                sidebarPanel(
                  selectInput("selectedWave", "Select a Wave Object:", choices = NULL),
                  textInput("specimen_id", "Specimen ID", value = "CORALB-01"),
                  numericInput("ssmooth", "Smoothing Window", value = 100, 
                               min = 10, max = 1000, step = 10),
                  numericInput("peakfinder_ws", "Peakfinder Window", value = 40,
                               min = 10, max = 200, step = 5),
                  numericInput("peakfinder_threshold", "Peakfinder Threshold", 
                               value = 0.005, min = 0.001, max = 0.5, step = 0.001),
                  numericInput("max_train_gap", "Max Train Gap", value = 0.08,
                               min = 0.01, max = 1, step = 0.01),
                  numericInput("max_peak_gap", "Max Peak Gap", value = 0.01,
                               min = 0.001, max = 0.1, step = 0.001),
                  actionButton("run", "Run Analysis"),
                  downloadButton("downloadData", "Download Data"),
                  actionButton("help", "Help"),
                  width = 12,
                  style = "height: 100%; overflow-y: auto; padding: 10px;"
                ),
                style = "width: 100%;"
              )
            ),
            column(
              width = 10,
              mainPanel(
                withSpinner(plotlyOutput("audioPlot"), type =6),
                DTOutput("echeme_data"),
                DTOutput("train_data"),
                DTOutput("peak_data"),
                DTOutput("params"),
                width = 12,
                style = "padding: 10px;"
              )
            )
          )
        )

      },
      
      # Define the server
      server <- function(input, output, session) {
        
        temporal_stats <- function(wave, 
                                   specimen.id,
                                   ssmooth = 100,
                                   peakfinder_ws = 50, 
                                   peakfinder_threshold = 0.005, 
                                   max_train_gap = 0.5, 
                                   max_peak_gap = 0.01,
                                   norm.env = TRUE) { 
          
          # Store input parameters in a tibble
          params <- tibble(
            specimen.id = specimen.id,
            ssmooth = ssmooth,
            peakfinder_ws = peakfinder_ws,
            peakfinder_threshold = peakfinder_threshold,
            max_train_gap = max_train_gap,
            max_peak_gap = max_peak_gap,
            norm.env = norm.env
          )
          
          window_size <- peakfinder_ws
          
          waveDuration <- seewave::duration(wave)
          
          envelope_vector <- seewave::env(wave, ssmooth = ssmooth)
          
          # warbleR's envelope is faster but the package is not available in CRAN at the moment
          # envelope_vector <- warbleR::envelope(as.numeric(wave@left), ssmooth = ssmooth)
          
          if (norm.env) {
            envelope_vector <- (envelope_vector - min(envelope_vector)) / (max(envelope_vector) - min(envelope_vector))
          }
          
          max_amplitude <- max(envelope_vector)
          amp_threshold <- max_amplitude * peakfinder_threshold
          peaks <- c()
          
          for (i in (window_size + 1):(length(envelope_vector) - window_size)) {
            window <- envelope_vector[(i - window_size):(i + window_size)]
            center_value <- envelope_vector[i]
            if (center_value == max(window) && (center_value - min(window)) > amp_threshold) {
              peaks <- c(peaks, i)
            }
          }
          
          # Create the time vector 
          sample_rate <- wave@samp.rate
          time_vector <- seq(0, (length(wave@left) - 1) / sample_rate, length.out = length(envelope_vector))  # In seconds
          time_vector_ms <- time_vector * 1000  # Convert to milliseconds
          
          peaks <- peaks[peaks <= length(time_vector)]
          
          peak_times_ms <- time_vector_ms[peaks] 
          
          peak_periods <- diff(peak_times_ms)
          peak_periods[peak_periods > max_peak_gap * 1000] <- NA
          
          peak_data <- tibble(
            specimen.id = rep(specimen.id, length(peaks)),
            echeme.id = integer(length(peaks)),
            train.id = integer(length(peaks)),
            peak.id = integer(length(peaks)),
            peak.time = peak_times_ms,
            peak.period = c(peak_periods, NA)
          )
          
          trains <- list()
          echemes <- list()
          echeme_id <- 1
          train_id <- 1
          train_start <- peaks[1]
          current_echeme <- list(c(train_start, NULL))
          
          peak_data <- peak_data %>%
            mutate(echeme.id = ifelse(row_number() == 1, echeme_id, echeme.id),
                   train.id = ifelse(row_number() == 1, train_id, train.id),
                   peak.id = ifelse(row_number() == 1, 1, peak.id))
          
          peak_data <- peak_data %>%
            mutate(peak.period = round(peak.period,3))
          
          peak_counter <- 1
          
          for (i in 2:length(peaks)) {
            if (peak_times_ms[i] - peak_times_ms[i - 1] > max_peak_gap * 1000) {
              train_end <- peaks[i - 1]
              trains <- append(trains, list(c(train_start, train_end)))
              train_start <- peaks[i]
              current_echeme[[length(current_echeme)]][2] <- train_end
              current_echeme <- append(current_echeme, list(c(train_start, NULL)))
              peak_counter <- 0
              if (peak_times_ms[i] - peak_times_ms[i - 1] > max_train_gap * 1000) {
                echemes <- append(echemes, list(current_echeme))
                current_echeme <- list(c(train_start, NULL))
                echeme_id <- echeme_id + 1
                train_id <- 1
              } else {
                train_id <- train_id + 1
              }
            }
            peak_counter <- peak_counter + 1
            peak_data <- peak_data %>%
              mutate(echeme.id = ifelse(peak.time == peak_times_ms[i], echeme_id, echeme.id),
                     train.id = ifelse(peak.time == peak_times_ms[i], train_id, train.id),
                     peak.id = ifelse(peak.time == peak_times_ms[i], peak_counter, peak.id))
          }
          
          # Round peak.time to 4 decimals
          peak_data <- peak_data %>%
            mutate(peak.time = round(peak.time, 4))
          
          train_end <- peaks[length(peaks)]
          trains <- append(trains, list(c(train_start, train_end)))
          current_echeme[[length(current_echeme)]][2] = train_end
          echemes <- append(echemes, list(current_echeme))
          
          # Create tibble for peak train measurements
          train_data <- peak_data %>%
            group_by(specimen.id, echeme.id, train.id) %>%
            summarize(
              train.start = round(min(peak.time),4),
              train.end = round(max(peak.time),4),
              train.duration = round((train.end - train.start),3),
              n.peaks = n(),
              peak.rate = round((n() / ((max(peak.time) - min(peak.time))/1000)),1)
            ) %>%
            ungroup() %>%
            # Set last train.period of each echeme to NA and round to 3 decimals
            mutate(train.period = ifelse(is.na(lead(echeme.id)) | lead(echeme.id) != echeme.id, NA, lead(train.start) - train.start)) %>%
            relocate(train.period, .after = train.duration) %>%
            mutate(train.period = round(train.period,3))
     
          
          # Create tibble for echeme measurements
          echeme_data <- train_data %>%
            group_by(specimen.id, echeme.id) %>%
            summarize(
              echeme.start = round(min(train.start),4),
              echeme.end = round(max(train.end),4),
              echeme.duration = round(echeme.end - echeme.start,3),
              echeme.period = round((lead(echeme.start) - echeme.start),3),
              n.trains = n(),
              train.rate = round(n() / ((max(train.end)/1000) - (min(train.start)/1000)),1),
              duty.cycle = round((sum(train.duration) / echeme.duration)*100,2)
            ) %>%
            ungroup()
          
          # Prepare annotations for the plot
          annotations <- list(
            list(
              x = 0.01, 
              y = 0.99, 
              xref = 'paper',
              yref = 'paper',
              text = paste("<b> Summary Statistics</b>", 
                           "<br> N. echemes:", length(echemes),
                           "<br> Echeme duration: ", round(mean(echeme_data$echeme.duration/1000),3), "s",
                           "<br> Duty Cycle: ", round(mean(echeme_data$duty.cycle),1), "%",
                           "<br> Trains / Echeme:", mean(echeme_data$n.trains),
                           "<br> Train Rate: " , round(mean(echeme_data$train.rate)), "tps",
                           "<br> Train Duration: ", round(mean(train_data$train.duration, na.rm = TRUE)), "ms",
                           "<br> Peaks / Train: ", round(mean(train_data$n.peaks, na.rm = TRUE)),
                           "<br> Peak Rate: ", round(mean(train_data$peak.rate, na.rm = TRUE)), "pps"),
              
              
              
              showarrow = FALSE,
              font = list(size = 12),
              align = "left",
              bgcolor = 'rgba(255, 255, 255, 0.5)', 
              bordercolor = 'rgba(0, 0, 0, 0.5)', 
              borderwidth = 1
            )
          )
          
          # Start the interactive plot
          p <- plot_ly() %>%
            add_lines(x = ~time_vector, y = ~envelope_vector, name = "Envelope", 
                      hoverinfo = "none",  line = list(color = 'rgb(20, 20, 20)', 
                                                       width = 2)) 
          
          # Add train lines to the plot
          for (i in seq_along(trains)) {
            train_start_time <- time_vector[trains[[i]][1]]
            train_end_time <- time_vector[trains[[i]][2]]
            show_legend <- if (i == 1) TRUE else FALSE
            p <- p %>%
              add_lines(x = c(train_start_time, train_end_time), y = c(0.98, 0.98), 
                        name = "Trains", line = list(color = "#009E73", width = 6), 
                        showlegend = show_legend, legendgroup = "Trains",
                        hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2)))
          }
          
          # Add echeme lines to the plot using echeme_data
          for (i in seq_len(nrow(echeme_data))) {
            echeme_start <- echeme_data$echeme.start[i]/1000
            echeme_end <- echeme_data$echeme.end[i]/1000
            show_legend <- if (i == 1) TRUE else FALSE
            p <- p %>%
              add_lines(x = c(echeme_start, echeme_end), y = c(1, 1), 
                        name = "Echemes", line = list(color = "#0072B2", width = 6), 
                        showlegend = show_legend, legendgroup = "Echemes",
                        hoverinfo = "x", text = paste("Time:", round(c(echeme_start, echeme_end), 2)))
          }
          
          # Add peak markers
          p <- p %>%
            add_markers(x = ~time_vector[peaks], y = ~envelope_vector[peaks],
                        name = "Peaks", marker = list(color = "#D55E00" , size = 8),
                        hoverinfo = "none")
          
          p <- p %>%
            layout(
              annotations = annotations,
              xaxis = list(
                title = list(text = "Time (s)", standoff = 10),
                ticklen = 5,
                automargin = TRUE,
                zeroline = FALSE, 
                showline = TRUE   
              ),
              yaxis = list(title = "Amplitude",
                           rangemode = "tozero",
                           ticklen = 5,
                           showline = TRUE),  
              legend = list(
                orientation = "h",
                x = 0.5,
                y = 1.1,
                xanchor = "center"
              ),
              margin = list(
                l = 70,  
                r = 10, 
                b = 50, 
                t = 50   
              )
            )
          
          list(plot = p, peak_data = peak_data, train_data = train_data, 
               echeme_data = echeme_data, params = params)
        }
        
        
        # Observer to update available wave objects in the environment
        observe({
          wave_names <- ls(envir = .GlobalEnv)
          wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "selectedWave", choices = wave_names)
        })
        
        result <- eventReactive(input$run, {
          req(input$selectedWave)
          wave <- get(input$selectedWave, envir = .GlobalEnv)
          
          
          temporal_stats(wave, 
                         specimen.id = input$specimen_id,
                         ssmooth = input$ssmooth,
                         peakfinder_ws = input$peakfinder_ws, 
                         peakfinder_threshold = input$peakfinder_threshold, 
                         max_train_gap = input$max_train_gap, 
                         max_peak_gap = input$max_peak_gap,
                         norm.env = TRUE)
        })
        
        

        
        output$audioPlot <- renderPlotly({
          req(result())
          result()$plot %>%
            layout(title = input$specimen_id,
              margin = list(
              l = 80, r = 0, t = 80, b = 80
            )
            )
        })
        
        
        output$echeme_data <- renderDT({
          req(result())
          datatable(result()$echeme_data, 
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: center;",
                      class = "caption-top",
                      "Echeme Data"
                    ),
                    options = list(
            pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = "_all"))
          ))
        })
        
        
        output$train_data <- renderDT({
          req(result())
          datatable(result()$train_data, 
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: center;",
                      class = "caption-top",
                      "Train Data"
                    ),
                    options = list(
            pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = "_all"))
          ))
        })
        
        
        output$peak_data <- renderDT({
          req(result())
          datatable(result()$peak_data, 
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: center;",
                      class = "caption-top",
                      "Peak Data"
                    ),
                    options = list(
            pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = "_all"))
          ))
        })
        
        output$params <- renderDT({
          req(result())
          datatable(result()$params, 
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: center;",
                      class = "caption-top",
                      "Parameter Data"
                    ),
                    options = list(
            pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = "_all"))
          ))
        })
        
       
        # Define the help modal dialog
        observeEvent(input$help, {
          showModal(modalDialog(
            title = "Help - Parameter Information",
            HTML("
        <b>Smoothing Window:</b> Window size (samples) used to smooth the envelope.<br><br>
        <b>Peakfinder Window:</b> Size of the moving window used for peak detection.<br><br>
        <b>Peakfinder Threshold:</b> Amplitude threshold for identifying peaks based on their distance to the 'valleys'.<br><br>
        <b>Max Train Gap:</b> Maximum gap allowed between trains to be considered in the same echeme.<br><br>
        <b>Max Peak Gap (max_peak_gap):</b> Maximum gap allowed between consecutive peaks to be considered in the same train.<br>
      "),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        })
        
        session$onSessionEnded(function() {
          stopApp()
        })
        
      }
      
    )
  }
  
  # Page 8 Export Spectrogram
  page_8 <- function() {
    brochure::page(
      href = "/page8",
      ui = function(request) {
        tagList(
          h1("Spectrogram", style = "font-size: 28px; margin-left: 15px;"),
          nav_links(),
          
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(
              tags$style(
                HTML(
                  "
              body {
                margin: 5px; /* Adds margin around the entire page */
              }
              #specPlot {
                height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 10px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }
              "
                )
              )
            ),
            fluidRow(
              column(
                4,
                selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%'),
                actionButton("refresh", "Refresh List"),
                actionButton("spectro", "Plot Spectrogram"),
                checkboxInput("meanspec", "Add Mean Spectrum", value = TRUE)
              ),
              
              column(2,
                     numericInput("imgWidth", "Image Width (in):", value = 15, min = 1, step = 1)
              ),
              column(2,
                     numericInput("imgHeight", "Image Height (in):", value = 3, min = 1, step = 1)
              ),
              
              column(
                2,
                verticalLayout(
                  checkboxInput("transparentBg", "Transparent Background", value = FALSE),
                  downloadButton("saveImage", "Save Image")
                )
              )
            ),
            mainPanel(
              uiOutput("specPlotOutput")
            )
          )
        )
      },
      server = function(input, output, session) {
        plotVisible <- reactiveVal(FALSE)
        spectrogramCache <- reactiveVal(NULL)
        
        
        spectrogram <- function(wave, floor = -50, meanspec = TRUE) {
          win_len <- 0.005 * wave@samp.rate
          hop_len <- 0.002 * wave@samp.rate
          overlap <- ((win_len - hop_len) / win_len) * 150
          
          spect <- wave |>
            seewave::spectro(
              wl = win_len,
              ovlp = overlap,
              plot = FALSE
            )
          
          colnames(spect$amp) <- spect$time
          rownames(spect$amp) <- spect$freq
          
          spect_df <- spect$amp |>
            as_tibble(rownames = "freq") |>
            pivot_longer(
              -freq,
              names_to = "time",
              values_to = "amp"
            ) |>
            mutate(
              freq = as.numeric(freq),
              time = as.numeric(time)
            )
          
          dyn = -50
          spect_df_floor <- spect_df |>
            mutate(
              amp_floor = case_when(
                amp < dyn ~ dyn,
                TRUE ~ amp
              )
            )
          
          if(meanspec){
            mean_spectrum <- seewave::meanspec(wave, f = wave@samp.rate, wl = win_len, ovlp = overlap, plot = FALSE)
            mean_spectrum_df <- data.frame(
              freq = mean_spectrum[, 2],
              mean_amp = mean_spectrum[, 1]
            )
          }
          
          spect_plot <- spect_df_floor |>
            ggplot(aes(time, freq)) +
            geom_raster(aes(fill = amp_floor)) +
            scale_fill_gradient(low = "white", high = "black", 
                                limits = c(dyn, max(spect_df$amp)), na.value = "white") +
            scale_y_continuous(expand = c(0,0),
                               breaks = scales::breaks_pretty(),#n.breaks = 4,
                               labels = scales::label_number(accuracy = 1, 
                                                             trim = TRUE, 
                                                             zero.print = "")) +
            scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                               breaks = scales::breaks_pretty(),
                               labels = scales::label_number(accuracy = 0.1, 
                                                             trim = TRUE, 
                                                             zero.print = "")) +
            
            theme_minimal(base_size = 15) +
            theme_bw()+
            theme(
              plot.margin = margin(t=0, r=0, b=10, l=10, unit = 'pt'),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white", color = NA),
              axis.ticks = element_line(colour = "black"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = "none"
            ) +
            labs(
              x = "Time (s)",
              y = "Frequency (kHz)",
              title = ""
            )
          
          if(meanspec){
            spectrum_plot <- mean_spectrum_df |>
              ggplot(aes(x = mean_amp, y = freq)) +
              geom_line(color = "black") +
              geom_ribbon(aes(ymin = 0, ymax = freq), fill = "black") +
              theme_minimal(base_size = 15) +
              
              scale_y_continuous(breaks = c(0, 0.5, 1), 
                                 expand = expansion(mult = c(0, .1)),
                                 position = 'right',
                                 labels = function(x) ifelse(x == 0 | x == 1, 
                                                             as.character(as.integer(x)), 
                                                             as.character(x))) +
              
              
              
              scale_x_continuous(expand = c(0,0), position = "top",
                                 breaks = scales::breaks_pretty(), 
                                 labels = scales::label_number(zero.print = "")) +
              theme_bw() +
              theme(
                plot.margin = margin(t=0, r=10, b=10, l=0, unit="pt"),
                panel.grid = element_blank(),
                panel.background = element_rect(fill = "white", color = NA),
                axis.ticks.y = element_line(colour = "black"),  
                axis.ticks.x = element_line(colour = "black"),
                axis.title = element_text(size = 12),
                axis.text.y = element_text(size = 10),  
                axis.text.x = element_text(size = 10),
                panel.border = element_rect(colour = "black", fill=NA, size=1),
                legend.position = "none"
              ) +
              labs(
                x = NULL,
                y = "Mean Amplitude",
                title = ""
              ) +
              coord_flip()
            
            combined_plot <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = c(6, 0.5))
            
            return(combined_plot)
            
          } else {
            return(spect_plot)
          }
        }
        
        observe({
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
            inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "waveObject", choices = waveObjects)
        })
        
        observeEvent(input$refresh, {
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
            inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "waveObject", choices = waveObjects)
        })
        
        observeEvent(input$spectro, {
          req(input$waveObject)
          wave <- get(input$waveObject, envir = .GlobalEnv)
          
          plotParams <- list(
            wave = wave,
            meanspec = input$meanspec
          )
          
          spectrogramCache(plotParams)
          plotVisible(TRUE)
          
          output$specPlotOutput <- renderUI({
            withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 6)
          })
          
          output$specPlot <- renderPlot({
            tryCatch({
              plotParams <- spectrogramCache()
              combined_plot <- spectrogram(plotParams$wave, meanspec = plotParams$meanspec)
              print(combined_plot)
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("An error occurred:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
              NULL
            })
          }, height = function() {
            input$imgHeight * 100
          }, width = function() {
            input$imgWidth * 100
          })
        })
        
        observeEvent(input$clearPlot, {
          plotVisible(FALSE)
          output$specPlot <- renderPlot(NULL)
        })
        
        output$saveImage <- downloadHandler(
          filename = function() {
            paste("spectrogram", "_saved_", Sys.Date(), ".png", sep = "")
          },
          content = function(file) {
            req(spectrogramCache())
            plotParams <- spectrogramCache()
            bg <- ifelse(input$transparentBg, "transparent", "white")
            ggsave(file, plot = spectrogram(plotParams$wave, meanspec = plotParams$meanspec), width = input$imgWidth, height = input$imgHeight, units = "in", dpi = 300, bg = bg)
          }
        )
        
        session$onSessionEnded(function() {
          stopApp()
        })
      }
    )
  }
  
  # Page 9 Export Oscillogram
  page_9 <- function() {
    brochure::page(
      href = "/page9",
    ui = function(request) {
      tagList(
        # nav_links(),
        h1("Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
        fluidPage(
          theme = bslib::bs_theme(bootswatch = "darkly"),
          tags$head(tags$style(
            HTML(
              "
                 body {
                margin: 5px; /* Adds margin around the entire page */
              }
          #wavePlot {
            height: calc(100vh - 200px); /* Adjusts height taking into account other elements */
            width: 100%;
          }
          
            #zoomedPlot {
            height: calc(100vh - 200px); /* Adjusts height taking into account other elements */
            width: 100%;
          }
    
          
          .btn-group-vertical > .btn {
            margin-bottom: 10px; /* Adds space between vertical buttons */
          }
          .row {
            margin-bottom: 10px; /* Adds vertical space between rows */
          }
          .shiny-input-container {
            margin-right: 2px; /* Reduces horizontal space between inputs */
          }
          
          "
            )
          ))),
        
        div(style = "padding-left: 15px;padding-right: 15px;",
            fluidRow(
              column(
                4,
                selectInput("waveObject", "Select a wave object:", 
                            choices = NULL, width = '100%'),
              ),
              column(2, verticalLayout(
                actionButton("oscillogram", "Plot Oscillogram"),
                actionButton("zoomedPortion", "Stack Selection")
              )),
              column(2, verticalLayout(
                downloadButton("saveImage", "Save Image"),
                downloadButton("saveInteractivePlot", "Save Interactive")
              )
              ),
              column(2,
                     numericInput("imgWidth", "Image Width (in):", 
                                  value = 16, min = 1)),
              column(
                2,
                verticalLayout(
                  numericInput("imgHeight", "Image Height (in):", 
                               value = 4, min = 1),
                  checkboxInput("transparentBg", "Transparent Background", 
                                value = FALSE)
                )
              )
            )
        ),
        mainPanel(
          plotOutput("wavePlot", height = "250px", width = "1500px", 
                     brush = brushOpts(id = "waveBrush", direction = "x")),
          plotOutput("zoomedPlot", height = "250px", width = "1500px")
        )
      )
      
    },
    
    server = function(input, output, session) {
      
      plotVisible <- reactiveVal(TRUE)
      selectedRegion <- reactiveVal(NULL)
      finalZoomedRegion <- reactiveVal(NULL)
      
      # Store the plots in reactive values
      fullPlot <- reactiveVal(NULL)
      zoomedPlot <- reactiveVal(NULL)
      
      # Custom function to create an interactive waveform
      intWaveform <- function(wave, line_color = 'black') {
        
        df <- wave_df(wave)
        
        # Create an interactive plot with plotly
        p <- plot_ly(data = df, x = ~time, y = ~amplitude,
                     type = 'scatter', mode = 'lines',
                     line = list(shape = "spline", 
                                 color = line_color)) %>%
          layout(xaxis = list(title = 'Time'),
                 yaxis = list(title = 'Amplitude'))
        
        return(p)
      }
      
      # Extract data frame from wave
      wave_df <- function(wave){
        srate <- wave@samp.rate
        amplitude <- wave@left
        tbl <- tibble(amplitude = amplitude)
        tbl <- tbl %>%
          mutate(index = row_number(),
                 time = (index - 1) / srate) %>%
          select(c(amplitude, time)) %>%
          mutate(amplitude = 2* (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
        return(tbl)
      }
      
      # Create oscillogram
      createOscillogram <- function(wave, brush_data = list(), colors = NULL) {
        
        tbl <- wave_df(wave)
        
        p <- ggplot(tbl, aes(x = time, y = amplitude)) +
          geom_line(color = "black", linewidth = 0.4) +
          theme_minimal() +
          theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(color = "black"),
            axis.title = element_text(size = 14) 
          ) +
          expand_limits(y = c(-1.2, 1.2)) +
          labs(x = "Time (s)", y = "Amplitude")
        
        if (!is.null(colors) && length(brush_data) > 0) {
          for (i in seq_along(brush_data)) {
            range <- brush_data[[i]]
            selected_data <- tbl %>% filter(time >= range[1] & time <= range[2])
            p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude), 
                               color = colors[i], linewidth = 0.5)
          }
        }
        
        return(p)
      }
      
      selected_wave <- reactiveVal()
      brushed_ranges <- reactiveVal(list())
      
      observeEvent(input$waveObject, {
        if (input$waveObject != "") {
          wave_obj <- get(input$waveObject, envir = .GlobalEnv)
          selected_wave(wave_obj)
          brushed_ranges(list())
        }
      })
      
      # List all Wave objects and tables in the environment for the select input
      observe({
        waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
          inherits(get(x, envir = .GlobalEnv), "Wave"))]
        updateSelectInput(session, "waveObject", choices = waveObjects)
      })
      
      # Refresh list button
      observeEvent(input$refresh, {
        waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
          inherits(get(x, envir = .GlobalEnv), "Wave"))]
        updateSelectInput(session, "waveObject", choices = waveObjects)
      })
      
      observeEvent(input$oscillogram, {
        req(input$waveObject)
        wave <- get(input$waveObject, envir = .GlobalEnv)
        
        
        output$wavePlot <- renderUI({
          withSpinner(plotOutput("wavePlot", height = "auto", width = "auto"), type = 6)
        })
        
        output$wavePlot <- renderPlot({
          tryCatch({
            if (!is.null(wave)) {
              p <- createOscillogram(wave, brushed_ranges())
              if (!is.null(finalZoomedRegion()) && length(finalZoomedRegion()) == 2) {
                p <- p + geom_vline(xintercept = finalZoomedRegion(), col = "black", 
                                    linetype = "dashed")
              }
              fullPlot(p)  # Store the plot in the reactive value
              p
            }
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              paste("An error occurred:", e$message),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            NULL
          }, finally = {
          })
        }, height = 250, width = 1500)
      })
      
      
      
      output$zoomedPlot <- renderUI({
        withSpinner(plotOutput("zoomedPlot", height = "auto", width = "auto"), type = 6)
      })
      
      
      output$zoomedPlot <- renderPlot({
        req(finalZoomedRegion())
        wave <- get(input$waveObject, envir = .GlobalEnv)
        
        tryCatch({
          if (!is.null(wave) && length(finalZoomedRegion()) == 2) {
            extract <- extractWave(
              wave,
              from = finalZoomedRegion()[1],
              to = finalZoomedRegion()[2],
              xunit = "time"
            )
            p <- createOscillogram(extract)
            zoomedPlot(p)  # Store the plot in the reactive value
            p
          }
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred:", e$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          NULL
        })
      }, height = 250, width = 1500)
      
      observeEvent(input$waveBrush, {
        currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
        selectedRegion(currentRegion)
      })
      
      observeEvent(input$zoomedPortion, {
        req(selectedRegion())
        finalZoomedRegion(selectedRegion())
      })
      
      
      observeEvent(input$plot, {
        plotVisible(TRUE)
        selectedRegion(NULL)
        finalZoomedRegion(NULL)
      })
      
      
      output$saveImage <- downloadHandler(
        filename = function() {
          paste("oscillogram_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          req(fullPlot(), zoomedPlot())
          
          combinedPlot <- plot_grid(fullPlot(), zoomedPlot(), ncol = 1)
          
          bg <- ifelse(input$transparentBg, "transparent", "white")
          ggsave(file, combinedPlot, width = input$imgWidth, height = input$imgHeight, units = "in", dpi = 300, bg = bg)
        }
      )
      
      output$saveInteractivePlot <- downloadHandler(
        filename = function() {
          paste("interactive_oscillogram_", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
          req(input$waveObject)
          
          wave <- get(input$waveObject, envir = .GlobalEnv)
          
          interactivePlot <- intWaveform(wave, line_color = "black")
          
          saveWidget(interactivePlot, file, selfcontained = TRUE)
        }
      )
      
      observeEvent(input$refresh, {
        refresh()
      })
      
      session$onSessionEnded(function() {
        stopApp()
      })
    }
    
    )
    # shinyApp(ui = ui, server = server)
  }
  
  # Page 10 Export Multiplot
  page_10 <- function() {
    brochure::page(
      href = "/page10",
      ui = function(request) {
        tagList(
          h1("Multiplot", style = "font-size: 28px; margin-left: 15px;"),
          nav_links(),
          fluidPage(
            theme = bslib::bs_theme(bootswatch = "darkly"),
            tags$head(
              tags$style(
                HTML(
                  "
                   body {
                margin: 5px; /* Adds margin around the entire page */
              }
                #specPlot {
                  height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                  width: 100%;
                }
                .btn-group-vertical > .btn {
                  margin-bottom: 10px; /* Adds space between vertical buttons */
                }
                .row {
                  margin-bottom: 10px; /* Adds vertical space between rows */
                }
                .shiny-input-container {
                  margin-right: 2px; /* Reduces horizontal space between inputs */
                }
                
                "
                )
              ),
              
            ),
            fluidRow(
              column(4,
                     selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%'),
                     actionButton("refresh", "Refresh List"),
                     actionButton("spectroscillo", "Plot")
              ),
              column(2,
                     numericInput("imgWidth", "Image Width (in):", value = 15, min = 1)),
              column(2,
                     numericInput("imgHeight", "Image Height (in):", value = 5, min = 1),
                     checkboxInput("transparentBg", "Transparent Background", value = FALSE)),
              column(1,
                     downloadButton("saveImage", "Save Image")
                     
              )
            ),
            mainPanel(
              uiOutput("specPlotOutput", height = "auto", width = "auto"),
            )
          )
        )
      },
      
      server = function(input, output, session) {
        plotVisible <- reactiveVal(FALSE)
        spectrogramCache <- reactiveVal(NULL)
        
        spectroscillo <- function(wave, floor = -50) {
          win_len <- 0.005 * wave@samp.rate
          hop_len <- 0.002 * wave@samp.rate
          overlap <- ((win_len - hop_len) / win_len) * 150
          
          spect <- wave |> seewave::spectro(wl = win_len, ovlp = overlap, plot = FALSE)
          colnames(spect$amp) <- spect$time
          rownames(spect$amp) <- spect$freq
          
          spect_df <- spect$amp |> as_tibble(rownames = "freq") |> pivot_longer(-freq, names_to = "time", values_to = "amp") |> mutate(freq = as.numeric(freq), time = as.numeric(time))
          
          dyn <- -50
          spect_df_floor <- spect_df |> mutate(amp_floor = ifelse(amp < dyn, dyn, amp))
          
          mean_spectrum <- seewave::meanspec(wave, f = wave@samp.rate, wl = win_len, ovlp = overlap, plot = FALSE)
          mean_spectrum_df <- data.frame(freq = mean_spectrum[, 2], mean_amp = mean_spectrum[, 1])
          
          spect_plot <- spect_df_floor |>
            ggplot(aes(time, freq)) +
            geom_raster(aes(fill = amp_floor)) +
            scale_fill_gradient(low = "white", high = "black", 
                                limits = c(dyn, max(spect_df$amp)), na.value = "white") +
            scale_y_continuous(expand = c(0,0),
                               breaks = scales::breaks_pretty(),#n.breaks = 4,
                               labels = scales::label_number(accuracy = 1, 
                                                             trim = TRUE, 
                                                             zero.print = "")) +
            scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                               breaks = scales::breaks_pretty(),
                               labels = scales::label_number(accuracy = 0.1, 
                                                             trim = TRUE, 
                                                             zero.print = "")) +
            theme_minimal(base_size = 15) +
            theme_bw()+
            theme(
              plot.margin = margin(t=0, r=0, b=10, l=10, unit = 'pt'),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white", color = NA),
              axis.ticks = element_line(colour = "black"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = "none"
            ) +
            labs(
              x = "Time (s)",
              y = "Frequency (kHz)",
              title = ""
            )
          
          
          
          spectrum_plot <- mean_spectrum_df |>
            ggplot(aes(x = mean_amp, y = freq)) +
            geom_line(color = "black") +
            geom_ribbon(aes(ymin = 0, ymax = freq), fill = "black") +
            theme_minimal(base_size = 15) +
            
            scale_y_continuous(breaks = c(0, 0.5, 1), 
                               expand = expansion(mult = c(0, .1)),
                               position = 'right',
                               labels = function(x) ifelse(x == 0 | x == 1, 
                                                           as.character(as.integer(x)), 
                                                           as.character(x))) +
            scale_x_continuous(expand = c(0,0), position = "top",
                               breaks = scales::breaks_pretty(), 
                               labels = scales::label_number(zero.print = "")) +
            theme_bw() +
            theme(
              plot.margin = margin(t=0, r=10, b=10, l=0, unit="pt"),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white", color = NA),
              axis.ticks.y = element_line(colour = "black"),  
              axis.ticks.x = element_line(colour = "black"),
              axis.title = element_text(size = 12),
              axis.text.y = element_text(size = 10),  
              axis.text.x = element_text(size = 10),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = "none"
            ) +
            labs(
              x = NULL,
              y = "Mean Amplitude",
              title = ""
            ) +
            coord_flip()
          
          oscillo_data <- seewave::oscillo(wave, plot = FALSE)
          time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
          amplitude <- oscillo_data / max(abs(oscillo_data))
          oscillo_df <- data.frame(time = time, amplitude = amplitude)
          
          oscillo_plot <- ggplot(oscillo_df, aes(x = time, y = amplitude)) +
            geom_line(color = "black") +
            theme_minimal(base_size = 15) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(n.breaks = 3, expand = c(0.03, 0.03)) +
            theme(
              plot.margin = margin(t = 0, r = 0, b = 0, l = 10, unit = "pt"),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white", color = NA),
              axis.line.y = element_line(colour = "black"),
              axis.line.x = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_text(size = 10),
              axis.text = element_text(size = 10),
              legend.position = "none"
            ) +
            labs(y = "Relative Amplitude", x = "")
          
          combined_spect_mean <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = c(5, 0.5))
          combined_spect_oscillo <- (spect_plot / oscillo_plot) + plot_layout(heights = c(5, 1))
          final_plot <- combined_spect_mean / (oscillo_plot + plot_spacer() + plot_layout(ncol = 2, widths = c(5, 0.5)))
          
          return(final_plot)
        }
        
        observe({
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
            inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "waveObject", choices = waveObjects)
        })
        
        observeEvent(input$refresh, {
          waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
            inherits(get(x, envir = .GlobalEnv), "Wave"))]
          updateSelectInput(session, "waveObject", choices = waveObjects)
        })
        
        observeEvent(input$spectroscillo, {
          req(input$waveObject)
          wave <- get(input$waveObject, envir = .GlobalEnv)
          
          plotParams <- list(wave = wave)
          
          spectrogramCache(plotParams)
          plotVisible(TRUE)
          
          
          output$specPlotOutput <- renderUI({
            withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 6)
          })
          
          
          output$specPlot <- renderPlot({
            tryCatch({
              plotParams <- spectrogramCache()
              combined_plot <- spectroscillo(plotParams$wave)
              print(combined_plot)
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("An error occurred:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
              NULL
            })
          }, height = function() {
            input$imgHeight * 100
          }, width = function() {
            input$imgWidth * 100
          })
        })
        
        
        output$saveImage <- downloadHandler(
          filename = function() {
            paste("spectroscillo", "_saved_", Sys.Date(), ".png", sep = "")
          },
          content = function(file) {
            req(spectrogramCache())
            plotParams <- spectrogramCache()
            bg <- ifelse(input$transparentBg, "transparent", "white")
            ggsave(file, plot = spectroscillo(plotParams$wave), width = input$imgWidth, height = input$imgHeight, units = "in", dpi = 300, bg = bg)
          }
        )
        
        session$onSessionEnded(function() {
          stopApp()
        })
      }
      
    )
  }
  
  # Page 11 Export Multi-oscillogram
  page_11 <- function() {
  # multioscillo_app <- function() {
    brochure::page(
      href = "/page11",
    ui = function(request) {
      tagList(
        h1("Multi-Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
        nav_links(),
        fluidPage(
          theme = bslib::bs_theme(bootswatch = "darkly"),
          tags$head(tags$style(
            HTML(
              "
                 body {
                margin: 5px; /* Adds margin around the entire page */
              }
              #audioPlot {
                height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 10px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }
              
              "
            )
          )),
          
          fluidRow(
            column(3, selectInput("waveObjects", "Select Sounds:", choices = NULL, selected = NULL, multiple = TRUE, width = '100%')),
            column(1, actionButton("plotMultiOscillogram", "Plot")),
            column(2, numericInput("maxLength", "Max Length (s):", value = 1, min = 0.5, step = 0.5, width = '100%')),
            column(3, verticalLayout(
              numericInput("scalebarLength", "Scalebar Length (s):", value = 0.1, min = 0.05, step = 0.01, width = '100%'),
              checkboxInput("allScalebar", "Add Scalebar to Each Oscillogram", value = FALSE),
              
            )), 
            column(1, checkboxInput("transparentBg", "Transparent Background", value = FALSE)),
            column(1, downloadButton("saveImage", "Save Image"))
            ),
            # column(
            #   3,
            #   verticalLayout(
            #     # actionButton("clearPlot", "Clear Plot"),
            #     actionButton("colorCode", "Color-code"),
            #     selectInput("colorSelect", "Select Color:", choices = c("Red", "Blue", "Green", "Yellow", "Purple", "Orange")),
            #     actionButton("paint", "Paint"),
            #     actionButton("undo", "Undo")
            #   )
            # ),
            # column(
            #   3,
            #   verticalLayout(
            #     actionButton("closeApp", "Close App")
            #   )
            # ),
            mainPanel(
              uiOutput("plotOutput"),
            )
          )
        )
      
    },
    
    server = function(input, output, session) {
      # Function to create the multi-oscillogram plot
      multiOscillogram <- function(waves, maxLength, scalebarLength, allScalebar = FALSE) {
        max_samples <- maxLength * waves[[1]]@samp.rate
        wave_labels <- LETTERS[1:length(waves)]
        
        all_waves_df <- lapply(seq_along(waves), function(i) {
          wave <- waves[[i]]
          samp_rate <- wave@samp.rate
          wave_length <- length(wave@left)
          duration <- wave_length / samp_rate
          
          time <- seq(-maxLength / 2, maxLength / 2, length.out = max_samples)
          amplitude <- rep(0, max_samples)
          
          start_index <- round((max_samples - wave_length) / 2)
          end_index <- start_index + wave_length - 1
          
          amplitude[start_index:end_index] <- wave@left
          
          # Subtract mean and rescale to [-1, 1]
          amplitude <- amplitude - mean(amplitude)
          amplitude <- 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1
          
          data.frame(
            time = time,
            amplitude = amplitude,
            wave_name = wave_labels[i],
            id = i
          )
        }) |> bind_rows()
        
        plots <- lapply(unique(all_waves_df$id), function(id) {
          p <- ggplot(all_waves_df %>% filter(id == !!id), aes(x = time, y = amplitude)) +
            geom_line(color = "black", size = 0.5) +
            theme_void() +
            theme(legend.position = "none") +
            coord_cartesian(xlim = c(-maxLength / 2, maxLength / 2)) +
            expand_limits(y = c(-1.2, 1.2))
          
          # Add scalebar to each oscillogram
          if (allScalebar) {
            if (scalebarLength < 1) {
              scalebarText <- paste0(scalebarLength * 1000, " ms")
            } else {
              scalebarText <- paste0(scalebarLength, " s")
            }
            p <- p + geom_segment(aes(x = -maxLength / 2 + 0.05, xend = -maxLength / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05), color = "black", size = 1) +
              annotate("text", x = -maxLength / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
          } else {
            if (id == length(waves)) {  # Add scale bar only to the last plot
              if (scalebarLength < 1) {
                scalebarText <- paste0(scalebarLength * 1000, " ms")
              } else {
                scalebarText <- paste0(scalebarLength, " s")
              }
              p <- p + geom_segment(aes(x = -maxLength / 2 + 0.05, xend = -maxLength / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05), color = "black", size = 1) +
                annotate("text", x = -maxLength / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
            }
          }
          
          return(p)
        })
        
        combined_plot <- wrap_plots(plots, ncol = 1)
        
        return(combined_plot)
      }
      
      # Store reactive values
      values <- reactiveValues(
        colorSelections = list(),
        history = list()
      )
      
      # Observer to update available wave objects in the environment
      observe({
        waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
          inherits(get(x, envir = .GlobalEnv), "Wave"))]
        updateSelectInput(session, "waveObjects", choices = waveObjects)
      })
      
      # This reactive expression will re-run only when the "Plot Multi-Oscillogram" button is clicked
      result <- eventReactive(input$plotMultiOscillogram, {
        req(input$waveObjects)
        waves <- lapply(input$waveObjects, function(x) get(x, envir = .GlobalEnv))
        # withProgress(message = 'Processing...', {
          multiOscillogram(waves, input$maxLength, input$scalebarLength, input$allScalebar)
        # })
      })
      
      output$plotOutput <- renderUI({
        req(result())
        req(input$plotMultiOscillogram)
        withSpinner(plotOutput("multiWavePlot", 
                               height = paste0(100 * length(input$waveObjects), "px"), 
                               width = "1450px"), type = 6)
      })
      
      output$multiWavePlot <- renderPlot({
        req(result())
        result()
      })
      
      
      # Download handler for saving the image
      output$saveImage <- downloadHandler(
        filename = function() {
          paste("multi_oscillogram_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          req(input$waveObjects)
          waves <- lapply(input$waveObjects, function(x) get(x, envir = .GlobalEnv))
          combined_plot <- multiOscillogram(waves, input$maxLength, input$scalebarLength, input$allScalebar)
          bg <- ifelse(input$transparentBg, "transparent", "white")
          ggsave(file, plot = combined_plot, width = 20, height = 2 * length(waves), units = "in", dpi = 300, bg = bg)
        }
      )
      
      # # Handle color coding
      # observeEvent(input$paint, {
      #   req(input$colorSelect, input$waveObjects)
      #   wave_name <- input$waveObjects[1]
      #   color <- input$colorSelect
      #   selection <- data.frame(wave_name = wave_name, color = color)
      #   values$colorSelections <- append(values$colorSelections, list(selection))
      #   values$history <- append(values$history, list(values$colorSelections))
      #   output$multiWavePlot <- renderPlot({
      #     plot <- result()
      #     for (selection in values$colorSelections) {
      #       plot <- plot + geom_line(data = selection, aes(x = time, y = amplitude, color = color))
      #     }
      #     plot
      #   })
      # })
      
      # # Handle undo
      # observeEvent(input$undo, {
      #   if (length(values$history) > 0) {
      #     values$colorSelections <- values$history[[length(values$history) - 1]]
      #     values$history <- values$history[-length(values$history)]
      #     output$multiWavePlot <- renderPlot({
      #       plot <- result()
      #       for (selection in values$colorSelections) {
      #         plot <- plot + geom_line(data = selection, aes(x = time, y = amplitude, color = color))
      #       }
      #       plot
      #     })
      #   }
      # })
      
      session$onSessionEnded(function() {
        stopApp()
      })
    }
    )
  }
  
  brochureApp(
    req_handlers = list(log_where),
    # Pages
    page_1(),
    page_2(),
    page_3(),
    page_4(),
    page_5(),
    page_6(),
    page_7(),
    page_8(),
    page_9(),
    page_10(),
    page_11(),

    brochure::page(
      href = "/healthcheck",
      ui =  tagList(),
      req_handlers = list(~ shiny::httpResponse(200, content = "OK"))
    )
  )
  
}

# Usage
Rthoptera()


