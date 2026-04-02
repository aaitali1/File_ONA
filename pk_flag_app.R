# =============================================================================
# Interactive PK Profile Viewer — Flag Outliers
# With Zoom (manual), CMT filter, dosing overlays, predose markers
#
# Usage: Open in RStudio, click "Run App"
# Required packages: shiny, ggplot2, dplyr, DT
#
# install.packages(c("shiny", "ggplot2", "dplyr", "DT"))
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# ---- CHANGE THIS PATH TO YOUR CSV ----
DATA_PATH <- "pool_BP42135_Phase_I_1901.csv"

# CMT choices for observation data
CMT_CHOICES <- c(
  "1 — Plasma PK"   = 1,
  "4 — Plasma 2AG"  = 4,
  "6 — CSF PK"      = 6,
  "8 — CSF 2AG"     = 8
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("MAGLi PK Profile Viewer — Flag Outliers"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("cmt", "Compartment (CMT):", choices = CMT_CHOICES, selected = 1),
      selectInput("cohort", "Cohort:", choices = NULL),
      selectInput("patient", "Patient:", choices = NULL),
      hr(),
      h4("Display Options"),
      checkboxInput("show_magli", "Show MAGLi doses (CMT=0)", value = FALSE),
      checkboxInput("show_itra", "Show ITRA doses (CMT=11)", value = FALSE),
      checkboxInput("show_predose", "Highlight predoses (TROUGHFL)", value = TRUE),
      hr(),
      h4("Zoom"),
      p("Enter limits manually to zoom. Click Reset to go back.",
        style = "font-size: 11px; color: grey;"),
      fluidRow(
        column(6, numericInput("xmin", "Time min:", value = NA)),
        column(6, numericInput("xmax", "Time max:", value = NA))
      ),
      fluidRow(
        column(6, numericInput("ymin", "DV min:", value = NA)),
        column(6, numericInput("ymax", "DV max:", value = NA))
      ),
      actionButton("reset_zoom", "Reset Zoom", class = "btn-info btn-sm"),
      hr(),
      h4("Flagged Points"),
      verbatimTextOutput("flag_count"),
      hr(),
      downloadButton("download_flags", "Export Flagged Points (CSV)"),
      hr(),
      actionButton("clear_flags", "Clear All Flags", class = "btn-warning")
    ),

    mainPanel(
      width = 9,
      p("Click on a point to flag/unflag it"),
      p("Circle = observation | Triangle = predose (TROUGHFL)"),
      plotOutput("pk_plot",
        click = "plot_click",
        height = "500px"
      ),
      hr(),
      h4("Flagged Points Table"),
      DTOutput("flag_table")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # Load full data once
  full_dat <- reactive({
    df <- read.csv(DATA_PATH, header = TRUE)
    df %>%
      filter(STDY == 45248, COHORT %in% c(542, 543, 544, 545, 546)) %>%
      mutate(DV = as.numeric(DV))
  })

  # Observation data filtered by CMT
  dat <- reactive({
    full_dat() %>%
      filter(CMT == as.numeric(input$cmt), !is.na(DV))
  })

  # Dosing data for MAGLi (CMT=0)
  dose_magli <- reactive({
    full_dat() %>%
      filter(CMT == 0, EVID == 1) %>%
      select(COHORT, PT, TIME) %>%
      distinct()
  })

  # Dosing data for ITRA (CMT=11)
  dose_itra <- reactive({
    full_dat() %>%
      filter(CMT == 11, EVID == 1) %>%
      select(COHORT, PT, TIME) %>%
      distinct()
  })

  # Store flagged points
  rv <- reactiveValues(flagged = data.frame(
    CMT = integer(),
    COHORT = integer(),
    PT = integer(),
    TIME = numeric(),
    DV = numeric(),
    TROUGHFL = integer(),
    FLAG = character(),
    stringsAsFactors = FALSE
  ))

  # Update cohort choices when CMT changes
  observe({
    cohorts <- sort(unique(dat()$COHORT))
    updateSelectInput(session, "cohort", choices = cohorts)
  })

  # Update patient choices based on cohort
  observe({
    req(input$cohort)
    pts <- dat() %>%
      filter(COHORT == as.numeric(input$cohort)) %>%
      pull(PT) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "patient", choices = pts)
  })

  # Reset zoom when patient, cohort, or CMT changes
  observeEvent(c(input$patient, input$cohort, input$cmt), {
    updateNumericInput(session, "xmin", value = NA)
    updateNumericInput(session, "xmax", value = NA)
    updateNumericInput(session, "ymin", value = NA)
    updateNumericInput(session, "ymax", value = NA)
  }, ignoreInit = TRUE)

  # Current patient data
  pk_pt <- reactive({
    req(input$cohort, input$patient)
    d <- dat() %>%
      filter(COHORT == as.numeric(input$cohort), PT == as.numeric(input$patient)) %>%
      arrange(TIME)
    if (!"TROUGHFL" %in% names(d)) {
      d$TROUGHFL <- 0
    }
    d$TROUGHFL[is.na(d$TROUGHFL)] <- 0
    d$is_predose <- d$TROUGHFL == 1
    d
  })

  # Reset zoom button
  observeEvent(input$reset_zoom, {
    updateNumericInput(session, "xmin", value = NA)
    updateNumericInput(session, "xmax", value = NA)
    updateNumericInput(session, "ymin", value = NA)
    updateNumericInput(session, "ymax", value = NA)
  })

  # CMT label for plot title
  cmt_label <- reactive({
    names(CMT_CHOICES)[CMT_CHOICES == input$cmt]
  })

  # Plot
  output$pk_plot <- renderPlot({
    req(nrow(pk_pt()) > 0)

    d <- pk_pt()

    p <- ggplot(d, aes(x = TIME, y = DV)) +
      geom_line(color = "steelblue")

    # Points: different shape for predoses
    if (input$show_predose && any(d$is_predose)) {
      d_regular <- d %>% filter(!is_predose)
      d_predose <- d %>% filter(is_predose)

      if (nrow(d_regular) > 0) {
        p <- p + geom_point(data = d_regular, aes(x = TIME, y = DV),
                            color = "steelblue", size = 3, shape = 16)
      }
      if (nrow(d_predose) > 0) {
        p <- p + geom_point(data = d_predose, aes(x = TIME, y = DV),
                            color = "darkorange", size = 4, shape = 17)
      }
    } else {
      p <- p + geom_point(color = "steelblue", size = 3, shape = 16)
    }

    p <- p +
      labs(
        title = paste0("Cohort ", input$cohort, " — Patient ", input$patient,
                       " — ", cmt_label()),
        x = "Time",
        y = "Concentration (DV)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))

    # MAGLi dosing lines
    if (input$show_magli) {
      magli_pt <- dose_magli() %>%
        filter(COHORT == as.numeric(input$cohort), PT == as.numeric(input$patient))
      if (nrow(magli_pt) > 0) {
        p <- p + geom_vline(data = magli_pt, aes(xintercept = TIME),
                            linetype = "dashed", color = "red", alpha = 0.6)
      }
    }

    # ITRA dosing lines
    if (input$show_itra) {
      itra_pt <- dose_itra() %>%
        filter(COHORT == as.numeric(input$cohort), PT == as.numeric(input$patient))
      if (nrow(itra_pt) > 0) {
        p <- p + geom_vline(data = itra_pt, aes(xintercept = TIME),
                            linetype = "dashed", color = "green4", alpha = 0.6)
      }
    }

    # Apply zoom from manual inputs
    has_x <- !is.na(input$xmin) && !is.na(input$xmax)
    has_y <- !is.na(input$ymin) && !is.na(input$ymax)

    if (has_x && has_y) {
      p <- p + coord_cartesian(xlim = c(input$xmin, input$xmax), ylim = c(input$ymin, input$ymax))
    } else if (has_x) {
      p <- p + coord_cartesian(xlim = c(input$xmin, input$xmax))
    } else if (has_y) {
      p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }

    # Highlight flagged points
    fl <- rv$flagged
    if (nrow(fl) > 0) {
      fl_pt <- fl %>%
        filter(CMT == as.numeric(input$cmt),
               COHORT == as.numeric(input$cohort),
               PT == as.numeric(input$patient))
      if (nrow(fl_pt) > 0) {
        p <- p + geom_point(data = fl_pt, aes(x = TIME, y = DV),
                            color = "red", size = 5, shape = 4, stroke = 2)
      }
    }

    p
  })

  # Click handler — toggle flag on nearest point
  observeEvent(input$plot_click, {
    req(nrow(pk_pt()) > 0)
    click <- input$plot_click

    pt_data <- pk_pt()

    x_range <- diff(range(pt_data$TIME, na.rm = TRUE))
    y_range <- diff(range(pt_data$DV, na.rm = TRUE))
    if (x_range == 0) x_range <- 1
    if (y_range == 0) y_range <- 1

    dist <- sqrt(
      ((pt_data$TIME - click$x) / x_range)^2 +
      ((pt_data$DV - click$y) / y_range)^2
    )
    nearest <- pt_data[which.min(dist), ]

    fl <- rv$flagged

    match_idx <- which(
      fl$CMT == as.numeric(input$cmt) &
      fl$COHORT == nearest$COHORT &
      fl$PT == nearest$PT &
      abs(fl$TIME - nearest$TIME) < 1e-6
    )

    if (length(match_idx) > 0) {
      fl <- fl[-match_idx, , drop = FALSE]
    } else {
      new_flag <- data.frame(
        CMT = as.integer(input$cmt),
        COHORT = nearest$COHORT,
        PT = nearest$PT,
        TIME = nearest$TIME,
        DV = nearest$DV,
        TROUGHFL = ifelse(nearest$is_predose, 1, 0),
        FLAG = "OUTLIER",
        stringsAsFactors = FALSE
      )
      fl <- rbind(fl, new_flag)
    }

    rv$flagged <- fl
  })

  # Flag count
  output$flag_count <- renderText({
    paste0(nrow(rv$flagged), " point(s) flagged")
  })

  # Flag table
  output$flag_table <- renderDT({
    fl <- rv$flagged
    if (nrow(fl) == 0) {
      data.frame(CMT = integer(), COHORT = integer(), PT = integer(),
                 TIME = numeric(), DV = numeric(), TROUGHFL = integer(),
                 FLAG = character())
    } else {
      fl %>% arrange(CMT, COHORT, PT, TIME)
    }
  }, options = list(pageLength = 10))

  # Export
  output$download_flags <- downloadHandler(
    filename = function() {
      paste0("flagged_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$flagged %>% arrange(CMT, COHORT, PT, TIME), file, row.names = FALSE)
    }
  )

  # Clear all flags
  observeEvent(input$clear_flags, {
    rv$flagged <- data.frame(
      CMT = integer(),
      COHORT = integer(),
      PT = integer(),
      TIME = numeric(),
      DV = numeric(),
      TROUGHFL = integer(),
      FLAG = character(),
      stringsAsFactors = FALSE
    )
  })
}

# ---- Run ----
shinyApp(ui = ui, server = server)
