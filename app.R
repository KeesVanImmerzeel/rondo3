# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(reshape2)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
theme_update(text = element_text(size=15))

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Rondo"),
  tabsetPanel(
    tabPanel(title = "Model",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "r1",
                   label = "r1 (m):",
                   min = 500,
                   max = 1500,
                   value = 1000,
                   step = 10,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "r2",
                   label = "r2 (m):",
                   min = 1500,
                   max = 2500,
                   value = 2000,
                   step = 10,
                   width = '100%'
                 ),
                 
                 sliderInput(
                   inputId = "H1",
                   label = "H1 (m):",
                   min = -5,
                   max = 5,
                   value = 2,
                   step = .05,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "H2",
                   label = "H2 (m):",
                   min = -5,
                   max = 5,
                   value = 1,
                   step = .05,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "H3",
                   label = "H3 (m):",
                   min = -5,
                   max = 5,
                   value = 0,
                   step = .05,
                   width = '100%'
                 ),
                 
                 sliderInput(
                   inputId = "c1",
                   label = "c1 (d):",
                   min = 500,
                   max = 1500,
                   value = 1000,
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "c2",
                   label = "c2 (d):",
                   min = 1500,
                   max = 2500,
                   value = 2000,
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "c3",
                   label = "c3 (d):",
                   min = 2500,
                   max = 3500,
                   value = 3000,
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "kD1",
                   label = "kD1 (m2/d):",
                   min = 500,
                   max = 1500,
                   value = 1000,
                   step = 10,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "kD2",
                   label = "kD2 (m2/d):",
                   min = 500,
                   max = 1500,
                   value = 1000,
                   step = 10,
                   width = '100%'
                 ),
                 sliderInput(
                   inputId = "kD3",
                   label = "kD3 (m2/d):",
                   min = 500,
                   max = 1500,
                   value = 1000,
                   step = 1,
                   width = '100%'
                 ),
                 checkboxInput("logscale_xaxis", "Log scale", value = FALSE),
                 sliderInput(
                   inputId = "xmax",
                   label = "x-axis (max value)",
                   min = 10,
                   max = 10000,
                   value = 4000,
                   step = 10,
                   width = '100%'
                 )
               ),
               
               mainPanel(
                 plotOutput("plot_head"),
                 plotOutput("plot_q"),
                 plotOutput("plot_seepage")
                 
                 
                 #conditionalPanel(
                 #    condition = "input.plotMeasurements == true",
                 #    uiOutput("SelectGroup")
                 #   checkboxGroupInput(
                 #       "improve",
                 #       "Adjust these parameters to improve fit:",
                 #       c("kD1", "kD2", "KD3", "c1", "c2", "c3")
                 #   ),
                 #   actionButton("improve", "Improve")
                 # )
               )
             )),
    tabPanel(title = "Ranges",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "r1range",
                   label = "Range r1 (m)",
                   min = 10,
                   max = 10000,
                   value = c(500, 1500),
                   step = 10,
                   width = '100%'
                 ),
                 sliderInput(
                   "r2range",
                   label = "Range r2 (m)",
                   min = 10,
                   max = 10000,
                   value = c(1500, 2500),
                   step = 10,
                   width = '100%'
                 ),
                 
                 sliderInput(
                   "H1range",
                   label = "Range H1 (m)",
                   min = -100,
                   max = 100,
                   value = c(-5, 5),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "H2range",
                   label = "Range H2 (m)",
                   min = -100,
                   max = 100,
                   value = c(-5, 5),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "H3range",
                   label = "Range H3 (d)",
                   min = -100,
                   max = 100,
                   value = c(-5, 5),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "c1range",
                   label = "Range c1 (d)",
                   min = 0.1,
                   max = 10000,
                   value = c(500, 1500),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "c2range",
                   label = "Range c2 (d)",
                   min = 0.1,
                   max = 10000,
                   value = c(1500, 2500),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "c3range",
                   label = "Range c3 (d)",
                   min = 0.1,
                   max = 10000,
                   value = c(2500, 3500),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "kD1range",
                   label = "Range kD1 (m2/d)",
                   min = 0.1,
                   max = 10000,
                   value = c(500, 1500),
                   step = 10,
                   width = '100%'
                 ),
                 sliderInput(
                   "kD2range",
                   label = "Range kD2 (m2/d)",
                   min = 0.1,
                   max = 10000,
                   value = c(500, 1500),
                   step = 1,
                   width = '100%'
                 ),
                 sliderInput(
                   "kD3range",
                   label = "Range kD3 (m2/d)",
                   min = 0.1,
                   max = 10000,
                   value = c(500, 1500),
                   step = 1,
                   width = '100%'
                 ),
                 # Horizontal line ----
                 tags$hr(),
                 fileInput(
                   "file2",
                   "Choose CSV File with ranges",
                   multiple = TRUE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                 ),
                 
                 # Input: Select separator ----
                 radioButtons(
                   "sep2",
                   "Separator",
                   choices = c(
                     Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"
                   ),
                   selected = ";"
                 ),
                 
                 # Input: Select quotes ----
                 radioButtons(
                   "quote2",
                   "Quote",
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = ''
                 )
               ),
               mainPanel(tableOutput("ranges"))
             )),
    
    tabPanel(title = "Measurements",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Select a file ----
                 fileInput(
                   "file1",
                   "Choose CSV File with measurements",
                   multiple = TRUE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                 ),
                 # Horizontal line ----
                 tags$hr(),
                 # Input: Select separator ----
                 radioButtons(
                   "sep",
                   "Separator",
                   choices = c(
                     Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"
                   ),
                   selected = ";"
                 ),
                 
                 # Input: Select quotes ----
                 radioButtons(
                   "quote",
                   "Quote",
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = ''
                 ),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons(
                   "disp",
                   "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"
                 )
                 
               ),
               # Main panel for displaying outputs ----
               mainPanel(# Output: Data file ----
                         tableOutput("contents"))
             )),
    tabPanel(title = "Output",
             mainPanel(tableOutput("model_data"))),
    #tabPanel(title = "mmd",
    #         mainPanel(tableOutput("mmd"))
    #),
    tabPanel(title = "Documentation",
             includeHTML("www/Rondo3.html"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
############## Functions from package Rondo #######################################
  #' Internal function to determine the number of areas for which kD- and c-values are specified.
  #'
  #' @param kD Transmissivity (m2/d) (numeric vector).
  #' @return The number of areas for which kD- and c-values are specified (-) (integer).
  #' @details The number of circular areas is equal to the length of the vector kD minus 1.
  .create_m <- function(kD) {
    return(length(kD))
  }
  
  #' Internal function to calculate the leakage factor.
  #'
  #' @inheritParams .create_m
  #' @param c Hydraulic resistance of top layer (d) (numeric vector).
  #' @return Leakage factor (m) (numeric vector).
  #' @details The length of both input vectors should be the same.
  .create_l <- function(kD, c) {
    sqrt(kD * c)
  }
  
  #' Internal function to calculate the quotient of the transmissivity and the leakage factor.
  #'
  #' @inheritParams .create_m
  #' @param l Leakage factor (m) (numeric vector).
  #' @return Quotient of the transmissivity and the leakage factor (m/d) (numeric vector).
  #' @details The length of both input vectors should be the same.
  .create_kDl <- function(kD, l) {
    kD / l
  }
  
  #' Internal function to calculate the matrix with the quotients of the radius of the areas and the leakage factors.
  #'
  #' @inheritParams .create_kDl
  #' @param r Radius of areas (m) (numeric vector).
  #' @return Matrix with the quotients of the radius of the areas and the leakage factors (-) (matrix).
  #' @details The dimension of the matrix is (2, length(l)-1).
  .create_rl <- function(r, l) {
    m <- length(l)
    i <- c(2:m)
    rbind(r / l[1:(m - 1)], r / l[i])
  }
  
  #' Internal function to create a zero two-row matrix.
  #'
  #' @param m The number of areas for which kD- and c-values are specified (-) (integer).
  #' @return A zero two-row matrix. (matrix).
  #' @details The dimension of the matrix is (2, length(l)-1).
  .zero_rows <- function(m) {
    matrix(rep(0, 4 * m), nrow = 2)
  }
  
  #' Internal function to create two rows of the system matrix corresponding with area boundary j.
  #'
  #' @inheritParams .zero_rows
  #' @param j area boundary index (-) (integer).
  #' @param rl Matrix with the quotients of the radius of the areas and the leakage factors (-) (matrix).
  #' @param kDl Quotient of the transmissivity and the leakage factor (m/d) (numeric vector).
  #' @return Two rows of the system matrix corresponding with area boundary j (matrix).
  #' @details The dimension of the matrix is (2, 2*m).
  .create_rows <- function(j, m, rl, kDl) {
    res <- .zero_rows(m)
    i1 <- 1  + (j - 1) * 2
    i2 <- i1 + (m - 1) * 2 - 1
    res[1, i1:i2] <-
      c(besselI(rl[1, j], 0),
        besselK(rl[1, j], 0),-besselI(rl[2, j], 0),-besselK(rl[2, j], 0))
    res[2, i1:i2] <-
      c(
        kDl[j] * besselI(rl[1, j], 1),-kDl[j] * besselK(rl[1, j], 1),-kDl[j + 1] * besselI(rl[2, j], 1),
        kDl[j + 1] * besselK(rl[2, j], 1)
      )
    return(res)
  }
  
  #' Internal function to create the system matrix.
  #'
  #' @inheritParams .zero_rows
  #' @inheritParams .create_kDl
  #' @inheritParams .create_rows
  #' @return System matrix (matrix).
  #' @details The dimension of the matrix is (2m, 2*m).
  .create_A_matrix <- function(m, l, kDl, rl) {
    res <- lapply(1:(m - 1), function(j)
      rbind(.create_rows(j, m, rl, kDl))) %>% do.call(rbind, .) %>% rbind(.zero_rows(m))
    res[2 * m - 1, 2] <- 1
    res[2 * m, 2 * m - 1] <- 1
    return(res)
  }
  
  #' Internal function to create the right hand vector b in the linear equations.
  #'
  #' @param h Polder heads in the different areas (m) (numeric vector).
  #' @return right hand vector b in the linear equations.
  #' @details The length of the vector h should be equal to the length of the vectors kD and c.
  .create_b_vector <- function(h) {
    diff(h) %>% lapply(function(x)
      c(x, 0)) %>% unlist() %>% append(c(0, 0))
  }
  
  #' Internal function to determine the area number corresponding to a radius x.
  #'
  #' @inheritParams .create_rl
  #' @param x Radius (m) (numeric).
  #' @return Area number corresponding to a radius x (-) (integer).
  .get_index <- function(x, r) {
    r <- c(0, r)
    stats::approx(
      r,
      y = 1:length(r),
      xout = x,
      method = "constant",
      rule = c(2:2)
    )$y
  }
  
  #' Initialise a list (y) with parameters used for the calculations of the groundwater heads and fluxes.
  #'
  #' @inheritParams .create_m
  #' @inheritParams .create_l
  #' @inheritParams .create_rl
  #' @inheritParams .create_b_vector
  #' @return List with parameters used for the calculations of the groundwater heads and fluxes.
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' @export
  rd_init <- function(kD, c, r, h) {
    m <- .create_m(kD)
    l <- .create_l(kD, c)
    kDl <- .create_kDl(kD, l)
    rl <- .create_rl(r, l)
    A <- .create_A_matrix(m, l, kDl, rl)
    b <- .create_b_vector(h)
    coeff <- solve(A, b)
    list(
      m = m,
      l = l,
      kDl = kDl,
      rl = rl,
      c = c,
      r = r,
      h = h,
      coeff = coeff
    )
  }
  
  #' Internal function to create a list of the parameters corresponding to a distance x to be used for the calculations of the groundwater heads and fluxes (list).
  #'
  #' @inheritParams .get_index
  #' @param y List with parameters used for the calculations of the groundwater heads and fluxes.
  #' @return List of the parameters corresponding to a radius x to be used for the calculations of the groundwater heads and fluxes.
  .get_coeff <- function(x, y) {
    i <- .get_index(x, y$r)
    list(
      Ai = y$coeff[1 + 2 * (i - 1)],
      Bi = y$coeff[2 + 2 * (i - 1)],
      rl = max(x,.001) / y$l[i],
      h = y$h[i],
      kDl = y$kDl[i],
      c = y$c[i]
    )
  }
  
  #' Calculate the head at radius x.
  #'
  #' @inheritParams .get_coeff
  #' @return Head at radius x (m) (numeric).
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- 500
  #' .rd_phi(x, y)
  .rd_phi <- function(x, y) {
    y <- .get_coeff(x, y)
    y$Ai * besselI(y$rl, 0) + y$Bi * besselK(y$rl, 0) + y$h
  }
  
  #' Calculate the head at radius x (numeric vector).
  #'
  #' @inheritParams .get_coeff
  #' @param x Radius (m) (numeric vector).
  #' @return Head at radius x (m) (numeric vector).
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- seq(0,3000,by=100)
  #' rd_phi(x, y)
  #' @export
  rd_phi <- function(x, y) {
    .f <- Vectorize(.rd_phi,vectorize.args="x")
    .f(x,y)
  }
  
  #' Calculate the lateral discharge at radius x.
  #'
  #' @inheritParams .get_coeff
  #' @return Lateral discharge (m3/d) (numeric).
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- 500
  #' .rd_q(x, y)
  .rd_q <- function(x, y) {
    y <- .get_coeff(x, y)
    - 2 * pi * y$kDl * x * (y$Ai * besselI(y$rl, 1) - y$Bi * besselK(y$rl, 1))
  }
  
  #' Calculate the lateral discharge at radius x (numeric vector).
  #'
  #' @inheritParams rd_phi
  #' @return Lateral discharge (m3/d) (numeric).
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- seq(0,3000,by=100)
  #' rd_q(x, y)
  #' @export
  rd_q <- function(x, y) {
    .f <- Vectorize(.rd_q,vectorize.args="x")
    .f(x,y)
  }
  
  #' Internal function to determine if the radius x corresponds with a area boundary.
  #'
  #' @inheritParams .get_index
  #' @inheritParams .create_rl
  #' @return TRUE if radius x corresponds with a area boundary (Boolean).
  .is_on_border <- function(x, r){
    which(r==x) %>% length() == 1
  }
  
  #' Calculate the seepage intensity at radius x.
  #'
  #' @inheritParams .get_coeff
  #' @return Seepage intensity (m/d) (numeric).
  #' @details Positive = downwards flow; negative = upwards flow.
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- 500
  #' .rd_seep(x, y)
  .rd_seep <- function(x, y) {
    if (!.is_on_border(x, y$r)) {
      phi <- rd_phi(x, y)
      y <- .get_coeff(x, y)
      (y$h - phi) / y$c
    } else {
      s1 <- rd_seep(x - .01, y)
      s2 <- rd_seep(x + .01, y)
      mean(c(s1,s2))
    }
  }
  
  #' Calculate the seepage intensity at radius x.
  #'
  #' @inheritParams rd_phi
  #' @return Seepage intensity (m/d) (numeric).
  #' @details Positive = downwards flow; negative = upwards flow.
  #' @examples
  #' kD <- c(1000, 1000, 1000)
  #' c <- c(1000, 2000, 3000)
  #' r <- c(1000, 2000)
  #' h <- c(10, 9, 8)
  #' y <- rd_init(kD, c, r, h)
  #' x <- seq(0,3000,by=100)
  #' rd_seep(x, y)
  #' @export
  rd_seep <- function(x, y){
    .f <- Vectorize(.rd_seep,vectorize.args="x")
    .f(x,y)
  }  
  
  ############## Functions from package Rondo #######################################  
  
  
  
    cbr <- brewer.pal(n = 3, name = 'Set1')
    MyPalette <-
        c("Aq1" = cbr[1],
          "Aq2" = cbr[2],
          "Aq3" = cbr[3])
    
    rv <- reactiveValues(observations = NULL, ranges = NULL)
    
    # Upload measurements
    observeEvent(input$file1, {
        req(input$file1)
        df <- read.csv2(
            input$file1$datapath,
            header = TRUE,
            sep = input$sep,
            quote = input$quote
        )
        df$observation <- TRUE #for plotting
        rv$observations <- df
    })
    
    # Upload ranges
    observeEvent(input$file2, {
        req(input$file2)
        df <- read.csv2(
            input$file2$datapath,
            header = TRUE,
            sep = input$sep2,
            quote = input$quote2
        )
        rv$ranges <- df
        updateSliderInput(session,
                          "kD1range",
                          value = c(df[1, 1], df[1, 3]))
        updateSliderInput(session,
                          "kD2range",
                          value = c(df[2, 1], df[2, 3]))
        updateSliderInput(session,
                          "kD3range",
                          value = c(df[3, 1], df[3, 3]))
        updateSliderInput(session,
                          "c1range",
                          value = c(df[4, 1], df[4, 3]))
        updateSliderInput(session,
                          "c2range",
                          value = c(df[5, 1], df[5, 3]))
        updateSliderInput(session,
                          "c3range",
                          value = c(df[6, 1], df[6, 3]))
        updateSliderInput(session,
                          "H1range",
                          value = c(df[7, 1], df[7, 3]))
        updateSliderInput(session,
                          "H2range",
                          value = c(df[8, 1], df[8, 3]))
        updateSliderInput(session,
                          "H3range",
                          value = c(df[9, 1], df[9, 3]))
        updateSliderInput(session,
                          "r1range",
                          value = c(df[10, 1], df[10, 3]))
        updateSliderInput(session,
                          "r2range",
                          value = c(df[11, 1], df[11, 3]))
        
    })
    
    observeEvent(input$kD1range, {
        updateSliderInput(
            session,
            "kD1",
            min = input$kD1range[1],
            max = input$kD1range[2],
            value = rv$ranges[1, 2]
        )
        
    })
    observeEvent(input$kD2range, {
        updateSliderInput(
            session,
            "kD2",
            min = input$kD2range[1],
            max = input$kD2range[2],
            value = rv$ranges[2, 2]
        )
    })
    observeEvent(input$kD3range, {
        updateSliderInput(
            session,
            "kD3",
            min = input$kD3range[1],
            max = input$kD3range[2],
            value = rv$ranges[3, 2]
        )
    })
    
    observeEvent(input$c1range, {
        updateSliderInput(
            session,
            "c1",
            min = input$c1range[1],
            max = input$c1range[2],
            value = rv$ranges[4, 2]
        )
    })
    observeEvent(input$c2range, {
        updateSliderInput(
            session,
            "c2",
            min = input$c2range[1],
            max = input$c2range[2],
            value = rv$ranges[5, 2]
        )
    })
    observeEvent(input$c3range, {
        updateSliderInput(
            session,
            "c3",
            min = input$c3range[1],
            max = input$c3range[2],
            value = rv$ranges[6, 2]
        )
    })
    observeEvent(input$H1range, {
        updateSliderInput(
            session,
            "H1",
            min = input$H1range[1],
            max = input$H1range[2],
            value = rv$ranges[7, 2]
        )
    })
    observeEvent(input$H2range, {
        updateSliderInput(
            session,
            "H2",
            min = input$H2range[1],
            max = input$H2range[2],
            value = rv$ranges[8, 2]
        )
    })
    observeEvent(input$H3range, {
        updateSliderInput(
            session,
            "H3",
            min = input$H3range[1],
            max = input$H3range[2],
            value = rv$ranges[9, 2]
        )
    })
    
    observeEvent(input$r1range, {
        updateSliderInput(
            session,
            "r1",
            min = input$r1range[1],
            max = input$r1range[2],
            value = rv$ranges[10, 2]
        )
    })
    observeEvent(input$r2range, {
        updateSliderInput(
            session,
            "r2",
            min = input$r2range[1],
            max = input$r2range[2],
            value = rv$ranges[11, 2]
        )
    })
    
    output$contents <- renderTable({
        if (input$disp == "head") {
            return(head(rv$observations))
        }
        else {
            return(rv$observations)
        }
    })
    
    output$ranges <- renderTable({
        rv$ranges
    })
    
    r <- eventReactive(c(input$r1,
                         input$r2), {
                             c(input$r1, input$r2) # (m)
                         })
    c_ <- eventReactive(c(input$c1,
                          input$c2,
                          input$c3), {
                              c(input$c1, input$c2, input$c3) # (d)
                          })
    kD <- eventReactive(c(input$kD1,
                          input$kD2,
                          input$kD3), {
                              c(input$kD1, input$kD2, input$kD3) # (m2/d)
                          })
    h <- eventReactive(c(input$H1,
                         input$H2,
                         input$H3), {
                             c(input$H1, input$H2, input$H3) # (m)
                         })
    
    y <- reactive({
        rd_init(kD(), c_(), r(), h())
    })
    
    xmin <- reactive({
        0
    })
    xmax <- reactive({
        input$xmax
    })
    xstep <- reactive({
        25
    })
    x <- reactive({
        seq(xmin(), xmax(), xstep())
    })
    
    model_data <- reactive({
        df <-
            data.frame(
                x = x(),
                phi = rd_phi(x(), y()),
                q = rd_q(x(), y()),
                seep = rd_seep(x(), y())
            )
        names(df) <- c("x", "Head", "Q", "Seepage")
        return(df)
    })
    
    melted_model_data <- reactive({
        mmd <- model_data()
        mmd$x %<>% as.character()
        mmd %<>%
            reshape2::melt(id.vars = c("x"))
        mmd$x %<>%  as.numeric(.)
        #mmd$variable <- NULL
        mmd$observation <- FALSE
        mmd$location <- "model"
        return(mmd)
    })
    
    output$model_data <- renderTable({
        df <- model_data()
        data.frame(x=df$x, head=df$Head, q=df$Q, Seepage=format(df$Seepage,digits=4,scientific=FALSE))
    })
    
    observations_are_available <- reactive({
        !is.null(rv$observations)
    })
    
    ranges_are_read <- reactive({
        !is.null(rv$ranges)
    })
    
    nr_of_observations <- reactive({
        if (observations_are_available()) {
            nrow(rv$observations)
        } else {
            0
        }
    })
    
    observation_filename <- reactive({
        if (observations_are_available()) {
            input$file1[[1]]
        } else {
            ""
        }
    })
    
    graph_title <- reactive({
        observation_filename() %>% sub(pattern = "(.*?)\\..*$",
                                       replacement = "\\1",
                                       basename(.))
    })
    
    melted_data <- reactive({
        md <- melted_model_data()
        if (observations_are_available()) {
            rv$observations <-
                select(rv$observations, names(md))
            md <-
                rbind(md, rv$observations) #Combine observations with model output for plot
        }
        return(md)
    })
    
    
    output$mmd <- renderTable({
        melted_model_data()
    })
    
    plt <- reactive({
        plt <- ggplot(melted_data(),
               aes(
                   x = x,
                   y = value
               )) +
            xlab("Distance (m)") +
            scale_colour_manual(values = MyPalette) + 
            theme(legend.title = element_blank())
        if (input$logscale_xaxis) {
            plt <- plt + scale_x_continuous(trans = 'log10')
        }
        return(plt)
    })
    
    output$plot_head <- renderPlot({
        plt <- plt() +
            geom_line(
                data = dplyr::filter(.data = melted_data(), observation == FALSE, variable ==
                                         "Head"),
                size = 1,
                linetype = "dashed"
            )  +
            ylab("Head (m+ref)")
        if (observations_are_available()) {
            .data <- melted_data() %>% filter(observation == TRUE)
            plt <- plt + geom_point(data = .data, size = 3) +
                ggtitle(graph_title())
            if (input$Labels) {
                plt <-
                    plt + ggrepel::geom_label_repel(
                        aes(label = ifelse(
                            observation == TRUE, location, ""
                        )),
                        box.padding   = 0.35,
                        point.padding = 0.5,
                        segment.color = 'grey50',
                        show.legend = FALSE
                    )
            }
        }
        return(plt)
    })
    
    output$plot_q <- renderPlot({
        plt <- plt() +
            geom_line(
                data = dplyr::filter( .data = melted_data(), observation == FALSE, variable=="Q"),
                size = 1,
                linetype = "dashed"
            )  +
            ylab("Q (m3/d)") 
        return(plt)
    })
    
    output$plot_seepage <- renderPlot({
        plt <- plt() +
            geom_line(
                data = dplyr::filter( .data = melted_data(), observation == FALSE, variable=="Seepage"),
                size = 1,
                linetype = "dashed"
            )  +
            ylab("Seepage (mm/d)") 
        return(plt)
    })    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
