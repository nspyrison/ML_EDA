### "Primary" ui.R -----
require("spinifex")
require("tourr")
require("Rdimtools")
require("ggplot2")
require("plotly")
require("tibble")
require("tidyr")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shiny")
require("shinyjs")     ## Extend JavaScript (Think HTML interactivity) control and formating, 
## Also see ?shinyjs::toggle   &   https://daattali.com/shiny/shinyjs-basic/
##### Additionally used in 'primary' and 'devUnderConstruction':
require("shinyBS")  ## BootStrap functionality, such as tooltips and popovers
## Also see ?shinyBS::bsTooltip   &   https://github.com/ebailey78/shinyBS/
require("DT")       ## HTML tabbles for the gallery table

require("mlbench")
mlb_dat <- data(package = "mlbench")$results[, 3L]

do_show_dev_disp <- TRUE
palette(RColorBrewer::brewer.pal(8L, "Dark2"))

##### tab1_input -----
### Input data, default to flea
tab1_input <- tabPanel("Input", fluidPage(
    sidebarPanel(width = 3L,
      ## Input csv file
      selectInput("data_select", "Data select {mlbench}",
                  choices = mlb_dat
      ),

      # ## TODO: Apply load data
      # fileInput("data_file", "Data file (.csv format)",
      #           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      # ),
      ## TODO: wrap into raw_dat_dt, really should be converted into a RenderUI
      ## variables in projection, and target variable
      checkboxGroupInput("inc_vars",
                         label = "Projection variables",
                         choices =  names(tourr::flea[, 1L:6L]),
                         selected = names(tourr::flea[, 1L:6L])
      ),
      radioButtons("aes_var", label = "Color and shape variable",
                   choices =  names(tourr::flea[]),
                   selected =  names(tourr::flea[])[7]
      ),
      ## Preprocessing
      verbatimTextOutput("na_msg"),
      radioButtons("scale_mode", "scale",
                   choices = c("std dev",
                               "[0, 1]",
                               "none"),
                   selected = "std dev"),
      checkboxInput("do_sphere", "sphere", 
                    value = FALSE),
      ### TODO: these need to have not too many levels, 8 or so?
      ## Aesthetic selection 
      # h3("Aesthic options")
      # fluidRow(column(6L, selectInput("col_var_nm", "Point color", "<none>")),
      #          column(6L, selectInput("pch_var_nm", "Point shape", "<none>")))
    ),
    ## Main panel display
    mainPanel(h3("Input data"),
              verbatimTextOutput("raw_dat_str"),
              h3("Processed data summary"),
              verbatimTextOutput("proc_dat_smry"),
    ) ## close mainPanel
)) ## Assign tab1_input


##### tab2_eda ----
###  Explore PC-space
tab2_eda <- tabPanel("Explore PC-space", fluidPage(
  ### Row 1, PC screeplot, tour 
  fluidRow(
    ## Left column, screeplot, buttons
    column(width = 6L,
           h3("Screeplot"),
           plotOutput("pc_screeplot"),
           column(width = 6L,
                  actionButton("sw_less", "< Remove a variable"),
           ),
           column(width = 6L,
                  actionButton("sw_more", "Add a variable >"),
           ),
    ), ## Close column, left
    ## Right column, Stepwise plot, tour mode buttons
    column(width = 6L,
           h3("Linear embedding"),
           plotly::plotlyOutput("tour_plotly"),
           radioButtons("tour_mode", "Tour mode",
                        choices = c("stepwise (WIP)",
                                    "local",
                                    "grand"),
                        selected = "grand",
                        inline = TRUE)
    ), ## Close column, right
    ### Row 2
    fluidRow(
      ## Left column, pc_density_plot
      column(width = 6L,
             h3("PC densities"),
             plotOutput("pc_density_plot") ## wants + facet_wrap(~var(PC_num))
      ),
      ## Left column, pc_density_plot
      column(width = 6L,
             h3("Non-linear embedding"),
             plotOutput("tsne_plot") ## wants + facet_wrap(~var(PC_num))
      ),
    )
    ))) ## Assign tab2_eda

##### tab3_output -----
tab3_output <- tabPanel("Output", fluidPage(
  h3("Work in progress"),
  h2("Thoughts:"),
  p("-PC space (.rds, .csv)"),
  p("-processed data (.rds)"),
  p("-graphics (subset, all?, static?) (.png, .gif?)")
)) ## Assign tab3_output

##### tab3_output -----
tab4_about <- tabPanel("About", fluidPage(
  h3("Work in progress"),
  h2("Thoughts:"),
  p("-Context and scope"),
  p("-ML pyramid/ pipeline"),
  p("Sources/refferences")
)) ## Assign tab4_about

##### dev_disp -----
dev_disp <- if(do_show_dev_disp == T){
  fluidPage(
    h3("===== Dev Display below ====="),
    actionButton("dev_browser", "Run browser()"),
    verbatimTextOutput("dev_disp")
  )
}else{NULL}

###### Full app layout ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), 
                ## Esp see the themes: "flatly", "spacelab", "journal"
                ## Make the lines, hr() black:
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                shinyjs::useShinyjs(),
                ## Content:
                navbarPage("Machine learning exploratory data analysis",
                           tab1_input,
                           tab2_eda,
                           tab3_output,
                           tab4_about),
                #h5(contextLine, style = "color: #A9A9A9"),
                dev_disp
)
