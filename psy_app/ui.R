#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#



page_sidebar(
  title = "Psychrometer cleaning",
  sidebar = sidebar(
    title = "Inputs",
    selectInput("month", "Select month", 
                unique(psy$month))
  ),
  layout_columns(col_widths = c(8, 4),
                 card(
                   card_header("Timeseries"),
                   uiOutput("dyn_range"),
                   plotOutput("p",
                              brush = brushOpts(id = "plot1_brush"),
                              dblclick = "plot_reset")
                 ),
                 card(
                   card_header("Points to remove"),
                   verbatimTextOutput("brush_info_remove"),
                   downloadButton("download_all", "Download")
                 ))
  
)

