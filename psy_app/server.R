#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


function(input, output) {
  
  # Dynamically updated slider input based on month selected
  output$dyn_range <- renderUI({
    req(input$month)
    
    foo <- psy %>%
      filter(month == input$month) %>%
      pull(date) %>%
      range()
    
    sliderInput("daterange", "Select date",
                min = foo[1],
                max = foo[2],
                value = foo,
                width = '100%'
    )
    
  })
  
  # Initial selection of no points
  selected <- reactiveVal(rep(FALSE, nrow(psy)))
  
  # Create observed points within brush area
  observeEvent(input$plot1_brush, {
               brushed <- brushedPoints(psy, input$plot1_brush, allRows = TRUE)$selected_
               selected(brushed | selected())
  })
  
  # Reset point if double clicked
  observeEvent(input$plot_reset, {
    dblclick <- !nearPoints(psy, input$plot_reset, allRow = TRUE)$selected_
    selected(dblclick | selected())
    # selected(rep(FALSE, nrow(psy)))
  })
  
  # Initial selection of whole dataset
  psy_out <- reactiveValues(data = psy)
  
  # ggplot timeseries of psy data
  output$p <- renderPlot({
    req(input$daterange)
    
    # Update with selected
    psy$sel <- selected()
    
    # Filter and plot
    psy %>%
      filter(month == input$month) %>%
      filter(date >= input$daterange[1],
             date <= input$daterange[2]) %>%
      ggplot() +
      geom_point(aes(x = dt, y = corrected_water_potential_m_pa,
                     color = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE")) +
      theme_bw(base_size = 16)
  })
  
  output$brush_info_remove <- renderPrint({
    brushedPoints(psy_out$data, brush = input$plot1_brush, allRows = FALSE) 
  })
  
  brush_info_all <- reactive({
    brushedPoints(psy_out$data, brush = input$plot1_brush, allRows = TRUE) 
  })
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(brush_info_all(), file)
    }
  )
  
}