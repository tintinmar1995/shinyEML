#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    my_plot <- reactive({
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      h <- hist(
        x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
      
      return(h)
    })
  
    output$distPlot <- renderPlot({my_plot()})

    output$go <- downloadHandler(
      filename = function() {
        paste("auto-email-", Sys.Date(), ".eml", sep="")
      },
      content = function(file) {
        library(emayili)
        library(htmltools)
        options(envelope.details = TRUE)
        
        email <- emayili::envelope()
        email <- email %>%
          to("bob@google.com") %>%
          cc("craig@google.com") %>% 
          subject("Créer automatiquement des emails de rapport depuis Shiny!")
        
        # Prepare image and attach image
        png("my_plot.png")
        plot(my_plot())
        dev.off()
        img_base64 <- base64enc::base64encode("my_plot.png")
        #email <- email %>% attachment(path = "my_plot.png", cid = "my_plot")
        
        # Write email body
        email <- email %>% html(
          tagList(
            h2("Hello"),
            p("World!"),
            img(src=sprintf("data:image/png;base64,%s", img_base64), width='200px', height='180px'),
            #img(src='cid:my_plot', width='50px', height='50px')
          )
        )
        
        # Write EML file
        # writeLines instead of cat to ensure consistent CRLF
        # Thanks to X-Unset, Outlook will open eml file as draft
        eml <- as.character(email)
        eml <- strsplit(eml, "\\r\\n")
        writeLines(c("X-Unsent: 1", eml[[1]]), con = file)
      }
    )
    
}
