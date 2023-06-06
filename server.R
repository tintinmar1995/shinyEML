#
# An example of a shiny application where user can download
# an .EML file ready to be sent to co-workers.
#
# Pretty useful for report automation.
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
          to("bob@protonmail.com") %>%
          cc("craig@protonmail.com") %>% 
          subject("Créer automatiquement des emails de rapport depuis Shiny!")
        
        # Prepare image and save it in a temporary folder
        png("my_plot.png")
        plot(my_plot())
        dev.off()
        
        addInlineImage <- function(img, height, width){
          img_base64 <- base64enc::base64encode(img)
          # Insert fame balise
          return(p(paste0(
            '<iimmgg width="', width,'", height="', height,'" src="data:image/jpeg;base64,', img_base64,'"></iimmgg>')))
        }
        
        # Write email body
        email <- email %>% html(
          tagList(
            h2("Hello World!"),
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
            addInlineImage("my_plot.png", "180px", "200px")))
        
        # Write EML file
        # writeLines instead of cat to ensure consistent CRLF
        # Thanks to X-Unset, Outlook will open eml file as draft
        eml <- as.character(email)
        # Transform fake balise into real balise
        eml <- gsub("<p>&lt;iimmgg", "<img ", eml, fixed=TRUE)
        eml <- gsub("&gt;/iimmgg&lt;</p>", "/>", eml, fixed=TRUE)
        eml <- strsplit(eml, "\\r\\n")
        writeLines(c("X-Unsent: 1", eml[[1]]), con = file)
      }
    )
    
}
