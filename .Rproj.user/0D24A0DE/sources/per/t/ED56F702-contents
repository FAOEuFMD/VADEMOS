userdata <- function(){
  wellPanel(
  div(
    style = "text-align: center;",
    #h4("You must login to view this content"),
    tags$iframe(# src="https://forms.gle/6S1647i639bVk4mw7",
                src="https://docs.google.com/forms/d/e/1FAIpQLSf6ed-K6ba_wXiTC7PtPTlyd-A7Fp6TLlwcMVIMlt2q4VADQA/viewform",
                width=700, height=900, frameborder=0, marginheight=10)
  ),
  br(),
  br(),
  # div(
  #   style = "text-align: center;",
  #   actionButton("submitbutton", "Go to VADEMOS", class = "btn btn-primary",
  #                icon = icon("arrow-alt-circle-right"),
  #                style = "background-color: #073f23; border-color:transparent")
  # )
  )
}

feedback <- function() {
                wellPanel(
                  tags$h4("GET IN TOUCH", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                  tags$h5("Send us a question or comment", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                  br(),
                  textInput("contact_name", "Name"),
                  textInput("contact_email", "Email"),
                  textAreaInput("contact_message", "Message", height=150),
                  br(),
                  div(
                    style = "text-align: center;",
                    actionButton("sendbutton", "Send", style = "color: white; background-color:#16834c;
                                                                      padding: 10px 15px; width: 150px; cursor: pointer;
                                                                      font-size: 18px; font-weight: 600;"),
                    
                    shinyjs::hidden(
                      div(id = "missingfield",
                          tags$p("Please leave a message",
                                 style = "color: red; font-weight: 600; 
                                                                      padding-top: 5px;font-size:16px;", 
                                 class = "text-center"))),
                    br(),
                    br()
                  ) #div
                ) # wellPanel
}