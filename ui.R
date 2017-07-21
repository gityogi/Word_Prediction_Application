library(shinythemes)

shinyUI(fluidPage( theme = shinytheme("darkly"),
  titlePanel("Word Prediction Application"),
  img(src='coursera.jpg', align = "right" , height = "50", width ="200"),
  img(src='swiftkey.jpg', align = "right" , height = "50", width ="200"),
  img(src='hopkins.png', align = "right" , height = "50", width ="200"),
  
  
  
    
    fluidRow(
           
             sidebarPanel( textInput('inputString', label = h4('What is in your mind...')))

                 
    
  ),
  fluidRow(
    
    column(12,
           wellPanel(
            
             submitButton("Submit")
             
           )       
    )
  ),
  
  fluidRow(
    
    column(3,
           wellPanel(
             h3('Predicted Text :'), 
             htmlOutput("dynamicText")
             
             
           )       
    ),
    column(5,
           wellPanel(
             h3('Graph :'), 
             plotOutput("wordPlot")
             
           )       
    ),
    column(4,
           wellPanel(
             h3("Instructions"),
             h5("This Shiny App predicts the next word based on the text you typed."),
             HTML("<ul><li>Enter your word/phrase for prediction.</li><li>Click on the submit button.</li><li>The Predicted text will show all the predicited next word(s).</li><li>Graph showing percentage for respective word.</li></ul>"),
             br(),
             p(span("Please allow a few seconds fort the output  to appear",style = "color:red"))
             
           )       
    )
  )
))




