## Farkle Probability Simulator ##
## Jefferson Twombly ##
## 04-21-2026 ##

library(shiny)

ui <- fluidPage(
  titlePanel("Farkle Roll Simulation"),
  
  selectInput("type_prob", "Type of Event",
              choices = list("Of A Kind" = 1, "Unique Pair" = 2, "Farkle" = 3),
              selected = 1),
  
  numericInput("num_sims", "Number of Simulations", value = 1000),
  
  # conditional oak panel
  conditionalPanel(condition = "input.type_prob == 1",
                   
                   selectInput("num_dice", "Select # of Dice",
                               choices = c(6,5,4,3), selected = 6),
          
                   selectInput("num_oak", "Select # of a kind",
                               choices = c(6,5,4,3), selected = 5),
                   
                   verbatimTextOutput("text1")
                   ),
  
  # conditional unique pair panel
  conditionalPanel(condition = "input.type_prob == 2",
                   
                   
                   selectInput("pair_type", "Which Unique Pair",
                               choices = list("Triple Pairs" = 2, "Two Triples" = 3, "Straight" = 1)),
                   
                   verbatimTextOutput("text2")
                   ),
  
  # conditional farkle panel
  conditionalPanel(condition = "input.type_prob == 3",
                   
                   selectInput("num_dice", "Select # of Dice",
                               choices = c(6,5,4,3,2), selected = 6),
                   
                   verbatimTextOutput("text3"),
                   
                   verbatimTextOutput("text4")
                   )

  
)

server <- function(input, output) {
  
  ## universal dice object ##
  dice <- seq(1,6,1)
  
  ## OAK CODE ##
  dice_sims <- reactive({
    replicate(input$num_sims,
              sample(dice, input$num_dice, replace = TRUE))
  })
  
  OAK_test <- function(x, y) {
    any(table(x) == y)
  }
  
  output$text1 <- renderText({
    
    sims <- dice_sims()
    
    # apply function to rows
    results <- apply(sims, 2, function(col) {
      OAK_test(col, input$num_oak)
    })
    
    prop <- mean(results)
    
    paste0("Across ", input$num_sims,
           " simulations, the proportion of ",
           input$num_oak, "-of-a-kind is ",
           prop, " (", prop*100, "%)")
    
  })
    ## END OAK CODE ##
    
    ## Unique pairs CODE ##
    dice_sims <- reactive({
      replicate(input$num_sims,
                sample(dice, 6, replace = TRUE))
    })
    
    up_test <- function(x, y) {
      all(table(x) == y)
    }
    
    output$text2 <- renderText({
      
      sims <- dice_sims()
      
      results <- apply(sims, 2, function(col) {
        up_test(col, input$pair_type)
      })
      
      prop <- mean(results)
      
      paste0("Across ", input$num_sims,
             " simulations, the proportion of this combination is ",
             prop, " (", prop*100, "%)")
      
    })
    ## End UP CODE ##
    
    ## Farkle CODE ##
    
    dice_sims <- reactive({
      replicate(input$num_sims,
                sample(dice, input$num_dice, replace = TRUE))
    })
    
    farkle_test <- function(x) {
      counts <- table(x)
      n <- length(x)
      
      has_1_or_5 <- any(x %in% c(1, 5))
      has_n_of_kind <- any(counts >= 3)
      
      # only possible with 6 dice
      three_pairs <- (n == 6) && length(counts) == 3 && all(counts == 2)
      two_triplets <- (n == 6) && length(counts) == 2 && all(counts == 3)
      
      has_1_or_5 ||
        has_n_of_kind ||
        three_pairs ||
        two_triplets
    }
    
    num_dice <- reactive({input$num_dice})
    
        output$text3 <- renderText({

                sims <- dice_sims()
                
                # apply function to rows
                results <- apply(sims, 2, function(col) {
                  farkle_test(col)
                })
                
                # results that do not have 1s or 5s (binary t/f)
                prop <- round(1-mean(results),4)
                
                paste("Across ", input$num_sims,
                       " simulations, with ", input$num_dice, " dice, the proportion of Farkles is ",
                       prop, " (", prop*100, "%)")
          
        }) 
        
        output$text4 <- renderText({
          
          paste("Probability of a farkle with one die is 2/3. Not coding that!")
          
        })
    
        ## End FARKLE CODE ##
      
}

shinyApp(ui, server)