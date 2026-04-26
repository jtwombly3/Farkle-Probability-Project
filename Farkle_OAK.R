## Farkle Probability Simulator ##
## Jefferson Twombly ##
## 04-21-2026 ##
 # 
 # 
 # Oh man.
 # This was pretty interesting because it was my first time using conditional panels.
 # I think it gives a little structure to the shiny app and keeps it from looking cluttered.
 # 
 # On the server side, the OAK probabilities and the unique type probabilites were relatively
 # easy to calculate because the logic was simple using any/all operators.
 # Farkle was pretty difficult however.
 # The conditions and the mutual exclusivity threw me through a loop.
 # The "|" operator was the saving grace.
 # Also, building a function that was adaptable to farkles with different dice numbers
 # was critical so that using if statements was not necessary.
 # To be honest, if statements give me tunnel vision. When I see an if statement I want to write
 # one million of them and hardcode every contingency.
 # 
 # Main sources for this are r shiny gallery, r shiny postit, r documentation, and chat gpt
 # Just for my own ego, know that I tried to minimize my use of chatgpt and only used it when I was legitimately flummoxed
 # Which was defining the function for farkle_test and fighting with if statements which ended up not being used

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
                               choices = list("Three Pairs" = 2,
                                              "Two Triples" = 3,
                                              "Straight" = 1,
                                              "Four of a Kind w/ Pair" = 4)),
                   
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
      ifelse(y %in% c(1,2,3),
             all(table(x) == y),
             (any(table(x) == y) & any(table(x) == (6-as.numeric(y))))
             )
    }
    
    fourpair_test <- function(x, y, z=(6-y)) {
      any(table(x) == y) & any(table(x) == z)
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