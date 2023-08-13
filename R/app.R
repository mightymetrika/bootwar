bootwar <- function(){

  ui <- shiny::fluidPage(
    shiny::titlePanel("Bootwar Game"),

    # Settings Panel
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("mode", "Select Mode:", choices = c("t", "pt")),
        shiny::selectInput("deck", "Select Deck:", choices = c("Standard", "Anonymous")),
        shiny::numericInput("conf.level", "Confidence Level:", 0.95),
        shiny::numericInput("nboot", "Number of Bootstrap Resamples:", 1000),
        shiny::numericInput("seed", "Seed:", 123),
        shiny::sliderInput("rounds", "Number of Rounds:", min = 1, max = 26, value = 5),
        shiny::actionButton("new_game", "Start New Game"),
        shiny::actionButton("deal_card", "Deal Card")
      ),

      # Gameplay and Results Area
      shiny::mainPanel(
        shiny::h3("Player"),
        shiny::verbatimTextOutput("player_card"),
        shiny::verbatimTextOutput("player_value"),
        shiny::verbatimTextOutput("player_stats"),

        shiny::h3("Computer"),
        shiny::verbatimTextOutput("comp_card"),
        shiny::verbatimTextOutput("comp_value"),
        shiny::verbatimTextOutput("comp_stats"),

        shiny::h3("Statistics"),
        shiny::verbatimTextOutput("effect_stats"),

        shiny::h3("Results"),
        shiny::verbatimTextOutput("winner"),
        shiny::tableOutput("test_stats"),
        shiny::tableOutput("effect_size")
        # Add histograms here in the future
      )
    )
  )

  server <- function(input, output, session) {
    # Game state
    game_state <- shiny::reactiveVal(list(deck = NULL, player_values = numeric(0), comp_values = numeric(0)))

    shiny::observeEvent(input$new_game, {
      # Shuffle the deck
      new_deck <- shuffle_deck(seed = as.integer(input$seed))
      game_state(list(deck = new_deck, player_values = numeric(0), comp_values = numeric(0)))
    })

    shiny::observeEvent(input$deal_card, {
      # Deal a card for the round
      current_state <- game_state()
      round_result <- play_round(cdeck = current_state$deck,
                                 comp_cv = character(0), comp_vv = current_state$comp_values,
                                 plyr_cv = character(0), plyr_vv = current_state$player_values)

      # Update game state
      game_state(list(deck = round_result$updated_deck,
                      player_cards = round_result$plyr_cv,
                      player_values = round_result$plyr_vv,
                      comp_cards = round_result$comp_cv,
                      comp_values = round_result$comp_vv))

      # Now, after updating the game state, calculate the scores
      scores <- score_keeper(game_state()$player_values, game_state()$comp_values, mode = input$mode)

      # Add the scores to the game state
      current_game_state <- game_state()
      current_game_state$player_scores <- scores
      game_state(current_game_state)

    })

    # Update UI based on game state
    output$player_card <- shiny::renderText({ utils::tail(game_state()$player_cards, 1) })
    output$comp_card <- shiny::renderText({ utils::tail(game_state()$comp_cards, 1) })
    output$player_value <- shiny::renderText({ utils::tail(game_state()$player_values, 1) })
    output$comp_value <- shiny::renderText({ utils::tail(game_state()$comp_values, 1) })

    output$player_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Sum: ", scores$player_sum,
             ", Mean: ", scores$player_mean)
    })

    output$comp_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Sum: ", scores$comp_sum,
             ", Mean: ", scores$comp_mean)
    })

    output$effect_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Effect Size: ", scores$effect_size)
    })

    # When all rounds are played, analyze game and show results
    # ... Add logic here
  }

  shiny::shinyApp(ui = ui, server = server)

}
