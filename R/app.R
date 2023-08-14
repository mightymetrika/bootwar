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
        shiny::actionButton("deal_card", "Deal Card"),
        shiny::verbatimTextOutput("round_counter"), # New round counter output
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
    game_state <- shiny::reactiveVal(list(deck = NULL, player_values = numeric(0), comp_values = numeric(0), current_round = 0))

    shiny::observeEvent(input$new_game, {
      # Shuffle the deck
      new_deck <- shuffle_deck(seed = as.integer(input$seed))
      game_state(list(deck = new_deck, player_values = numeric(0), comp_values = numeric(0), current_round = 0))
    })

    shiny::observeEvent(input$deal_card, {
      # Get current state
      current_state <- game_state()

      # Play the round
      round_result <- play_round(cdeck = current_state$deck,
                                 comp_cv = character(0), comp_vv = current_state$comp_values,
                                 plyr_cv = character(0), plyr_vv = current_state$player_values)

      # Calculate scores
      scores <- score_keeper(round_result$plyr_vv, round_result$comp_vv, mode = input$mode)

      # Update game state
      game_state(list(deck = round_result$updated_deck,
                      player_cards = round_result$plyr_cv,
                      player_values = round_result$plyr_vv,
                      comp_cards = round_result$comp_cv,
                      comp_values = round_result$comp_vv,
                      player_scores = scores,
                      current_round = current_state$current_round + 1))

      # Check if it's the last round
      if (game_state()$current_round == input$rounds) {
        # Analyze game
        analysis <- analyze_game(game_state()$comp_values, game_state()$player_values,
                                 mode = input$mode, conf.level = input$conf.level,
                                 nboot = input$nboot, seed = input$seed)

        # Update game state with the analysis
        current_game_state <- game_state()
        current_game_state$analysis <- analysis
        game_state(current_game_state)

        # Notify user
        shiny::showNotification("Game has ended! Check out the results below.", type = "message", duration = NULL)
      }
    })

    # Update UI based on game state

    # Update the UI with the round counter
    output$round_counter <- shiny::renderText({
      paste("Current Round:", game_state()$current_round, "of", input$rounds)
    })

    output$player_card <- shiny::renderText(paste0("Card: ", { utils::tail(game_state()$player_cards, 1) }))
    output$comp_card <- shiny::renderText(paste0("Card: ", { utils::tail(game_state()$comp_cards, 1) }))
    output$player_value <- shiny::renderText(paste0("Card Value: ", { utils::tail(game_state()$player_values, 1) }))
    output$comp_value <- shiny::renderText(paste0("Card Value: ", { utils::tail(game_state()$comp_values, 1) }))

    output$player_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Running Sum: ", scores$player_sum,
             ", Running Mean: ", scores$player_mean)
    })

    output$comp_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Running Sum: ", scores$comp_sum,
             ", Running Mean: ", scores$comp_mean)
    })

    output$effect_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Running Effect Size: ", scores$effect_size)
    })

    # Update UI with results from analyze_game (assuming it's stored in the game_state)
    output$winner <- shiny::renderText({
      game_state()$analysis$winner
    })

    output$test_stats <- shiny::renderTable({
      shiny::req(game_state()$analysis$bootstrap_results, game_state()$analysis$classical_results)

      # Bootstrap results
      boot_res <- game_state()$analysis$bootstrap_results

      # Classical test results
      classical_res <- game_state()$analysis$classical_results

      df_res_stat <- data.frame(
        Test_Statistic = boot_res$orig.stat,
        CI_Lower = boot_res$ci.stat[1],
        CI_Upper = boot_res$ci.stat[2],
        Bootstrap_P_Value = boot_res$p.value,
        Classical_P_Value = classical_res$p.value
      )

      names(df_res_stat) <- c("Test Statistic", "Bootstrap CI (Lower)", "Bootstrap CI (Upper)",
                              "Bootstrap P-value", "Classical P-value")
      df_res_stat
    }, row.names = FALSE)

    output$effect_size <- shiny::renderTable({
      shiny::req(game_state()$analysis$bootstrap_results)

      # Bootstrap results
      boot_res <- game_state()$analysis$bootstrap_results

      df_res_effect <- data.frame(
        Effect_Size = boot_res$effect.size,
        CI_Lower = boot_res$ci.effect.size[1],
        CI_Upper = boot_res$ci.effect.size[2]
      )

      names(df_res_effect) <- c("Effect Size", "CI (Lower)", "CI (Upper)")
      df_res_effect
    }, row.names = FALSE)


  }

  shiny::shinyApp(ui = ui, server = server)

}
