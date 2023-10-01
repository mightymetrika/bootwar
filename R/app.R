#' Bootwar Shiny App
#'
#' Launches a Shiny application for the Bootwar card game.
#' The app allows users to play a card game where they can analyze the game
#' results using nonparametric bootstrap test with pooled resampling methods.
#'
#' @details
#' The Bootwar card game is a bootstrap variation of the card game War. The
#' Bootwar application has options to select different modes ('t' for
#' independent t-test and 'pt' for paired t-test) and decks.
#' Players can use a standard 52 card deck and they can also input a custom
#' anonymous function to generate a deck. The app will let users deal cards,
#' play the game, and then score and analyze results using nonparametric
#' bootstrap test with pooled resampling methods. The game is designed to help
#' users gain greater intuition on nonparametric bootstrap test with pooled
#' resampling methods; as such, players are encouraged to experiment with
#' different confidence levels, number of rounds, number of bootstrap resamples,
#' and custom decks.
#'
#' @return
#' A Shiny application object. Running this function will launch the Shiny app
#' in the user's default web browser.
#'
#' @examples
#' if(interactive()){
#'   bootwar()
#' }
#'
#' @export
bootwar <- function(){

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("united"),
    shinyjs::useShinyjs(),  # Initialize shinyjs
    shiny::titlePanel("Bootwar Game"),

    # Settings Panel
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("mode", "Select Mode:", choices = c("t", "pt")),
        shiny::selectInput("deck", "Select Deck:", choices = c("Standard", "Anonymous")),
        shiny::conditionalPanel(
          condition = "input.deck === 'Anonymous'",
          shiny::textInput("anon_func", "Enter Anonymous Function:", value = "function(x) { runif(52, 1, 52) }"),
          shiny::actionButton("eval_anon_func", "Evaluate Anonymous Function")
        ),
        shiny::numericInput("conf.level", "Confidence Level:", 0.95),
        shiny::numericInput("nboot", "Number of Bootstrap Resamples:", 1000),
        shiny::numericInput("seed", "Seed:", NA),
        shiny::sliderInput("rounds", "Number of Rounds:", min = 1, max = 26, value = 5),
        shiny::actionButton("new_game", "Start New Game"),
        shinyjs::disabled( shiny::actionButton("deal_card", "Deal Card")),
        shiny::verbatimTextOutput("round_counter"), # New round counter output
      ),

      # Gameplay and Results Area
      shiny::mainPanel(
        shiny::fluidRow(
          # Player Column
          shiny::column(6,
                        shiny::h3("Player"),
                        shiny::conditionalPanel(
                          condition = "input.deck === 'Anonymous'",
                          shiny::verbatimTextOutput("player_card")
                        ),
                        shiny::conditionalPanel(
                          condition = "input.deck === 'Standard'",
                          shiny::imageOutput("player_card_image", width = "100px", height="auto")
                        ),
                        shiny::br(),  # Add a line break
                        shiny::verbatimTextOutput("player_value"),
                        shiny::verbatimTextOutput("player_running_sum"),
                        shiny::verbatimTextOutput("player_running_mean")
          ),

          # Computer Column
          shiny::column(6,
                        shiny::h3("Computer"),
                        shiny::conditionalPanel(
                          condition = "input.deck === 'Anonymous'",
                          shiny::verbatimTextOutput("comp_card")
                        ),
                        shiny::conditionalPanel(
                          condition = "input.deck === 'Standard'",
                          shiny::imageOutput("comp_card_image", width = "100px", height="auto")
                        ),
                        shiny::br(),  # Add a line break
                        shiny::verbatimTextOutput("comp_value"),
                        shiny::verbatimTextOutput("comp_running_sum"),
                        shiny::verbatimTextOutput("comp_running_mean")
          )
        ),

        shiny::h3("Score Board"),
        shiny::verbatimTextOutput("effect_stats"),
        shiny::uiOutput("resultsUI")
      )
    )
  )

  server <- function(input, output, session) {
    # Switch out globals
    x <- NULL

    # Game state
    game_state <- shiny::reactiveVal(list(deck = NULL, player_values = numeric(0), comp_values = numeric(0), current_round = 0))

    shiny::observeEvent(input$eval_anon_func, {

      # Try to evaluate the entered R code to get the anonymous function
      tryCatch({
        user_func <- eval(parse(text = input$anon_func))
        if (is.function(user_func)) {
          # Generate the custom deck
          custom_deck <- mmcards::shuffle_deck(deck_of_cards = user_func, seed = process_seed(input$seed))

          # Update game state with this custom deck
          current_state <- game_state()
          current_state$deck <- custom_deck
          game_state(current_state)

          shiny::showNotification("Anonymous function evaluated successfully and deck created!", type = "message")
        } else {
          shiny::showNotification("Provided input is not a valid anonymous function.", type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })


    shiny::observeEvent(input$new_game, {

      # Shuffle the deck
      if (input$deck == "Anonymous" && !is.null(input$anon_func)) {
        # If Anonymous is selected and there is a previously evaluated function,
        # shuffle the deck using the anonymous function
        user_func <- tryCatch(eval(parse(text = input$anon_func)), error = function(e) NULL)
        if (is.function(user_func)) {
          current_deck <- mmcards::shuffle_deck(deck_of_cards = user_func, seed = process_seed(input$seed))
        } else {
          # Default to a standard deck if the function evaluation fails
          current_deck <- mmcards::i_deck(deck = mmcards::shuffle_deck(seed = process_seed(input$seed)),
                                          i_path = "www",
                                          i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                                      "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                                      "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                                      "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                                      "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                                      "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                                      "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                                      "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                                      "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                                      "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                                      "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                                      "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                                      "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades2"
                                          ))

        }
      } else {
        # Shuffle the standard deck if Standard is selected
        current_deck <- mmcards::i_deck(deck = mmcards::shuffle_deck(seed = process_seed(input$seed)),
                                        i_path = "www",
                                        i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                                    "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                                    "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                                    "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                                    "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                                    "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                                    "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                                    "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                                    "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                                    "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                                    "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                                    "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                                    "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades2"
                                        ))
      }

      game_state(list(deck = current_deck, player_values = numeric(0), comp_values = numeric(0), current_round = 0))

      # Enable the "Deal Card" button when a new game starts
      shinyjs::enable("deal_card")
    })

    shiny::observeEvent(input$deal_card, {
      # Get current state
      current_state <- game_state()

      # Play the round
      round_result <- play_round(cdeck = current_state$deck,
                                 plyr_cv = current_state$player_cards, plyr_vv = current_state$player_values, plyr_ic = current_state$player_icards,
                                 comp_cv = current_state$comp_cards, comp_vv = current_state$comp_values, comp_ic = current_state$comp_icards)

      # Calculate scores
      scores <- score_keeper(round_result$plyr_vv, round_result$comp_vv, mode = input$mode)

      # Update game state
      game_state(list(deck = round_result$updated_deck,
                      player_cards = round_result$plyr_cv,
                      player_values = round_result$plyr_vv,
                      player_icards = round_result$plyr_ic,
                      comp_cards = round_result$comp_cv,
                      comp_values = round_result$comp_vv,
                      comp_icards = round_result$comp_ic,
                      player_scores = scores,
                      current_round = current_state$current_round + 1))

      # Check if it's the last round
      if (game_state()$current_round == input$rounds) {
        # Analyze game
        analysis <- game_analysis()  # Use the eventReactive result

        # Update game state with the analysis
        current_game_state <- game_state()
        current_game_state$analysis <- analysis
        game_state(current_game_state)

        # Notify user
        shiny::showNotification("Game has ended! Check out the results below.", type = "message", duration = NULL)

        # Disable the "Deal Card" button when the game ends
        shinyjs::disable("deal_card")
      }
    })

    # 1. Use eventReactive to determine when game is complete
    game_analysis <- shiny::eventReactive(game_state()$current_round == input$rounds, {

      analyze_game(game_state()$player_values, game_state()$comp_values,
                   mode = input$mode, conf.level = input$conf.level,
                   nboot = input$nboot, seed = process_seed(input$seed))
    })

    # Update UI based on game state

    # Update the UI with the round counter
    output$round_counter <- shiny::renderText({
      paste("Current Round:", game_state()$current_round, "of", input$rounds)
    })
    output$player_card <- shiny::renderText({
      shiny::req(input$deck == "Anonymous")
      paste0("Card: ", { utils::tail(game_state()$player_cards, 1) })
    })
    output$comp_card <- shiny::renderText({
      shiny::req(input$deck == "Anonymous")
      paste0("Card: ", { utils::tail(game_state()$comp_cards, 1) })
    })
    output$player_value <- shiny::renderText(paste0("Card Value: ", { utils::tail(game_state()$player_values, 1) }))
    output$comp_value <- shiny::renderText(paste0("Card Value: ", { utils::tail(game_state()$comp_values, 1) }))

    # Render player card image
    output$player_card_image <- shiny::renderImage({
      # Ensure that there are cards in the player_icards list
      shiny::req(length(game_state()$player_icards) > 0)

      icard <- utils::tail(game_state()$player_icards, 1)

      list(src = system.file(icard, package = "bootwar"), contentType = "image/png", width=200, height="auto", alt = utils::tail(game_state()$player_cards, 1))
    }, deleteFile = FALSE)

    # Render computer card image
    output$comp_card_image <- shiny::renderImage({
      # Ensure that there are cards in the comp_icards list
      shiny::req(length(game_state()$comp_icards) > 0)

      icard <- utils::tail(game_state()$comp_icards, 1)

      list(src = system.file(icard, package = "bootwar"), contentType = "image/png", width=200, height="auto", alt = utils::tail(game_state()$comp_cards, 1))
    }, deleteFile = FALSE)

    output$player_running_sum <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Running Sum: ", scores$player_sum)
    })

    output$player_running_mean <- shiny::renderText({
      scores <- game_state()$player_scores
      formatted_mean <- formatC(scores$player_mean, format = "f", digits = 2)
      paste0("Running Mean: ", formatted_mean)
    })

    output$comp_running_sum <- shiny::renderText({
      scores <- game_state()$player_scores
      paste0("Running Sum: ", scores$comp_sum)
    })

    output$comp_running_mean <- shiny::renderText({
      scores <- game_state()$player_scores
      formatted_mean <- formatC(scores$comp_mean, format = "f", digits = 2)
      paste0("Running Mean: ", formatted_mean)
    })

    output$effect_stats <- shiny::renderText({
      scores <- game_state()$player_scores
      formatted_effect_size <- formatC(scores$effect_size, format = "f", digits = 2)
      paste0("Running Effect Size: ", formatted_effect_size)
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
                              "Bootstrap P-value", "Welch P-value")
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

    # Test Statistic Histogram
    output$hist_stat <- shiny::renderPlot({
      shiny::req(game_state()$analysis$bootstrap_results)

      # Bootstrap results
      boot_res <- game_state()$analysis$bootstrap_results

      ggplot2::ggplot(data.frame(x = boot_res$bootstrap.stat.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white", bins = input$bins_stat) +
        ggplot2::geom_vline(xintercept = c(boot_res$ci.stat), color = "red", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = boot_res$orig.stat, color = "blue") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency")
    })

    # Effect Size Histogram
    output$hist_effect <- shiny::renderPlot({
      shiny::req(game_state()$analysis$bootstrap_results)

      # Bootstrap results
      boot_res <- game_state()$analysis$bootstrap_results

      ggplot2::ggplot(data.frame(x = boot_res$bootstrap.effect.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white", bins = input$bins_effect) +
        ggplot2::geom_vline(xintercept = c(boot_res$ci.effect.size), color = "red", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = boot_res$effect.size, color = "blue") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency")
    })

    # 2. Conditional UI Rendering
    output$resultsUI <- shiny::renderUI({
      shiny::req(game_state()$current_round == input$rounds)

      list(
        shiny::h3("Results"),
        shiny::verbatimTextOutput("winner"),
        shiny::h3("Test Statistic"),
        shiny::tableOutput("test_stats"),
        shiny::plotOutput("hist_stat"),
        shiny::sliderInput("bins_stat", "Number of bins for Test Statistic:", min = 10, max = 50, value = 30),
        shiny::h3("Effect Size"),
        shiny::tableOutput("effect_size"),
        shiny::plotOutput("hist_effect"),
        shiny::sliderInput("bins_effect", "Number of bins for Effect Size:", min = 10, max = 50, value = 30)
      )
    })

  }

  shiny::shinyApp(ui = ui, server = server)

}
