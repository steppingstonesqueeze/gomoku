# OPTIMIZED Shiny Gomoku with proper threat detection and fast UI
# Save as optimized_app.R

library(shiny)

ui <- fluidPage(
  titlePanel("🎯 Optimized Gomoku MCTS"),
  
  tags$head(
    tags$style(HTML("
      .game-cell {
        width: 35px; height: 35px; border: 2px solid black;
        display: inline-block; text-align: center; vertical-align: top;
        cursor: pointer; background-color: wheat; font-size: 24px;
        line-height: 31px; font-weight: bold; margin: 0; padding: 0;
        transition: background-color 0.1s;
      }
      .game-cell:hover { background-color: tan; }
      .game-cell.last-move { background-color: yellow; }
      .game-cell.black-piece { color: black; }
      .game-cell.white-piece { color: white; text-shadow: 1px 1px black; }
      .board-row { height: 35px; line-height: 0; margin: 0; padding: 0; }
      .game-board { 
        display: inline-block; border: 3px solid black; 
        background-color: burlywood; padding: 10px; margin: 20px; 
      }
      .status-thinking { color: #007bff; font-weight: bold; }
      .status-win { color: #dc3545; font-size: 18px; font-weight: bold; }
      .status-turn { color: #28a745; font-weight: bold; }
    "))
  ),
  
  fluidRow(
    column(8,
      div(style = "text-align: center;",
        # Static board structure - only content changes
        div(id = "game-board-container",
          uiOutput("game_board", inline = TRUE)
        )
      )
    ),
    column(4,
      wellPanel(
        h4("🎮 Game Controls"),
        actionButton("start_new_game", "New Game", class = "btn-primary btn-block"),
        br(),
        radioButtons("player_choice", "You play as:",
          choices = list(
            "Black (●) - Go First" = 1,
            "White (○) - Go Second" = -1
          ),
          selected = 1
        ),
        
        hr(),
        h4("📊 Game Status"),
        div(id = "status-display", 
          textOutput("game_status")
        ),
        textOutput("move_info"),
        
        hr(),
        h4("🧠 AI Debug"),
        textOutput("ai_debug"),
        verbatimTextOutput("threat_analysis"),
        
        hr(),
        h4("⚙️ Settings"),
        sliderInput("ai_strength", "AI Strength:", 
          min = 50, max = 300, value = 150, step = 25),
        checkboxInput("show_threats", "Show threat analysis", FALSE)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Optimized reactive values
  game_state <- reactiveVal({
    list(
      board = matrix(0L, 13, 13),
      current_player = 1L,
      human_player = 1L,
      game_active = FALSE,
      move_count = 0L,
      last_move = NULL,
      winner = NULL,
      ai_thinking = FALSE,
      threat_info = ""
    )
  })
  
  # Start new game
  observeEvent(input$start_new_game, {
    new_state <- list(
      board = matrix(0L, 13, 13),
      current_player = 1L,
      human_player = as.integer(input$player_choice),
      game_active = TRUE,
      move_count = 0L,
      last_move = NULL,
      winner = NULL,
      ai_thinking = FALSE,
      threat_info = "Game started"
    )
    
    # If human chose white, AI makes first move immediately
    if (new_state$human_player == -1L) {
      new_state$board[7, 7] <- 1L  # Center
      new_state$current_player <- -1L
      new_state$move_count <- 1L
      new_state$last_move <- c(7, 7)
      new_state$threat_info <- "AI opened in center"
    }
    
    game_state(new_state)
  })
  
  # Handle board clicks with immediate state update
  observeEvent(input$board_click, {
    state <- game_state()
    
    if (!state$game_active || state$ai_thinking || !is.null(state$winner)) return()
    
    # Check if it's human's turn
    if (state$current_player != state$human_player) return()
    
    click_data <- input$board_click
    row <- as.integer(click_data$row)
    col <- as.integer(click_data$col)
    
    # Validate move
    if (state$board[row, col] != 0L) return()
    
    # Apply human move immediately
    state$board[row, col] <- state$current_player
    state$last_move <- c(row, col)
    state$move_count <- state$move_count + 1L
    state$threat_info <- paste("You played (", row, ",", col, ")")
    
    # Check human win
    if (check_winner(state$board, row, col, state$current_player)) {
      state$winner <- state$current_player
      state$threat_info <- "You win!"
      game_state(state)
      return()
    }
    
    # Switch to AI and start thinking
    state$current_player <- -state$human_player
    state$ai_thinking <- TRUE
    state$threat_info <- "AI analyzing position..."
    game_state(state)  # Update UI immediately
    
    # Schedule AI move
    invalidateLater(100, session)
  })
  
  # AI move processing
  observe({
    state <- game_state()
    
    if (state$ai_thinking && state$game_active && is.null(state$winner)) {
      
      # AI makes move with threat analysis
      result <- get_smart_ai_move(state$board, -state$human_player)
      move <- result$move
      analysis <- result$analysis
      
      if (!is.null(move)) {
        row <- move[1]; col <- move[2]
        
        state$board[row, col] <- -state$human_player
        state$last_move <- c(row, col)
        state$move_count <- state$move_count + 1L
        state$threat_info <- paste("AI played (", row, ",", col, ") -", analysis)
        
        # Check AI win
        if (check_winner(state$board, row, col, -state$human_player)) {
          state$winner <- -state$human_player
          state$threat_info <- "AI wins!"
        } else {
          state$current_player <- state$human_player
        }
      }
      
      state$ai_thinking <- FALSE
      game_state(state)
    }
  })
  
  # Optimized board rendering - only updates cells that changed
  output$game_board <- renderUI({
    state <- game_state()
    
    if (!state$game_active) {
      return(div(h3("Click 'New Game' to start!"),
                 p("Choose your color and difficulty, then start playing!")))
    }
    
    # Create board structure
    board_div <- div(class = "game-board")
    
    for (row in 1:13) {
      row_cells <- list()
      
      for (col in 1:13) {
        cell_value <- state$board[row, col]
        is_last <- !is.null(state$last_move) && 
                   state$last_move[1] == row && state$last_move[2] == col
        
        # Build cell classes
        cell_classes <- c("game-cell")
        cell_content <- ""
        
        if (cell_value == 1L) {
          cell_classes <- c(cell_classes, "black-piece")
          cell_content <- "●"
        } else if (cell_value == -1L) {
          cell_classes <- c(cell_classes, "white-piece")  
          cell_content <- "○"
        }
        
        if (is_last) {
          cell_classes <- c(cell_classes, "last-move")
        }
        
        # Create cell with unique ID for potential future optimization
        row_cells[[col]] <- tags$div(
          class = paste(cell_classes, collapse = " "),
          id = paste0("cell-", row, "-", col),
          onclick = sprintf("Shiny.setInputValue('board_click', {row: %d, col: %d}, {priority: 'event'});", row, col),
          cell_content
        )
      }
      
      board_div <- tagAppendChild(board_div, 
        tags$div(class = "board-row", row_cells))
    }
    
    board_div
  })
  
  # Status outputs
  output$game_status <- renderText({
    state <- game_state()
    
    if (!state$game_active) return("Click 'New Game' to start")
    
    if (!is.null(state$winner)) {
      winner_name <- if (state$winner == state$human_player) "You" else "AI"
      return(paste("🎉", winner_name, "WINS!"))
    }
    
    if (state$ai_thinking) return("🤔 AI thinking...")
    
    if (state$current_player == state$human_player) {
      return("🎯 Your turn!")
    } else {
      return("🤖 AI's turn")
    }
  })
  
  output$move_info <- renderText({
    state <- game_state()
    paste("Move:", state$move_count)
  })
  
  output$ai_debug <- renderText({
    state <- game_state()
    state$threat_info
  })
  
  output$threat_analysis <- renderText({
    if (!input$show_threats) return("")
    
    state <- game_state()
    if (!state$game_active) return("")
    
    # Analyze current position
    analysis <- analyze_position(state$board, state$human_player)
    paste("Human threats:", analysis$human_threats,
          "\nAI threats:", analysis$ai_threats,
          "\nCritical moves:", paste(analysis$critical, collapse = ", "))
  })
}

# IMPROVED AI LOGIC
get_smart_ai_move <- function(board, ai_player) {
  moves <- get_candidate_moves(board)
  if (length(moves) == 0) return(list(move = NULL, analysis = "No moves"))
  
  human_player <- -ai_player
  
  # 1. IMMEDIATE WINS
  for (move in moves) {
    if (creates_win(board, move[1], move[2], ai_player)) {
      return(list(move = move, analysis = "Taking win!"))
    }
  }
  
  # 2. BLOCK IMMEDIATE HUMAN WINS  
  for (move in moves) {
    if (creates_win(board, move[1], move[2], human_player)) {
      return(list(move = move, analysis = "Blocking immediate threat"))
    }
  }
  
  # 3. BLOCK CRITICAL THREATS (this was missing!)
  critical_blocks <- find_critical_blocks(board, human_player)
  if (length(critical_blocks) > 0) {
    # Pick the best critical block
    best_block <- critical_blocks[[1]]
    return(list(move = best_block, analysis = "Blocking critical threat"))
  }
  
  # 4. CREATE OUR OWN THREATS
  our_threats <- find_threat_moves(board, ai_player)
  if (length(our_threats) > 0) {
    moves <- our_threats  # Focus on attacking moves
  }
  
  # 5. MCTS on remaining moves
  iterations <- 200  # Increased for better play
  best_move <- run_focused_mcts(board, ai_player, moves, iterations)
  
  return(list(move = best_move, analysis = "Strategic move"))
}

# PROPER THREAT DETECTION
find_critical_blocks <- function(board, opponent) {
  critical <- list()
  
  # Find all empty positions
  for (r in 1:13) {
    for (c in 1:13) {
      if (board[r, c] == 0L) {
        
        # Check if opponent playing here creates multiple threats
        test_board <- board
        test_board[r, c] <- opponent
        
        # Count how many ways opponent could win next turn
        win_moves <- 0
        for (r2 in 1:13) {
          for (c2 in 1:13) {
            if (test_board[r2, c2] == 0L) {
              if (creates_win(test_board, r2, c2, opponent)) {
                win_moves <- win_moves + 1
                if (win_moves >= 2) {
                  critical <- append(critical, list(c(r, c)), 0)
                  break
                }
              }
            }
          }
          if (win_moves >= 2) break
        }
      }
    }
  }
  
  return(unique(critical))
}

creates_win <- function(board, row, col, player) {
  if (board[row, col] != 0L) return(FALSE)
  
  test_board <- board
  test_board[row, col] <- player
  
  return(check_winner(test_board, row, col, player))
}

find_threat_moves <- function(board, player) {
  threats <- list()
  
  for (r in 1:13) {
    for (c in 1:13) {
      if (board[r, c] == 0L) {
        threat_level <- evaluate_position_threat(board, r, c, player)
        if (threat_level >= 3) {  # Significant threat
          threats <- append(threats, list(c(r, c)), 0)
        }
      }
    }
  }
  
  return(threats)
}

evaluate_position_threat <- function(board, row, col, player) {
  directions <- list(c(0,1), c(1,0), c(1,1), c(1,-1))
  max_threat <- 0
  
  for (dir in directions) {
    dr <- dir[1]; dc <- dir[2]
    
    # Count consecutive stones
    count <- 0
    
    # Forward direction
    r <- row + dr; c <- col + dc
    while (r >= 1 && r <= 13 && c >= 1 && c <= 13 && board[r, c] == player) {
      count <- count + 1
      r <- r + dr; c <- c + dc
    }
    
    # Backward direction
    r <- row - dr; c <- col - dc
    while (r >= 1 && r <= 13 && c >= 1 && c <= 13 && board[r, c] == player) {
      count <- count + 1
      r <- r - dr; c <- c - dc
    }
    
    max_threat <- max(max_threat, count)
  }
  
  return(max_threat)
}

run_focused_mcts <- function(board, player, moves, iterations) {
  if (length(moves) == 0) return(NULL)
  if (length(moves) == 1) return(moves[[1]])
  
  # Simple MCTS implementation
  move_stats <- list()
  for (move in moves) {
    key <- paste(move[1], move[2])
    move_stats[[key]] <- list(visits = 0, wins = 0)
  }
  
  for (iter in 1:iterations) {
    # Select move
    selected_move <- if (iter <= length(moves)) {
      moves[[iter]]
    } else {
      select_ucb1_move(moves, move_stats, iter)
    }
    
    # Simulate
    result <- simulate_game(board, selected_move, player)
    
    # Update stats
    key <- paste(selected_move[1], selected_move[2])
    move_stats[[key]]$visits <- move_stats[[key]]$visits + 1
    
    if (result == player) {
      move_stats[[key]]$wins <- move_stats[[key]]$wins + 1
    } else if (result == 0) {
      move_stats[[key]]$wins <- move_stats[[key]]$wins + 0.5
    }
  }
  
  # Return best move
  best_move <- moves[[1]]
  best_rate <- -1
  
  for (move in moves) {
    key <- paste(move[1], move[2])
    stats <- move_stats[[key]]
    if (stats$visits > 0) {
      rate <- stats$wins / stats$visits
      if (rate > best_rate) {
        best_rate <- rate
        best_move <- move
      }
    }
  }
  
  return(best_move)
}

select_ucb1_move <- function(moves, move_stats, total_visits) {
  best_ucb <- -Inf
  best_move <- moves[[1]]
  
  for (move in moves) {
    key <- paste(move[1], move[2])
    stats <- move_stats[[key]]
    
    if (stats$visits == 0) return(move)
    
    exploitation <- stats$wins / stats$visits
    exploration <- sqrt(log(total_visits) / stats$visits)
    ucb <- exploitation + 1.4 * exploration
    
    if (ucb > best_ucb) {
      best_ucb <- ucb
      best_move <- move
    }
  }
  
  return(best_move)
}

simulate_game <- function(board, first_move, first_player) {
  sim_board <- board
  sim_board[first_move[1], first_move[2]] <- first_player
  
  if (check_winner(sim_board, first_move[1], first_move[2], first_player)) {
    return(first_player)
  }
  
  current_player <- -first_player
  
  # Short simulation
  for (i in 1:8) {
    moves <- get_candidate_moves(sim_board)
    if (length(moves) == 0) break
    
    # Check for immediate wins first
    win_move <- NULL
    for (move in moves) {
      if (creates_win(sim_board, move[1], move[2], current_player)) {
        win_move <- move
        break
      }
    }
    
    selected_move <- if (!is.null(win_move)) {
      win_move
    } else {
      moves[[sample(length(moves), 1)]]
    }
    
    sim_board[selected_move[1], selected_move[2]] <- current_player
    
    if (check_winner(sim_board, selected_move[1], selected_move[2], current_player)) {
      return(current_player)
    }
    
    current_player <- -current_player
  }
  
  return(0)  # Draw
}

get_candidate_moves <- function(board) {
  if (sum(board != 0) == 0) return(list(c(7, 7)))
  
  moves <- list()
  for (r in 1:13) {
    for (c in 1:13) {
      if (board[r, c] == 0L) {
        # Check adjacency to existing stones
        adjacent <- FALSE
        for (dr in -1:1) {
          for (dc in -1:1) {
            if (dr == 0 && dc == 0) next
            nr <- r + dr; nc <- c + dc
            if (nr >= 1 && nr <= 13 && nc >= 1 && nc <= 13 && board[nr, nc] != 0L) {
              adjacent <- TRUE
              break
            }
          }
          if (adjacent) break
        }
        if (adjacent) moves <- append(moves, list(c(r, c)), 0)
      }
    }
  }
  
  # Limit for performance
  if (length(moves) > 25) moves <- moves[1:25]
  
  return(moves)
}

check_winner <- function(board, row, col, player) {
  directions <- list(c(0,1), c(1,0), c(1,1), c(1,-1))
  
  for (dir in directions) {
    count <- 1
    dr <- dir[1]; dc <- dir[2]
    
    # Forward
    r <- row + dr; c <- col + dc
    while (r >= 1 && r <= 13 && c >= 1 && c <= 13 && board[r, c] == player) {
      count <- count + 1
      r <- r + dr; c <- c + dc
    }
    
    # Backward
    r <- row - dr; c <- col - dc
    while (r >= 1 && r <= 13 && c >= 1 && c <= 13 && board[r, c] == player) {
      count <- count + 1
      r <- r - dr; c <- c - dc
    }
    
    if (count >= 5) return(TRUE)
  }
  
  return(FALSE)
}

analyze_position <- function(board, human_player) {
  ai_player <- -human_player
  
  human_threats <- length(find_threat_moves(board, human_player))
  ai_threats <- length(find_threat_moves(board, ai_player))
  critical <- find_critical_blocks(board, human_player)
  
  return(list(
    human_threats = human_threats,
    ai_threats = ai_threats,
    critical = if(length(critical) > 0) sapply(critical, function(x) paste0("(", x[1], ",", x[2], ")")) else "None"
  ))
}

shinyApp(ui, server)