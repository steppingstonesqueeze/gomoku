# BLAZING FAST R MCTS - Heavily Optimized
# Save as fast_app.R and run with shiny::runApp()

library(shiny)

# ==============================================================================
# ULTRA-OPTIMIZED MCTS FOR GOMOKU
# ==============================================================================

# Pre-allocate direction vectors for speed
DIRECTIONS <- matrix(c(0,1, 1,0, 1,1, 1,-1), ncol=2, byrow=TRUE)

# Fast game state (using environment for reference semantics)
create_fast_state <- function(board_size = 13) {
  env <- new.env()
  env$board <- matrix(0L, board_size, board_size)  # Use integers
  env$size <- as.integer(board_size)
  env$current_player <- 1L
  env$move_count <- 0L
  env$last_move <- NULL
  env$winner <- NULL
  env
}

# Lightning fast win check using vectorized operations
fast_win_check <- function(state, row, col, player) {
  board <- state$board
  size <- state$size
  
  for (d in 1:4) {
    dr <- DIRECTIONS[d, 1]
    dc <- DIRECTIONS[d, 2]
    count <- 1L
    
    # Vectorized counting in positive direction
    r <- row + dr; c <- col + dc
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == player) {
      count <- count + 1L
      if (count >= 5L) return(TRUE)  # Early exit
      r <- r + dr; c <- c + dc
    }
    
    # Vectorized counting in negative direction  
    r <- row - dr; c <- col - dc
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == player) {
      count <- count + 1L
      if (count >= 5L) return(TRUE)  # Early exit
      r <- r - dr; c <- c - dc
    }
  }
  FALSE
}

# Super fast move generation with caching
get_fast_moves <- function(state) {
  if (state$move_count == 0L) {
    center <- ceiling(state$size / 2)
    return(list(c(center, center)))
  }
  
  board <- state$board
  size <- state$size
  moves <- vector("list", 50)  # Pre-allocate
  count <- 0L
  
  # Find occupied positions once
  occupied <- which(board != 0L, arr.ind = TRUE)
  
  # Use set for O(1) lookups
  candidates <- matrix(FALSE, size, size)
  
  # Tight loop for candidate generation
  for (i in seq_len(nrow(occupied))) {
    base_r <- occupied[i, 1]
    base_c <- occupied[i, 2]
    
    # Unrolled loop for nearby positions
    for (dr in c(-1L, 0L, 1L)) {
      for (dc in c(-1L, 0L, 1L)) {
        r <- base_r + dr
        c <- base_c + dc
        if (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == 0L) {
          candidates[r, c] <- TRUE
        }
      }
    }
  }
  
  # Convert to list efficiently
  pos <- which(candidates, arr.ind = TRUE)
  if (nrow(pos) == 0) return(list())
  
  # Limit moves aggressively for speed
  max_moves <- min(15L, nrow(pos))
  result <- vector("list", max_moves)
  
  for (i in seq_len(max_moves)) {
    result[[i]] <- pos[i, ]
  }
  
  result
}

# Blazing fast heuristic evaluation
fast_eval <- function(state, row, col, player) {
  board <- state$board
  size <- state$size
  score <- 0L
  opponent <- -player
  
  for (d in 1:4) {
    dr <- DIRECTIONS[d, 1]
    dc <- DIRECTIONS[d, 2]
    
    # Count player stones
    count <- 0L
    r <- row + dr; c <- col + dc
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == player) {
      count <- count + 1L
      r <- r + dr; c <- c + dc
    }
    r <- row - dr; c <- col - dc  
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == player) {
      count <- count + 1L
      r <- r - dr; c <- c - dc
    }
    
    # Quick scoring
    if (count >= 4L) return(10000L)  # Immediate return for wins
    if (count == 3L) score <- score + 100L
    else if (count == 2L) score <- score + 10L
    
    # Count opponent stones (blocking)
    opp_count <- 0L
    r <- row + dr; c <- col + dc
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == opponent) {
      opp_count <- opp_count + 1L
      r <- r + dr; c <- c + dc
    }
    r <- row - dr; c <- col - dc
    while (r >= 1L && r <= size && c >= 1L && c <= size && board[r, c] == opponent) {
      opp_count <- opp_count + 1L
      r <- r - dr; c <- c - dc
    }
    
    if (opp_count >= 4L) score <- score + 5000L
    else if (opp_count == 3L) score <- score + 50L
  }
  
  score
}

# Ultra-fast MCTS node (minimal overhead)
mcts_node <- function(state, parent = NULL, move = NULL) {
  list(
    state = state,
    parent = parent, 
    move = move,
    children = vector("list", 20),  # Pre-allocated
    child_count = 0L,
    visits = 0L,
    wins = 0.0,
    untried = get_fast_moves(state),
    untried_count = length(get_fast_moves(state))
  )
}

# Lightning UCB1 selection
fast_select <- function(node) {
  if (node$child_count == 0L) return(NULL)
  
  best_val <- -Inf
  best_child <- NULL
  log_visits <- log(node$visits)
  
  for (i in seq_len(node$child_count)) {
    child <- node$children[[i]]
    if (child$visits == 0L) return(child)
    
    exploitation <- child$wins / child$visits
    exploration <- 1.4 * sqrt(log_visits / child$visits)
    ucb <- exploitation + exploration
    
    if (ucb > best_val) {
      best_val <- ucb
      best_child <- child
    }
  }
  
  best_child
}

# Hyper-fast expansion
fast_expand <- function(node) {
  if (node$untried_count == 0L) return(NULL)
  
  # Get best untried move using heuristics
  best_score <- -1L
  best_idx <- 1L
  
  # Check only first 5 moves for speed
  check_count <- min(5L, node$untried_count)
  for (i in seq_len(check_count)) {
    move <- node$untried[[i]]
    score <- fast_eval(node$state, move[1], move[2], node$state$current_player)
    if (score > best_score) {
      best_score <- score
      best_idx <- i
    }
  }
  
  # Create new state
  move <- node$untried[[best_idx]]
  new_state <- create_fast_state(node$state$size)
  new_state$board <- node$state$board  # Copy reference
  new_state$board[move[1], move[2]] <- node$state$current_player
  new_state$current_player <- -node$state$current_player
  new_state$move_count <- node$state$move_count + 1L
  new_state$last_move <- move
  
  # Check for immediate win
  if (fast_win_check(new_state, move[1], move[2], node$state$current_player)) {
    new_state$winner <- node$state$current_player
  }
  
  # Create child node
  child <- mcts_node(new_state, parent = node, move = move)
  
  # Add to parent
  node$child_count <- node$child_count + 1L
  node$children[[node$child_count]] <- child
  
  # Remove from untried
  node$untried[[best_idx]] <- node$untried[[node$untried_count]]
  node$untried_count <- node$untried_count - 1L
  
  child
}

# Lightning simulation (very short and fast)
fast_simulate <- function(state) {
  if (!is.null(state$winner)) return(state$winner)
  
  current_state <- create_fast_state(state$size)
  current_state$board <- state$board  # Reference copy
  current_state$current_player <- state$current_player
  current_state$move_count <- state$move_count
  
  # Very short simulation (max 10 moves)
  for (sim_moves in 1:10) {
    moves <- get_fast_moves(current_state)
    if (length(moves) == 0) break
    
    # Pick move (70% random, 30% best heuristic)
    if (runif(1) < 0.7 || length(moves) == 1) {
      move <- moves[[sample.int(length(moves), 1)]]
    } else {
      # Quick heuristic on first 3 moves only
      best_move <- moves[[1]]
      best_score <- fast_eval(current_state, best_move[1], best_move[2], current_state$current_player)
      
      check_count <- min(3L, length(moves))
      for (i in 2:check_count) {
        move <- moves[[i]]
        score <- fast_eval(current_state, move[1], move[2], current_state$current_player)
        if (score > best_score) {
          best_score <- score
          best_move <- move
        }
      }
      move <- best_move
    }
    
    # Apply move
    current_state$board[move[1], move[2]] <- current_state$current_player
    
    # Quick win check
    if (fast_win_check(current_state, move[1], move[2], current_state$current_player)) {
      return(current_state$current_player)
    }
    
    current_state$current_player <- -current_state$current_player
    current_state$move_count <- current_state$move_count + 1L
  }
  
  0L  # Draw
}

# Blazing fast backpropagation
fast_backprop <- function(node, result) {
  current <- node
  while (!is.null(current)) {
    current$visits <- current$visits + 1L
    if (result == current$state$current_player) {
      current$wins <- current$wins + 1.0
    } else if (result == 0L) {
      current$wins <- current$wins + 0.5
    }
    current <- current$parent
  }
}

# MAIN OPTIMIZED MCTS
blazing_mcts <- function(state, iterations = 100L) {
  root <- mcts_node(state)
  
  for (i in seq_len(iterations)) {
    # Selection
    node <- root
    while (!is.null(node$state$winner) == FALSE && node$untried_count == 0L && node$child_count > 0L) {
      node <- fast_select(node)
      if (is.null(node)) break
    }
    
    # Expansion
    if (is.null(node$state$winner) && node$untried_count > 0L) {
      node <- fast_expand(node)
    }
    
    # Simulation
    result <- fast_simulate(node$state)
    
    # Backpropagation
    fast_backprop(node, result)
  }
  
  # Get best move
  if (root$child_count == 0L) return(NULL)
  
  best_child <- root$children[[1]]
  best_visits <- best_child$visits
  
  for (i in seq_len(root$child_count)) {
    child <- root$children[[i]]
    if (child$visits > best_visits) {
      best_visits <- child$visits
      best_child <- child
    }
  }
  
  best_child$move
}

# ==============================================================================
# OPTIMIZED SHINY UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("🚀 BLAZING FAST R MCTS"),
  
  tags$head(tags$style(HTML("
    .cell { 
      width: 30px; height: 30px; border: 2px solid #333; 
      display: inline-block; text-align: center; cursor: pointer;
      background: #f4e6d4; font-size: 20px; line-height: 26px; font-weight: bold;
      margin: 0; padding: 0;
      color: #000000 !important;
    }
    .cell:hover { 
      background: #e6d4c4; 
      border-color: #666;
    }
    .cell.last { 
      box-shadow: 0 0 0 3px red !important; 
      border-color: red !important;
    }
    .row { 
      white-space: nowrap; 
      line-height: 0; 
      margin: 0; 
      padding: 0;
    }
    .board { 
      margin: 20px auto; 
      display: inline-block; 
      border: 3px solid #000; 
      padding: 5px; 
      background: #d4b896;
      font-family: monospace;
    }
    .black-stone {
      color: #000000 !important;
      text-shadow: 1px 1px 1px #ffffff;
    }
    .white-stone {
      color: #ffffff !important;
      text-shadow: 1px 1px 1px #000000;
      background: #f0f0f0 !important;
    }
  "))),
  
  fluidRow(
    column(8, div(class = "text-center", uiOutput("board"))),
    column(4, 
      h4("⚡ MCTS Status"),
      uiOutput("status"),
      br(),
      selectInput("iterations", "MCTS Iterations:",
        choices = list(
          "Lightning (50)" = 50,
          "Fast (100)" = 100, 
          "Medium (200)" = 200,
          "Strong (400)" = 400
        ),
        selected = 100
      ),
      actionButton("new_game", "New Game", class = "btn-primary"),
      br(), br(),
      radioButtons("human_color", "You play:", 
        choices = list("Black ● (first)" = 1, "White ○ (second)" = -1), 
        selected = 1),
      textOutput("info")
    )
  )
)

server <- function(input, output, session) {
  game <- reactiveVal(create_fast_state())
  human_player <- reactiveVal(1L)
  ai_thinking <- reactiveVal(FALSE)
  
  observeEvent(input$new_game, {
    new_game <- create_fast_state()
    human_player(as.integer(input$human_color))
    
    # If human is white, AI goes first
    if (human_player() == -1L) {
      ai_thinking(TRUE)
      
      # AI first move (center)
      center <- ceiling(new_game$size / 2)
      new_game$board[center, center] <- 1L
      new_game$current_player <- -1L
      new_game$move_count <- 1L
      new_game$last_move <- c(center, center)
      
      ai_thinking(FALSE)
    }
    
    game(new_game)
  })
  
  # Handle cell clicks with proper reactive structure
  observeEvent(input$cell_click, {
    cat("Click received:", input$cell_click$row, input$cell_click$col, "\n")  # Debug
    
    if (ai_thinking() || !is.null(game()$winner)) {
      cat("Blocked: AI thinking or game over\n")
      return()
    }
    
    click <- input$cell_click
    r <- as.integer(click$row)
    c <- as.integer(click$col)
    current_game <- game()
    
    cat("Current player:", current_game$current_player, "Human player:", human_player(), "\n")
    cat("Board value at", r, c, ":", current_game$board[r, c], "\n")
    
    # Validate move
    if (current_game$board[r, c] != 0L || current_game$current_player != human_player()) {
      cat("Invalid move\n")
      return()
    }
    
    # Make human move - modify board directly
    current_game$board[r, c] <- human_player()
    current_game$last_move <- c(r, c)
    current_game$move_count <- current_game$move_count + 1L
    current_game$current_player <- -human_player()
    
    cat("Move made! Board updated. New move count:", current_game$move_count, "\n")
    
    # Check human win
    if (fast_win_check(current_game, r, c, human_player())) {
      current_game$winner <- human_player()
      cat("Human wins!\n")
      game(current_game)
      return()
    }
    
    # Update game state to trigger reactivity
    game(current_game)
    
    # Schedule AI move
    ai_thinking(TRUE)
    invalidateLater(100, session)
  })
  
  # Separate observer for AI moves
  observe({
    req(ai_thinking() == TRUE)
    
    current_game <- game()
    
    # RUN BLAZING MCTS
    move <- blazing_mcts(current_game, as.integer(input$iterations))
    
    if (!is.null(move)) {
      r <- move[1]; c <- move[2]
      current_game$board[r, c] <- -human_player()
      current_game$last_move <- c(r, c)
      current_game$move_count <- current_game$move_count + 1L
      
      # Check AI win
      if (fast_win_check(current_game, r, c, -human_player())) {
        current_game$winner <- -human_player()
      }
      
      current_game$current_player <- human_player()
    }
    
    game(current_game)
    ai_thinking(FALSE)
  })
  
  output$board <- renderUI({
    current_game <- game()  # This should trigger reactivity
    
    if (is.null(current_game)) {
      return(div("Click New Game to start!"))
    }
    
    board_div <- div(class = "board")
    
    for (r in 1:current_game$size) {
      row_cells <- list()
      for (c in 1:current_game$size) {
        value <- current_game$board[r, c]
        is_last <- !is.null(current_game$last_move) && 
                   length(current_game$last_move) == 2 &&
                   current_game$last_move[1] == r && current_game$last_move[2] == c
        
        cell_class <- "cell"
        if (is_last) cell_class <- paste(cell_class, "last")
        
        content <- ""
        stone_class <- ""
        
        if (value == 1L) {
          content <- "●"  # Black stone
          stone_class <- "black-stone"
          cell_class <- paste(cell_class, stone_class)
        } else if (value == -1L) {
          content <- "○"  # White stone  
          stone_class <- "white-stone"
          cell_class <- paste(cell_class, stone_class)
        }
        
        # Debug: add position info
        if (value != 0L) {
          cat("Rendering stone at", r, c, "value:", value, "content:", content, "\n")
        }
        
        row_cells[[c]] <- div(
          class = cell_class,
          onclick = sprintf("Shiny.setInputValue('cell_click', {row: %d, col: %d}, {priority: 'event'})", r, c),
          style = "margin: 0; padding: 0;",
          content
        )
      }
      board_div <- tagAppendChild(board_div, div(class = "row", style = "margin: 0; padding: 0;", row_cells))
    }
    board_div
  })
  
  output$status <- renderUI({
    current_game <- game()
    
    if (!is.null(current_game$winner)) {
      if (current_game$winner == human_player()) {
        return(h3("🎉 You win!", style = "color: green"))
      } else {
        return(h3("🤖 MCTS wins!", style = "color: red"))  
      }
    }
    
    if (ai_thinking()) {
      return(h4("⚡ MCTS thinking...", style = "color: blue"))
    }
    
    if (current_game$current_player == human_player()) {
      return(h4("Your turn!", style = "color: green"))
    } else {
      return(h4("MCTS turn", style = "color: orange"))
    }
  })
  
  output$info <- renderText({
    current_game <- game()
    paste("Moves:", current_game$move_count, "| Real MCTS with", input$iterations, "iterations")
  })
}

shinyApp(ui, server)