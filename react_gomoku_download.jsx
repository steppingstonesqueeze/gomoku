# React Gomoku MCTS Component


```jsx
import React, { useState, useCallback, useEffect } from 'react';

const PlayableGomokuMCTS = () => {
  const BOARD_SIZE = 13;
  const EMPTY = 0;
  const BLACK = 1;
  const WHITE = -1;
  
  const [board, setBoard] = useState(() => 
    Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(EMPTY))
  );
  const [currentPlayer, setCurrentPlayer] = useState(BLACK);
  const [gameStatus, setGameStatus] = useState('playing'); // 'playing', 'black_wins', 'white_wins', 'draw'
  const [isThinking, setIsThinking] = useState(false);
  const [humanPlayer, setHumanPlayer] = useState(BLACK);
  const [gameStarted, setGameStarted] = useState(false);
  const [moveHistory, setMoveHistory] = useState([]);
  const [lastMove, setLastMove] = useState(null);
  const [thinkingTime, setThinkingTime] = useState(0);

  // Check for five in a row
  const checkWin = (board, row, col, player) => {
    const directions = [
      [0, 1],   // horizontal
      [1, 0],   // vertical  
      [1, 1],   // diagonal \
      [1, -1]   // diagonal /
    ];

    for (const [dr, dc] of directions) {
      let count = 1;
      
      // Count in positive direction
      let r = row + dr, c = col + dc;
      while (r >= 0 && r < BOARD_SIZE && c >= 0 && c < BOARD_SIZE && board[r][c] === player) {
        count++;
        r += dr;
        c += dc;
      }
      
      // Count in negative direction
      r = row - dr;
      c = col - dc;
      while (r >= 0 && r < BOARD_SIZE && c >= 0 && c < BOARD_SIZE && board[r][c] === player) {
        count++;
        r -= dr;
        c -= dc;
      }
      
      if (count >= 5) return true;
    }
    return false;
  };

  // Get valid moves near existing stones
  const getValidMoves = (board) => {
    const moves = [];
    const occupied = [];
    
    // Find all occupied positions
    for (let i = 0; i < BOARD_SIZE; i++) {
      for (let j = 0; j < BOARD_SIZE; j++) {
        if (board[i][j] !== EMPTY) {
          occupied.push([i, j]);
        }
      }
    }
    
    // If no stones, return center
    if (occupied.length === 0) {
      const center = Math.floor(BOARD_SIZE / 2);
      return [[center, center]];
    }
    
    const candidates = new Set();
    
    // Find positions within 2 squares of existing stones
    for (const [r, c] of occupied) {
      for (let dr = -2; dr <= 2; dr++) {
        for (let dc = -2; dc <= 2; dc++) {
          const newR = r + dr;
          const newC = c + dc;
          
          if (newR >= 0 && newR < BOARD_SIZE && 
              newC >= 0 && newC < BOARD_SIZE && 
              board[newR][newC] === EMPTY) {
            candidates.add(`${newR},${newC}`);
          }
        }
      }
    }
    
    // Convert back to array
    for (const pos of candidates) {
      const [r, c] = pos.split(',').map(Number);
      moves.push([r, c]);
    }
    
    return moves;
  };

  // Evaluate move heuristically
  const evaluateMove = (board, row, col, player) => {
    const opponent = -player;
    let score = 0;
    
    const directions = [[0, 1], [1, 0], [1, 1], [1, -1]];
    
    for (const [dr, dc] of directions) {
      const playerCount = countConsecutive(board, row, col, dr, dc, player);
      const opponentCount = countConsecutive(board, row, col, dr, dc, opponent);
      
      // Prioritize winning moves
      if (playerCount >= 4) score += 10000;
      if (opponentCount >= 4) score += 5000; // Block opponent win
      if (playerCount === 3) score += 100;
      if (playerCount === 2) score += 10;
      if (opponentCount === 3) score += 50;
      if (opponentCount === 2) score += 5;
    }
    
    // Prefer center positions slightly
    const center = Math.floor(BOARD_SIZE / 2);
    const distFromCenter = Math.abs(row - center) + Math.abs(col - center);
    score += Math.max(0, 5 - distFromCenter);
    
    return score;
  };

  const countConsecutive = (board, row, col, dr, dc, player) => {
    let count = 0;
    
    // Count in positive direction
    let r = row + dr, c = col + dc;
    while (r >= 0 && r < BOARD_SIZE && c >= 0 && c < BOARD_SIZE && board[r][c] === player) {
      count++;
      r += dr;
      c += dc;
    }
    
    // Count in negative direction
    r = row - dr;
    c = col - dc;
    while (r >= 0 && r < BOARD_SIZE && c >= 0 && c < BOARD_SIZE && board[r][c] === player) {
      count++;
      r -= dr;
      c -= dc;
    }
    
    return count;
  };

  // Simple MCTS implementation
  const mcts = async (board, player, iterations = 500) => {
    const moves = getValidMoves(board);
    if (moves.length === 0) return null;
    
    // For immediate wins or blocks, return immediately
    for (const [r, c] of moves) {
      if (evaluateMove(board, r, c, player) >= 5000) {
        return [r, c];
      }
    }
    
    const moveScores = new Map();
    const moveVisits = new Map();
    
    // Initialize move stats
    for (const move of moves) {
      const key = `${move[0]},${move[1]}`;
      moveScores.set(key, 0);
      moveVisits.set(key, 0);
    }
    
    // Run simulations
    for (let i = 0; i < iterations; i++) {
      // Select move (epsilon-greedy for simplicity)
      let selectedMove;
      if (Math.random() < 0.3 || i < moves.length) {
        // Exploration: random move or ensure each move tried once
        selectedMove = moves[i < moves.length ? i : Math.floor(Math.random() * moves.length)];
      } else {
        // Exploitation: choose best move so far
        let bestScore = -Infinity;
        let bestMove = moves[0];
        
        for (const move of moves) {
          const key = `${move[0]},${move[1]}`;
          const visits = moveVisits.get(key);
          const wins = moveScores.get(key);
          
          if (visits === 0) {
            bestMove = move;
            break;
          }
          
          // UCB1-like selection
          const exploitation = wins / visits;
          const exploration = Math.sqrt(Math.log(i + 1) / visits);
          const ucb = exploitation + 1.4 * exploration;
          
          if (ucb > bestScore) {
            bestScore = ucb;
            bestMove = move;
          }
        }
        selectedMove = bestMove;
      }
      
      // Simulate game from this move
      const result = simulateGame(board, selectedMove, player);
      
      // Update statistics
      const key = `${selectedMove[0]},${selectedMove[1]}`;
      moveVisits.set(key, moveVisits.get(key) + 1);
      
      if (result === player) {
        moveScores.set(key, moveScores.get(key) + 1);
      } else if (result === 0) {
        moveScores.set(key, moveScores.get(key) + 0.5);
      }
    }
    
    // Select move with highest win rate
    let bestMove = moves[0];
    let bestWinRate = -1;
    
    for (const move of moves) {
      const key = `${move[0]},${move[1]}`;
      const visits = moveVisits.get(key);
      const wins = moveScores.get(key);
      
      if (visits > 0) {
        const winRate = wins / visits;
        if (winRate > bestWinRate) {
          bestWinRate = winRate;
          bestMove = move;
        }
      }
    }
    
    return bestMove;
  };

  // Simulate a random game
  const simulateGame = (initialBoard, firstMove, firstPlayer) => {
    const simBoard = initialBoard.map(row => [...row]);
    const [r, c] = firstMove;
    simBoard[r][c] = firstPlayer;
    
    // Check if first move wins
    if (checkWin(simBoard, r, c, firstPlayer)) {
      return firstPlayer;
    }
    
    let currentSimPlayer = -firstPlayer;
    let moveCount = 0;
    const maxMoves = 20; // Limit simulation length
    
    while (moveCount < maxMoves) {
      const moves = getValidMoves(simBoard);
      if (moves.length === 0) break;
      
      // Choose move with some heuristic guidance
      let selectedMove;
      if (Math.random() < 0.7) {
        // Heuristic move
        const moveScores = moves.map(([r, c]) => ({
          move: [r, c],
          score: evaluateMove(simBoard, r, c, currentSimPlayer)
        }));
        
        moveScores.sort((a, b) => b.score - a.score);
        
        // Weighted selection from top moves
        const topMoves = moveScores.slice(0, Math.min(5, moveScores.length));
        const weights = topMoves.map((_, i) => Math.pow(2, topMoves.length - i));
        const totalWeight = weights.reduce((sum, w) => sum + w, 0);
        
        let random = Math.random() * totalWeight;
        for (let i = 0; i < topMoves.length; i++) {
          random -= weights[i];
          if (random <= 0) {
            selectedMove = topMoves[i].move;
            break;
          }
        }
      } else {
        // Random move
        selectedMove = moves[Math.floor(Math.random() * moves.length)];
      }
      
      const [simR, simC] = selectedMove;
      simBoard[simR][simC] = currentSimPlayer;
      
      if (checkWin(simBoard, simR, simC, currentSimPlayer)) {
        return currentSimPlayer;
      }
      
      currentSimPlayer = -currentSimPlayer;
      moveCount++;
    }
    
    return 0; // Draw
  };

  // Handle human move
  const handleCellClick = async (row, col) => {
    if (!gameStarted || gameStatus !== 'playing' || board[row][col] !== EMPTY || 
        currentPlayer !== humanPlayer || isThinking) {
      return;
    }

    // Make human move
    const newBoard = board.map(r => [...r]);
    newBoard[row][col] = humanPlayer;
    setBoard(newBoard);
    setLastMove([row, col]);
    setMoveHistory(prev => [...prev, { player: humanPlayer, row, col }]);

    // Check for win
    if (checkWin(newBoard, row, col, humanPlayer)) {
      setGameStatus(humanPlayer === BLACK ? 'black_wins' : 'white_wins');
      return;
    }

    // Switch to AI
    setCurrentPlayer(-humanPlayer);
    setIsThinking(true);
    
    // Add small delay to show thinking
    setTimeout(async () => {
      const startTime = Date.now();
      const aiMove = await mcts(newBoard, -humanPlayer, 800);
      const endTime = Date.now();
      setThinkingTime(endTime - startTime);
      
      if (aiMove) {
        const [aiRow, aiCol] = aiMove;
        const aiBoard = newBoard.map(r => [...r]);
        aiBoard[aiRow][aiCol] = -humanPlayer;
        setBoard(aiBoard);
        setLastMove([aiRow, aiCol]);
        setMoveHistory(prev => [...prev, { player: -humanPlayer, row: aiRow, col: aiCol }]);

        if (checkWin(aiBoard, aiRow, aiCol, -humanPlayer)) {
          setGameStatus(-humanPlayer === BLACK ? 'black_wins' : 'white_wins');
        } else {
          setCurrentPlayer(humanPlayer);
        }
      }
      setIsThinking(false);
    }, 100);
  };

  const startGame = (humanColor) => {
    setHumanPlayer(humanColor);
    setCurrentPlayer(BLACK);
    setGameStatus('playing');
    setGameStarted(true);
    setMoveHistory([]);
    setLastMove(null);
    setIsThinking(false);
    
    // Reset board
    setBoard(Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(EMPTY)));
    
    // If human is white, AI goes first
    if (humanColor === WHITE) {
      setIsThinking(true);
      setTimeout(async () => {
        const center = Math.floor(BOARD_SIZE / 2);
        const newBoard = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(EMPTY));
        newBoard[center][center] = BLACK;
        setBoard(newBoard);
        setLastMove([center, center]);
        setMoveHistory([{ player: BLACK, row: center, col: center }]);
        setCurrentPlayer(WHITE);
        setIsThinking(false);
      }, 500);
    }
  };

  const resetGame = () => {
    setGameStarted(false);
    setBoard(Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(EMPTY)));
    setCurrentPlayer(BLACK);
    setGameStatus('playing');
    setMoveHistory([]);
    setLastMove(null);
    setIsThinking(false);
  };

  const renderCell = (row, col) => {
    const cellValue = board[row][col];
    const isLastMove = lastMove && lastMove[0] === row && lastMove[1] === col;
    
    let cellClass = "w-6 h-6 border border-gray-400 cursor-pointer flex items-center justify-center text-sm font-bold transition-colors ";
    
    if (cellValue === EMPTY) {
      cellClass += "bg-amber-100 hover:bg-amber-200 ";
    } else {
      cellClass += "bg-amber-50 ";
    }
    
    if (isLastMove) {
      cellClass += "ring-2 ring-red-500 ";
    }
    
    if (gameStatus !== 'playing' || isThinking || currentPlayer !== humanPlayer) {
      cellClass += "cursor-not-allowed ";
    }

    return (
      <div
        key={`${row}-${col}`}
        className={cellClass}
        onClick={() => handleCellClick(row, col)}
      >
        {cellValue === BLACK && <span className="text-black">●</span>}
        {cellValue === WHITE && <span className="text-gray-600">○</span>}
      </div>
    );
  };

  if (!gameStarted) {
    return (
      <div className="flex flex-col items-center space-y-4 p-6">
        <h1 className="text-2xl font-bold text-gray-800">Gomoku vs MCTS AI</h1>
        <p className="text-gray-600 text-center">
          Get 5 in a row to win! The AI uses Monte Carlo Tree Search with 800 iterations per move.
        </p>
        <div className="space-y-2">
          <button
            onClick={() => startGame(BLACK)}
            className="block w-48 bg-black text-white px-4 py-2 rounded hover:bg-gray-800 transition-colors"
          >
            Play as Black (●) - You go first
          </button>
          <button
            onClick={() => startGame(WHITE)}
            className="block w-48 bg-gray-600 text-white px-4 py-2 rounded hover:bg-gray-700 transition-colors"
          >
            Play as White (○) - AI goes first
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="flex flex-col items-center space-y-4 p-4">
      <div className="flex justify-between items-center w-full max-w-md">
        <h1 className="text-xl font-bold text-gray-800">Gomoku vs MCTS</h1>
        <button
          onClick={resetGame}
          className="bg-gray-500 text-white px-3 py-1 rounded text-sm hover:bg-gray-600 transition-colors"
        >
          New Game
        </button>
      </div>
      
      <div className="flex space-x-6">
        <div className="text-center">
          <div className="text-sm text-gray-600 mb-1">You</div>
          <div className={`text-lg font-bold ${humanPlayer === BLACK ? 'text-black' : 'text-gray-600'}`}>
            {humanPlayer === BLACK ? '●' : '○'}
          </div>
        </div>
        <div className="text-center">
          <div className="text-sm text-gray-600 mb-1">MCTS AI</div>
          <div className={`text-lg font-bold ${-humanPlayer === BLACK ? 'text-black' : 'text-gray-600'}`}>
            {-humanPlayer === BLACK ? '●' : '○'}
          </div>
        </div>
      </div>

      {gameStatus === 'playing' && (
        <div className="text-center">
          {isThinking ? (
            <div className="text-blue-600 font-semibold">🤔 AI is thinking...</div>
          ) : currentPlayer === humanPlayer ? (
            <div className="text-green-600 font-semibold">Your turn!</div>
          ) : (
            <div className="text-orange-600 font-semibold">AI's turn</div>
          )}
        </div>
      )}

      {gameStatus !== 'playing' && (
        <div className="text-center">
          {gameStatus === 'black_wins' && (
            <div className="text-lg font-bold text-black">
              {humanPlayer === BLACK ? '🎉 You win!' : '💻 AI wins!'}
            </div>
          )}
          {gameStatus === 'white_wins' && (
            <div className="text-lg font-bold text-gray-600">
              {humanPlayer === WHITE ? '🎉 You win!' : '💻 AI wins!'}
            </div>
          )}
          {gameStatus === 'draw' && (
            <div className="text-lg font-bold text-gray-500">It's a draw!</div>
          )}
        </div>
      )}

      <div className="grid gap-0 border-2 border-gray-600 p-2 bg-amber-50" style={{gridTemplateColumns: 'repeat(13, 1fr)', display: 'grid'}}>
        {board.map((row, rowIndex) =>
          row.map((_, colIndex) => renderCell(rowIndex, colIndex))
        )}
      </div>

      {thinkingTime > 0 && (
        <div className="text-xs text-gray-500">
          AI thought for {(thinkingTime / 1000).toFixed(1)}s
        </div>
      )}
      
      <div className="text-xs text-gray-500 max-w-md text-center">
        Click on empty intersections to place your stone. Get 5 in a row (horizontal, vertical, or diagonal) to win!
      </div>
    </div>
  );
};

export default PlayableGomokuMCTS;
```

## How to Use:

**Option 1: Create React App**
```bash
npx create-react-app gomoku-game
cd gomoku-game
# Replace src/App.js with the code above
npm start
```

**Option 2: Next.js**
```bash
npx create-next-app@latest gomoku-game
cd gomoku-game
# Create pages/index.js with the component above
npm run dev
```

**Option 3: Standalone HTML** (add this wrapper):
```html
<!DOCTYPE html>
<html>
<head>
    <script src="https://unpkg.com/react@18/umd/react.development.js"></script>
    <script src="https://unpkg.com/react-dom@18/umd/react-dom.development.js"></script>
    <script src="https://unpkg.com/@babel/standalone/babel.min.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
</head>
<body>
    <div id="root"></div>
    <script type="text/babel">
        // Paste the React component code here
        ReactDOM.render(<PlayableGomokuMCTS />, document.getElementById('root'));
    </script>
</body>
</html>
```

The component needs **Tailwind CSS** for styling, so make sure you include it in your project!
