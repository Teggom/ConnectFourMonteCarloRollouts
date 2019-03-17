source("~/Desktop/MonteCarlo/MonteCarlo Connect 4 bot.R")
if(!("crayon" %in% installed.packages())){install.packages("crayon");library(crayon)}else{library(crayon)}
GameOver = F

# AI PARAMETERS
REDDEPTH = 4
REDITERATIONS = 1
REDPLUS = T

BLACKDEPTH = 6
BLACKITERATIONS = 60
BLACKPLUS = F

while(!GameOver){
  Player = "B"
  NextPlayer = "R"
  Moves = c(0,0,0,0,0,0,0)
  cat("Time for Black to move!\n")
  cat("------Thinking-------\n")
  if(!CheckNextMoveWin(board=GameBoard, Player = Player)){
    for(col in 1:7){
      FastBoard = AddMove(board = GameBoard, Player = Player, col = col)[[1]]
      for(each in 1:BLACKITERATIONS){
        Moves[col] = Moves[col]+GetBestMove(board = FastBoard, forplayer = Player, player = NextPlayer, depth = BLACKDEPTH,PLUS = BLACKPLUS)
      }
      cat("[",col,":",Moves[col],"]\n")
    }
    cat("\n")
    Moved = F
    while(!Moved){
      if(" " %in% GameBoard[,which.max(Moves)]){
        GameBoard = AddMove(board = GameBoard, Player = Player, which.max(Moves))[[1]]
        Moved = T
      } else {
        Moves[which.max(Moves)] = min(Moves)-1
      }
    }
  } else {
    GameBoard = AddMove(board = GameBoard, Player = Player, col = CheckNextMoveWin(board=GameBoard, Player = Player, retCOL = T))[[1]]
  }
  PrintBoard(GameBoard)
  if(CheckWin(board = GameBoard)){
    print("BLACK WINS!")
    GameOver = T
    break
  }

  Player = "R"
  NextPlayer = "B"
  Moves = c(0,0,0,0,0,0,0)
  cat("Time for Red move!\n")
  cat(red("------Thinking------\n"))
  if(!(CheckNextMoveWin(board = GameBoard, Player = Player))){
    for(col in 1:7){
      FastBoard = AddMove(board = GameBoard, Player = Player, col = col)[[1]]
      for(each in 1:REDITERATIONS){
        Moves[col] = Moves[col]+GetBestMove(board = FastBoard, forplayer = Player, player = NextPlayer, depth = REDDEPTH-1, PLUS = REDPLUS)
      }
      cat("[",col,":",Moves[col],"]\n")
    }
    cat("\n")
    Moved = F
    while(!Moved){
      if(!REDPLUS){
        if(" " %in% GameBoard[,which.max(Moves)]){
          GameBoard = AddMove(board = GameBoard, Player = Player, which.max(Moves))[[1]]
          Moved = T
        } else {
          Moves[which.max(Moves)] = min(Moves)-1
        }
      } else {
        if(" " %in% GameBoard[,which.min(Moves)]){
          Moves[Moves==-1] = 1.1
          Moves[Moves==1] = -1.1
          GameBoard = AddMove(board = GameBoard, Player = Player, which.min(Moves))[[1]]
          Moved = T
        } else {
          Moves[which.min(Moves)] = max(Moves)+1
        }
      }
    }
  } else {
    GameBoard = AddMove(board = GameBoard, Player = Player, col = CheckNextMoveWin(board=GameBoard, Player = Player, retCOL = T))[[1]]
  }
  PrintBoard(GameBoard)
  if(CheckWin(board = GameBoard)){
    print("RED WINS!")
    #print("Strange since the white player AI was worse than the other.")
    GameOver = T
    break
  }
  
}

