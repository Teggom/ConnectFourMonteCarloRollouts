GameBoard = matrix(rep(x = c(" "), 6*7), nrow = 6, ncol = 7)
colnames(GameBoard) = c("1", "2", "3", "4", "5", "6", "7")
rownames(GameBoard) = c("F", "E", "D", "C", "B", "A")


CheckWin <- function(board = GameBoard){
  isWin = F
  #Vertical
  if(!isWin){
    for(col in 1:7){
      for(row in 1:3){
        if(board[row,col]!=" " && board[row, col] == board[row+1,col]&&board[row,col]==board[row+2,col] && board[row,col]==board[row+3,col]){
          isWin = T
        }  
      }
    }
  }
  #Horizontal
  if(!isWin){
    for(col in 1:4){
      for(row in 1:6){
        if(board[row,col] != " " && board[row,col]==board[row,col+1]&&board[row,col]==board[row,col+2]&&board[row,col]==board[row,col+3]){
          isWin = T
        }
      }
    }
  }
  
  # Diagonal UpRight
  if(!isWin){
    for(col in 1:4){
      for(row in 4:6){
        if(board[row,col]!=" " &&board[row,col]==board[row-1,col+1]&&board[row,col]==board[row-2,col+2]&&board[row,col]==board[row-3,col+3]){
          isWin = T
        }
      }
    }
  }
  
  #Diagonal DownLeft
  if(!isWin){
    for(col in 1:4){
      for(row in 1:3){
        if(board[row,col]!=" " &&board[row,col]==board[row+1,col+1]&&board[row,col]==board[row+2,col+2]&&board[row,col]==board[row+3,col+3]){
          isWin=T
        }
      }
    }
  }
  return(isWin)
}

AddMove = function(board = GameBoard, Player = "B", col = 1){

  if(!(" " %in% board[,col])){
    return(list(UpdatedBoard = board, Success = F))
  } else {
    moved = F
    row = 2
    while(!moved){
      if(board[row,col]!=" "){
        board[row-1,col] = Player
        moved = T
      } else {
        row = row + 1
      }
      if(row == 7){
        board[row-1,col] = Player
        moved=T
      }
    }
  }
  return(list(UpdatedBoard = board, Success = T))
}

CheckBoardFull = function(board = GameBoard){
  if(" " %in% GameBoard){
    return(F)
  }
  return(T)
}

CheckNextMoveWin = function(board = GameBoard, Player="B", retCOL = F){
  NextMoveWin = F
  for(col in 1:7){
    holdBoard = board
    if(" " %in% board[,col]){
      holdBoard = AddMove(board = board, Player = Player, col = col)[[1]]
      if(CheckWin(board = holdBoard)){
        NextMoveWin = T
        if(retCOL){
          return(col)
        }
      }
    }
  }  
  return(NextMoveWin)
}

GetBestMove = function(board=GameBoard, forplayer="B", player="B", depth=0, PLUS=F){
  if(CheckBoardFull(board)){
    return(0)
  }
  
  # Checks if this move will be a win
  if(CheckNextMoveWin(board = board, Player = player)){
    if(player == forplayer){
      return(1) # Computer just won :)
    } else {
      return(-1) # Computer just lost :(
    }
  }
  
  if(player == "R"){
    NextPlayer = "B"
  }else{
    NextPlayer = "R"
  }
  
  # Checks if current player is forced to prevent a win
  if(CheckNextMoveWin(board = board, Player=NextPlayer)){
    sendBoard = AddMove(board = board, Player = player, col = CheckNextMoveWin(board=board, Player = NextPlayer, retCOL = T))[[1]]
  } else {
    #next move isnt a win so go wherever
    # Get valid moves
    # Could rollout depth 3 here 5 times to improve pathing
    ValidMoves <- c()
    for(col in 1:7){
      if(" " %in% board[,col]){
        ValidMoves <- c(ValidMoves, col)
      }
    }
    if(PLUS){
      #print("PLUSING")
      if(depth<=0){
        return(-.2)
      }
      Moves = rep(0,length(ValidMoves))
      pos = 1
      for(Pos in ValidMoves){
        FastBoard = AddMove(board = board, Player = player, col = Pos)[[1]]
        Moves[pos] = Moves[pos] + GetBestMove(board = FastBoard, forplayer = player, player = NextPlayer, depth = depth-1, PLUS = PLUS)
        #cat(pos, depth, "\n")
        pos = pos + 1
      }
      return(sum(Moves)/length(ValidMoves))
      sendBoard = AddMove(board = board, Player = player, col = ValidMoves[which.max(Moves)])[[1]]
    } else {
      sendBoard = AddMove(board = board, Player = player, col = sample(ValidMoves,1))[[1]]
    }
  }
  
  if(depth == 0){
    #print("LOWDEPTH")
    # If last time, check and see if next move is a win +1 to checking
    if(CheckNextMoveWin(board = board, Player = NextPlayer)){
      if(NextPlayer == forplayer){
        return(1) # Computer wins next turn!
      } else {
        return(-1) # Player wins next turn :(
      }
    } else {
      if(PLUS){
        return(-.5)
      } else {
      return(-.1) # Since it was a tie this is sub-optimal, thus return a slightly negative score
      }
    }
  } else { #depth not 0, so keep rolling out. 
    return(GetBestMove(board = sendBoard, forplayer = forplayer, player = NextPlayer, depth = depth-1))
  }
  
  
}


ComputerMakeMove <- function(GBoard = GameBoard, ComputerColor = "B", Iter = BotIterations, Dep = BotDepth, PLUS = F){
  NextPlayer="R"
  Moves = c(0,0,0,0,0,0,0)
  cat("Time for the computer to move!\n")
  cat("------Thinking-------\n")
  if(!CheckNextMoveWin(board=GBoard, Player = ComputerColor)){
    cat("|")
    for(col in 1:7){
      FastBoard = AddMove(board = GBoard, Player = ComputerColor, col = col)[[1]]
      for(each in 1:Iter){
        if(each%%floor(Iter/4)==0){
          cat(col)
        }
        Moves[col] = Moves[col]+GetBestMove(board = FastBoard, forplayer = ComputerColor, player = NextPlayer, depth = Dep, PLUS = PLUS)
      }
      cat("|")
      #hide verbosity
      #cat("[",col,":",Moves[col],"]\n")
    }
    cat("\n")
    Moved = F
    while(!Moved){
      if(" " %in% GameBoard[,which.max(Moves)]){
        GameBoard = AddMove(board = GameBoard, Player = ComputerColor, which.max(Moves))[[1]]
        Moved = T
      } else {
        Moves[which.max(Moves)] = min(Moves)-1
      }
    }
  } else {
    GameBoard = AddMove(board = GBoard, Player = ComputerColor, col = CheckNextMoveWin(board=GBoard, Player = ComputerColor, retCOL = T))[[1]]
  }
  return(GameBoard)
}

PrintBoard = function(Board = GameBoard){
  cat(" _1__2__3__4__5__6__7__  \n")
  for(row in 1:6){
    cat("| ")
    for(col in 1:7){
      if(Board[row,col]=="R"){
        cat(red(Board[row,col])," ")
      } else {
        cat(Board[row,col]," ")
      }
    }
    cat("|\n")
  }
  cat("--#--#--#--#--#--#--#-- \n")
}
PlayerMove = function(Board = GameBoard){
  PrintBoard(Board = Board)
  getMove = T
  while(getMove){
    Move = as.numeric(readline(prompt = "Which column [1-7] would you like to move into: "))
    cat("\n")
    if(!(is.na(Move)) && is.numeric(Move) && Move>0 && Move<8){
      if(" " %in% Board[,Move]){
        getMove = F
      } 
    } else {
      cat("Please try again.\n")
    }
  }
  return(Move)
}

