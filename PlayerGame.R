source("~/Desktop/MonteCarlo/MonteCarlo Connect 4 bot.R")
if(!("crayon" %in% installed.packages())){install.packages("crayon");library(crayon)}else{library(crayon)}
#Player is red
#Computer is black
{
  GameOver = F
  BotIterations = 1
  BotDepth = 1
  PlusDiff = F
  Difficulty = as.numeric(readline(prompt = "Which Difficulty would you like: \n[1]-Easy     \t~1 seconds per move\n[2]-Normal   \t~4 seconds per move\n[3]-Hard    \t~35 seconds per move\n[4]-Extreme1\t~40 seconds per move\tBROKEN\n"))
  if(Difficulty == 1){
    BotIterations = 10
    BotDepth = 2
  } else if (Difficulty == 2){
    BotIterations = 40
    BotDepth = 6
  } else if (Difficulty == 3){
    BotIterations = 200
    BotDepth = 12
  } else if (Difficulty == 4){
    BotIterations = 1
    BotDepth = 5
    PlusDiff = T
  } else {
    stop("Please try again and put in a valid score")
  }
  
  cat("Selecting who goes first...\n")
  for(each in 1:40){
    Sys.sleep(.02)
    cat(sample(x = c(".","*","%","$","#","@","!","B","R","?"), size = 1))
  }
  cat("\n")
  First = sample(x = c("Player", "Computer"), size = 1)
  cat("The", First, "has been selected to go first.\n")
  Sys.sleep(2)
  cat("Building Game State.")
  for(each in 1:5){
    Sys.sleep(.5)
    cat(".")
  }
  cat("\n")
  while(!GameOver){
    if(First=="Player"){
      # Player Moves
      GameBoard = AddMove(board = GameBoard, Player = "R", col = PlayerMove(GameBoard))[[1]]
      if(CheckWin(GameBoard)){
        cat("Player Won!\n")
        break;
      }
      # Computer Moves
      GameBoard = ComputerMakeMove(GBoard = GameBoard, ComputerColor = "B",Iter = BotIterations, Dep = BotDepth, PLUS = PlusDiff)
      if(CheckWin(GameBoard)){
        cat("Computer Wins!\n")
        break;
      }
    } else {
      # Computer Moves
      GameBoard = ComputerMakeMove(GBoard = GameBoard, Iter = BotIterations, Dep = BotDepth, PLUS = PlusDiff)
      if(CheckWin(GameBoard)){
        cat("Computer Wins!\n")
        break;
      }
      # Player Moves
      GameBoard = AddMove(board = GameBoard, Player = "R", col = PlayerMove(GameBoard))[[1]]
      if(CheckWin(GameBoard)){
        cat("Player Won!\n")
        break;
      }
    }
  }
  PrintBoard(GameBoard)
  
}