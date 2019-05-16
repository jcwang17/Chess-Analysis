library("rchess")
data(chesswc)
str(chesswc)

xsort <- function() {
  df <- data.frame("SN" = integer(), "Name" = character(), "Square" = character(), "Captor" = character(), "Captured" = character())
  
  pgn <- system.file("extdata/pgn/kasparov_vs_topalov.pgn", package = "rchess")
  pgn <- readLines(pgn, warn = FALSE)
  pgn <- strsplit(paste(pgn, collapse = " "), ' ')
  count = 0
  for (x in pgn) 
    {
    if (grepl("x",x,fixed=FALSE))
      {
      print(x)
      count = count + 1
    }
  }
  print(count)
  
  df <- rbind(df, list(count,,x,))
}

survivalPlot <- function(df) {
  write.csv(df, file = "df", append = False)
  library("ggplot2")
  library("reshape2")
  library("plyr")
  library("scales")
  chss <- read.csv("df")
  chss$Name <- with(chss, reorder(Name, PTS))
  chss.m <- melt(chss)
  chss.m <- ddply(chss.m, .(variable), transform, rescale = rescale(value))
  p <- ggplot(chss.m, aes(variable, Name)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
  
}

fen <- function(x) {
  ggchessboard(fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  cellcols = c("#D2B48C", "#F5F5DC"), perspective = "white", piecesize = 15)
}
move <- function(x) {
  chss <- Chess$new()
  chss$move("" + x)
  plot(chss, type = "ggplot")
}
gradient <- function() {
  
}