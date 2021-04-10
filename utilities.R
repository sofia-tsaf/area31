## Function to add categories describing the relationship between the
## true and approximate categories
compCat <- function(dat, method="cmsy.naive")
{
  tmpDat <- dat
  sep <- ifelse(method=="", "", ".")
  ## for 4 categories
  tmpDat$trueCat4 <- ifelse(dat$bbmsy>1, ifelse(dat$ffmsy<1,1,2),
                     ifelse(dat$ffmsy<1,3,4))
  tmpDat$estCat4 <- ifelse(dat[,paste("bbmsy", method, sep=sep)]>1,
                    ifelse(dat[,paste("ffmsy", method, sep=sep)]<1,1,2),
                    ifelse(dat[,paste("ffmsy", method, sep=sep)]<1,3,4))
  tmpDat$confMat4 <- (tmpDat$trueCat4-1)*4 + tmpDat$estCat4

  # for 3 categories
  tmpDat$trueCat3 <- ifelse(dat$bbmsy<0.8, 3, ifelse(dat$bbmsy<1.2,2,1))
  tmpDat$estCat3 <- ifelse(dat[,paste("bbmsy",method,sep=sep)]<0.8,3,
                    ifelse(dat[,paste("bbmsy",method,sep=sep)]<1.2,2,1))
  tmpDat$confMat3 <- (tmpDat$trueCat3-1)*3 + tmpDat$estCat3

  tmpDat
}

plotComp3cats <- function()
{
  txt <- c("b>1.2", "0.8<b<1.2", "b<0.8")
  vals <- c("E","C","C",  "O","E","C",  "O","O","E")
  cols <- c(E="green", O="red", A="gray", C="blue")
  catNames <- c(E="Equal", O="Over optimistic", A="Ambiguous", C="Conservative")
  plot(1, 1, xlim=c(0,3), ylim=c(0,3), xaxs="i", yaxs="i", type="n", xlab="",
       ylab="", xaxt="n", yaxt="n")
  for(i in 1:3){ # true
    for(j in 1:3){ # est
      numVal <- (i-1)*3 + j
      rect(xleft=i-1, xright=i, ybottom=j-1, ytop=j, col=cols[vals[numVal]])
      text(x=i-0.5, y=j-0.5, labels=paste(numVal))
    }
  }
  axis(side=1, at=(1:3)-0.5, labels=txt)
  axis(side=2, at=(1:3)-0.5, labels=txt)
  mtext(side=1, text="Truth", line=2.5)
  mtext(side=2, text="Approximation", line=2.5)
}

plotComp4cats <- function()
{
  txt <- c("b>1,f<1", "b>1,f>1", "b<1,f<1", "b<1,f>1")
  vals <- c("E","C","C","C",  "O","E","A","C",
            "O","A","E","C",  "O","O","O","E")
  cols <- c(E="green", O="red", A="gray", C="blue")
  catNames <- c(E="Equal", O="Over optimistic", A="Ambiguous", C="Conservative")
  plot(1, 1, xlim=c(0,4), ylim=c(0,4), xaxs="i", yaxs="i", type="n", xlab="",
       ylab="", xaxt="n", yaxt="n")
  for(i in 1:4){ # true
    for(j in 1:4){ # est
      numVal <- (i-1)*4 + j
      rect(xleft=i-1, xright=i, ybottom=j-1, ytop=j, col=cols[vals[numVal]])
      text(x=i-0.5, y=j-0.5, labels=paste(numVal))
    }
  }
  axis(side=1, at=(1:4)-0.5, labels=txt)
  axis(side=2, at=(1:4)-0.5, labels=txt)
  mtext(side=1, text="Truth", line=2.5)
  mtext(side=2, text="Approximation", line=2.5)
}

plotComp <- function(dat,method="cmsy.naive", cats=4, type="prop")
{
  vals4 <- c("E","C","C","C",  "O","E","A","C",
             "O","A","E","C",  "O","O","O","E")
  vals3 <- c("E","C","C",  "O","E","C",  "O","O","E")
  catNames <- c(E="Equal", O="Over optimistic", A="Ambiguous", C="Conservative")

  ## Create a new data frame with the categories
  tDat <- compCat(dat, method=method)
  tDat <- tDat[,c("Stock","yr","confMat3","confMat4")]
  tDat$cat3 <- vals3[tDat$confMat3]
  tDat$category3 <- catNames[tDat$cat3]
  tDat$cat4 <- vals4[tDat$confMat4]
  tDat$category4 <- catNames[tDat$cat4]

  if(cats == 3)
    tDat$category <- tDat$category3
  else
    tDat$category <- tDat$category4

  ## Plot
  library(ggplot2)
  if(type == "prop"){
    ggplot(data=tDat, aes(x=yr, color=category)) +
      geom_bar(aes(fill=category), width=0.5) +
      theme_minimal()
  } else if(type == "all"){
    ggplot(tDat, aes(x=yr, y=Stock, fill=category)) +
      geom_raster() +
      theme_minimal()
  }
}

plotProp <- function(dat, method="cmsy.naive", cats=4, type="prop"){
  txt3 <- c("b>1.2", "0.8<b<1.2", "b<0.8")
  txt4 <- c("b>1,f<1", "b>1,f>1", "b<1,f<1", "b<1,f>1")

  ## Create a new data frame with the categories
  tDat <- compCat(dat, method=method)
  tDat <- tDat[,c("Stock","yr","estCat3","estCat4")]

  if(cats == 3){
    tDat$estCat <- factor(txt3[tDat$estCat3], levels=txt3)
    cols <- c("darkgreen", "yellow", "red")
  } else {
    tDat$estCat <- factor(txt4[tDat$estCat4], levels=txt4)
    cols <- c("darkgreen", "orange", "yellow", "red")
  }

  ## Plot
  library(ggplot2)
  if(type == "prop"){
    ggplot(data=tDat, aes(x=yr, color=estCat)) +
      geom_bar(aes(fill=estCat), width=0.5) +
      theme_minimal() +
      scale_fill_manual(values=cols)
  } else if(type == "all"){
    ggplot(tDat, aes(x=yr, y=Stock, fill=estCat)) +
      geom_raster() +
      theme_minimal() +
      scale_fill_manual(values=cols)
  }
}
