




file <- "letters.txt"
dades <- read.table(file, header = FALSE, sep = " ")
colnames(dades)[36] <- "label"
#data <- as.factor(data$label)


corrompreLletra <- function(lletra) {
  #label <- as.character(as.vector(lletra[1,36]))
  #lletraCorruptaDf <- data.frame(matrix(ncol = 36, nrow = 0))
  #label <- as.character(as.vector(lletra[36]))
  #lletraCorrupta <- as.numeric(as.vector(lletra[1,1:35]))
  lletraCorrupta <- as.numeric(as.vector(lletra[1:35]))
  #return(lletraCorrupta)
  nBitsCorruptes <- rpois(n=1,lambda=1.01)
  while (nBitsCorruptes > 35) {
    nBitsCorruptes <- rpois(n=1,lambda=1.01)
  }
  #if (nBitsCorruptes > 0) {
  #  print("he")
  #}
  bitsACorrompre <- sample(lletraCorrupta,nBitsCorruptes)
  lletraCorrupta[bitsACorrompre] <-  as.numeric(as.vector(mapply((function (x) if(x==1) 0 else 1),lletraCorrupta[bitsACorrompre])))
  #lletraCorrupta <- data.frame(t(lletraCorrupta),label)
  #colnames(data)[36] <- "label"
  #colnames(lletraCorrupta)[36] <- "label"
  #return()
  lletraCorrupta <- as.data.frame(c(lletraCorrupta,lletra[36]))
  #lletraCorrupta[36,] <- as.factor(lletra[36])
  #lletraCorrupta <- t(lletraCorrupta)
  #lletraCorrupta[36] <- lletra[36]
  #print(label)
  #return(rbind(lletraCorruptaDf,as.data.frame(lletraCorrupta)))
  #return(as.data.frame(lletraCorrupta))
  #return(c(lletraCorrupta,label))
  return(lletraCorrupta)
}


generarLletresCorruptes <- function(lletres,n) {
  if (n == 0) {
    return(data.frame())
  }
  #lletresCorruptes <- data.frame(mapply(corrompreLletra,lletres))
  #lletresCorruptes <- data.frame(lapply(corrompreLletra,lletres))
  lletresCorruptes<-as.data.frame(apply(lletres,1,corrompreLletra))
  lletresCorruptes <- as.data.frame(t(lletresCorruptes))
  #lletresCorruptes<-as.data.frame(apply(lletres,1,(function(x) {return(x)})))
  i <- 1
  while (i < n) {
    #lletresCorruptes[nrow(lletresCorruptes) + 1,] <- mapply(corrompreLletra,lletres)
    #lletresCorruptes[nrow(lletresCorruptes) + 1,] <- apply(lletres,1,corrompreLletra)
    aux <-as.data.frame(apply(lletres,1,corrompreLletra))
    aux <- as.data.frame(t(aux))
    lletresCorruptes <- rbind(lletresCorruptes,aux)
    #lletresCorruptes <- rbind(lletresCorruptes,as.data.frame(apply(lletres,1,corrompreLletra)))
    i <- i + 1
  }
  rownames(lletresCorruptes) <- c()
  return(lletresCorruptes)
}






#library(caret)
#library(MASS)
library(nnet)
#set.seed (4567)

learn <- dades
learn[,1:35] <- lapply(data[,1:35], as.numeric)
learn$label <- as.factor(learn$label)
nn1 <- nnet(label~.,data=learn,size=5,decay=0.1,maxit=2000,trace=T)
test <- generarLletresCorruptes(dades,1)
test[,1:35] <- lapply(test[,1:35], as.numeric)
colnames(test)[36] <- "label"
test$label <- as.factor(test$label)
p <- predict (nn1, newdata=test, type="raw")


#(sizes <- seq(1,40,by=2)+1)

## specify 10x10 CV
#trc <- trainControl (method="repeatedcv", number=10, repeats=10,verboseIter = TRUE)

#(decays <- 10^seq(-3,0,by=0.05))
## WARNING: this takes some minutes
#model.10x10CV <- train (y ~ x, data = mydata, method='nnet', maxit = 300, trace = FALSE,
#                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)
#model.10x10CV <- train (y ~ x, data = mydata, method='nnet', maxit = 300,trace = FALSE,linout = TRUE,metric = 'RMSE',
#                        tuneGrid = expand.grid(.size=40,.decay=decays), trControl=trc)

