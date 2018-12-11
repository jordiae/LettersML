file <- "letters.txt"
dades <- read.table(file, header = FALSE, sep = " ")


corrompreLletra <- function(lletra) {

  lletraCorrupta <- as.numeric(as.vector(lletra[1:35]))
  nBitsCorruptes <- rpois(n=1,lambda=1.01)
  while (nBitsCorruptes > 35) {
    nBitsCorruptes <- rpois(n=1,lambda=1.01)
  }
  bitsACorrompre <- sample(seq(1,35),nBitsCorruptes)
  lletraCorrupta[bitsACorrompre] <-  as.numeric(as.vector(mapply((function (x) if(x==1) 0 else 1),lletraCorrupta[bitsACorrompre])))

  lletraCorrupta <- as.data.frame(c(lletraCorrupta,lletra[36]))

  return(lletraCorrupta)
}


generarLletresCorruptes <- function(lletres,n) {
  if (n == 0) {
    return(data.frame())
  }
  i <- 1
  mostres <- sample(1:26,n,replace=TRUE)
  lletresCorruptes<-as.data.frame(apply(lletres[mostres[i],],1,corrompreLletra))
  lletresCorruptes <- as.data.frame(t(lletresCorruptes))
  
  while (i < n) {
    i <- i + 1
    aux <-as.data.frame(apply(lletres[mostres[i],],1,corrompreLletra))
    aux <- as.data.frame(t(aux))
    lletresCorruptes <- rbind(lletresCorruptes,aux)

  }
  rownames(lletresCorruptes) <- c(1:n)
  colnames(lletresCorruptes) <- c(1:35,"label")
  lletresCorruptes$label <- as.factor(lletresCorruptes$label)
  return(lletresCorruptes)
}



library(caret)
library(MASS)
library(nnet)
set.seed (4567)

learn <- generarLletresCorruptes(dades,500)

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10,verboseIter = TRUE)

(decays <- 10^seq(-3,0,by=0.05))
## WARNING: this takes some minutes
model.10x10CV <- train (label ~., data = learn, method='nnet', maxit = 300, trace = FALSE, metric="Accuracy", tuneGrid = expand.grid(.size=15,.decay=decays), trControl=trc)

save(model.10x10CV,file="problema_7_model.mod")
load ("problema_7_model.mod")

test <- generarLletresCorruptes(dades,1000)
test$label <- as.factor(test$label)

p <- factor(predict (model.10x10CV, newdata=test, type="raw"),levels=dades[,36])
c <- confusionMatrix(test$label,p)
