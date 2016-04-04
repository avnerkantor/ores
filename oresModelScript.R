library(jsonlite)
library(dplyr)
library(reshape2)
library(ggplot2)

### Download revs_id
document <- fromJSON("http://labels.wmflabs.org/campaigns/enwiki/4/?tasks")

### Find revs_is.labels.damaging
revs_id<- document$tasks$data$rev_id
damagings<- sapply(1:20000, function(x) document$tasks$labels[[x]]$data$damaging)

### Find damaging predictions
df<-as.data.frame(cbind(revs_id, damagings))
dfFalse<-select(filter(df, damagings!="TRUE"), revs_id)
dfTrue<-select(filter(df, damagings=="TRUE"), revs_id)

### Get Damaging Probabilities
getStats <- function(id) {
  rdTrue <- fromJSON(paste0("https://ores.wmflabs.org/v2/scores/enwiki/damaging/", id, "/?feature.is_anon=true"))
  rdFalse<-fromJSON(paste0("https://ores.wmflabs.org/v2/scores/enwiki/damaging/", id, "/?feature.is_anon=false"))
  
  rdTruePredictionTrue<-rdTrue$scores$enwiki$damaging$scores[[1]]$probability$true
  rdTruePredictionFalse<-rdFalse$scores$enwiki$damaging$scores[[1]]$probability$true
  
  return(list(rdTruePredictionTrue, rdTruePredictionFalse))
}

### Test  Model - Prediction is False
anonFalseModel<-sapply(1:500, function(x) getStats(dfFalse[x,]))

### Manipulating
anonFalseModelDF<-t(anonFalseModel)
colnames(anonFalseModelDF)<-c("anon_true", "anon_false")
anonFalseModelDF<-as.data.frame(anonFalseModelDF)
anonFalseModelDFManipulated<-melt(lapply(anonFalseModelDF, unlist))

### PlotFalse
ggplot(anonFalseModelDFManipulated, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is false", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")

### Test  Model - Prediction is True
anonTrueModel<-sapply(1:500, function(x) getStats(dfTrue[x,]))

### Manipulating
anonTrueModelDF<-t(anonTrueModel)
colnames(anonTrueModelDF)<-c("anon_true", "anon_false")
anonTrueModelDF<-as.data.frame(anonTrueModelDF)
anonTrueModelDFManipulated<-melt(lapply(anonTrueModelDF, unlist))

### PlotTrue
ggplot(anonTrueModelDFManipulated, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is true", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")

###Calculate the difference
anonTrueModelDF$difference<-as.numeric(as.character(anonTrueModelDF$anon_true))-as.numeric(as.character(anonTrueModelDF$anon_false))
anonTrueModelDF<-anonTrueModelDF[, c("anon_true", "anon_false", "difference")]
anonTrueModelDFManipulated2<-melt(lapply(anonTrueModelDF, unlist))

### PlotTrue2
ggplot(anonTrueModelDFManipulated2, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is true", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")
