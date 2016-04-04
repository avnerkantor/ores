# https://ores-newmodels.wmflabs.org/v2/scores/enwiki/damaging/623818349/
# Machine Learning as a Service.
# Evaluating
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
  rdTrue <- fromJSON(paste0("https://ores-newmodels.wmflabs.org/v2/scores/enwiki/damaging/", id, "/?feature.is_anon=true"))
  rdFalse<-fromJSON(paste0("https://ores-newmodels.wmflabs.org/v2/scores/enwiki/damaging/", id, "/?feature.is_anon=false"))
  
  rdTruePredictionTrue<-rdTrue$scores$enwiki$damaging$scores[[1]]$probability$true
  rdTruePredictionFalse<-rdFalse$scores$enwiki$damaging$scores[[1]]$probability$true
  
  return(list(rdTruePredictionTrue, rdTruePredictionFalse))
}

### Test New Model - Prediction is False
anonFalseNewModel<-sapply(1:500, function(x) getStats(dfFalse[x,]))

### Manipulating
anonFalseNewModelDF<-t(anonFalseNewModel)
colnames(anonFalseNewModelDF)<-c("anon_true", "anon_false")
anonFalseNewModelDF<-as.data.frame(anonFalseNewModelDF)
anonFalseNewModelDFManipulated<-melt(lapply(anonFalseNewModelDF, unlist))

### PlotFalse
ggplot(anonFalseNewModelDFManipulated, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is false", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")

### Test New Model - Prediction is True
anonTrueNewModel<-sapply(1:500, function(x) getStats(dfTrue[x,]))

### Manipulating
anonTrueNewModelDF<-t(anonTrueNewModel)
colnames(anonTrueNewModelDF)<-c("anon_true", "anon_false")
anonTrueNewModelDF<-as.data.frame(anonTrueNewModelDF)
anonTrueNewModelDFManipulated<-melt(lapply(anonTrueNewModelDF, unlist))

### PlotTrue
ggplot(anonTrueNewModelDFManipulated, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is true", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")

###Calculate the difference
anonTrueNewModelDF$difference<-as.numeric(as.character(anonTrueNewModelDF$anon_true))-as.numeric(as.character(anonTrueNewModelDF$anon_false))
anonTrueNewModelDF<-anonTrueNewModelDF[, c("anon_true", "anon_false", "difference")]
anonTrueNewModelDFManipulated2<-melt(lapply(anonTrueNewModelDF, unlist))

### PlotTrue2
ggplot(anonTrueNewModelDFManipulated2, aes(x=value)) + 
  geom_density(aes(group=L1, colour=L1, fill=L1), alpha=0.3) +
  labs(title="ORES damaging prediction is true", x="N=500") + 
  geom_vline(xintercept = c(0.8, 0.87, 0.94),  colour="green", linetype = "longdash")
