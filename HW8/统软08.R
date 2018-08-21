dm <- read.csv('subscore-subtype.csv', header=TRUE,
               stringsAsFactors=FALSE)
ds <- read.csv('subscore-subscore.csv', header=TRUE,
               stringsAsFactors=FALSE)

score_summary<-function(dm,ds){
  resm<-data.frame(ds[1])#第一列是学号
  for (qtype in unique(dm[,2])){
    index<-which(dm[,2]==qtype)
    if (length(index)>1){
      resm[qtype]<-rowSums(ds[, paste('Y', index,sep='')])}
    else {
      resm[qtype]<-ds[, paste('Y', index,sep='')]
    }
  }
  resm
}

resm<-score_summary(dm,ds)
resm[order(resm[,'学号']),]