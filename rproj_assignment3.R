data<- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
head(data)
data[,11]<-as.numeric(data[,11])
hist(data[,11])
state <-"XXXH"
outcome <- "Pneumonia Hi"
x<-strsplit(outcome," ")
length(x[[1]])
best <- function(state, outcome) {
  ## Read outcome data
  data<- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  x<-strsplit(outcome," ")
  if(length(x[[1]])>1){
  mod_outcome<-paste(x[[1]][1],'.',x[[1]][2], sep ="")
  col_name<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",mod_outcome, sep="")
  }
    else{
    col_name<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome, sep="")
    }
  if(is.na(match(col_name,names(data))))
  {
    stop("invalid outcome")}
  t<-data[data$State==state,]
  if(nrow(t)==0){
    stop("invalid state")
  }
  ## Return hospital name in that state with lowest 30-day death
  data[,col_name]<-as.numeric(data[,col_name])
  data<-data[complete.cases(data[,col_name]),]
  filter_state = data[data$State==state,]
  df<-aggregate(filter_state[,col_name], filter_state[,c(7,2)], FUN = sum)
  bestscore<-min(df[,'x'])
  final<-df[df['x']==bestscore,]
  final_ordered<-final[order('x'),]
  final_ordered
  ## rate
}

