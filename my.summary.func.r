#############	my.summary function

my.summary <- function(in.df){
  
  VAR.NAME <- colnames(in.df);
  
  # Here we will embed the functions not.na() and count.na()
  # directly into the function my.summary().
  # There are other ways to do this, but this is easiest for
  # classroom discussion.
  not.na <- function(x){
    out <- sum(-1*(is.na(x)-1));
    return(out);
  }
  NOT.NA <- apply(X=in.df,MARGIN=2,FUN='not.na');
  
  count.na <- function(x){
    out <- sum(is.na(x));
    return(out);
  }
  
  IS.NA <- apply(X=in.df,MARGIN=2,FUN='count.na');
  PCT.NA <- IS.NA/(NOT.NA+IS.NA);
  # Note that we need to account for computations with NA values;
  MIN <- apply(X=in.df,MARGIN=2,FUN='min',na.rm=TRUE);
  p <- c(0.01,0.05,0.25,0.50,0.75,0.95,0.99);
  Q <- apply(X=in.df,MARGIN=2,FUN='quantile',p,na.rm=TRUE);
  tQ.df <- as.data.frame(t(Q));
  colnames(tQ.df) <- c('Q01','Q05','Q25','Q50','Q75','Q95','Q99');
  MAX <- apply(X=in.df,MARGIN=2,FUN='max',na.rm=TRUE);
  MEDIAN <- apply(X=in.df,MARGIN=2,FUN='median',na.rm=TRUE)
  MEAN <- apply(X=in.df,MARGIN=2,FUN='mean',na.rm=TRUE)
  SD <- apply(X=in.df,MARGIN=2,FUN='sd',na.rm=TRUE)
  
  wide.df <- cbind(VAR.NAME,NOT.NA,IS.NA,PCT.NA,MIN,tQ.df,MAX,MEDIAN,MEAN,SD);
  rownames(wide.df) <- seq(1,dim(wide.df)[1],1);
  
  return(wide.df);
}