# The complete R code for Sociology Paper

library(ggplot2)
library(plyr)
library(reshape)
library(lme4)
#library(lubridate)


# getting the data and some common functions

raw.dat1 <- read.csv("../data/raw_data_turk1.csv")
raw.dat2 <- read.csv("../data/raw_data_turk2.csv")
raw.dat3 <- read.csv("../data/raw_data_turk3.csv")

source("calculate_ump_power.R") # functions to compute power

# cleaning the data based on criteria 6 and removing the duplicated data
# (id,pic_id) should be unique since no person gets a lineup more than once.
clean_data <- function(raw_dat){
  easy_dat <- subset(raw_dat, p_value < 0.0002)
  if (raw_dat$experiment[1]=='turk3') easy_dat <- subset(raw_dat, difficulty==0)
  d <- ddply(subset(easy_dat),.(id),summarise,
             easy_cnt = length(response),
             percent_correct = mean(response)*100,
             include_id = response[1],
             excluded_lineup = paste(id[1],"_",pic_id[1],sep=""),
             excluded_lineup_p_value = round(p_value[1],4)
             )
  included_id <- d$id[d$include_id]
  indx <- raw_dat$id %in% included_id
  excluded_lineup <- paste(raw_dat$id,"_",raw_dat$pic_id,sep="") %in% d$excluded_lineup
  indx[excluded_lineup] <- FALSE
  cleaned_dat <- subset(raw_dat,indx)
  indx_dup <- with(cleaned_dat, !duplicated(data.frame(id,pic_id))) #duplication index
  # now returning cleaned data removing duplication
  return(subset(cleaned_dat,indx_dup))
}

dat1 <- clean_data(raw.dat1)
dat2 <- clean_data(raw.dat2)
dat3 <- clean_data(raw.dat3)


# Examining learning trend while giving feedback

get_trend <- function(dat){
  return(
    ddply(dat,.(id), summarize,
          attempt = 1:length(start_time),
          pic_id = pic_id[order(start_time)],
          start_time=start_time[order(start_time)],
          response=as.numeric(response[order(start_time)]),
          p_value=p_value[order(start_time)])
  )
}


dtrend1 <- subset(get_trend(dat1), attempt<10)
dtrend2 <- subset(get_trend(dat2), attempt<10)
dtrend3 <- subset(get_trend(dat3), attempt<10)


dtrend <- rbind(data.frame(dtrend1, Experiment = "1"),
                data.frame(dtrend2, Experiment = "2"),
                data.frame(dtrend3, Experiment = "3"))

qplot(data=dtrend, attempt, response, geom="point", colour=Experiment) +
  stat_smooth(method="loess")

unique(dtrend$id[dtrend$attempt>45])

subset(dtrend, id==278)

dtrend1 <- get_trend(dat1)
qplot(data=dtrend1, attempt, response, geom="point", group=id) +
  stat_smooth(method="loess")

get_smooth_loess <- function(dtrend){
  smooth_fitted =NULL
  for (i in unique(dtrend1$id)){
    #i=221
    dat <- subset(dtrend1, id==i)
    max_attempt <- max(dat$attempt)
    attempt  <- seq(1,max_attempt, by=.1) 
    fit <- loess(response~attempt, data=dat)
    fitted <- predict(fit, attempt)
    fdat <- data.frame(id=i, attempt, fitted)
    smooth_fitted <- rbind(smooth_fitted, fdat)
  }
  return(smooth_fitted)
  }
  
dt1 <- get_smooth_loess(dtrend1)

  
qplot(data=smooth_fitted, attempt, fitted, group=id, geom="line")

qplot(data=subset(dt1, id==4), attempt, fitted, group=id, geom="line")




dt1 <- ddply(dtrend1,.(id), summarize,  
      lineups = seq(0.1,max(attempt), by=.1),
      fitted = predict(loess(response~attempt), 
                       attempt=seq(0.1,max(attempt), by=.1))
     )


dt2 <- ddply(dtrend1,.(id), summarize,      
             attempt=seq(0.1,max(attempt), by=.1)  )

dt <- merge(dt1,dt2, by="id")

qplot(data=dt, attempt, fitted, geom="line", group=id)

  
ddply(dtrend1,.(id), summarize,
      attempt=attempt,
      fitted = predict(loess(response~attempt)))
  
  

tr <- ddply(subset(dtrend, attempt < 12),.(attempt), summarize,
            percent_correct=mean(response)*100,
            mean_pval=mean(p_value)*100)

tr <- ddply(dtrend,.(attempt), summarize,
            percent_correct=mean(response)*100,
            mean_pval=mean(p_value)*100)

qplot(attempt,percent_correct, data=tr) +
  geom_line(aes(attempt,mean_pval))


# fiting model with attempt and p-value as covariate
# library(lme4)


fit_model <- function(dat){
  model <- as.formula(response ~ attempt + p_value + (attempt -1|id))
  fit <- lmer(model,family="binomial",data=dat)
  return(summary(fit))
}

fit_model(subset(dtrend, Experiment =="1"))
fit_model(subset(dtrend, Experiment =="2"))
fit_model(subset(dtrend, Experiment =="3"))


pred.mixed <- function(X, subject=0,fit) {
  eta <- fixef(fit)[1] + X * fixef(fit)[2] + 0.05 * fixef(fit)[3] + subject*X
  g.eta <- exp(eta)/(1+exp(eta))
  return(g.eta)
}

get_predict_mixed <- function(dat, newdat, intercept=F){
  fit.mixed <- fit_model(dat)
  X <- newdat$attempt
  if(intercept){
    subject <- ranef(fit.mixed)[[1]][,1]
    d <- data.frame(expand.grid(attempt=X, subject=subject))
    pred <- pred.mixed(X=d$attempt, subject=d$subject, fit=fit.mixed)
    res <- data.frame(attempt=d$attempt, subject=d$subject, pred)
  } else res <- data.frame(attempt=X,pred=pred.mixed(X, fit=fit.mixed))
  return(res)
}
attempt <- 1:9
get_predict_mixed(dtrend1, newdat=data.frame(attempt))
#get_predict_mixed(dat1, newdat=data.frame(effect), intercept=T)

pi_attempt <- rbind(data.frame(Experiment="Experiment 1",get_predict_mixed(dtrend1, newdat=data.frame(attempt))),
                   data.frame(Experiment="Experiment 2",get_predict_mixed(dtrend2, newdat=data.frame(attempt))),
                   data.frame(Experiment="Experiment 3",get_predict_mixed(dtrend3, newdat=data.frame(attempt))))

qplot(attempt, pred, data=pi_attempt, geom="line", colour=Experiment) +
  ylab("Probability of correct response")

pi_subject <- rbind(data.frame(Experiment="Experiment 1",get_predict_mixed(dtrend1, newdat=data.frame(attempt), intercept=T)),
                    data.frame(Experiment="Experiment 2",get_predict_mixed(dtrend2, newdat=data.frame(attempt), intercept=T)),
                    data.frame(Experiment="Experiment 3",get_predict_mixed(dtrend3, newdat=data.frame(attempt), intercept=T)))

ggplot() + ylab("Probability of correct response") + xlab("Attempt") + 
  facet_grid(.~Experiment) +
  geom_line(aes(attempt, pred, group=subject), data=pi_subject, alpha=I(.1)) +
  geom_line(aes(attempt,pred), data=pi_attempt, color="hotpink", size=1.5)

ggsave("../images/learning_trend.pdf", width=10, height = 4)




