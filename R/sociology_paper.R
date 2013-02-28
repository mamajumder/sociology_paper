# The complete R code for Sociology Paper

library(ggplot2)
library(plyr)
library(reshape)
library(lme4)
library(xtable)
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


# Summary statistics of the data

with(dat3, table(age))



# Examining performance based on plot location in lineup

dat <- dat2

loc.performance <- ddply(dat, .(plot_location),summarize,
                         per_correct=mean(response)*100,
                         n=length(response),
                         p_value = mean(p_value))

model <- as.formula(response ~ factor(plot_location) + (plot_location|id) + (1|pic_id))
model <- as.formula(response ~ p_value + factor(plot_location) + (plot_location|id) )
fit <- lmer(model,family="binomial",data=dat)
summary(fit)

X <- diag(rep(1,(length(unique(dat$plot_location))-1)))
eta0 <- fixef(fit)[1]+ 0.01* fixef(fit)[2]
eta <- c(eta0, eta0 + X %*% fixef(fit)[-(1:2)]) 
loc.performance$prob_correct <- round(exp(eta)/(1+exp(eta)), 5)

no_loc <- (1:20) [!(1:20 %in% loc.performance$plot_location)]
loc.dat <- rbind(loc.performance, 
                 cbind(plot_location=no_loc,per_correct=0,
                       n=0,p_value=0,prob_correct=0))

ggplot(loc.dat, aes(1,prob_correct))+
  geom_bar(stat="identity", fill="darkgreen")+
  facet_wrap(~plot_location, ncol=5) + 
  ylab("Probability of correct evaluation") +
  xlab("True plot location in the lineup") +
  theme(axis.text.x = element_blank())

ggsave("../images/location_effect2.pdf")


qplot(p_value, per_correct, data = loc.performance, geom="point")

qplot(plot_location,per_correct, geom="point", data = loc.performance)+
  facet_wrap(~plot_location, ncol=5)

qplot(x=1,y=per_correct, geom="bar", binwidth=1, data = loc.performance)+
  facet_wrap(~plot_location, ncol=5)

qplot(1,1, geom="bar", binwidth=1,data = loc.performance, fill=per_correct)+
  facet_wrap(~plot_location, ncol=5)+ scale_x_continuous()+ scale_y_continuous()
  + opts(axis.ticks=theme_blank())




# Examining learning trend while giving feedback

get_trend <- function(dat){
  return(
    ddply(dat,.(id), summarize,
          attempt = rank(start_time),
          pic_id = pic_id,
          start_time=start_time,
          response=as.numeric(response),
          p_value=p_value)
  )
}

dtrend1 <- subset(get_trend(dat1), attempt <= 10)
dtrend2 <- subset(get_trend(dat2), attempt <= 10)
dtrend3 <- subset(get_trend(dat3), attempt <= 10)

dtrend <- rbind(data.frame(dtrend1, Experiment = "1"),
                data.frame(dtrend2, Experiment = "2"),
                data.frame(dtrend3, Experiment = "3"))

qplot(data=dtrend, attempt, response, geom="point", colour=Experiment) +
  stat_smooth(method="loess")

unique(dtrend$id[dtrend$attempt>45])

subset(dtrend, id==278)

# dtrend1 <- get_trend(dat1)
qplot(data=dtrend1, attempt, response, geom="point", colour=id) +
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
  #model <- as.formula(response ~ attempt + p_value + (attempt -1|id))
  model <- as.formula(response ~ attempt + (attempt|id) + (1|pic_id))
  fit <- lmer(model,family="binomial",data=dat)
  return(summary(fit))
}

f1 <- fit_model(dtrend1)
f2 <- fit_model(dtrend2)
f3 <- fit_model(dtrend3)

model.out <- rbind(round(f1@coefs,2), round(f2@coefs,2), round(f3@coefs,2))
parameters <- rownames(model.out)
print(xtable(data.frame(parameters,model.out)), include.rownames=FALSE)

print(xtable(pval_sm), include.rownames=FALSE)

pred.mixed <- function(X, subject=0,fit) {
  eta <- fixef(fit)[1] + X * fixef(fit)[2] + subject*X
  g.eta <- exp(eta)/(1+exp(eta))
  return(g.eta)
}

get_predict_mixed <- function(dat, newdat, intercept=F){
  fit.mixed <- fit_model(dat)
  X <- newdat$attempt
  if(intercept){
    #subject <- ranef(fit.mixed)[[1]][,1]
    subject <- ranef(fit.mixed)[[1]]
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

dt <- dtrend1
fit0 <- lmer(response ~ attempt + p_value + (attempt -1|id), family="binomial", data=dt)

fit1 <- lmer(response ~ attempt + (attempt|id) , family="binomial", data=dt)
fit2 <- lmer(response ~ attempt + (attempt|id) + (1|pic_id), family="binomial", data=dt)
anova(fit2, fit1)
fit3 <- lmer(response ~ factor(attempt) + (attempt|id) + (1|pic_id), family="binomial", data=dt)
anova(fit3,fit2)



# Some exploratory study with the data

# investigating picture with beta=1, sigma=12, replica=2, sample_size=300
# It is interesting since most people picked a plot with high p_value
# Most people pick 14 since the difference in spread is clearly visible

pic_dat <- subset(dat1, beta==1 & sigma== 12 & replica==2 & sample_size == 300)
pic_dat[1,c("pic_id","pic_name","plot_location","p_value")]
xtabs(data=pic_dat, ~choice_reason+response_no)


# ---------------------  description of coded variables -----------

## gender 1 = male 
#         2 = female
## age 1 = below 18 
#      2 = 18-25 
#      3 = 26-30 
#      4 = 31-35 
#      5 = 36-40
#      6 = 41-45
#      7 = 46-50
#      8 = 51-55
#      9 = 56-60
#     10 = above 60
## academic_study 1 = High school or less
#                 2 = Some under graduate courses 
#                 3 = Under graduate degree
#                 4 = Some graduate courses
#                 5 = Graduate degree
## conf_level 1 = most, 5 = least
## choice_reason 1 = Big vertical difference 
#                2 = Medians are different
#                3 = Outliers
#                4 = others
## unit(time_taken) = second 


