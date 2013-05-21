# The complete R code for Sociology Paper

library(ggplot2)
library(plyr)
library(reshape)
library(lme4)
library(xtable)
library(grid)
#library(lubridate)


# getting the data and some common functions

raw.dat1 <- read.csv("../data/raw_data_turk1.csv")
raw.dat2 <- read.csv("../data/raw_data_turk2.csv")
raw.dat3 <- read.csv("../data/raw_data_turk3.csv")
raw.dat4 <- read.csv("../data/raw_data_turk4.csv")
raw.dat5 <- read.csv("../data/raw_data_turk5.csv")
raw.dat6 <- read.csv("../data/raw_data_turk6.csv")
raw.dat7 <- read.csv("../data/raw_data_turk7.csv")

source("calculate_ump_power.R") # functions to compute power

# cleaning the data based on criteria 6 and removing the duplicated data
# (id,pic_id) should be unique since no person gets a lineup more than once.
clean_data <- function(raw_dat){
  easy_dat <- subset(raw_dat, p_value < 0.0002)
  if (raw_dat$experiment[1]=='turk3') easy_dat <- subset(raw_dat, difficulty==0)
  d <- ddply(easy_dat,.(id),summarise,
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

remove_duplicated <- function(dat){
  indx_dup <- with(dat, !duplicated(data.frame(id,pic_id))) #duplication index
  # now returning cleaned data removing duplication
  return(subset(dat,indx_dup))
}

dat1 <- clean_data(raw.dat1)
dat2 <- clean_data(raw.dat2)
dat3 <- clean_data(raw.dat3)
dat4 <- remove_duplicated(raw.dat4)
dat4$p_value=0
dat5 <- remove_duplicated(raw.dat5)
dat6 <- remove_duplicated(raw.dat6)
dat7 <- remove_duplicated(raw.dat7)


# Summary statistics of the data

with(dat3, table(age))



# Examining performance based on plot location effect in lineup

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

# difference with minimum p-value and location of actual plot

pval1 <- read.csv("../data/pvalue_turk1.csv")
pval2 <- read.csv("../data/pvalue_turk2.csv")

get_pval_success <- function (dat){
  dat <- subset(dat, abs(beta)>0)
  res <- ddply(dat, .(pic_name), summarize,      
               prop_correct=mean(response),
               p_value=p_value[1],
               actual_plot = plot_location[1])
  res$experiment=paste("Experiment", substr(dat$experiment,5,5)[1])
  return(res)
}
pval.success <- rbind(get_pval_success(dat1),get_pval_success(dat2))
pval.dat <- merge(pval.success,rbind(pval1,pval2), by="pic_name")
pval.dat.m <- melt(pval.dat, id=c("pic_name","prop_correct",
                                  "p_value","actual_plot","experiment"))
pval.diff <- ddply(pval.dat.m, .(pic_name), summarise,
                   actual_plot = actual_plot[1],
                   experiment=experiment[1],
                   prop_correct=prop_correct[1],
                   pval_diff = p_value[1]- min(value[-actual_plot[1]]))
pval.diff$plot_region <- "Outer"
pval.diff$plot_region[pval.diff$actual_plot %in% c(7:9,12:14)] <- "Centre"

ggplot(pval.diff) +
  geom_point(aes(pval_diff, prop_correct, colour=plot_region)) +
  facet_grid(.~experiment) +
  xlab("Difference between p-value of actual data and minimum p-value of null data") +
  ylab("Proportion correct") +xlim(-.15,.15)

ggsave(file="../images/pval_difference_plot_region.pdf", height=4, width=8)

# getting those three unusual lineups for experiment 1
subset(pval.diff, prop_correct <.25 & plot_region == "Centre" & pval_diff <0)

# Examining learning trend while giving feedback

get_trend <- function(dat){
  return(
    ddply(dat,.(id), summarize,
          attempt = rank(start_time),
          pic_id = pic_id,
          start_time=start_time,
          response=as.numeric(response),
          pic_name=pic_name,
          p_value=p_value)
  )
}

dtrend1 <- subset(get_trend(dat1), attempt <= 10)
dtrend2 <- subset(get_trend(dat2), attempt <= 10)
dtrend3 <- subset(get_trend(dat3), attempt <= 10)
dtrend4 <- subset(get_trend(dat4), attempt <= 10)
dtrend5 <- subset(get_trend(dat5), attempt <= 10)
dtrend6 <- subset(get_trend(dat6), attempt <= 10)
dtrend7 <- subset(get_trend(dat7), attempt <= 10)

dtrend <- rbind(data.frame(dtrend1, Experiment = "1"),
                data.frame(dtrend2, Experiment = "2"),
                data.frame(dtrend3, Experiment = "3"),
                data.frame(dtrend4, Experiment = "4"),
                data.frame(dtrend5, Experiment = "5"),
                data.frame(dtrend6, Experiment = "6"),
                data.frame(dtrend7, Experiment = "7"))

qplot(data=dtrend, attempt, response, geom="point", colour=Experiment) +
  stat_smooth(method="loess")

#unique(dtrend$id[dtrend$attempt>45])

#subset(dtrend, id==278)

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
  # model <- as.formula(response ~ attempt + (attempt|id) + (1|pic_id))
  model <- as.formula(response ~ (1|pic_name) + (1|id))
  fit <- lmer(model,family="binomial",data=dat)
  return(summary(fit))
}

f1 <- fit_model(dtrend1)
f2 <- fit_model(dtrend2)
f3 <- fit_model(dtrend3)
f4 <- fit_model(dtrend4)
f5 <- fit_model(dtrend5)
f6 <- fit_model(dtrend6)
f7 <- fit_model(dtrend7)

trend.dat <- NULL
for (i in 5:7){
  d <- get(paste("dtrend",i,sep=""))
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(response ~ (1|pic_name) + (1|id))
  fit <- lmer(model,family="binomial",data=dd)
  dd$resid <- abs(dd$response - fitted(fit))
  dd$experiment = paste("experiment",i)
  trend.dat <- rbind(trend.dat ,dd)
}

ggplot(trend.dat, aes(attempt,resid, colour=experiment))+
  geom_smooth(method = "loess", size = 1.5) +
  ylab("Residuals")

ddt <- ddply(trend.dat,.(attempt, experiment), summarise,
             mean_resid = mean(resid))

qplot(attempt,mean_resid, data= ddt, colour=experiment) +
  geom_smooth(method="lm") + ylab("Mean absolute residual") +
  scale_x_continuous(breaks = seq(2,10,by=2))

ggsave("../images/learning_trend.pdf", width=6, height = 4)

dd <- subset(dtrend4, id %in% id[attempt > 9])
fit <- lmer(model,family="binomial",data=dd)
dd$resid <- abs(dd$response - fitted(fit))
qplot(attempt, resid, colour=id, data=subset(dd, id %in% sample(1:125,12)), geom="line", facets=~id)
qplot(attempt, resid, colour=id, data=dd, geom="line")
qplot(attempt, resid, colour=id, data=dd) + geom_smooth()


predict(fit)


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


# some analysis of turk9 data and location effect of the plot

# keep only three responses data. Any responses after the first three are ignored.
# Also data with less than three responses are discarded considering incomplete.

raw.dat9 <- read.csv("../data/raw_data_turk9.csv", header=T)
indx_dup <- with(raw.dat9, !duplicated(data.frame(id,pic_id))) #duplication index
uniq.dat9 <- raw.dat9[indx_dup,] # removing duplicated data if any
uniq.dat9 <- ddply(uniq.dat9,.(id), transform,
                  response_order = rank(start_time), 
                  tot_response=length(response) )
dat9 <- subset(uniq.dat9, response_order < 4 & tot_response > 2) # filering applied
dat9$plot_type <- factor(dat9$difficulty, labels=c("Interaction", "Genotype", "Filter"))

qplot(response, geom="bar", data=subset(dat9, plot_type=="Genotype"))
qplot(response, geom="bar", data=subset(dat9, plot_type=="Filter"))
qplot(response, geom="bar", data=dat9) + facet_wrap(~difficulty)
qplot(response, geom="bar", data=dat9) + facet_wrap(~plot_type)

ggsave("../images/performance_plot_type.pdf")

# write.csv(raw.dat9$choice_reason, file= "../data/choice_reason.csv")

with(dat9, table(difficulty))
with(dat9, table(param_value[plot_type=="Genotype"]))
with(dat9, table(param_value[plot_type=="Interaction"]))

dd <- ddply(dat9, .(plot_type, plot_location), summarise,
            percent_correct = mean(response)*100)

ddd <- ddply(subset(dd, plot_type !="Filter"), .(plot_type), summarise,
             plot_location= c(plot_location, (1:20)[-plot_location]),
             percent_correct = c(percent_correct, rep(0, 20-length(percent_correct)))) 

ggplot(subset(ddd, plot_type=="Genotype"), aes(1, percent_correct))+
  geom_bar(stat="identity", fill="darkgreen")+
  facet_wrap(~plot_location, ncol=5) + 
  ylab("Proportion of correct evaluation") +
  xlab("True plot location in the lineup") +
  theme(axis.text.x = element_blank())

ggsave("../images/location_effect_genotype.pdf")


ggplot(subset(ddd, plot_type=="Interaction"), aes(1, percent_correct))+
  geom_bar(stat="identity", fill="darkgreen")+
  facet_wrap(~plot_location, ncol=5) + 
  ylab("Proportion of correct evaluation") +
  xlab("True plot location in the lineup") +
  theme(axis.text.x = element_blank())

ggsave("../images/location_effect_Interaction.pdf")

ggplot(subset(dd, plot_type!="Filter")) +
  geom_bar(aes(factor(plot_location),percent_correct), stat="identity") +
  facet_wrap(~plot_type)

ggplot(subset(dd, plot_type=="Genotype")) +
  geom_bar(aes(factor(plot_location),percent_correct), stat="identity") +
  facet_wrap(~plot_type)

ggplot(subset(dd, plot_type=="Interaction")) +
  geom_bar(aes(factor(plot_location),percent_correct), stat="identity") +
  facet_wrap(~plot_type)


# proportion of correct evaluation by location and null

nulls <- strsplit(as.character(dat9$param_value),split="_")
dat9$nulls <- paste("null_",sapply(nulls, function(x) return(x[2])), sep="")

p.dat <- ddply(subset(dat9, plot_type != "Filter"), 
               .(plot_type, plot_location, nulls),summarise,
               prop_correct = mean(response), n = length(response))
m.dat <- ddply(subset(dat9, plot_type != "Filter"), 
               .(plot_type, plot_location),summarise,
               prop_correct = mean(response),
               nulls="Average")
p.dat$plot_loc <- factor(p.dat$plot_location)
m.dat$plot_loc <- factor(m.dat$plot_location)

p_prop <- ggplot(p.dat, aes(plot_loc, prop_correct,  group=nulls))+
  geom_point(aes(color=nulls, size=n)) + 
  geom_line(aes(color=nulls)) + 
  facet_wrap(~plot_type, scales = "free_x") + 
  geom_line(data=m.dat, linetype=5)+
  xlab("Actual plot location in the lineup")+
  ylab("Proportion correct") + theme(plot.margin=unit(c(5,1,1,1), "cm"))

ggsave(p_prop,file= "../images/proportion_nulls.pdf", width=9, height=4)

l.dat <- data.frame(plot_location=1:20, plot_int=0, plot_gen=0)
plot_int <- unique(subset(dat9, plot_type=="Interaction")$plot_location)
plot_gen <- unique(subset(dat9, plot_type=="Genotype")$plot_location)
l.dat$plot_int[plot_int] <- 1
l.dat$plot_gen[plot_gen] <- 1
p_int <- qplot(1,0, data=l.dat, geom="blank")+ facet_wrap(~plot_location)+ 
  geom_bar(aes(1,plot_int), stat="identity", alpha=.6) +
  theme(axis.text = element_blank(), axis.title = element_blank(), plot.margin=unit(c(0,0,-1,-1), "cm")) 
p_gen <- qplot(1,0, data=l.dat, geom="blank")+ facet_wrap(~plot_location)+ 
  geom_bar(aes(1,plot_gen), stat="identity", alpha=.6) +
  theme(axis.text = element_blank(), axis.title = element_blank(), plot.margin=unit(c(0,0,-1,-1), "cm")) 

pushViewport(viewport(layout = grid.layout(2, 2)))  
print(p_int, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))     
print(p_gen, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))  
print(p_prop,vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))

vp1 <- viewport(x=0.46, y=0.7, height=unit(4.5, "cm"), width=unit(4.5, "cm"), just=c( "right", "bottom"))
vp2 <- viewport(x=0.86, y=0.7, height=unit(4.5, "cm"), width=unit(4.5, "cm"), just=c( "right", "bottom"))

pdf( "../images/proportion_nulls_guide.pdf", width=11, height=5.9)
p_prop
print(p_int,vp = vp1)
print(p_gen,vp = vp2)
dev.off()

# Fitting MANOVA model
df <- ddply(subset(dat9, plot_type != "Filter"), 
            .(plot_type, plot_location, nulls, pic_name),summarise,
            prop_correct = mean(response))
df <- ddply(subset(dat9, plot_type != "Filter"), 
            .(plot_type, plot_location, nulls, pic_name),transform,
            replicates = 1:length(response))

# dd <- subset(dat9, plot_type=="Interaction", select=c(response,plot_location, nulls, pic_name))

model.dat <- cast(df,plot_type+plot_location+replicates~nulls, value="response", fun=mean)

model <- as.formula(cbind(null_1,null_2,null_3,null_4,null_5)~factor(plot_location))
int.dat <- subset(model.dat, plot_type=="Interaction")
fit1 <- manova(model, data=int.dat )
summary(fit1)

fit2 <- manova(model, data=subset(model.dat, plot_type=="Genotype") )
summary(fit2)


int.dat$locs <- "out"
int.dat$locs[int.dat$plot_location %in% c(9,12)] <- "in"
summary(manova(cbind(null_1,null_2,null_3,null_4,null_5)~factor(locs), data=int.dat))

# push view port
# test for equivalency



# Getting visual p-values for turk9 experiment with gene expression data

get_pval <- function(m,K,x){
  p <- pbinom(size=K,prob=1/m,q=x) # gives P(X<=x)
  px <- dbinom(size=K,prob=1/m,x=x)
  pval <- 1-p + px
  return(pval)
}

get_response_wt <- function(response_no, plot_location){
  wt <- NULL
  for (i in 1:length(plot_location)){
    response_list <- as.numeric(unlist(strsplit(as.character(response_no[i]),split=",")))
    wt <- c(wt,(plot_location[i] %in% response_list)/length(response_list))
  }
  return(round(sum(wt),0))
}

pval.dat <- ddply(dat9, .(pic_id), summarise,
                  plot_type=plot_type[1],
                  pic_name=pic_name[1],
                  K = length(response),
                  x=get_response_wt(response_no, plot_location),
                  visual_pval = get_pval(m=20, K = length(response),
                                         x=get_response_wt(response_no, plot_location)))
# write.csv(pval.dat, file="../data/visual_pval_turk9.csv", row.names=F)

qplot(1,visual_pval, geom="boxplot", data=subset(pval.dat, visual_pval<.05))+
  facet_wrap(~plot_type)

ddt <- subset(dat9, plot_type=="Interaction")
get_response_wt(ddt$response_no, ddt$plot_location)

pval.dat <- ddply(dat9, .(plot_type), summarise,
                  plot_type=plot_type[1],
                  pic_name=pic_name[1],
                  K = length(response),
                  x=get_response_wt(response_no, plot_location),
                  visual_pval = get_pval(m=20, K = length(response),
                                         x=get_response_wt(response_no, plot_location)))

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


