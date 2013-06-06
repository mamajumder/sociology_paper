# The complete R code for Sociology Paper

library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(xtable)
library(grid)
library(lubridate)

# ===================================================
# Loading the data and some common functions
# ---------------------------------------------------

raw.dat1 <- read.csv("../data/raw_data_turk1.csv")
raw.dat2 <- read.csv("../data/raw_data_turk2.csv")
raw.dat3 <- read.csv("../data/raw_data_turk3.csv")
raw.dat4 <- read.csv("../data/raw_data_turk4.csv")
raw.dat5 <- read.csv("../data/raw_data_turk5.csv")
raw.dat6 <- read.csv("../data/raw_data_turk6.csv")
raw.dat7 <- read.csv("../data/raw_data_turk7.csv")
raw.dat8 <- read.csv("../data/raw_data_turk8.csv")
raw.dat9 <- read.csv("../data/raw_data_turk9.csv")
raw.dat10 <- read.csv("../data/raw_data_turk10.csv")

ip.details <- read.csv("../data/ip_details.csv")
turk.summary <- read.csv("../data/turk_summary.csv")

source("calculate_ump_power.R") # functions to compute power

# ========================================================================
# cleaning the data based on criteria 6 and removing the duplicated data
# (id,pic_id) should be unique since no person gets a lineup more than once.
# --------------------------------------------------------------------------
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
dat8 <- remove_duplicated(raw.dat8)
dat10 <- remove_duplicated(raw.dat10)

# Cleaning up experiment 9 data
# keep only three responses data. Any responses after the first three are ignored.
# Also data with less than three responses are discarded considering incomplete.

indx_unique <- with(raw.dat9, !duplicated(data.frame(id,pic_id))) #unique data index
uniq.dat9 <- raw.dat9[indx_unique,] # removing duplicated data from raw.dat if any
uniq.dat9 <- ddply(uniq.dat9,.(id), transform,
                   response_order = rank(start_time), 
                   tot_response=length(response) )
dat9 <- subset(uniq.dat9, response_order < 4 & tot_response > 2) # filering applied
dat9$plot_type <- factor(dat9$difficulty, labels=c("Interaction", "Genotype", "Filter"))

# dat9 response_all is true if at least one of the multiple selection is correct
dat9$response_all <- apply(subset(dat9, select=c(plot_location,response_no)), 1,
                  function(x){
                    plot <- as.numeric(x[1])
                    res_no <- as.numeric(unlist(strsplit(x[2],split=",")))
                    return(plot %in% res_no)
                  })


# ==============================================================
# Summary statistics of demographic data
# --------------------------------------------------------------

# Function to obtain real ip from proxy and local part of ip
# First ip of two ip's is the real ip, 2nd one is local
# in case where no proxy is used, the ip is real ip
get_real_ip <- function(ip) {
  ip_vector <- strsplit(as.character(ip),split=',')
  return(ip_vector[[1]][1])
}

# merging demographic data from all the experiment together
demographics <- NULL
for (i in 1:10){
  di <- subset(get(paste("dat",i, sep="")), age > 1,
                 select = c(id, response, start_time, time_taken, 
                            ip_address, gender, academic_study, age))
  di$age[di$age==0] <- NA # for experiment 1, age=0 means NA
  di$academic_study[di$academic_study==0] <- NA
  di$experiment = paste("experiment_",i, sep="")
  di$id = paste("exp",i,"_",di$id, sep="")
  degree <- factor(c("High school or less", "Some under graduate courses",
                  "Under graduate degree","Some graduate courses",
                  "Graduate degree")[di$academic_study])
  di$degree <- factor(degree, levels=levels(degree)[order(c(5,1,4,2,3))], ordered=T)
  di$age_level <- factor(c("below 18","18-25","26-30","31-35","36-40",
                  "41-45","46-50","51-55","56-60","above 60")[di$age])
  di$gender_level <- factor(c("Male","Female")[di$gender])
  di$ip_address <- sapply(di$ip_address, get_real_ip)
  demographics <- rbind(demographics, di[complete.cases(di),])
}

# merging geogrphical location information
# obtained from www.ipaddressapi.com using real ip address
# based on ip, it gets longitude, latitude, country, city etc.
demographics <- merge(demographics,ip.details,all.x=TRUE, by="ip_address")
demographics$country <- as.character(demographics$country_name)
demographics$country[(demographics$country_code != "IN") & (demographics$country_code != "US")] <- "Rest of the world"
demographics$country[demographics$country=="Namibia"] <- "Rest of the world"




# getting unique participants for plotting purpose only
# Since each participant has multiple responses in demographics data
turker <- demographics[!duplicated(demographics$id),]
turker$study <- turker$academic_study
turker$study[turker$age_level=="36-40"] <- c("High school or less (1)","Some under grad course (2)",
                                             "Under graduate degree (3)", "Some graduate courses (4)",
                                             "Graduate degree (5)")[turker$study[turker$age_level=="36-40"]]
turker$exp <- factor(turker$experiment, levels=names(table(turker$experiment))[order(c(1,10,2:9))])
levels(turker$age_level)[7:9] <- 7
levels(turker$age_level)[1:7] <- c("18-25","26-30","31-35","36-40",
                                "41-45", "46-50", "above 50")

# qplot(factor(academic_study), facets=age_level~gender_level, data=turker) +
#   coord_flip() + xlab("Academic Study") + labs(title="Gender") +
#   scale_x_discrete( aes(breaks=academic_study,labels=as.charater(study)))


# Plotting age, gender and country together
p1 <- qplot(degree, facets=age_level~gender_level, data=turker) +
  coord_flip() + xlab("Academic Study") + labs(title="Gender")

ggsave("../images/age_study_gender_bar.pdf", width=6, height=7)

p2 <- qplot(degree, facets=age_level~country, data=turker[complete.cases(turker),]) +
  coord_flip() + xlab("Academic Study") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  labs(title="Geographical location")

ggsave("../images/age_study_country_bar.pdf", width=7.5, height=7)

# Saving gender and countriwise plots after merging them together
pdf( "../images/demographic_info.pdf", width=10, height=8)
pushViewport(viewport(layout = grid.layout(1, 2)))  
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))     
print(p2 ,vp = viewport(layout.pos.row = 1, layout.pos.col = 2)) 
dev.off()

# Potting Education and Gender within country by age
qplot(degree, data=turker[complete.cases(turker),]) +
  facet_grid(age_level~country+gender_level) + coord_flip() + xlab("Academic Study") 

ggsave("../images/age_gender_within_country_bar.pdf", width=10, height=8)


# Time of the work
turker$hours <- as.factor(hour(as.POSIXlt(turker$start_time)))
qplot(hours, data=subset(turker, complete.cases(turker))) +
  facet_grid(exp~country,scales="free_y") +
  xlab("Hour of the day") + ylab("Number of subjects")
ggsave("../images/participation_time.pdf", width=10, height=12)


# getting demographic summary table
get_summary <- function(dat, var){
  res <- ddply(dat,c(var), summarize,
               subsjects = length(unique(id)),
               avg_time = round(mean(time_taken),2),
               response = length(response),
               prop_correct = mean(response)
  )
  return(data.frame(var=var,lbls=res[,1], res[,-1]))
}



sg <- get_summary(demographics, "gender")
se <- get_summary(demographics, "degree")
sa <- get_summary(demographics, "age_level")
sl <- get_summary(demographics, "country")

sdat <- rbind(sg,se,sa,sl)
xtable(sdat[,-1])

qplot(lbls, avg_time, geom="bar", stat="identity", data=sdat) + coord_flip()
  facet_wrap(var~.)

# Turk experiment summary including payments and duration
turk.summary$duration_hour <- round(turk.summary$duration/3600,2)
turk.summary$avg_minute <- round(turk.summary$time_per_task/60,2)
turk.summary$duration_rate <- with(turk.summary, round(duration_hour*100/submitted,2))
turk.summary$percent_rejected <- with(turk.summary, round(rejected*100/submitted,2))
xtable(subset(turk.summary,
              select=c(plot,submitted, rejected,avg_minute,
                       duration_hour,duration_rate,pay_per_task,pay_rate)))

  

ggplot(turk.summary, aes(x = factor(plot, levels=plot[order(duration_rate)]), y=duration_rate))+
  geom_bar(stat="identity") + ylab("Duration in hour per 100 task") + 
  xlab("Experiment") + coord_flip() 
ggsave("../images/task_duration.pdf", width=6, height=4)


ggplot(turk.summary, aes(x = factor(plot, levels=plot[order(percent_rejected)]), y=percent_rejected))+
  geom_bar(stat="identity") + xlab("Experiment") +
  coord_flip() + ylab("Percentage of rejected task")
ggsave("../images/rejected_task.pdf", width=6, height=4)



# Drawing the map of turk participants

library(maps)
map.dat <- as.data.frame(map("world",ylim=c(-45,70), plot=FALSE)[c("x","y")])
map.dat <- map_data("world")

ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) +
  geom_point(aes(longitude,latitude, colour=factor("A")), data=ip.details, alpha=.6) +
  theme_bw()+
  theme(legend.position="none", axis.text = element_blank(), axis.title=element_blank()) 
ggsave("../images/turker_location.pdf", width=8, height=4)


# Drawing map by experiment
p<- ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) +
  geom_point(aes(longitude,latitude, colour=factor("A")), data=turker, alpha=.6) +
  facet_wrap(~exp, ncol=2)+ theme_bw()+
  theme(legend.position="none", axis.text = element_blank(), axis.title=element_blank()) 
ggsave("../images/turker_location_experiment.pdf", width=8, height=10)

# country-wise participants counts
qplot(factor(country, levels=names(table(country))[order(table(country))]), 
      geom="bar", fill="A", data=subset(turker,complete.cases(turker)))+ 
  coord_flip() +ylab("Number of turk worker") + xlab("Country") +
  theme(legend.position="none")
ggsave("../images/turker_country.pdf", width=7, height=2) 


# ==============================================================
# Playing with wordle plot with experiment 9 data
# --------------------------------------------------------------

# write.csv(dat9$choice_reason[dat9$response_all==T], file="../data/dat9_reasoning_correct.csv")
# write.csv(dat9$choice_reason[dat9$response_all==F], file="../data/dat9_reasoning_wrong.csv")

reason.correct <- tolower(as.character(dat9$choice_reason[dat9$response_all==T]))
gregexpr("\\W+", reason.correct)


# =====================================================
# Examining learning trend while giving feedback

# function two obtain sequential attempt information from start_time
get_trend <- function(dat){
  return(
    ddply(dat,.(id), summarize,
          attempt = rank(start_time),
          pic_id = pic_id,
          start_time=start_time,
          response=as.numeric(response),
          pic_name=pic_name,
          p_value=p_value,
          time_taken = time_taken,
          uid = paste(experiment,"_",id, sep="")
    ))
}

# merging all data for trend analysis
dtrend <-NULL
for (i in 1:7){
  di <- get(paste("dat",i, sep=""))
  dti <- subset(get_trend(di), attempt <= 10) 
  dtrend <- rbind(dtrend, data.frame(dti, experiment = i))
}


# Attempt vs mean time taken for each lineup

qplot(attempt, time_taken, data=subset(dtrend, time_taken< 200),colour=factor(experiment)) +
  stat_smooth(method="loess")

# fitting linear random effect model to check 
# if time_taken has any trend over sequential attempts

dpt <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(time_taken ~ (1|pic_name) + (1|id))
  fit <- lmer(model,data=dd)
  dd$resid <- (dd$time_taken - fitted(fit))
  dpt <- rbind(dpt ,dd)
}

dmt <- ddply(subset(dpt, resid<500), .(experiment,attempt), summarise,
             mean_resid = mean(resid))

qplot(attempt,mean_resid, data= dmt) + geom_point(size=2.5) +
  geom_smooth(method="lm", se=F) + ylab("Mean residual time taken") +
  facet_wrap(~experiment, scales="free_y") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 

ggsave("../images/learning_trend_time.pdf", width=10.5, height = 3.5)


# Fitting generalized mixed effect model with time taken
# Link function is inverse link (1/mu) since response is gamma

qplot(time_taken, geom="density", data=subset(dpt, time_taken<300), color=factor(experiment)) +
  scale_colour_hue(name="Experiment")


# Checking if the performance increases with attempts
# Fiting generalized mixed effect model with prportion correct

trend.dat <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(response ~ (1|pic_name) + (1|id))
  fit <- lmer(model,family="binomial",data=dd)
  dd$resid <- (dd$response - fitted(fit))
  trend.dat <- rbind(trend.dat ,dd)
}

ggplot(trend.dat, aes(attempt,resid, colour=factor(experiment)))+
  geom_smooth(method = "loess", size = 1.5) +
  ylab("Residuals")

ddt <- ddply(trend.dat,.(experiment, attempt), summarise,
             mean_resid = mean(resid))

qplot(attempt,mean_resid, data= ddt) + geom_point(size=2.5) +
  geom_smooth(method="lm", se=F) + ylab("Mean residual proportion correct") +
  facet_wrap(~experiment, scales="free_y") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 

ggsave("../images/learning_trend.pdf", width=10.5, height = 3.5)


# Checking if the trend shown in the plot is significant or not
# For all experiments 5,6,7 slope is not statistically significant

fit1 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==5))
fit2 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==6))
fit3 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==7))


dt <- subset(dtrend, experiment==5)
fit0 <- lmer(response ~ attempt + p_value + (attempt -1|id), family="binomial", data=dt)

fit1 <- lmer(response ~ attempt + (attempt|id) , family="binomial", data=dt)
fit2 <- lmer(response ~ attempt + (attempt|id) + (1|pic_id), family="binomial", data=dt)
anova(fit2, fit1)
fit3 <- lmer(response ~ factor(attempt) + (attempt|id) + (1|pic_id), family="binomial", data=dt)
anova(fit3,fit2)

# ==============================================================
# Analysis of location effect of the plot using turk 9 data
# Data has 5 locatios each having 5 null sets 

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

vp1 <- viewport(x=0.46, y=0.7, height=unit(4.5, "cm"), width=unit(4.5, "cm"), just=c( "right", "bottom"))
vp2 <- viewport(x=0.86, y=0.7, height=unit(4.5, "cm"), width=unit(4.5, "cm"), just=c( "right", "bottom"))

pdf( "../images/proportion_nulls_guide.pdf", width=10.8, height=5.9)
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

model.dat <- dcast(df,plot_type+plot_location+replicates~nulls, value.var="response", fun=mean)

model <- as.formula(cbind(null_1,null_2,null_3,null_4,null_5)~factor(plot_location))
int.dat <- subset(model.dat, plot_type=="Interaction")
fit1 <- manova(model, data=int.dat )
summary(fit1)

fit2 <- manova(model, data=subset(model.dat, plot_type=="Genotype") )
summary(fit2)


int.dat$locs <- "out"
int.dat$locs[int.dat$plot_location %in% c(9,12)] <- "in"
summary(manova(cbind(null_1,null_2,null_3,null_4,null_5)~factor(locs), data=int.dat))

# test for equivalency
library(equivalence)

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


# ================== Effect og age and education =================

dat.age <- ddply(dat2, .(age), summarise,
                 percent_correct = mean(response))

qplot(age, percent_correct, data=dat.age)

dat.study <- ddply(dat2, .(academic_study), summarise,
                 percent_correct = mean(response))

qplot(academic_study, percent_correct, data=dat.study)




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

qplot(x=1,y=per_correct, geom="bar", binwidth=1, data = loc.performance, stat="identity")+
  facet_wrap(~plot_location, ncol=5)

qplot(1,1, geom="bar", binwidth=1,data = loc.performance, fill=per_correct, stat="identity")+
  facet_wrap(~plot_location, ncol=5)+ scale_x_continuous()+ scale_y_continuous() + 
  theme(axis.ticks=element_blank())

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


