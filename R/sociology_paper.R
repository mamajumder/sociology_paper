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
# responses on the plot used for defense talk
subset(dat9, pic_name=="plot_turk9_interaction_1_3.svg")

# responses on the plot used for defense nsf grant
subset(dat9, pic_name=="plot_turk9_geno_1_2.svg")
subset(dat9, pic_name=="plot_turk9_interaction_2_1.svg")


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
                 select = c(id, response, start_time, time_taken, pic_name,
                            ip_address, gender, academic_study, age))
  di$age[di$age==0] <- NA # for experiment 1, age=0 means NA
  di$academic_study[di$academic_study==0] <- NA
  di$experiment = paste("experiment_",i, sep="")
  di$id = paste("exp",i,"_",di$id, sep="")
  degree <- factor(c("High school or less", "Under grad courses",
                  "Under grad degree","Graduate courses",
                  "Graduate degree")[di$academic_study])
  di$degree <- factor(degree, levels=levels(degree)[order(c(4,5,1,2,3))])
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
demographics$country[is.na(demographics$country)] <- "Rest of the world"
country <- factor(demographics$country)
demographics$country <- factor(country,levels=levels(country)[c(3,1,2)])

levels(demographics$age_level)[7:9] <- 7
levels(demographics$age_level)[1:7] <- c(levels(demographics$age_level)[1:6], "above 50")




# getting unique participants for plotting purpose only
# Since each participant has multiple responses in demographics data
turker <- demographics[!duplicated(demographics$id),]
turker$study <- turker$academic_study
turker$study[turker$age_level=="36-40"] <- c("High school or less (1)","Some under grad course (2)",
                                             "Under graduate degree (3)", "Some graduate courses (4)",
                                             "Graduate degree (5)")[turker$study[turker$age_level=="36-40"]]
turker$exp <- factor(turker$experiment, levels=names(table(turker$experiment))[order(c(1,10,2:9))])
#levels(turker$age_level)[7:9] <- 7
#levels(turker$age_level)[1:7] <- c(levels(turker$age_level)[1:6], "above 50")

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

# Potting average time taken by Education and Gender within country vs age
pdat <- ddply(demographics, .(country,gender_level, age_level,degree), summarize,
              per_correct = mean(response),
              avg_time = mean(time_taken))
qplot(degree, avg_time, geom="bar", stat="identity", data=pdat[complete.cases(pdat),]) +
  facet_grid(age_level~country+gender_level) + coord_flip() + 
  xlab("Academic Study") + ylab("Average time taken")

ggsave("../images/age_gender_within_country_time.pdf", width=10, height=8)

qplot(degree, per_correct*100, geom="bar", stat="identity", data=pdat[complete.cases(pdat),]) +
  facet_grid(age_level~country+gender_level) + coord_flip() + 
  xlab("Academic Study") + ylab("Percent correct") +
  scale_y_continuous(breaks=c(25,50,75,100)) 

ggsave("../images/age_gender_within_country_correct.pdf", width=10, height=8)

# Demographic factor main effect for percent correct and average time aggregated by lineup
get_effect <- function(dat, var){
  res <- ddply(dat,c(var), summarize,
               log_avg_time = round(log(mean(time_taken)),2),
               detection_rate = mean(response)
  )
  res$variable_name <- var[1]
  colnames(res) <- c("variable_level", colnames(res)[-1])
  return(res)
}
gdat <- get_effect(demographics, c("gender_level", "pic_name"))
edat <- get_effect(demographics, c("degree", "pic_name"))
cdat <- get_effect(demographics, c("country", "pic_name"))
adat <- get_effect(demographics, c("age_level", "pic_name"))

mdat <- melt(rbind(gdat,edat,cdat,adat), id=c("variable_level", "pic_name", "variable_name"))
levels(mdat$variable) <- c("(Log) Time Taken in seconds", "Detection Rate")
mdat$variable_name <- factor(mdat$variable_name)
levels(mdat$variable_name) <- c("Age Categories", "Country", "Education", "Gender")

qplot(variable_level, value, geom="boxplot",data=mdat[complete.cases(mdat),]) +
  facet_grid(variable~variable_name, scales="free", space="free_x") +
  stat_summary(fun.y=mean, geom="point") + xlab("Levels of demographic factors")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggsave("../images/demographic_effect.pdf", width=6.5, height=6)

#----------------------------------------------------------------
# Function to obtain estimates of fitted model for latex xtable
# Getting results of mixed effect model with log(time_taken)
#----------------------------------------------------------------
get_estimates0 <- function(fit){
  Vcov <- vcov(fit, useScale = FALSE)
  betas <- fixef(fit)
  se <- sqrt(diag(Vcov))
  zval <- betas / se
  pval <- round(2 * pnorm(abs(zval), lower.tail = FALSE),4)
  pval[pval==0] <- "$<$0.001"
  fe <- data.frame(Est=round(betas,3),SE= round(se,3), Zval=round(zval,2), pvalue=pval)
#  browser()
  rem <- as.data.frame(VarCorr(fit))
  re <- data.frame(Est=rem$vcov, SE= rem$sdcor, Zval="",pvalue="")
  re$Est <- round(as.numeric(as.character(re$Est)),3)
  re$SE <- round(as.numeric(as.character(re$SE)),3)
  rownames(re) <- paste(rem[,1],rem[,2])
  return(rbind(fe,re[order(rownames(re)),]))
}

# switch to confidence levels ...

get_estimates <- function(fit){
  stars <- function(pval) {
    res <- rep("", length(pval))
    idx <- which(pval < 0.05)
    if (length(idx) > 0) res[idx] <- paste(res[idx],"*", sep="")
    idx <- which(pval < 0.01)
    if (length(idx) > 0) res[idx] <- paste(res[idx],"*", sep="")
    idx <- which(pval < 0.001)
    if (length(idx) > 0) res[idx] <- paste(res[idx],"*", sep="")
    idx <- which((pval < 0.1) & (pval > 0.05))
    if (length(idx) > 0) res[idx] <- "."
    return(res)
  }
  cis <- na.omit(data.frame(confint(fit, method="Wald")))
  betas <- fixef(fit)
  
  Vcov <- vcov(fit, useScale = FALSE)
  se <- sqrt(diag(Vcov))
  zval <- betas / se
  pval <- round(2 * pnorm(abs(zval), lower.tail = FALSE),4)
  pval <- stars(pval)
  fe <- data.frame(Est=round(betas,3),LB= round(cis[,1],3), UB= round(cis[,2],3), pvalue=pval)
#    browser()
  rem <- as.data.frame(VarCorr(fit))
  re <- data.frame(Est=rem$sdcor, LB=NA, UB=NA, pvalue=NA)
  re$Est <- round(as.numeric(as.character(re$Est)),3)
  nvars <- ncol(rem) - 2
  nms <- rem[,1]
  if (nvars > 2) {
    for (i in 2:nvars) nms <- paste(nms, rem[,i], sep="|")
  }
  rownames(re) <- nms
  return(rbind(fe,re[order(rownames(re)),]))
}

# --------------------------------------------------------------
# Model fitting with demographic factors
# testing significance of demographic factor main effects
# --------------------------------------------------------------
ft <- lmer(log(time_taken)~age_level+country+degree+gender_level+(1|pic_name), 
           data=demographics)
summary(ft)

fp <- glmer(response~age_level+country+degree+gender_level+(1|pic_name), 
           family="binomial", data=demographics, control=glmerControl(optimizer="bobyqa"))

summary(fp)
resid <- residuals(fp)
# residual error
sqrt(sum(resid^2)/(length(resid)-length(fixef(fp))))

est.factor <- cbind(get_estimates(ft)[-16,],g1=" ",get_estimates(fp))
rownames(est.factor) <- c("$mu$",substr(rownames(est.factor)[2:7],10,nchar(rownames(est.factor)[2:7])),
                          levels(demographics$country)[-1], levels(demographics$degree)[-1],
                          "Male", "lineup")
dgt <- c(rep(0,15), rep(2,45), rep(0,15))
dgts <- matrix(rep(dgt,2), ncol=10)
print(xtable(est.factor, digits=dgts),  sanitize.text.function = function(x){x})

ftdata <- ft@frame
names(ftdata)[1] <- "time_taken"
ftdata$time_taken <- exp(ftdata$time_taken)

ft <- update(ft, data=ftdata)
fta <- update(ft, .~.-age_level, data=ftdata)
ftc <- update(ft, .~.-country, data=ftdata)
ftd <- update(ft, .~.-degree, data=ftdata)
ftg <- update(ft, .~.-gender_level, data=ftdata)

tag <- anova(ft,fta)
tcn <- anova(ft,ftc)
tde <- anova(ft,ftd)
tge <- anova(ft,ftg)

anova.time <- data.frame(rbind(round(tag[2,5:8]/1,2),
                    round(tcn[2,5:8]/1,2),
                    round(tde[2,5:8]/1,2),
                    round(tge[2,5:8]/1,2)))
anova.time <- rbind(anova.time[1,], anova.time)
rownames(anova.time) <- c("Full", "Age", "Country", "Degree", "Gender")
anova.time$Chisq[1] <- 0
anova.time$deviance <- anova.time$deviance - anova.time$Chisq

fpdata <- fp@frame
fp <- update(fp, data=fpdata)
fpa <- update(fp, .~.-age_level, data=fpdata)
fpc <- update(fp, .~.-country, data=fpdata)
fpd <- update(fp, .~.-degree, data=fpdata)
fpg <- update(fp, .~.-gender_level, data=fpdata)

pag <- anova(fp,fpa)
pcn <- anova(fp,fpc)
pde <- anova(fp,fpd)
pge <- anova(fp,fpg)
anova.prop <- data.frame(rbind(round(pag[2,5:8]/1,2),
                    round(pcn[2,5:8]/1,2),
                    round(pde[2,5:8]/1,2),
                    round(pge[2,5:8]/1,2)))
anova.prop <- rbind(anova.prop[1,], anova.prop)
rownames(anova.prop) <- c("Full", "Age", "Country", "Degree", "Gender")
anova.prop$Chisq[1] <- 0
anova.prop$deviance <- anova.prop$deviance - anova.prop$Chisq


anova.reseult <- cbind(anova.time,g1="", anova.prop)
xtable(anova.reseult)

# Model with demographic factor interaction
# India:Undergrad course is hghly significant
fit <- glmer(response~age_level+country+degree+gender_level+country:degree+(1|pic_name), 
               family="binomial", demographics, control=glmerControl(optimizer="bobyqa"))


fit
# ==============================================================
# Exploring the practical significance of demographics
# ---------------------------------------------------------------

et <- 6
xb <- seq(-et,et, by=.1)
px <- function(xb){exp(xb)/(1+exp(xb))}
pdat <- data.frame(xb,px=px(xb))

s1 <- 2.293
s2 <- s1*2
xx <- c(0,0.182)

ldat <- data.frame(x =c(xx,-et,-et,xx+s1,-et,-et,xx+s2,-et,-et),
                   xend = c(xx,xx,xx+s1,xx+s1,xx+s2,xx+s2 ),
                   y = c(0,0,px(xx),0,0,px(xx+s1),0,0,px(xx+s2)),
                   yend= px(c(xx,xx,xx+s1,xx+s1,xx+s2,xx+s2 )))

ggplot()+geom_line(aes(xb,px), data=pdat)+
  geom_segment(aes(x=x,xend=xend,y=y,yend=yend), data=ldat, linetype="dashed") + 
  theme_bw() + xlab(expression(eta)) + ylab("Prability of correct evaluation") +
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_continuous(expand = c(0.01,0))


get_change <- function(s){
  xb0 <- -0.683 + s
  xb1 <- xb0 + 0.182 
  px(xb1) - px(xb0)
}

get_change(c(0,s1,s2))
get_change(2.293)

xx <- (1:2)*s1
ldat <- data.frame(x=c(xx,0,0), xend=c(xx,xx),y=c(0,0,get_change(xx)), yend=get_change(c(xx,xx)))

ss <- seq(0,6.5, by=.1)
qplot(ss,get_change(ss), geom="line") + 
  geom_segment(aes(x=x,xend=xend,y=y,yend=yend), data=ldat, linetype="dashed") +
  ylab("Change in probability of correct response \n due to graduate degree") +
  xlab("Lineup variability") +
  scale_y_continuous(breaks =c(0,.003237246,.01,.02,.02376092,.03,.04), expand = c(0.02,0),
                     labels =c(0,0.003,.01,.02,.024,.03,.04)) +
  scale_x_continuous(breaks=c(0,2,2.293,4,4.586,6), expand = c(0.02,0),
                     labels=c(0,2, expression(sigma[l],4,2*sigma[l],6)))+
  theme_bw() +theme(panel.grid = element_blank())

ggsave("../images/practical_impact_demographics.pdf", width=5.5, height=5)


difficulty <- seq(-6,6, by=.1)
hs_xb <- -0.61335621 + difficulty
gd_xb <- hs_xb + -0.17436466
ugd_xb <- hs_xb + 0.12496329 

ddat <- data.frame(difficulty=difficulty, prop_hs = px(ugd_xb),prop_gd=px(gd_xb))

# Maximum difference in prop correct is 0.045
qplot(difficulty, prop_gd-prop_hs, data=ddat, geom="line")
with(ddat, max(prop_gd-prop_hs))

mddat <- melt(ddat,id=c("difficulty"))
qplot(difficulty, value, data=mddat, linetype=variable, geom="line")+
  xlab("Lineup difficulty") + ylab("Proportion of data identification") +
  scale_x_continuous(breaks=c(-6,-2*2.293,-2.293,0,2.293,2*2.293,6), expand = c(0.02,0),
                     labels=c("Difficult",expression(-2*sigma[l],-sigma[l],0,sigma[l], 
                                                     2*sigma[l],"Easy"))) +
  scale_linetype_discrete(name="Education",
                      labels=c("U.Grad degree", "Grad. courses")) 

ggsave("../images/practical_impact_graduate.pdf", width=6.5, height=4.5)


diff <- c(-2*2.293, -0.683, 2*2.293)
hs_xb <- -0.683 + diff
uc_xb <- hs_xb -0.083
ud_xb <- hs_xb -0.044
gc_xb <- hs_xb + 0.070
gd_xb <- hs_xb + 0.182
ddat.all <- data.frame(difficulty=diff, high.school = px(hs_xb),u.grad.course = px(uc_xb),
                   u.grad.degree=px(ud_xb), grad.course=px(gc_xb), grad.degree=px(gd_xb))
mddat.all <- melt(ddat.all,id=c("difficulty"))

qplot(variable,value, shape=factor(difficulty), geom=c("point"),data=mddat.all, size=I(3.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Proportion of correct responses") + xlab("Education")+
  scale_shape_discrete(name="Lineup difficulty", 
                       labels=c("Difficult", "Medium","Easy"))

qplot(variable,value, shape=factor(difficulty),data=mddat.all, size=I(3.5)) +
  geom_line(aes(variable,value, group=factor(difficulty)))+
  ylab("Proportion of data identification") + xlab("Education")+
  scale_x_discrete(labels=c("h.school","u.gd.course","u.grad.deg","grad.course","grad.deg"))+
  scale_shape_discrete(name="Lineup difficulty", 
                       labels=c("Difficult", "Medium","Easy")) 


ggsave("../images/practical_impact_degree.pdf", width=6.5, height=4.5)


# Time of the day when Mturk worker works
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

ttt <- get_summary(demographics, c("gender_level", "pic_name"))

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

map_theme <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.margin=unit(c(0,0,-1,-1), unit="line")))

library(maps)
map.dat <- as.data.frame(map("world",ylim=c(-45,70), plot=FALSE)[c("x","y")])
map.dat <- map_data("world")

ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) +
  geom_point(aes(longitude,latitude, colour=factor("A")), data=ip.details, alpha=.6) +
  theme_bw()+ map_theme +
  theme(legend.position="none")

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
  coord_flip() +ylab("Number of participants") + xlab("Country") +
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
# Model fitting with trend
# Examining learning trend while giving feedback
# -----------------------------------------------------

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

# merging all data for learning trend analysis
dtrend <-NULL
for (i in 1:7){
  di <- get(paste("dat",i, sep=""))
  dti <- subset(get_trend(di), attempt <= 10) 
  dtrend <- rbind(dtrend, data.frame(dti, experiment = i))
}


# Attempt vs mean time taken for each lineup
qplot(attempt, log(time_taken), data=dtrend,colour=factor(experiment)) +
  stat_smooth(method="loess")

# Log time taken appears to be normal
qplot(log(time_taken), geom="density",
      data=subset(dtrend, experiment %in% c(5,6,7)), color=factor(experiment)) +
  scale_colour_hue(name="Experiment")

# ---------------------------------------------------------------
# Modeling time taken
# fitting linear random effect model to check 
# if log time_taken has any trend over sequential attempts
# ---------------------------------------------------------------

dpt <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(log(time_taken) ~ (1|pic_name) + (1|id))
  fit <- lmer(model,data=dd)
  dd$resid <- (log(dd$time_taken) - fitted(fit))
  dpt <- rbind(dpt ,dd)
}

qplot(attempt,resid, group=id, data= dpt, geom="line", alpha=I(0.1)) +
  facet_wrap(~experiment, scales="free_y") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 



dmt <- ddply(dpt, .(experiment,attempt), summarise,
             mean_resid = mean(resid))

ggplot() + 
  geom_line(aes(attempt,resid, group=id), data= dpt, alpha=I(0.1)) +
  geom_smooth(aes(attempt,mean_resid), data= dmt, size=1.5, method="lm", se=F) +
  facet_wrap(~experiment, scales="free_y") + ylab("Residual time taken") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 

ggplot() +
  geom_smooth(aes(attempt,mean_resid), data= dmt, size=1.5, method="lm", se=F) +
  geom_smooth(aes(attempt,resid, group=id),method="lm", se=F, data=dpt, alpha=I(.01))+
  facet_wrap(~experiment, scales="free_y")+
  scale_x_continuous(breaks = seq(2,10,by=2)) 

dpt$Experiment <- dpt$experiment
dmt$Experiment <- dmt$experiment
ggplot() + 
  geom_smooth(aes(attempt,resid, group=id),method="lm", se=F, data=subset(dpt, attempt>1), colour=rgb(0,0,0, alpha=.05))+
  geom_smooth(aes(attempt,mean_resid), data= subset(dmt, attempt>1), method="lm", se=F, size=I(1.2)) +
  geom_point(aes(attempt,mean_resid), data= dmt) +
  facet_grid(.~Experiment,  labeller="label_both") + ylab("Residual log(time taken)") +
  scale_x_continuous(breaks = 1:10) + xlab("Attempt")
#  coord_cartesian(ylim=c(-0.5,0.5))

ggsave("../images/learning_trend_time_subject.pdf", width=10.5, height = 3.5)

qplot(attempt,mean_resid, data= dmt) + geom_point(size=2.5) +
  geom_smooth(method="lm", se=F) + ylab("Mean residual time taken") +
  facet_wrap(~experiment, scales="free_y") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 

ggplot() + 
  geom_point(aes(attempt,mean_resid), data= dmt) +
  geom_smooth(aes(attempt,mean_resid), data= subset(dmt, attempt>1), method="lm", se=F) +
  facet_grid(.~Experiment,  labeller="label_both") + ylab("Residual log(time taken)") +
  scale_x_continuous(breaks = seq(2,10,by=2))

ggsave("../images/learning_trend_time.pdf", width=10.5, height = 3.5)


model <- as.formula(log(time_taken) ~I(attempt==1)+ attempt + (attempt|id) + (1|pic_id))
dt5 <- subset(dtrend, experiment==5)
f5 <- lmer(model, data=dt5) 
estimates5 <- get_estimates(f5)
dt6 <- subset(dtrend, experiment==6)
f6 <- lmer(model, data=dt6)
estimates6 <- get_estimates(f6)
dt7 <- subset(dtrend, experiment==7)
f7 <- lmer(model, data=dt7)
estimates7 <- get_estimates(f7)

estimates <- data.frame(estimates5,g1=" ",estimates6,g2=" ", estimates7)
estimates <- estimates[-4,]
rownames(estimates) <- c("$\\mu$","$\\alpha_1$","$\\alpha$", 
                         "$\\sigma^2_u$","$\\sigma^2_a$", "$\\sigma^2_l$","$\\sigma^2$")
dgt <- c(rep(0,7), rep(2,14), rep(2,7), rep(0,7))
dgts <- matrix(rep(dgt,3), ncol=15)
print(xtable(estimates, digits=dgts),  sanitize.text.function = function(x){x})

# ---------------------------------------------------------------
# Modeling proportion correct
# Fiting generalized mixed effect model with prportion correct
# Checking if the performance increases with attempts
# ---------------------------------------------------------------

trend.dat <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(response ~ (1|pic_name) + (1|id))
  fit <- glmer(model,family="binomial",data=dd, control=glmerControl(optimizer="bobyqa"))
  dd$resid <- (dd$response - fitted(fit))
  trend.dat <- rbind(trend.dat ,dd)
}

ggplot(trend.dat, aes(attempt,resid, colour=factor(experiment)))+
  geom_smooth(method = "loess", size = 1.5) +
  ylab("Residuals")

ddt <- ddply(trend.dat,.(experiment, attempt), summarise,
             mean_resid = mean(resid))

ddt$Experiment <- ddt$experiment
qplot(attempt,mean_resid, data= ddt) + 
  geom_smooth(method="lm", se=F) + geom_point(size=2.5) +
  ylab("Mean residual proportion correct") +
  facet_grid(.~Experiment, scales="free_y", labeller="label_both") +
  scale_x_continuous(breaks = seq(2,10,by=2)) 

ggsave("../images/learning_trend.pdf", width=10.5, height = 3.5)

trend.dat$Experiment <- trend.dat$experiment
ggplot() + 
  geom_smooth(aes(attempt,resid, group=id),method="lm", se=F,  data=trend.dat, colour=rgb(0,0,0, alpha=0.05))+
  geom_smooth(aes(attempt,mean_resid, group=1), data= ddt, method="lm", se=F, size=I(1.2)) +
  geom_point(aes(attempt,mean_resid, group=1), data= ddt) +
  facet_grid(.~Experiment, scales="free_y", labeller="label_both") + ylab("Mean residual proportion correct") +
  scale_x_continuous(breaks = 1:10) + xlab("Attempt")

ggsave("../images/learning_trend_subject.pdf", width=10.5, height = 3.5)


# Checking if the trend shown in the plot is significant or not
# For all experiments 5,6,7 slope is not statistically significant
# It appears that none of them are significant
fit1 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==5))
fit2 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==6))
fit3 <- lm(resid ~ attempt, data=subset(trend.dat, experiment==7))

# Getting results of model with proportion correct
model <- as.formula(response ~ factor(attempt) + (1|pic_name) + (attempt|id))
dt5 <- subset(dtrend, experiment==5)
fp5 <- glmer(model,family="binomial",data=dt5, control=glmerControl(optimizer="bobyqa"))
res5 <- get_estimates(fp5)

dt6 <- subset(dtrend, experiment==6)
fp6 <- glmer(model,family="binomial",data=dt6, control=glmerControl(optimizer="bobyqa"))
res6 <- get_estimates(fp6)

dt7 <- subset(dtrend, experiment==7)
library(optimx)
fp7 <- glmer(model,family="binomial",data=dt7, control=glmerControl(optimizer="optimx",
                                                                    optCtrl=list(method="L-BFGS-B")))
res7 <- get_estimates(fp7)

results <- data.frame(res5,g1=" ",res6,g2=" ", res7)
# remove covariance term
results <- results[-11, ]
rownames(results) <- c("$\\mu$", paste("$\\alpha", 2:10, "$",sep=""),
                         "$\\sigma^2_u$","$\\sigma^2_a$","$\\sigma^2_l$")

print(xtable(results, digits=2),  sanitize.text.function = function(x){x})



# ---------------------------------------------------------------
# Examining different models 
# with response (percent correct)
# ---------------------------------------------------------------

dt <- subset(dtrend, experiment %in% c(5,6,7))
fit0 <- lmer(response ~ factor(attempt) + (attempt|id) + (1|pic_id), family="binomial", data=dt)
fit1 <- lmer(response ~ I(attempt==1)+ attempt + (attempt|id) + (1|pic_id), family="binomial", data=dt)
fit2 <- lmer(response ~ attempt + (attempt|id) + (1|pic_id), family="binomial", data=dt)
fit3 <- lmer(response ~ attempt + (attempt|id) , family="binomial", data=dt)

anova(fit0,fit1)
anova(fit0,fit2)
anova(fit1,fit2)

# ---------------------------------------------------------------
# Examining different models 
# with response (log time taken)
# ft1 apears to be the best model since no difference with ft0
# ---------------------------------------------------------------

dt <- subset(dtrend, experiment %in% c(5,6,7))

ft0 <- lmer(log(time_taken) ~ factor(attempt) + (attempt|id) + (1|pic_id),  data=dt, method="ML")
ft1 <- lmer(log(time_taken) ~ I(attempt==1)+ attempt + (attempt|id) + (1|pic_id),  data=dt, method="ML")
ft2 <- lmer(log(time_taken) ~ attempt + (attempt|id) + (1|pic_id),  data=dt, method="ML")
ft3 <- lmer(log(time_taken) ~ attempt + (attempt|id) , data=dt, method="ML")

anova(ft0,ft1)
anova(ft0,ft2)
anova(ft1,ft2)


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
summary(fit1, test="W")

fit2 <- manova(model, data=subset(model.dat, plot_type=="Genotype") )
summary(fit2, test="W")


int.dat$locs <- "out"
int.dat$locs[int.dat$plot_location %in% c(9,12)] <- "in"
summary(manova(cbind(null_1,null_2,null_3,null_4,null_5)~factor(locs), data=int.dat))

# ===========================================================
# Modeling location effect
# Fiting generalized mixed model for location effect
# Locations are not significant
# Model fi1 shows the effect of null plots variability is negligible 
# as location effects are much bigger compared to null variability
# -----------------------------------------------------------

colnames(df)
fi0 <- lmer(response~factor(plot_location)+nulls+(1|pic_name), family="binomial",data=subset(df,plot_type=="Interaction"))
fi1 <- lmer(response~factor(plot_location)+(1|nulls), family="binomial",data=subset(df,plot_type=="Interaction"))
fi2 <- glm(response~factor(plot_location), family="binomial",data=subset(df,plot_type=="Interaction"))

# this model shows null set 3 for interaction is significant
fi3 <- lmer(response~factor(nulls)+(1|plot_location), family="binomial",data=subset(df,plot_type=="Interaction"))

fg <- lmer(response~factor(plot_location)+nulls+(1|pic_name), family="binomial",data=subset(df,plot_type=="Genotype"))

# the following plot shows null set 3 is different in interaction lineup
dd <- ddply(df, .(plot_type,nulls), summarize, prop_correct=mean(response))
qplot(nulls, prop_correct, geom="bar", stat="identity", facets=~plot_type, data=dd)


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


