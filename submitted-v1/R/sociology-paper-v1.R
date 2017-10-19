# The complete R code for Sociology Paper

library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(xtable)
library(grid)
library(lubridate)
library(dplyr)
library(maps)

# ==================================================================
# Loading the data 
# ------------------------------------------------------------------

demographics <- readRDS("../data/sociology-data.rds")
dat9 <- readRDS("../data/exp9.RDS")


# ==================================================================
# generating statistics for section 4.1 "Overview of the Data"
# ------------------------------------------------------------------

# showing number of countries the subjects are coming from
length(unique(demographics$country_code))
# showing countries with more than 10 participants
countryCount <- demographics %>% 
  group_by(country_name) %>%
  summarise(counts = length(unique(id))) %>%
  arrange(desc(counts))
# showing number of linesups evaluated
length(unique(demographics$lineup_id))



# ==================================================================
# figure 3 map of turk participants and country-wise bar chart
# ----------------------------------------------------------------
# getting unique participants information
# Since each participant has multiple responses in demographics data
turker <- demographics[!duplicated(demographics$id),]
turker$country <- factor(turker$country, 
                         levels=names(table(turker$country))[order(table(turker$country))])

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


map.dat <- as.data.frame(map("world",ylim=c(-45,70), plot=FALSE)[c("x","y")])
map.dat <- map_data("world")

ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) +
  geom_point(aes(longitude,latitude, colour=factor("A")), 
             data=turker, alpha=.6) +
  theme_bw()+ map_theme +
  theme(legend.position="none")

ggsave("../images/turker_location.pdf", width=8, height=4)


# country-wise participants counts
# This will produce figure 3 bar chart
ggplot(data=subset(turker,complete.cases(turker)),aes(country)) +
  geom_bar(aes(fill="A")) + 
  coord_flip() + ylab("Number of participants") + xlab("Country") +
  theme(legend.position="none")

ggsave("../images/turker_country.pdf", width=7, height=2) 



# ==================================================================
# table 2 demographic summary
# ----------------------------------------------------------------
get_summary <- function(dat, var){
  dat$totSubject <- length(unique(dat$id))
  res <- ddply(dat,c(var), summarize,
               subsjects = length(unique(id)),
               percents = length(unique(id))* 100 /totSubject[1],
               avg_time = round(mean(time_taken),2),
               response = length(response),
               prop_correct = mean(response)
  )
  return(data.frame(var=var,lbls=res[,1], res[,-1]))
}

sg <- get_summary(demographics, "gender_level")
se <- get_summary(demographics, "degree")
sa <- get_summary(demographics, "age_level")
sl <- get_summary(demographics, "country")

sdat <- rbind(sg,se,sa,sl)
print(xtable(sdat[,-7]), include.rownames=FALSE)


# ==================================================================
# table 4 Mixed Model results of demographic factor main effects
# ----------------------------------------------------------------

ft <- lmer(log(time_taken)~age_level+country+degree+gender_level+(1|lineup_id), 
           data=demographics)
summary(ft)

fp <- glmer(response~age_level+country+degree+gender_level+(1|lineup_id), 
            family="binomial", data=demographics, control=glmerControl(optimizer="bobyqa"))

summary(fp)
resid <- residuals(fp)
# residual error
sqrt(sum(resid^2)/(length(resid)-length(fixef(fp))))

# function to get estimates with confidence interval
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


est.factor <- cbind(get_estimates(ft)[-16,],g1=" ",get_estimates(fp))
rownames(est.factor) <- c("$\\mu$",substr(rownames(est.factor)[2:7],10,nchar(rownames(est.factor)[2:7])),
                          levels(demographics$country)[-1], levels(demographics$degree)[-1],
                          "Male", "lineup")
dgt <- c(rep(0,15), rep(2,45), rep(0,15))
dgts <- matrix(rep(dgt,2), ncol=10)
print(xtable(est.factor, digits=dgts),  sanitize.text.function = function(x){x})

# ==================================================================
# table 3 ANOVA Model results of demographic factor main effects
# ----------------------------------------------------------------

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

# ==================================================================
# figure 4 demographic factor main effects on log(time) and detection
# ------------------------------------------------------------------

# function to compute effect as percent correct and average time
get_effect <- function(dat, var){
  res <- ddply(dat,c(var), summarize,
               log_avg_time = round(log(mean(time_taken)),2),
               detection_rate = mean(response)
  )
  res$variable_name <- var[1]
  colnames(res) <- c("variable_level", colnames(res)[-1])
  return(res)
}
gdat <- get_effect(demographics, c("gender_level", "lineup_id"))
edat <- get_effect(demographics, c("degree", "lineup_id"))
cdat <- get_effect(demographics, c("country", "lineup_id"))
adat <- get_effect(demographics, c("age_level", "lineup_id"))

mdat <- melt(rbind(gdat,edat,cdat,adat), id=c("variable_level", "lineup_id", "variable_name"))
levels(mdat$variable) <- c("(Log) Time Taken in seconds", "Detection Rate")
mdat$variable_name <- factor(mdat$variable_name)
levels(mdat$variable_name) <- c("Age Categories", "Country", "Education", "Gender")

ggplot(data=mdat[complete.cases(mdat),], aes(variable_level, value)) +
  geom_boxplot() +
  facet_grid(variable~variable_name, scales="free", space="free_x") +
  stat_summary(fun.y=mean, geom="point") + xlab("Levels of demographic factors")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggsave("../images/demographic_effect.pdf", width=6.5, height=6)


# ==================================================================
# figure 5 practical significance of demographics
# ------------------------------------------------------------------

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
  theme_bw() + xlab(expression(eta)) + ylab("Prability of detection") +
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
  ylab("Change in probability of detection \n due to graduate degree") +
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
# saving figure 6 proportion vs lineup difficulty
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

qplot(variable,value, shape=factor(difficulty),data=mddat.all, size=I(3.5)) +
  geom_line(aes(variable,value, group=factor(difficulty)))+
  ylab("Proportion of data identification") + xlab("Education")+
  scale_x_discrete(labels=c("h.school","u.gd.course","u.grad.deg","grad.course","grad.deg"))+
  scale_shape_discrete(name="Lineup difficulty", 
                       labels=c("Difficult", "Medium","Easy")) 
# saving figure 6 proportion vs education
ggsave("../images/practical_impact_degree.pdf", width=6.5, height=4.5)



# ==================================================================
# learning trend analysis
# ------------------------------------------------------------------

dtrend <- demographics %>%
  filter(experiment < 8) %>%
  group_by(experiment, id) %>%
  mutate(
    attempt = rank(start_time),
    start_time = start_time,
    response = as.numeric(response),
    pic_name = pic_name,
    pic_id = pic_id,
    time_taken = time_taken,
    uid = paste(experiment,"_",id, sep="")) %>%
  filter(attempt <= 10)

# Attempt vs mean time taken for each lineup
qplot(attempt, log(time_taken), data=dtrend,colour=factor(experiment)) +
  stat_smooth(method="loess")

# Log time taken appears to be normal
qplot(log(time_taken), geom="density",
      data=subset(dtrend, experiment %in% c(5,6,7)), color=factor(experiment)) +
  scale_colour_hue(name="Experiment")

# ==================================================================
# Modeling time taken
# fitting linear random effect model to check 
# if log time_taken has any trend over sequential attempts
# ------------------------------------------------------------------

dpt <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(log(time_taken) ~ (1|pic_name) + (1|id))
  fit <- lmer(model,data=dd)
  dd$resid <- (log(dd$time_taken) - fitted(fit))
  dpt <- rbind(dpt ,dd)
}

dmt <- ddply(dpt, .(experiment,attempt), summarise,
             mean_resid = mean(resid))

dpt$Experiment <- dpt$experiment
dmt$Experiment <- dmt$experiment

# ==================================================================
# figure 7 residual of log(time taken) vs attempt in experiments 5-7
# ------------------------------------------------------------------
ggplot() + 
  geom_smooth(aes(attempt,resid, group=id),method="lm", se=F, 
              data=subset(dpt, attempt>1), colour=rgb(0,0,0, alpha=.05))+
  geom_smooth(aes(attempt,mean_resid), 
              data= subset(dmt, attempt>1), method="lm", se=F, size=I(1.2)) +
  geom_point(aes(attempt,mean_resid), data = dmt) +
  facet_grid(.~Experiment,  labeller="label_both") + 
  ylab("Residual log(time taken)") +
  scale_x_continuous(breaks = 1:10) + xlab("Attempt")
#  coord_cartesian(ylim=c(-0.5,0.5))

ggsave("../images/learning_trend_time_subject.pdf", width=10.5, height = 3.5)


# ==================================================================
# table 6 results of mixed effect model 5, log time taken vs attempt
# ------------------------------------------------------------------

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


# ==================================================================
# Modeling proportion correct (detection rate)
# Fiting generalized mixed effect model with detection rate
# Checking if the detection rate increases with attempts
# ------------------------------------------------------------------

trend.dat <- NULL
for (i in 5:7){
  d <- subset(dtrend, experiment==i)
  dd <- subset(d, id %in% id[attempt > 9])
  model <- as.formula(response ~ (1|pic_name) + (1|id))
  fit <- glmer(model,family="binomial",data=dd, control=glmerControl(optimizer="bobyqa"))
  dd$resid <- (dd$response - fitted(fit))
  trend.dat <- rbind(trend.dat ,dd)
}

ddt <- ddply(trend.dat,.(experiment, attempt), summarise,
             mean_resid = mean(resid))

ddt$Experiment <- ddt$experiment
trend.dat$Experiment <- trend.dat$experiment

# ==================================================================
# figure 6 mean residual detection rate vs attempt
# ------------------------------------------------------------------
ggplot() + 
  geom_smooth(aes(attempt,resid, group=id),method="lm", se=F,  
              data=trend.dat, colour=rgb(0,0,0, alpha=0.05))+
  geom_smooth(aes(attempt,mean_resid, group=1), 
              data= ddt, method="lm", se=F, size=I(1.2)) +
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

# ==================================================================
# table 5 results of model 3 with detection rate vs attempt
# This may take awhile to execute.
# ------------------------------------------------------------------
model <- as.formula(response ~ factor(attempt) + (1|pic_name) + (attempt|id))
dt5 <- subset(dtrend, experiment==5)
fp5 <- glmer(model,family="binomial",data=dt5, control=glmerControl(optimizer="bobyqa"))
res5 <- get_estimates(fp5)

dt6 <- subset(dtrend, experiment==6)
fp6 <- glmer(model,family="binomial",data=dt6, control=glmerControl(optimizer="bobyqa"))
res6 <- get_estimates(fp6)

dt7 <- subset(dtrend, experiment==7)
library(optimx)
fp7 <- glmer(model,family="binomial",data=dt7, 
             control=glmerControl(optimizer="optimx",
                                  optCtrl=list(method="L-BFGS-B")))
res7 <- get_estimates(fp7)

results <- data.frame(res5,g1=" ",res6,g2=" ", res7)
# remove covariance term
results <- results[-11, ]
rownames(results) <- c("$\\mu$", paste("$\\alpha", 2:10, "$",sep=""),
                       "$\\sigma^2_u$","$\\sigma^2_a$","$\\sigma^2_l$")

print(xtable(results, digits=2),  sanitize.text.function = function(x){x})


# ==================================================================
# table 7 MANOVA model results with experiment 9
# ----------------------------------------------------------------

df <- ddply(subset(dat9, plot_type != "Filter"), 
            .(plot_type, plot_location, nulls, pic_name),transform,
            replicates = 1:length(response))

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

# ==================================================================
# figure 8 proportion correct (detection rate) vs null plot location
# ------------------------------------------------------------------

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

