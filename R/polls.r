# url <- "http://freedomslighthouse.net/2012-presidential-election-electoral-vote-map/"
# library(XML)
# tabs <- readHTMLTable(url)
# polls <- tabs[[3]]
# 
# write.csv(polls, "polls.csv", row.names=F)
polls <- read.csv("data/polls.csv")
names(polls) <- c("State", "Electoral.Votes", "Results2008", "Poll2012", "Results2012")

library(plyr)
plist <- strsplit(as.character(polls$Poll2012), " ", fixed=TRUE)
p2012 <- ldply(plist, function(x) c(x,rep(NA, 5))[1:5])
names(p2012) <- paste(c("Candidate","Poll","Major", "Diss", "Minor"), "2012", sep="")
p2012$Poll2012 <- as.numeric(gsub("+","", as.character(p2012$Poll2012)))
p2012$Democrat <- p2012$Candidate2012=="Obama"

rlist <- strsplit(as.character(polls$Results2008), " ", fixed=TRUE)
r2008 <- ldply(rlist, function(x) c(x,rep(NA, 2))[1:2])
names(r2008) <- c("Candidate08", "Result08")
r2008$Result08 <- as.numeric(gsub("+|%","", as.character(r2008$Result08)))
r2008$Democrat <- r2008$Candidate08=="Obama"

r12list <- strsplit(as.character(polls$Results2012), " ", fixed=TRUE)
r2012 <- ldply(r12list, function(x) c(x,rep(NA, 2))[1:5])
names(r2012) <- paste(c("Candidate","Result","Major", "Diss", "Minor"), "2012", sep="")
r2012$Result2012 <- as.numeric(gsub("+","", as.character(r2012$Result2012)))
r2012$Democrat <- r2012$Candidate=="Obama"

simulatePoll <- function(x, sd=3, backup) {
  suppressWarnings(res <- rnorm(length(x), mean=x, sd=sd))
  idx <- which(is.na(res))
  res[idx] <- rnorm(length(idx), mean=backup[idx], sd=sd)
  res
}

simDemocrat <- function(margin, party, backup) {
  democrat <- party 
  idx <- which(is.na(democrat))
  democrat[idx] <- backup[idx]
  idx <- which(margin < 0)
  democrat[idx] <- !democrat[idx]
  
  democrat
}

res <- replicate(1000, {
  p2012$Sim.Margin <- simulatePoll(p2012$Poll2012, backup=r2008$Result08)
  p2012$Sim.Democrat <- simDemocrat(p2012$Sim.Margin, p2012$Democrat, r2008$Democrat)
  p2012$Sim.Margin <- abs(p2012$Sim.Margin)
  
  sum(polls$Electoral.Votes[p2012$Sim.Democrat])
})
qplot(res, geom="histogram") + geom_vline(xintercept=sum(polls$Electoral.Votes[r2012$Democrat]), colour="red")

# save data for lineups:
system("mkdir electoral")
for (null in 1:5) {
sims <- rdply(19,
{
  p2012$Sim.Margin <- simulatePoll(p2012$Poll2012, backup=r2008$Result08)
p2012$Sim.Democrat <- simDemocrat(p2012$Sim.Margin, p2012$Democrat, r2008$Democrat)
p2012$Sim.Margin <- abs(p2012$Sim.Margin)
  with(p2012, data.frame(Sim.Margin, Sim.Democrat))
}
)
p3 <- data.frame(polls, sims)
t3 <- data.frame(.n=20, polls, Sim.Margin=r2012$Result2012, Sim.Democrat=r2012$Democrat)
p3 <- rbind(p3[,names(t3)], t3)
p3$sample <- sample(20,20, replace=FALSE)[p3$.n]
location <- p3$sample[nrow(p3)]

p2 <- p3
p2 <- p2[order(p2$Sim.Margin, decreasing=TRUE),]
p2 <- ddply(p2, .(.n, Sim.Democrat), transform, tower=cumsum(Electoral.Votes[order(Sim.Margin, decreasing=TRUE)]))
p2$diff <- with(p2, Sim.Margin*c(-1,1)[as.numeric(Sim.Democrat)+1])

write.csv(p2, file=sprintf("electoral/electoral-%s-%s.csv", null, location))
}


######## 
# make lineups
system("mkdir lineups")

source("~/papers/sociology_chapter/R/add_interaction.R")
files <- dir("~/papers/sociology_chapter/turk11-prepping/electoral")
files <- files[grep("csv", files)]

library(grid)
setwd("lineups")
for (fname in files) {
  dframe <- read.csv(sprintf("../electoral/%s", fname))
  dframe$diff <- with(dframe, diff+sign(diff)*0.075)
  dframe$diff <- pmin(50, dframe$diff)
  print(ggplot(aes(x=diff, y=tower, colour = factor(Sim.Democrat)), data=dframe) + 
          #  geom_point() +
    #      geom_segment(aes(x=diff, xend=diff, y=0, yend=tower, colour=Sim.Democrat), size=1) +
          scale_colour_manual(values=c("red", "blue"), guide="none") + 
          scale_fill_manual(values=c("red", "blue"), guide="none") + 
    #      geom_hline(yintercept=270, colour="grey70") + 
    #      ylab("Electoral Votes")+xlab("Republican       ---          Democrat") + 
          scale_x_continuous(breaks=c(-25,0,25), labels=c("25", "0", "25"), limits=c(-50,50)) + 
          geom_rect(aes(xmin=pmin(0, diff), xmax=pmax(0,diff), ymin=0, ymax=tower, fill=Sim.Democrat), size=0) +
    #      geom_segment(aes(x=0,xend=diff, y=tower,yend=tower), colour="grey80", size=0.5) + 
          geom_vline(xintercept=0, colour="white") + facet_wrap(~sample) +
          	theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.title=element_blank(), plot.margin=unit(c(0.1,0.1,0,0), "cm"))
  )
  tmpfile <- tempfile(tmpdir="~")
  params <- unlist(strsplit(gsub(".csv", "", fname),"-"))
  null <- params[2]
  location <- params[3]
  for (click in c("multiple", "single")) {
    pic_name <- sprintf("%s-%s.svg", gsub("~/","", tmpfile), click)
    sample_size <- nrow(dframe)/20
    test_param <- sprintf("turk11-%s", click)
    param_value <- sprintf("electoral-%s", null)
    
    write.table(data.frame(
      sample_size=sample_size, 
      test_param=test_param,
      param_value=param_value,
      p_value=NA,
      obs_plot_location=location, 
      pic_name=pic_name,
      experiment="turk11",
      difficulty=fname,
      data_name=fname
    ), 
                file="../picture-details.csv", row.names=FALSE, sep=",",
                col.names=!file.exists("../picture-details.csv"), append=TRUE)
    
    toggle = "toggle"
    if (click =="single") toggle="select"
    make_interactive(filename= pic_name, 
                     script="http://www.hofroe.net/examples/lineup/action.js")
  }
}




ggplot(aes(x=diff, y=tower, colour = factor(Sim.Democrat)), data=p2) + 
#  geom_point() +
  geom_segment(aes(x=diff, xend=diff, y=0, yend=tower, colour=Sim.Democrat), size=1) +
  scale_colour_manual(values=c("red", "blue"), guide="none") + 
  scale_fill_manual(values=c("red", "blue"), guide="none") + 
  geom_hline(yintercept=270, colour="grey70") + 
  ylab("Electoral Votes")+xlab("Republican       ---          Democrat") + 
  scale_x_continuous(breaks=c(-50,0,50), labels=c("50", "0", "50"), limits=c(-90,90)) + 
  geom_rect(aes(xmin=0, xmax=diff, ymin=0, ymax=tower, fill=Sim.Democrat), size=0) +
  geom_segment(aes(x=0,xend=diff, y=tower,yend=tower), colour="grey80", size=0.5) + 
  geom_vline(xintercept=0, colour="white") + facet_wrap(~sample)
ggsave("tower.pdf", height=12, width=12)


