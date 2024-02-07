# -----------------------------------------------------------------------------
# Study 3: Distance, Resolution and Concreteness in New York Times Articles
# 
# Perceiving Distances and Construing Targets Based on Communication
# Chris Arnold, Cardiff University
# June 2022
# -----------------------------------------------------------------------------


# -- 0 Housekeeping --------------------------------------------------------------

cat("Running Experiment 3: Distance, Resolution and Concreteness in New York Times Articles \n")


# -- 1 Calculate the Measures --------------------------------------------------
# This is how we calculate the temporal distance and how broad temp. categories
# For copyright reasons, we can't provide access to the data. But if you 
# have the original data, feel free to reach out, we will help you with the 
# implementation. Here, we already report the full script necessary for under-
# standing how we did it all.



# First step is to take the original proprietary data and run the script in the 
# NlpParser folder that implements Heideltime, and thus annotates the corpus.
# The resulting data writes out the data in a folder structure like so:
# - nyt_corpus_annotated
#    - 1988
#    - 1992
#    - 1996 
#    - 2000
#    - 2004
# And each folder contains an .xml with the respective article, and all its 
# annotations

# This is the starting point for handing over the data into R for further 
# analysis
# The result 
copyright.run <- FALSE
if(copyright.run){
  # -- Read in Data --------------------------------------------------------------
  # Read in Dictionary 
  brysbaert.dat <- read.xlsx("../../data/brysbaert_et_al_concreteness_ratings.xlsx")
  
  # Parse NYT data 
  xml.dat.location <- ('/foo/nyt_corpus_annotated/')  
  filenames <- list.files(xml.dat.location, recursive = TRUE)
  
  # NYT.data.extractor() from read_NYT_data.r
  source('utils/read_NYT_data.r')
  # runtime: about 1-2 mins
  NYT.corpus <- vector(mode = "list", length = length(filenames))
  for (i in 1:length(filenames)){
    #  quick progress bar
    if (i==1) cat(strrep('=', floor(length(filenames)/1000)), '\n')
    if ((i %% 1000) == 0) cat('*') 
    # execute
    NYT.corpus[[i]] <- NYT.data.extractor(filenames[i])  
  }
  
  # -- Temp Horizon 
  temp.horizon <- rep(NA,length(NYT.corpus))
  for (i in seq(1,length(NYT.corpus))){
    temp.horizon[i] <- NYT.corpus[[i]]$temp.horizon
  }
  # Generate election variable
  election.dates.list <- lapply(NYT.corpus, function(x) x[2])
  election.dates <- unlist(election.dates.list, use.names = FALSE)
  election.dates <- car::recode(election.dates, "6886=1988; 8342=1992; 
  9805=1996; 11268=2000; 12729 = 2004")
  
  dat.from.NYT.corpus <- data.frame(election.dates, temp.horizon)
  save(dat.from.NYT.corpus, file = '../data/NYT_corpus_vars.RData')
  
  # -- Annotate Construal
  # This annotates the NYT corpus with the Brysbaert et al. (2014) concrentess
  # dictionary and calculates 
  # 'conc.article', "nr.words.in.dict",
  # "article.cleaned.sd", "article.cleaned.median",
  # "article.cleaned.mean"
  # runtime: OJO! Takes quite some time (> 4h)
  run <- FALSE
  if (run==TRUE){
    source("utils/annotate_concreteness.r")
  }
  save(concreteness.values, file = '../data/NYT_corpus_concreteness_values.RData')
  
  # -- Annotate Temp Resolution
  # runtime: 2-3 mins 
  source('utils/annotate_temp_resolution.r')
  save(tempres.counters.list.all.docs, '../data/NYT_corpus_temp_res_list.RData')
}


# -- Loading the data from annotations

# The temporal horizon of each document
load(file = '../data/NYT_corpus_temp_res_list.RData')

# Annotation of the NYT corpus with the Brysbaert et al. (2014) concreteness 
# dictionary. On that basis calculates:
# 'conc.article', "nr.words.in.dict",
# "article.cleaned.sd", "article.cleaned.median",
# "article.cleaned.mean"
load(file = '../data/NYT_corpus_concreteness_values.RData')

# Load Data calculated on NYT corpus
load('../data/NYT_corpus_vars.RData')



# -- 2 Analysis ----------------------------------------------------------------

# -- Prepare Data 

# Calculate share of mentions of one category per document
# function to calc
divider <- function(x, overall.counters) return(x/overall.counters)

# Role out
list.all.docs.prop.counters <- list()
for (i in seq(1,length(tempres.counters.list.all.docs))){
  overall.counters <- sum(unlist(tempres.counters.list.all.docs[[i]]))
  list.all.docs.prop.counters[[i]] <- lapply(
    tempres.counters.list.all.docs[[i]], divider, 
    overall.counters = overall.counters
  )
}

# construct a data set with the most important categories
dat.all.docs.prop.counters <- data.frame(
  past.ref.counter=integer(),
  future.ref.counter=integer(),
  mentions.counter.smaller.1.d=integer(),
  mentions.counter.1.d=integer(),
  mentions.counter.1.w=integer(),
  mentions.counter.1.m=integer(),
  mentions.counter.3.m=integer(),
  mentions.counter.1.y=integer(),
  mentions.counter.2.y=integer(),
  mentions.counter.3.y=integer(),
  mentions.counter.4.y=integer(),
  mentions.counter.5.y=integer(),
  mentions.counter.6.y=integer(),
  mentions.counter.7.y=integer(),
  mentions.counter.8.y=integer(),
  mentions.counter.9.y=integer(),
  mentions.counter.10.y=integer(),
  mentions.counter.100.y=integer(),
  mentions.counter.1000.y=integer()
)


# implement (20 s or so...)
for (i in seq(1,length(list.all.docs.prop.counters))){
  # quick progress bar
  if(i == 1) cat(strrep('=', floor(length(list.all.docs.prop.counters)/1000)), '\n')
  if ((i %% 1000) == 0) cat('*')
  # implement
  dat.all.docs.prop.counters[i,] <- c(
    list.all.docs.prop.counters[[i]]$past.ref.counter,
    list.all.docs.prop.counters[[i]]$future.ref.counter,
    list.all.docs.prop.counters[[i]]$mentions.counter.smaller.1.d,
    list.all.docs.prop.counters[[i]]$mentions.counter.1.d,
    list.all.docs.prop.counters[[i]]$mentions.counter.1.w,
    list.all.docs.prop.counters[[i]]$mentions.counter.1.m,
    list.all.docs.prop.counters[[i]]$mentions.counter.3.m,
    list.all.docs.prop.counters[[i]]$mentions.counter.1.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.2.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.3.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.4.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.5.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.6.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.7.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.8.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.9.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.10.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.100.y,
    list.all.docs.prop.counters[[i]]$mentions.counter.1000.y
  )
}


# -- Prepare Data 

temp.horizon <- dat.from.NYT.corpus$temp.horizon
election.dates <- dat.from.NYT.corpus$election.dates

dat.analysis.temp <- data.frame(
  temp.horizon, 
  dat.all.docs.prop.counters['mentions.counter.smaller.1.d'],
  dat.all.docs.prop.counters['mentions.counter.1.d'],
  dat.all.docs.prop.counters['mentions.counter.1.w'],
  dat.all.docs.prop.counters['mentions.counter.1.m'],
  dat.all.docs.prop.counters['mentions.counter.3.m'],
  dat.all.docs.prop.counters['mentions.counter.1.y'],
  dat.all.docs.prop.counters['mentions.counter.2.y'],
  dat.all.docs.prop.counters['mentions.counter.3.y'],
  dat.all.docs.prop.counters['mentions.counter.4.y'],
  dat.all.docs.prop.counters['mentions.counter.5.y'],
  dat.all.docs.prop.counters['mentions.counter.6.y'],
  dat.all.docs.prop.counters['mentions.counter.7.y'],
  dat.all.docs.prop.counters['mentions.counter.8.y'],
  dat.all.docs.prop.counters['mentions.counter.9.y'],
  dat.all.docs.prop.counters['mentions.counter.10.y'],
  dat.all.docs.prop.counters['mentions.counter.100.y'],
  dat.all.docs.prop.counters['mentions.counter.1000.y'],
  dat.all.docs.prop.counters['past.ref.counter'],
  dat.all.docs.prop.counters['future.ref.counter'],
  concreteness.values['article.cleaned.mean'], 
  election.dates)


# Further Subset the data: 
# Filter 1: All within 2 years
dat.analysis.temp <- subset(dat.analysis.temp, 
                            abs(dat.analysis.temp$temp.horizon) < 730)

#  Filter 2: select all those that are not 100% one temp resolution
# since this may introduce bias since some texts are very short
mask.temp <- (dat.analysis.temp$mentions.counter.smaller.1.d == 1 |
                dat.analysis.temp$mentions.counter.1.d == 1 |
                dat.analysis.temp$mentions.counter.1.w == 1 |
                dat.analysis.temp$mentions.counter.1.m == 1 |
                dat.analysis.temp$mentions.counter.3.m == 1 |
                dat.analysis.temp$mentions.counter.1.y ==1 |
                dat.analysis.temp$mentions.counter.2.y == 1 |
                dat.analysis.temp$mentions.counter.3.y == 1 |
                dat.analysis.temp$mentions.counter.4.y == 1 |
                dat.analysis.temp$mentions.counter.5.y == 1 |
                dat.analysis.temp$mentions.counter.6.y == 1 |
                dat.analysis.temp$mentions.counter.7.y == 1 |
                dat.analysis.temp$mentions.counter.8.y == 1 |
                dat.analysis.temp$mentions.counter.9.y == 1 |
                dat.analysis.temp$mentions.counter.10.y == 1 |
                dat.analysis.temp$mentions.counter.100.y == 1 |
                dat.analysis.temp$mentions.counter.1000.y)
# invert
mask <- !mask.temp
# select
dat.analysis <- subset(dat.analysis.temp, mask)



# Take a vote: Fine grained temporal resolution or coarse grained?
dat.analysis$fine.grained.temp.res <- dat.analysis$mentions.counter.1.d + 
  dat.analysis$mentions.counter.smaller.1.d 

dat.analysis$coarse.grained.temp.res <-  dat.analysis$mentions.counter.1.y +
  dat.analysis$mentions.counter.2.y +
  dat.analysis$mentions.counter.3.y +
  dat.analysis$mentions.counter.4.y +
  dat.analysis$mentions.counter.5.y +
  dat.analysis$mentions.counter.6.y +
  dat.analysis$mentions.counter.7.y +
  dat.analysis$mentions.counter.8.y +
  dat.analysis$mentions.counter.9.y +
  dat.analysis$mentions.counter.10.y +
  dat.analysis$mentions.counter.100.y +
  dat.analysis$mentions.counter.1000.y +
  dat.analysis$future.ref.counter +
  dat.analysis$past.ref.counter


dat.analysis$fine.grained.dummy <- dat.analysis$fine.grained.temp.res > 0.5

dat.analysis$coarse.grained.dummy <- dat.analysis$coarse.grained.temp.res > 0.5


# identify the two subsets
dat.analysis.coarse <- subset(dat.analysis, dat.analysis$coarse.grained.dummy)
dat.analysis.fine <- subset(dat.analysis, dat.analysis$fine.grained.dummy)

# Data Checks
run.check<-F
if(run.check){
  table(dat.analysis$fine.grained.dummy)
  table(dat.analysis$coarse.grained.dummy)
  dim(dat.analysis)
  dim(dat.analysis.coarse)
  dim(dat.analysis.fine)
}




# -- Analysing the Interaction between Resolution and Horizon ------------------
# Plot reproduction code from Figure 4 with both time plots


# Calculate running mean and SE
std.er <- function(x){
  x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

iterator <- seq(-754, 755)

# Fine grained data
running.mean.fine <- NA
running.std.er.fine <- NA

for (i in iterator){
  dat.temp <- subset(dat.analysis.fine, (dat.analysis.fine$temp.horizon > (i-45) &
                                           dat.analysis.fine$temp.horizon < (i+45)))
  running.mean.fine <- c(
    running.mean.fine, mean(dat.temp$article.cleaned.mean, na.rm = TRUE))
  running.std.er.fine <- c(
    running.std.er.fine, std.er(dat.temp$article.cleaned.mean))
}
running.mean.fine <- running.mean.fine[-1]
running.std.er.fine <- running.std.er.fine[-1]
running.upper.bound.fine <- running.mean.fine + 1.96*running.std.er.fine
running.lower.bound.fine <- running.mean.fine - 1.96*running.std.er.fine



# coarse grained data
running.mean.coarse <- NA
running.std.er.coarse <- NA

for (i in iterator){
  dat.temp <- subset(dat.analysis.coarse, (dat.analysis.coarse$temp.horizon > (i-45) &
                                             dat.analysis.coarse$temp.horizon < (i+45)))
  running.mean.coarse <- c(
    running.mean.coarse, mean(dat.temp$article.cleaned.mean, na.rm = TRUE))
  running.std.er.coarse <- c(
    running.std.er.coarse, std.er(dat.temp$article.cleaned.mean))
}
running.mean.coarse <- running.mean.coarse[-1]
running.std.er.coarse <- running.std.er.coarse[-1]
running.upper.bound.coarse <- running.mean.coarse + 1.96*running.std.er.coarse
running.lower.bound.coarse <- running.mean.coarse - 1.96*running.std.er.coarse



# When do they stop to overlap?
# iterator[running.lower.bound.fine > running.upper.bound.coarse]


# -- Plotting -------
# Define Colours 

v.colors <- viridis_pal(option = 'plasma')(5)

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red=curcoldata[1], green=curcoldata[2],
        blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

v.colors.t <- makeTransparent(v.colors, 80)



pdf('../out/difference_fine_coarse_grained.pdf', width = 10, height = 7)
plot(iterator, running.mean.coarse,  
     xlab = 'Days Before/After the Election', ylab = 'Average Concreteness', 
     type = 'l', las = 1, col = v.colors[1], 
     ylim = c(2.40, 2.6))
# fine grained
x.plot.fine <- c(iterator, rev(iterator))
y.plot.fine <- c(running.upper.bound.fine, rev(running.lower.bound.fine))
polygon(x.plot.fine, y.plot.fine, col = v.colors.t[4], border = NA)
lines(iterator, running.mean.fine, col = v.colors[4])
# coarse grained
x.plot.coarse <- c(iterator, rev(iterator))
y.plot.coarse <- c(running.upper.bound.coarse, rev(running.lower.bound.coarse))
polygon(x.plot.coarse, y.plot.coarse, col = v.colors.t[1], border = NA)
# lines(iterator, running.mean.coarse, col = v.colors[1])
abline(v=0, col = v.colors[3], lwd = 2, lty = 2)
legend('bottomright', pch = 15, col = c(v.colors[4], v.colors[1]),
       legend = c('Narrow Event Resolution', 
                  'Broad Event Resolution'))
dev.off()



# -- Interactions ----
# Replicate the paper 


# some more vars...
dat.analysis$temp.horizon.abs <- abs(dat.analysis$temp.horizon)
dat.analysis$temp.horizon.abs.log <- log(dat.analysis$temp.horizon.abs)
dat.analysis$before.election[dat.analysis$temp.horizon < 0] <- 1
dat.analysis$before.election[dat.analysis$temp.horizon >= 0] <- 0



# without the election day because of the log
dat.analysis <- subset(dat.analysis, dat.analysis$temp.horizon.abs != 0)

# Paper Regressions
m.paper.log <- lm(article.cleaned.mean ~ temp.horizon.abs.log + before.election +
                    factor(election.dates),
                  data = dat.analysis)

m.paper <- lm(article.cleaned.mean ~ temp.horizon.abs + before.election +
                factor(election.dates),
              data = dat.analysis)


dat.analysis$broad.grained.dummy <- dat.analysis$fine.grained.dummy * -1

m.fine.grained.ia.before <- lm(article.cleaned.mean ~ temp.horizon.abs + 
                                 factor(election.dates) + broad.grained.dummy,
                               # fine.grained.dummy:temp.horizon.abs,
                               data = subset(dat.analysis, dat.analysis$temp.horizon < 0 ))






summary(m.paper)
summary(m.paper.log)
summary(m.fine.grained.ia.before)







# -- 3 Appendix: Replication of Bhatia 2016 --------------------------------------



dat.analysis <- data.frame(temp.horizon, 
                           concreteness.values$article.cleaned.mean,
                           concreteness.values$article.cleaned.median)
names(dat.analysis) <- c("temp.horizon", "concreteness.mean", 
                         "concreteness.median")


# reduce data to more reasonable time frames
dat.analysis <- subset(dat.analysis, dat.analysis$temp.horizon > -2*365)
dat.analysis <- subset(dat.analysis, dat.analysis$temp.horizon < 2*365)

std.er <- function(x){
  x <- na.omit(x)
  sd(x)/sqrt(length(x))
}


# -- Calculate the Analysis with 'just' the mean -------------------------------
iterator <- seq(-754, 755)

running.mean <- NA
running.std.er <- NA

# defining colours
v.colors <- viridis_pal(option = 'plasma')(3)

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red=curcoldata[1], green=curcoldata[2],
        blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

v.colors.t <- makeTransparent(v.colors, 80)

for (i in iterator){
  dat.temp <- subset(dat.analysis, (dat.analysis$temp.horizon > (i-45) &
                                      dat.analysis$temp.horizon < (i+45)))
  running.mean <- c(running.mean, mean(dat.temp$concreteness.mean, na.rm = TRUE))
  running.std.er <- c(running.std.er, std.er(dat.temp$concreteness.mean))
}
running.mean <- running.mean[-1]
running.std.er <- running.std.er[-1]
running.upper.bound <- running.mean + running.std.er
running.lower.bound <- running.mean - running.std.er


# print an overview to check data
run <- FALSE
if (run){
  # both plots at a time
  par(mfrow = c(2,1))
  # Raw data
  plot(dat.analysis$temp.horizon, dat.analysis$concreteness.mean, 
       main ='Plot of the Raw Data', col = v.colors[1])
  # Running Mean
  plot(iterator, running.mean,  main = 'Bhatia and Walasek 2016 Running 90 Day Mean', 
       xlab = 'Days Before/After the Election', ylab = 'Average Concreteness', 
       type = 'l', las = 1, col = v.colors[1], 
       ylim = c(2.44, 2.55))
  x.plot <- c(iterator, rev(iterator))
  y.plot <- c(running.upper.bound, rev(running.lower.bound))
  polygon(x.plot, y.plot, col = v.colors.t[1], border = NA)
  lines(iterator, running.mean, col = v.colors[1])
  abline(v=0, col = v.colors[2], lwd = 2, lty = 2)
  par(mfrow = c(1,1))  
}


# print the paper version
pdf('../out/replicating_fig4_bhatia16.pdf', width = 10, height = 7)
plot(iterator, running.mean,  
     xlab = 'Days Before/After the Election', ylab = 'Average Concreteness', 
     type = 'l', las = 1, col = v.colors[1], 
     ylim = c(2.44, 2.55))

x.plot <- c(iterator, rev(iterator))
y.plot <- c(running.upper.bound, rev(running.lower.bound))
polygon(x.plot, y.plot, col = v.colors.t[1], border = NA)
lines(iterator, running.mean, col = v.colors[1])
abline(v=0, col = v.colors[2], lwd = 2, lty = 2)
dev.off()

















