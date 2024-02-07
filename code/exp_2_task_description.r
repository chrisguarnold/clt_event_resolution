# -----------------------------------------------------------------------------
# Study 2: Time-Dependent Construal in Language
# 
# Perceiving Distances and Construing Targets Based on Communication
# Chris Arnold, Cardiff University
# June 2022
# -----------------------------------------------------------------------------



# -- Housekeeping --------------------------------------------------------------
cat("Running Experiment 2: Time-Dependent Construal in Language \n")



# -- Load and Manage Data ----------------------------------------------------
dat.pt <- read.csv(
'../data/exp_2_task_description.csv')

# 5f359513105505187b34e85c is English language
# Message on prolific on July 6th: 
# "Really sorry, I accidentally put that English was not my native language, 
# however it is! Sorry about this, and I hope it can be adjusted to not 
# complicate your results!"
dat.pt$block_4_english_lang[dat.pt$block_1_prolific_ID == '5f359513105505187b34e85c'] <- 'Yes'

# add a treatment indicator
dat.pt$treatment <- NA
dat.pt$treatment[dat.pt$block_2_nh_db != ""] <- 'db'
dat.pt$treatment[dat.pt$block_2_nh_dn != ""] <- 'dn'
dat.pt$treatment[dat.pt$block_2_nh_cb != ""] <- 'cb'
dat.pt$treatment[dat.pt$block_2_nh_cn != ""] <- 'cn'

# create distances and resolutions
dat.pt$temp.distance.long <- dat.pt$temp.resolution.broad <- NA
dat.pt$temp.distance.long[dat.pt$treatment == 'db' | dat.pt$treatment == 'dn'] <- TRUE
dat.pt$temp.distance.long[dat.pt$treatment == 'cb' | dat.pt$treatment == 'cn'] <- FALSE
dat.pt$temp.resolution.broad[dat.pt$treatment == 'db' | dat.pt$treatment == 'cb'] <- TRUE
dat.pt$temp.resolution.broad[dat.pt$treatment == 'dn' | dat.pt$treatment == 'cn'] <- FALSE

# -- Quality checks  -----------------------------------------------------------
# Keep only native speakers
dat.pt <- subset(dat.pt, dat.pt$block_4_english_lang == 'Yes')

# => only one person keeps providing the same answer in all four categories
same.answer.provider <- dat.pt$block_1_prolific_ID[
  dat.pt$block_2_sf_db != "" & dat.pt$block_2_sf_db == dat.pt$block_2_nh_db]

dat.pt <- subset(dat.pt, dat.pt$block_1_prolific_ID != same.answer.provider)
# Attention Check
# DN
dat.pt <- subset(dat.pt, dat.pt$block_2_att_check == "June 30th next year" | 
                   dat.pt$block_2_att_check == "")
# DB
dat.pt <- subset(dat.pt, dat.pt$Q237 == "Next year" | dat.pt$Q237 == "")
# CN
dat.pt <- subset(
  dat.pt, dat.pt$Q226 == "Thursday next week" | dat.pt$Q226 == "")
# CB
dat.pt <- subset(dat.pt, dat.pt$Q215 == "Next week" | dat.pt$Q215 == "")
# How well understood the language (1,2,3 out)
dat.pt <- subset(dat.pt, dat.pt$block_3_lang_underst != '3')
dat.pt <- subset(dat.pt, dat.pt$block_3_lang_underst != '2')
dat.pt <- subset(dat.pt, dat.pt$block_3_lang_underst != 'Not at all')
# Questions without reading (the entire time out)
dat.pt <- subset(dat.pt, dat.pt$block_5_no_reading != 'The entire time')
# Did you hurry? (empty and 4 out. Sometimes is middle category)
dat.pt <- subset(dat.pt, dat.pt$block_5_hurry != '4')
# How long did people take?
# On average people took about 11 minutes
# summary(dat.pt$Duration..in.seconds.)
# exclude on the basis of rushing and lagging, but blocked by treatments
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. < quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'dn'], 0.95)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. > quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'dn'], 0.05)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. < quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'db'], 0.95)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. > quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'db'], 0.05)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. < quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'cn'], 0.95)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. > quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'cn'], 0.05)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. < quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'cb'], 0.95)
dat.pt <- subset(dat.pt, mask)
mask <- dat.pt$treatment != 'dn' | dat.pt$Duration..in.seconds. > quantile(
  dat.pt$Duration..in.seconds.[dat.pt$treatment == 'cb'], 0.05)
dat.pt <- subset(dat.pt, mask)


# -- 1 Annotate ----------------------------------------------------------------

# Brysbaert et al. --------
cat('Annotating Brysbaert measure. \n')
# 1=very abstract (language-based), 5=very concrete (experience-based)
# Annotate concreteness with Brysbaert et al. 2014 dictionary ---
brysbaert.dat <- read.xlsx(
  "../data/brysbaert_et_al_concreteness_ratings.xlsx")

# calc.concreteness() is from utils/annotate_concreteness.r
# function to loop over all responses 
annotate.responses <- function(all.responses, dictionary){
  concreteness.values <- data.frame(0,0,0,0,0)
  for (j in seq(1, length(all.responses))){
    # do if not NA
    if(!all.responses[j] == ''){
      concreteness.values <- rbind(concreteness.values,
                                   calc.concreteness(
                                     all.responses[j],dictionary))    
    } else if (all.responses[j] == ''){
      concreteness.values <- rbind(concreteness.values, c(NA, NA, NA, NA, NA))  
    }
  }
  concreteness.values <- concreteness.values[-1,]
  return(concreteness.values)
}  

# Annotate all experiments in all conditions
dat.pt$block_2_sf_dn_brysbaert <- annotate.responses(dat.pt$block_2_sf_dn, brysbaert.dat)[,5]
dat.pt$block_2_nh_dn_brysbaert <- annotate.responses(dat.pt$block_2_nh_dn, brysbaert.dat)[,5]
dat.pt$block_2_fh_dn_brysbaert <- annotate.responses(dat.pt$block_2_fh_dn, brysbaert.dat)[,5]
dat.pt$block_2_ph_dn_brysbaert <- annotate.responses(dat.pt$block_2_ph_dn, brysbaert.dat)[,5]
dat.pt$block_2_sf_db_brysbaert <- annotate.responses(dat.pt$block_2_sf_db, brysbaert.dat)[,5]
dat.pt$block_2_nh_db_brysbaert <- annotate.responses(dat.pt$block_2_nh_db, brysbaert.dat)[,5]
dat.pt$block_2_fh_db_brysbaert <- annotate.responses(dat.pt$block_2_fh_db, brysbaert.dat)[,5]
dat.pt$block_2_ph_db_brysbaert <- annotate.responses(dat.pt$block_2_ph_db, brysbaert.dat)[,5]
dat.pt$block_2_sf_cn_brysbaert <- annotate.responses(dat.pt$block_2_sf_cn, brysbaert.dat)[,5]
dat.pt$block_2_nh_cn_brysbaert <- annotate.responses(dat.pt$block_2_nh_cn, brysbaert.dat)[,5]
dat.pt$block_2_fh_cn_brysbaert <- annotate.responses(dat.pt$block_2_fh_cn, brysbaert.dat)[,5]
dat.pt$block_2_ph_cn_brysbaert <- annotate.responses(dat.pt$block_2_ph_cn, brysbaert.dat)[,5]
dat.pt$block_2_sf_cb_brysbaert <- annotate.responses(dat.pt$block_2_sf_cb, brysbaert.dat)[,5]
dat.pt$block_2_nh_cb_brysbaert <- annotate.responses(dat.pt$block_2_nh_cb, brysbaert.dat)[,5]
dat.pt$block_2_fh_cb_brysbaert <- annotate.responses(dat.pt$block_2_fh_cb, brysbaert.dat)[,5]
dat.pt$block_2_ph_cb_brysbaert <- annotate.responses(dat.pt$block_2_ph_cb, brysbaert.dat)[,5]

db_brysbaert <- c(dat.pt$block_2_sf_db_brysbaert, dat.pt$block_2_nh_db_brysbaert, 
                  dat.pt$block_2_fh_db_brysbaert, dat.pt$block_2_ph_db_brysbaert)
dn_brysbaert <- c(dat.pt$block_2_sf_dn_brysbaert, dat.pt$block_2_nh_dn_brysbaert, 
                  dat.pt$block_2_fh_dn_brysbaert, dat.pt$block_2_ph_dn_brysbaert)
cb_brysbaert <- c(dat.pt$block_2_sf_cb_brysbaert, dat.pt$block_2_nh_cb_brysbaert, 
                  dat.pt$block_2_fh_cb_brysbaert, dat.pt$block_2_ph_cb_brysbaert)
cn_brysbaert <- c(dat.pt$block_2_sf_cn_brysbaert, dat.pt$block_2_nh_cn_brysbaert, 
                  dat.pt$block_2_fh_cn_brysbaert, dat.pt$block_2_ph_cn_brysbaert)

brysbaert.measure <- c(db_brysbaert, dn_brysbaert, cb_brysbaert, cn_brysbaert)

# Paetzold and Specia --------
cat('Annotating Paetzold measure. \n')
# low is abstract, high is concrete 
dat.pt$block_2_sf_dn_paetzold <- annotate.responses(dat.pt$block_2_sf_dn, bootstrap_list)[,5]
dat.pt$block_2_nh_dn_paetzold <- annotate.responses(dat.pt$block_2_nh_dn, bootstrap_list)[,5]
dat.pt$block_2_fh_dn_paetzold <- annotate.responses(dat.pt$block_2_fh_dn, bootstrap_list)[,5]
dat.pt$block_2_ph_dn_paetzold <- annotate.responses(dat.pt$block_2_ph_dn, bootstrap_list)[,5]
dat.pt$block_2_sf_db_paetzold <- annotate.responses(dat.pt$block_2_sf_db, bootstrap_list)[,5]
dat.pt$block_2_nh_db_paetzold <- annotate.responses(dat.pt$block_2_nh_db, bootstrap_list)[,5]
dat.pt$block_2_fh_db_paetzold <- annotate.responses(dat.pt$block_2_fh_db, bootstrap_list)[,5]
dat.pt$block_2_ph_db_paetzold <- annotate.responses(dat.pt$block_2_ph_db, bootstrap_list)[,5]
dat.pt$block_2_sf_cn_paetzold <- annotate.responses(dat.pt$block_2_sf_cn, bootstrap_list)[,5]
dat.pt$block_2_nh_cn_paetzold <- annotate.responses(dat.pt$block_2_nh_cn, bootstrap_list)[,5]
dat.pt$block_2_fh_cn_paetzold <- annotate.responses(dat.pt$block_2_fh_cn, bootstrap_list)[,5]
dat.pt$block_2_ph_cn_paetzold <- annotate.responses(dat.pt$block_2_ph_cn, bootstrap_list)[,5]
dat.pt$block_2_sf_cb_paetzold <- annotate.responses(dat.pt$block_2_sf_cb, bootstrap_list)[,5]
dat.pt$block_2_nh_cb_paetzold <- annotate.responses(dat.pt$block_2_nh_cb, bootstrap_list)[,5]
dat.pt$block_2_fh_cb_paetzold <- annotate.responses(dat.pt$block_2_fh_cb, bootstrap_list)[,5]
dat.pt$block_2_ph_cb_paetzold <- annotate.responses(dat.pt$block_2_ph_cb, bootstrap_list)[,5]

db_paetzold <- c(dat.pt$block_2_sf_db_paetzold, dat.pt$block_2_nh_db_paetzold, 
                 dat.pt$block_2_fh_db_paetzold, dat.pt$block_2_ph_db_paetzold)
dn_paetzold <- c(dat.pt$block_2_sf_dn_paetzold, dat.pt$block_2_nh_dn_paetzold, 
                 dat.pt$block_2_fh_dn_paetzold, dat.pt$block_2_ph_dn_paetzold)
cb_paetzold <- c(dat.pt$block_2_sf_cb_paetzold, dat.pt$block_2_nh_cb_paetzold, 
                 dat.pt$block_2_fh_cb_paetzold, dat.pt$block_2_ph_cb_paetzold)
cn_paetzold <- c(dat.pt$block_2_sf_cn_paetzold, dat.pt$block_2_nh_cn_paetzold, 
                 dat.pt$block_2_fh_cn_paetzold, dat.pt$block_2_ph_cn_paetzold)

paetzold.measure <- c(db_paetzold, dn_paetzold, cb_paetzold, cn_paetzold)


# Yeomans -----------
cat('Annotating Yeomans measure. \n')
# low is abstract, high is concrete 
dat.pt$block_2_sf_dn_yeomans <- doc2concrete(dat.pt$block_2_sf_dn, domain = 'plans')
dat.pt$block_2_nh_dn_yeomans <- doc2concrete(dat.pt$block_2_nh_dn, domain = 'plans')
dat.pt$block_2_fh_dn_yeomans <- doc2concrete(dat.pt$block_2_fh_dn, domain = 'plans')
dat.pt$block_2_ph_dn_yeomans <- doc2concrete(dat.pt$block_2_ph_dn, domain = 'plans')
dat.pt$block_2_sf_db_yeomans <- doc2concrete(dat.pt$block_2_sf_db, domain = 'plans')
dat.pt$block_2_nh_db_yeomans <- doc2concrete(dat.pt$block_2_nh_db, domain = 'plans')
dat.pt$block_2_fh_db_yeomans <- doc2concrete(dat.pt$block_2_fh_db, domain = 'plans')
dat.pt$block_2_ph_db_yeomans <- doc2concrete(dat.pt$block_2_ph_db, domain = 'plans')
dat.pt$block_2_sf_cn_yeomans <- doc2concrete(dat.pt$block_2_sf_cn, domain = 'plans')
dat.pt$block_2_nh_cn_yeomans <- doc2concrete(dat.pt$block_2_nh_cn, domain = 'plans')
dat.pt$block_2_fh_cn_yeomans <- doc2concrete(dat.pt$block_2_fh_cn, domain = 'plans')
dat.pt$block_2_ph_cn_yeomans <- doc2concrete(dat.pt$block_2_ph_cn, domain = 'plans')
dat.pt$block_2_sf_cb_yeomans <- doc2concrete(dat.pt$block_2_sf_cb, domain = 'plans')
dat.pt$block_2_nh_cb_yeomans <- doc2concrete(dat.pt$block_2_nh_cb, domain = 'plans')
dat.pt$block_2_fh_cb_yeomans <- doc2concrete(dat.pt$block_2_fh_cb, domain = 'plans')
dat.pt$block_2_ph_cb_yeomans <- doc2concrete(dat.pt$block_2_ph_cb, domain = 'plans')

db_yeomans <- c(dat.pt$block_2_sf_db_yeomans, dat.pt$block_2_nh_db_yeomans, 
                dat.pt$block_2_fh_db_yeomans, dat.pt$block_2_ph_db_yeomans)
dn_yeomans <- c(dat.pt$block_2_sf_dn_yeomans, dat.pt$block_2_nh_dn_yeomans, 
                dat.pt$block_2_fh_dn_yeomans, dat.pt$block_2_ph_dn_yeomans)
cb_yeomans <- c(dat.pt$block_2_sf_cb_yeomans, dat.pt$block_2_nh_cb_yeomans, 
                dat.pt$block_2_fh_cb_yeomans, dat.pt$block_2_ph_cb_yeomans)
cn_yeomans <- c(dat.pt$block_2_sf_cn_yeomans, dat.pt$block_2_nh_cn_yeomans, 
                dat.pt$block_2_fh_cn_yeomans, dat.pt$block_2_ph_cn_yeomans)

yeomans.measure <- c(db_yeomans, dn_yeomans, cb_yeomans, cn_yeomans)


# Seih et al. ----------------
# The final LCM score ranges from 1 (very concrete) to 5 (very abstract).
cat('Annotating Seih measure. \n')
# This is what you need the spacy implementation for
spacy_initialize()
dat.verbcats <- read.csv('../data/LCM.csv')
dat.verbcats$category.name <- NA
dat.verbcats$category.name[dat.verbcats$category == 1] <- "DAV"
dat.verbcats$category.name[dat.verbcats$category == 2] <- "IAV"
dat.verbcats$category.name[dat.verbcats$category == 3] <- "SV"

calc.concreteness.seih <- function(texts, dat.verbcats){
  # POS tagging
  text <- spacyr::spacy_parse(texts, dependency=F,pos=T,tag=T,entity=F)
  # keep the words that matter: Verbs, nouns and adjectives
  text$pos <- car::recode(text$pos, "'PROPN' = 'NOUN'")
  mask <- text$pos %in% c("NOUN","ADJ","VERB")
  text <- subset(text, mask)
  text$token <- tolower(text$token)
  # differentiate the three 
  for (i in seq(1:length(text$token))){
    if(text$pos[i] == 'VERB'){
      # check if the verb is in the list, then do
      if(text$token[i] %in% dat.verbcats$word){
        text$pos[i] <- dat.verbcats$category.name[dat.verbcats$word %in% text$token[i]]      
      }
      
    }
  }
  # score the different values 
  text$pos.values <- car::recode(text$pos, "'DAV' = 1;
          'IAV' = 2 ;
          'SV' = 3;
          'ADJ' = 4;
          'NOUN' = 5;
          'VERB' = NA")
  conc.score <- sum(text$pos.values, na.rm = TRUE)/length(na.omit(text$pos.values))
  return(conc.score)
}


# function to loop over all responses 
annotate.responses.seih <- function(all.responses, dictionary){
  concreteness.values <- NA
  for (j in seq(1, length(all.responses))){
    # do if not NA
    if(!all.responses[j] == ''){
      concreteness.values <- rbind(concreteness.values,
                                   calc.concreteness.seih(all.responses[j],dictionary))    
    } else if (all.responses[j] == ''){
      concreteness.values <- rbind(concreteness.values, NA)  
    }
  }
  concreteness.values <- concreteness.values[-1,]
  return(concreteness.values)
}  

# Measuring
dat.pt$block_2_sf_dn_seih <- annotate.responses.seih(dat.pt$block_2_sf_dn, dat.verbcats)
dat.pt$block_2_nh_dn_seih <- annotate.responses.seih(dat.pt$block_2_nh_dn, dat.verbcats)
dat.pt$block_2_fh_dn_seih <- annotate.responses.seih(dat.pt$block_2_fh_dn, dat.verbcats)
dat.pt$block_2_ph_dn_seih <- annotate.responses.seih(dat.pt$block_2_ph_dn, dat.verbcats)
dat.pt$block_2_sf_db_seih <- annotate.responses.seih(dat.pt$block_2_sf_db, dat.verbcats)
dat.pt$block_2_nh_db_seih <- annotate.responses.seih(dat.pt$block_2_nh_db, dat.verbcats)
dat.pt$block_2_fh_db_seih <- annotate.responses.seih(dat.pt$block_2_fh_db, dat.verbcats)
dat.pt$block_2_ph_db_seih <- annotate.responses.seih(dat.pt$block_2_ph_db, dat.verbcats)
dat.pt$block_2_sf_cn_seih <- annotate.responses.seih(dat.pt$block_2_sf_cn, dat.verbcats)
dat.pt$block_2_nh_cn_seih <- annotate.responses.seih(dat.pt$block_2_nh_cn, dat.verbcats)
dat.pt$block_2_fh_cn_seih <- annotate.responses.seih(dat.pt$block_2_fh_cn, dat.verbcats)
dat.pt$block_2_ph_cn_seih <- annotate.responses.seih(dat.pt$block_2_ph_cn, dat.verbcats)
dat.pt$block_2_sf_cb_seih <- annotate.responses.seih(dat.pt$block_2_sf_cb, dat.verbcats)
dat.pt$block_2_nh_cb_seih <- annotate.responses.seih(dat.pt$block_2_nh_cb, dat.verbcats)
dat.pt$block_2_fh_cb_seih <- annotate.responses.seih(dat.pt$block_2_fh_cb, dat.verbcats)
dat.pt$block_2_ph_cb_seih <- annotate.responses.seih(dat.pt$block_2_ph_cb, dat.verbcats)

db_seih <- c(dat.pt$block_2_sf_db_seih, dat.pt$block_2_nh_db_seih, 
             dat.pt$block_2_fh_db_seih, dat.pt$block_2_ph_db_seih)
dn_seih <- c(dat.pt$block_2_sf_dn_seih, dat.pt$block_2_nh_dn_seih, 
             dat.pt$block_2_fh_dn_seih, dat.pt$block_2_ph_dn_seih)
cb_seih <- c(dat.pt$block_2_sf_cb_seih, dat.pt$block_2_nh_cb_seih, 
             dat.pt$block_2_fh_cb_seih, dat.pt$block_2_ph_cb_seih)
cn_seih <- c(dat.pt$block_2_sf_cn_seih, dat.pt$block_2_nh_cn_seih, 
             dat.pt$block_2_fh_cn_seih, dat.pt$block_2_ph_cn_seih)

seih.measure <- c(db_seih, dn_seih, cb_seih, cn_seih)

# Assemble data -----------
dat.res <- data.frame(brysbaert.measure, paetzold.measure, 
                      yeomans.measure, seih.measure)

dat.res$treatment.final <- c(rep('db', length(db_yeomans)),
                             rep('dn', length(dn_yeomans)),
                             rep('cb', length(cb_yeomans)),
                             rep('cn', length(cn_yeomans)))

dat.res$treatment.broad <- c(rep(TRUE, length(db_yeomans)),
                             rep(FALSE, length(dn_yeomans)),
                             rep(TRUE, length(cb_yeomans)),
                             rep(FALSE, length(cn_yeomans)))

dat.res$treatment.distant <- c(rep(TRUE, length(db_yeomans)),
                               rep(TRUE, length(dn_yeomans)),
                               rep(FALSE, length(cb_yeomans)),
                               rep(FALSE, length(cn_yeomans)))
dat.res <- na.omit(dat.res)


# calculate z-scores -----------------
z.scorer <- function(dat){
  z.score <- (dat - mean(dat))/sd(dat)
  return(z.score)
}
# implement
dat.res$brysbaert.measure.z <- z.scorer(dat.res$brysbaert.measure)
dat.res$paetzold.measure.z <- z.scorer(dat.res$paetzold.measure)
dat.res$yeomans.measure.z <- z.scorer(dat.res$yeomans.measure)
# this measure is the other way round
dat.res$seih.measure.z <- z.scorer(dat.res$seih.measure)* -1

# average over all z-scores
dat.res$mean.of.z.means <- rowMeans(cbind(dat.res$brysbaert.measure.z, 
                                          dat.res$paetzold.measure.z, 
                                          dat.res$yeomans.measure.z,
                                          dat.res$seih.measure.z))

# Principle Component Analysis -----------
pca.res <- prcomp(dat.res[c("brysbaert.measure.z", "paetzold.measure.z", 
                            "yeomans.measure.z", "seih.measure.z")])

dat.res$pca.dim.1 <- pca.res$x[,1]
dat.res$pca.dim.2 <- pca.res$x[,2]
dat.res$pca.dim.1.z <- z.scorer(dat.res$pca.dim.1)








# -- 2 Analysis ----------------------------------------------------------------

# Description of Sample 
# age
summary(dat.pt$block_4_age)
sd(dat.pt$block_4_age, na.rm = TRUE)

# gender
table(dat.pt$block_4_gender)
table(is.na(dat.pt$block_4_gender))




# Boxplots
# Use Viridis Colours
v.colors <- viridis_pal(option = 'plasma')(6)
v.colors.t <- adjustcolor(v.colors, alpha.f = 0.75)

pdf('../out/task_descr_time_treatments.pdf', width = 7, heigh = 5)
par(mar = c(4,1,1,8))
# Comparing broad and narrow 
boxplot(dat.res$pca.dim.1.z[dat.res$treatment.final == 'cn'],
        dat.res$pca.dim.1.z[dat.res$treatment.final == 'cb'],
        dat.res$pca.dim.1.z[dat.res$treatment.final == 'dn'],
        dat.res$pca.dim.1.z[dat.res$treatment.final == 'db'],
        dat.res$mean.of.z.means[dat.res$treatment.final == 'cn'],
        dat.res$mean.of.z.means[dat.res$treatment.final == 'cb'],
        dat.res$mean.of.z.means[dat.res$treatment.final == 'dn'],
        dat.res$mean.of.z.means[dat.res$treatment.final == 'db'],
        dat.res$yeomans.measure.z[dat.res$treatment.final == 'cn'],
        dat.res$yeomans.measure.z[dat.res$treatment.final == 'cb'],
        dat.res$yeomans.measure.z[dat.res$treatment.final == 'dn'],
        dat.res$yeomans.measure.z[dat.res$treatment.final == 'db'],
        dat.res$seih.measure.z[dat.res$treatment.final == 'cn'],
        dat.res$seih.measure.z[dat.res$treatment.final == 'cb'],
        dat.res$seih.measure.z[dat.res$treatment.final == 'dn'],
        dat.res$seih.measure.z[dat.res$treatment.final == 'db'],
        dat.res$paetzold.measure.z[dat.res$treatment.final == 'cn'],
        dat.res$paetzold.measure.z[dat.res$treatment.final == 'cb'],
        dat.res$paetzold.measure.z[dat.res$treatment.final == 'dn'],
        dat.res$paetzold.measure.z[dat.res$treatment.final == 'db'],
        dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cn'],
        dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cb'],
        dat.res$brysbaert.measure.z[dat.res$treatment.final == 'dn'],
        dat.res$brysbaert.measure.z[dat.res$treatment.final == 'db'],
        ylim = c(-5.5,4.5),
        xlab = 'Concreteness of Answers (z-Scores)', ylab = '',
        horizontal = TRUE,axes = FALSE, 
        col = c(rep(v.colors[1], 2), rep(v.colors.t[1], 2),
                rep(v.colors[2], 2), rep(v.colors.t[2], 2),
                rep(v.colors[3], 2), rep(v.colors.t[3], 2),
                rep(v.colors[4], 2), rep(v.colors.t[4], 2),
                rep(v.colors[5], 2), rep(v.colors.t[5], 2),
                rep(v.colors[6], 2), rep(v.colors.t[6], 2))
)  
legend(x= -6, y = 25, col = v.colors[seq(6,1)], pch = 15, bty = 'n',
       legend = c("Brysbaert et al.", "Paetzold et al.",
                  "Seih et al.", "Yeomans", "Mean of Means","PCA"))
axis(4, col = "white", las = 1, at = seq(1,24), cex = 0.8,
     # lab = rep(c("CN", "CB", "DN", "DB"),6 ))
     lab = rep(c("N. Week Thur.","Next Week","N. Year June 30th", "Next Year"),6))
axis(1)
dev.off()





# Table with means
mean.brysbaert.z <- c(mean(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'db']), 
  mean(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'dn']), 
  mean(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cb']), 
  mean(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cn']))

sd.brysbaert.z <- c(sd(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'db']), 
                          sd(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'dn']), 
                          sd(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cb']), 
                          sd(dat.res$brysbaert.measure.z[dat.res$treatment.final == 'cn']))

mean.paetzold.z <- c(mean(dat.res$paetzold.measure.z[dat.res$treatment.final == 'db']), 
  mean(dat.res$paetzold.measure.z[dat.res$treatment.final == 'dn']), 
  mean(dat.res$paetzold.measure.z[dat.res$treatment.final == 'cb']), 
  mean(dat.res$paetzold.measure.z[dat.res$treatment.final == 'cn']))

sd.paetzold.z <- c(sd(dat.res$paetzold.measure.z[dat.res$treatment.final == 'db']), 
                        sd(dat.res$paetzold.measure.z[dat.res$treatment.final == 'dn']), 
                        sd(dat.res$paetzold.measure.z[dat.res$treatment.final == 'cb']), 
                        sd(dat.res$paetzold.measure.z[dat.res$treatment.final == 'cn']))

mean.yeomans.z <- c(mean(dat.res$yeomans.measure.z[dat.res$treatment.final == 'db']), 
  mean(dat.res$yeomans.measure.z[dat.res$treatment.final == 'dn']), 
  mean(dat.res$yeomans.measure.z[dat.res$treatment.final == 'cb']), 
  mean(dat.res$yeomans.measure.z[dat.res$treatment.final == 'cn']))

sd.yeomans.z <- c(sd(dat.res$yeomans.measure.z[dat.res$treatment.final == 'db']), 
                       sd(dat.res$yeomans.measure.z[dat.res$treatment.final == 'dn']), 
                       sd(dat.res$yeomans.measure.z[dat.res$treatment.final == 'cb']), 
                       sd(dat.res$yeomans.measure.z[dat.res$treatment.final == 'cn']))

mean.seih.z <- c(mean(dat.res$seih.measure.z[dat.res$treatment.final == 'db']), 
  mean(dat.res$seih.measure.z[dat.res$treatment.final == 'dn']), 
  mean(dat.res$seih.measure.z[dat.res$treatment.final == 'cb']), 
  mean(dat.res$seih.measure.z[dat.res$treatment.final == 'cn']))

sd.seih.z <- c(sd(dat.res$seih.measure.z[dat.res$treatment.final == 'db']), 
                    sd(dat.res$seih.measure.z[dat.res$treatment.final == 'dn']), 
                    sd(dat.res$seih.measure.z[dat.res$treatment.final == 'cb']), 
                    sd(dat.res$seih.measure.z[dat.res$treatment.final == 'cn']))

mean.of.z.means <- c(mean(dat.res$mean.of.z.means[dat.res$treatment.final == 'db']), 
  mean(dat.res$mean.of.z.means[dat.res$treatment.final == 'dn']), 
  mean(dat.res$mean.of.z.means[dat.res$treatment.final == 'cb']), 
  mean(dat.res$mean.of.z.means[dat.res$treatment.final == 'cn']))

sd.of.z.means <- c(sd(dat.res$mean.of.z.means[dat.res$treatment.final == 'db']), 
                        sd(dat.res$mean.of.z.means[dat.res$treatment.final == 'dn']), 
                        sd(dat.res$mean.of.z.means[dat.res$treatment.final == 'cb']), 
                        sd(dat.res$mean.of.z.means[dat.res$treatment.final == 'cn']))

mean.pca.dim.1.z <-c(mean(dat.res$pca.dim.1.z[dat.res$treatment.final == 'db']), 
 mean(dat.res$pca.dim.1.z[dat.res$treatment.final == 'dn']), 
 mean(dat.res$pca.dim.1.z[dat.res$treatment.final == 'cb']), 
 mean(dat.res$pca.dim.1.z[dat.res$treatment.final == 'cn']))

sd.pca.dim.1.z <-c(sd(dat.res$pca.dim.1.z[dat.res$treatment.final == 'db']), 
                   sd(dat.res$pca.dim.1.z[dat.res$treatment.final == 'dn']), 
                   sd(dat.res$pca.dim.1.z[dat.res$treatment.final == 'cb']), 
                   sd(dat.res$pca.dim.1.z[dat.res$treatment.final == 'cn']))


dat.res.table <- round(t(data.frame(
  mean.brysbaert.z,
  mean.paetzold.z,
  mean.seih.z,
  mean.yeomans.z,
  mean.of.z.means,
  mean.pca.dim.1.z
)),2)


dat.res.table





# Multivariate Analysis
# report OLS instead of AOV, due to slightly unbalanced samples
lm.brysbaert <- lm(
  dat.res$brysbaert.measure.z ~ dat.res$treatment.distant + dat.res$treatment.broad)
lm.paetzold <- lm(
  dat.res$paetzold.measure.z ~ dat.res$treatment.distant + dat.res$treatment.broad)
lm.yeomans <- lm(
  dat.res$yeomans.measure.z ~ dat.res$treatment.distant + dat.res$treatment.broad)
lm.seih <- lm(
  dat.res$seih.measure.z ~ dat.res$treatment.distant + dat.res$treatment.broad)
lm.mean.of.z.means <- lm(
  dat.res$mean.of.z.means ~ dat.res$treatment.distant + dat.res$treatment.broad)
lm.pca.dim.1.z <-lm(
  dat.res$pca.dim.1.z ~ dat.res$treatment.distant + dat.res$treatment.broad)


summary(lm.brysbaert)
summary(lm.paetzold)
summary(lm.yeomans)
summary(lm.seih)
summary(lm.mean.of.z.means)
summary(lm.pca.dim.1.z)





