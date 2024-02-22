# -----------------------------------------------------------------------------
# Study 1: The Level of Agency in Time Dependent Construals
# 
# Perceiving Distances and Construing Targets Based on Communication
# Chris Arnold, Cardiff University
# June 2022
# -----------------------------------------------------------------------------

cat("Running Experiment 1: The Level of Agency in Time Dependent Construals \n")


# -- Load Data  ----------------------------------------------------------------
dat.pt <- read.csv(
 '../data/exp_1_BIF_task.csv')

dim(dat.pt)
# add a treatment indicator
dat.pt$treatment <- NA
dat.pt$treatment[dat.pt$reading_db != ""] <- 'db'
dat.pt$treatment[dat.pt$reading_dn != ""] <- 'dn'
dat.pt$treatment[dat.pt$reading_cb != ""] <- 'cb'
dat.pt$treatment[dat.pt$reading_cn != ""] <- 'cn'

# create long and short distances
dat.pt$temp.distance.long <- dat.pt$temp.resolution.broad <- NA
dat.pt$temp.distance.long[dat.pt$treatment == 'db' | dat.pt$treatment == 'dn'] <- TRUE
dat.pt$temp.distance.long[dat.pt$treatment == 'cb' | dat.pt$treatment == 'cn'] <- FALSE
dat.pt$temp.resolution.broad[dat.pt$treatment == 'db' | dat.pt$treatment == 'cb'] <- TRUE
dat.pt$temp.resolution.broad[dat.pt$treatment == 'dn' | dat.pt$treatment == 'cn'] <- FALSE


# -- Recoding the Answers ------------------------------------------------------
dat.pt$list.dn.d <- as.numeric(car::recode(dat.pt$list_dn, 
                              "'Writing things down' = 0;
  'Getting organized' = 1"))

dat.pt$reading.dn.d <- as.numeric(
  car::recode(dat.pt$reading_dn, 
              "'Following lines of print' = 0;
  'Gaining knowledge' = 1"))

dat.pt$army.dn.d <- as.numeric(
  car::recode(dat.pt$army_dn, 
              "'Signing up' = 0;
  'Helping the Nations defense' = 1"))

dat.pt$clothes.dn.d <- as.numeric(
  car::recode(dat.pt$clothes_dn, 
              "'Putting clothes into the machine' = 0;
  'Removing odors from clothes' = 1"))

dat.pt$apple.dn.d <- as.numeric(
  car::recode(dat.pt$apple_dn, 
              "'Pulling an apple off a branch' = 0;
  'Getting something to eat' = 1"))

dat.pt$tree.dn.d <- as.numeric(
  car::recode(dat.pt$tree_dn, 
              "'Wielding an axe' = 0;
  'Getting firewood' = 1"))

dat.pt$carpeting.dn.d <- as.numeric(
  car::recode(dat.pt$carpeting_dn, 
              "'Using a yard stick' = 0;
  'Getting ready to remodel' = 1"))

dat.pt$house.dn.d <- as.numeric(
  car::recode(dat.pt$house_dn, 
              "'Vacuuming the floor' = 0;
  'Showing ones cleanliness' = 1"))
       
dat.pt$room.dn.d <- as.numeric(
  car::recode(dat.pt$room_dn, 
              "'Applying brush strokes' = 0;
  'Making the room look fresh' = 1"))

dat.pt$rent.dn.d <- as.numeric(
  car::recode(dat.pt$rent_dn, 
              "'Writing a check' = 0;
  'Maintaining a place to live' = 1"))

dat.pt$houseplants.dn.d <- as.numeric(
  car::recode(dat.pt$houseplants_dn, 
              "'Watering plants' = 0;
  'Making the room look nice' = 1"))

dat.pt$door.dn.d <- as.numeric(
  car::recode(dat.pt$door_dn, 
              "'Putting a key in the lock' = 0;
  'Securing the house' = 1"))

dat.pt$voting.dn.d <- as.numeric(
  car::recode(dat.pt$voting_dn, 
              "'Marking a ballot' = 0;
  'Influencing the election' = 1"))

dat.pt$climbing.dn.d <- as.numeric(
  car::recode(dat.pt$tree_dn.1, 
              "'Holding on to branches' = 0;
  'Getting a good view' = 1"))

dat.pt$personality.dn.d <- as.numeric(
  car::recode(dat.pt$personality_dn, 
              "'Answering questions' = 0;
  'Revealing what you are like' = 1"))

dat.pt$toothbrushing.dn.d <- as.numeric(
  car::recode(dat.pt$toothbrushing_dn, 
              "'Moving a brush around in ones mouth' = 0;
  'Preventing tooth decay' = 1"))

dat.pt$test.dn.d <- as.numeric(
  car::recode(dat.pt$test_dn, 
              "'Answering questions' = 0;
  'Showing ones knowledge' = 1"))

dat.pt$greeting.dn.d <- as.numeric(
  car::recode(dat.pt$greeting_dn, 
              "'Saying hello' = 0;
  'Showing friendliness' = 1"))

dat.pt$temptation.dn.d <- as.numeric(
  car::recode(dat.pt$temptation_dn, 
              "'Saying \"no\"' = 0;
  'Showing moral courage' = 1"))

dat.pt$eating.dn.d <- as.numeric(
  car::recode(dat.pt$eathing_dn, 
              "'Chewing and swallowing' = 0;
  'Getting nutrition' = 1"))

dat.pt$garden.dn.d <- as.numeric(
  car::recode(dat.pt$garden_dn, 
              "'Planting seeds' = 0;
  'Getting fresh vegetables' = 1"))

dat.pt$car.dn.d <- as.numeric(
  car::recode(dat.pt$car_dn, 
              "'Following a map' = 0;
  'Seeing the countryside' = 1"))

dat.pt$cavity.dn.d <- as.numeric(
  car::recode(dat.pt$cavity_dn, 
              "'Going to the dentist' = 0;
  'Protecting your teeth' = 1"))

dat.pt$child.dn.d <- as.numeric(
  car::recode(dat.pt$child_dn, 
              "'Using simple words' = 0;
  'Teaching a child something' = 1"))

dat.pt$doorbell.dn.d <- as.numeric(
  car::recode(dat.pt$doorbell_dn, 
              "'Moving a finger' = 0;
  'Seeing if someones home' = 1"))
          








# Distant Broad
dat.pt$list.db.d <- as.numeric(car::recode(dat.pt$list_db, 
                                           "'Writing things down' = 0;
  'Getting organized' = 1"))

dat.pt$reading.db.d <- as.numeric(
  car::recode(dat.pt$reading_db, 
              "'Following lines of print' = 0;
  'Gaining knowledge' = 1"))

dat.pt$army.db.d <- as.numeric(
  car::recode(dat.pt$army_db, 
              "'Signing up' = 0;
  'Helping the Nations defense' = 1"))

dat.pt$clothes.db.d <- as.numeric(
  car::recode(dat.pt$clothes_db, 
              "'Putting clothes into the machine' = 0;
  'Removing odors from clothes' = 1"))

dat.pt$apple.db.d <- as.numeric(
  car::recode(dat.pt$apple_db, 
              "'Pulling an apple off a branch' = 0;
  'Getting something to eat' = 1"))

dat.pt$tree.db.d <- as.numeric(
  car::recode(dat.pt$tree_db, 
              "'Wielding an axe' = 0;
  'Getting firewood' = 1"))

dat.pt$carpeting.db.d <- as.numeric(
  car::recode(dat.pt$carpeting_db, 
              "'Using a yard stick' = 0;
  'Getting ready to remodel' = 1"))

dat.pt$house.db.d <- as.numeric(
  car::recode(dat.pt$house_db, 
              "'Vacuuming the floor' = 0;
  'Showing ones cleanliness' = 1"))

dat.pt$room.db.d <- as.numeric(
  car::recode(dat.pt$room_db, 
              "'Applying brush strokes' = 0;
  'Making the room look fresh' = 1"))

dat.pt$rent.db.d <- as.numeric(
  car::recode(dat.pt$rent_db, 
              "'Writing a check' = 0;
  'Maintaining a place to live' = 1"))

dat.pt$houseplants.db.d <- as.numeric(
  car::recode(dat.pt$houseplants_db, 
              "'Watering plants' = 0;
  'Making the room look nice' = 1"))

dat.pt$door.db.d <- as.numeric(
  car::recode(dat.pt$door_db, 
              "'Putting a key in the lock' = 0;
  'Securing the house' = 1"))

dat.pt$voting.db.d <- as.numeric(
  car::recode(dat.pt$voting_db, 
              "'Marking a ballot' = 0;
  'Influencing the election' = 1"))

dat.pt$climbing.db.d <- as.numeric(
  car::recode(dat.pt$tree_db.1, 
              "'Holding on to branches' = 0;
  'Getting a good view' = 1"))

dat.pt$personality.db.d <- as.numeric(
  car::recode(dat.pt$personality_db, 
              "'Answering questions' = 0;
  'Revealing what you are like' = 1"))

dat.pt$toothbrushing.db.d <- as.numeric(
  car::recode(dat.pt$toothbrushing_db, 
              "'Moving a brush around in ones mouth' = 0;
  'Preventing tooth decay' = 1"))

dat.pt$test.db.d <- as.numeric(
  car::recode(dat.pt$test_db, 
              "'Answering questions' = 0;
  'Showing ones knowledge' = 1"))

dat.pt$greeting.db.d <- as.numeric(
  car::recode(dat.pt$greeting_db, 
              "'Saying hello' = 0;
  'Showing friendliness' = 1"))

dat.pt$temptation.db.d <- as.numeric(
  car::recode(dat.pt$temptation_db, 
              "'Saying \"no\"' = 0;
  'Showing moral courage' = 1"))

dat.pt$eating.db.d <- as.numeric(
  car::recode(dat.pt$eating_db, 
              "'Chewing and swallowing' = 0;
  'Getting nutrition' = 1"))

dat.pt$garden.db.d <- as.numeric(
  car::recode(dat.pt$garden_db, 
              "'Planting seeds' = 0;
  'Getting fresh vegetables' = 1"))

dat.pt$car.db.d <- as.numeric(
  car::recode(dat.pt$car_db, 
              "'Following a map' = 0;
  'Seeing the countryside' = 1"))

dat.pt$cavity.db.d <- as.numeric(
  car::recode(dat.pt$cavity_db, 
              "'Going to the dentist' = 0;
  'Protecting your teeth' = 1"))

dat.pt$child.db.d <- as.numeric(
  car::recode(dat.pt$child_db, 
              "'Using simple words' = 0;
  'Teaching a child something' = 1"))

dat.pt$doorbell.db.d <- as.numeric(
  car::recode(dat.pt$doorbell_db, 
              "'Moving a finger' = 0;
  'Seeing if someones home' = 1"))






# Close broad
dat.pt$list.cb.d <- as.numeric(car::recode(dat.pt$list_cb, 
                                           "'Writing things down' = 0;
  'Getting organized' = 1"))

dat.pt$reading.cb.d <- as.numeric(
  car::recode(dat.pt$reading_cb, 
              "'Following lines of print' = 0;
  'Gaining knowledge' = 1"))

dat.pt$army.cb.d <- as.numeric(
  car::recode(dat.pt$army_cb, 
              "'Signing up' = 0;
  'Helping the Nations defense' = 1"))

dat.pt$clothes.cb.d <- as.numeric(
  car::recode(dat.pt$clothes_cb, 
              "'Putting clothes into the machine' = 0;
  'Removing odors from clothes' = 1"))

dat.pt$apple.cb.d <- as.numeric(
  car::recode(dat.pt$apple_cb, 
              "'Pulling an apple off a branch' = 0;
  'Getting something to eat' = 1"))

dat.pt$tree.cb.d <- as.numeric(
  car::recode(dat.pt$tree_cb, 
              "'Wielding an axe' = 0;
  'Getting firewood' = 1"))

dat.pt$carpeting.cb.d <- as.numeric(
  car::recode(dat.pt$carpeting_cb, 
              "'Using a yard stick' = 0;
  'Getting ready to remodel' = 1"))

dat.pt$house.cb.d <- as.numeric(
  car::recode(dat.pt$house_cb, 
              "'Vacuuming the floor' = 0;
  'Showing ones cleanliness' = 1"))

dat.pt$room.cb.d <- as.numeric(
  car::recode(dat.pt$room_cb, 
              "'Applying brush strokes' = 0;
  'Making the room look fresh' = 1"))

dat.pt$rent.cb.d <- as.numeric(
  car::recode(dat.pt$rent_cb, 
              "'Writing a check' = 0;
  'Maintaining a place to live' = 1"))

dat.pt$houseplants.cb.d <- as.numeric(
  car::recode(dat.pt$houseplants_cb, 
              "'Watering plants' = 0;
  'Making the room look nice' = 1"))

dat.pt$door.cb.d <- as.numeric(
  car::recode(dat.pt$door_cb, 
              "'Putting a key in the lock' = 0;
  'Securing the house' = 1"))

dat.pt$voting.cb.d <- as.numeric(
  car::recode(dat.pt$voting_cb, 
              "'Marking a ballot' = 0;
  'Influencing the election' = 1"))

dat.pt$climbing.cb.d <- as.numeric(
  car::recode(dat.pt$tree_cb.1, 
              "'Holding on to branches' = 0;
  'Getting a good view' = 1"))

dat.pt$personality.cb.d <- as.numeric(
  car::recode(dat.pt$presonality_cb, 
              "'Answering questions' = 0;
  'Revealing what you are like' = 1"))

dat.pt$toothbrushing.cb.d <- as.numeric(
  car::recode(dat.pt$toothbrushing_cb, 
              "'Moving a brush around in ones mouth' = 0;
  'Preventing tooth decay' = 1"))

dat.pt$test.cb.d <- as.numeric(
  car::recode(dat.pt$test_cb, 
              "'Answering questions' = 0;
  'Showing ones knowledge' = 1"))

dat.pt$greeting.cb.d <- as.numeric(
  car::recode(dat.pt$greeting_cb, 
              "'Saying hello' = 0;
  'Showing friendliness' = 1"))

dat.pt$temptation.cb.d <- as.numeric(
  car::recode(dat.pt$temptation_cb, 
              "'Saying \"no\"' = 0;
  'Showing moral courage' = 1"))

dat.pt$eating.cb.d <- as.numeric(
  car::recode(dat.pt$eating_cb, 
              "'Chewing and swallowing' = 0;
  'Getting nutrition' = 1"))

dat.pt$garden.cb.d <- as.numeric(
  car::recode(dat.pt$garden_cb, 
              "'Planting seeds' = 0;
  'Getting fresh vegetables' = 1"))

dat.pt$car.cb.d <- as.numeric(
  car::recode(dat.pt$travelling_cb, 
              "'Following a map' = 0;
  'Seeing the countryside' = 1"))

dat.pt$cavity.cb.d <- as.numeric(
  car::recode(dat.pt$cavity_cb, 
              "'Going to the dentist' = 0;
  'Protecting your teeth' = 1"))

dat.pt$child.cb.d <- as.numeric(
  car::recode(dat.pt$child_cb, 
              "'Using simple words' = 0;
  'Teaching a child something' = 1"))

dat.pt$doorbell.cb.d <- as.numeric(
  car::recode(dat.pt$doorbell_cb, 
              "'Moving a finger' = 0;
  'Seeing if someones home' = 1"))





# Close narrow
dat.pt$list.cn.d <- as.numeric(car::recode(dat.pt$list_cn, 
                                           "'Writing things down' = 0;
  'Getting organized' = 1"))

dat.pt$reading.cn.d <- as.numeric(
  car::recode(dat.pt$reading_cn, 
              "'Following lines of print' = 0;
  'Gaining knowledge' = 1"))

dat.pt$army.cn.d <- as.numeric(
  car::recode(dat.pt$army_cn, 
              "'Signing up' = 0;
  'Helping the Nations defense' = 1"))

dat.pt$clothes.cn.d <- as.numeric(
  car::recode(dat.pt$clothes_cn, 
              "'Putting clothes into the machine' = 0;
  'Removing odors from clothes' = 1"))

dat.pt$apple.cn.d <- as.numeric(
  car::recode(dat.pt$apple_cn, 
              "'Pulling an apple off a branch' = 0;
  'Getting something to eat' = 1"))

dat.pt$tree.cn.d <- as.numeric(
  car::recode(dat.pt$tree_cn, 
              "'Wielding an axe' = 0;
  'Getting firewood' = 1"))

dat.pt$carpeting.cn.d <- as.numeric(
  car::recode(dat.pt$carpeting_cn, 
              "'Using a yard stick' = 0;
  'Getting ready to remodel' = 1"))

dat.pt$house.cn.d <- as.numeric(
  car::recode(dat.pt$house_cn, 
              "'Vacuuming the floor' = 0;
  'Showing ones cleanliness' = 1"))

dat.pt$room.cn.d <- as.numeric(
  car::recode(dat.pt$room_cn, 
              "'Applying brush strokes' = 0;
  'Making the room look fresh' = 1"))

dat.pt$rent.cn.d <- as.numeric(
  car::recode(dat.pt$rent_cn, 
              "'Writing a check' = 0;
  'Maintaining a place to live' = 1"))

dat.pt$houseplants.cn.d <- as.numeric(
  car::recode(dat.pt$houseplants_cn, 
              "'Watering plants' = 0;
  'Making the room look nice' = 1"))

dat.pt$door.cn.d <- as.numeric(
  car::recode(dat.pt$door_cn, 
              "'Putting a key in the lock' = 0;
  'Securing the house' = 1"))

dat.pt$voting.cn.d <- as.numeric(
  car::recode(dat.pt$voting_cn, 
              "'Marking a ballot' = 0;
  'Influencing the election' = 1"))

dat.pt$climbing.cn.d <- as.numeric(
  car::recode(dat.pt$tree_cn.1, 
              "'Holding on to branches' = 0;
  'Getting a good view' = 1"))

dat.pt$personality.cn.d <- as.numeric(
  car::recode(dat.pt$personality_cn, 
              "'Answering questions' = 0;
  'Revealing what you are like' = 1"))


dat.pt$toothbrushing.cn.d <- as.numeric(
  car::recode(dat.pt$toothbrusing_cn, 
              "'Moving a brush around in ones mouth' = 0;
  'Preventing tooth decay' = 1"))

dat.pt$test.cn.d <- as.numeric(
  car::recode(dat.pt$test_cn, 
              "'Answering questions' = 0;
  'Showing ones knowledge' = 1"))

dat.pt$greeting.cn.d <- as.numeric(
  car::recode(dat.pt$greeting_cn, 
              "'Saying hello' = 0;
  'Showing friendliness' = 1"))

dat.pt$temptation.cn.d <- as.numeric(
  car::recode(dat.pt$temptation_cn, 
              "'Saying \"no\"' = 0;
  'Showing moral courage' = 1"))

dat.pt$eating.cn.d <- as.numeric(
  car::recode(dat.pt$eating_cn, 
              "'Chewing and swallowing' = 0;
  'Getting nutrition' = 1"))

dat.pt$garden.cn.d <- as.numeric(
  car::recode(dat.pt$garden_cn, 
              "'Planting seeds' = 0;
  'Getting fresh vegetables' = 1"))

dat.pt$car.cn.d <- as.numeric(
  car::recode(dat.pt$car_cn, 
              "'Following a map' = 0;
  'Seeing the countryside' = 1"))

dat.pt$cavity.cn.d <- as.numeric(
  car::recode(dat.pt$cavity_cn, 
              "'Going to the dentist' = 0;
  'Protecting your teeth' = 1"))

dat.pt$child.cn.d <- as.numeric(
  car::recode(dat.pt$child_cn, 
              "'Using simple words' = 0;
  'Teaching a child something' = 1"))

dat.pt$doorbell.cn.d <- as.numeric(
  car::recode(dat.pt$doorbell_cn, 
              "'Moving a finger' = 0;
  'Seeing if someones home' = 1"))






# -- Quality Checks ------------------------------------------------------------

# Attention Check 
# CN
table(dat.pt$attention.check)
dat.pt <- subset(dat.pt, (dat.pt$attention.check != 'Next week'))
dat.pt <- subset(dat.pt, (dat.pt$attention.check != 'Other (please specify)'))
# DN
table(dat.pt$Q101)
dat.pt <- subset(dat.pt, dat.pt$Q101 != "Other (please specify)")
dat.pt <- subset(dat.pt, dat.pt$Q101 != "Next year")
dat.pt <- subset(dat.pt, dat.pt$Q101 != "Thursday next week")
# CB
table(dat.pt$Q147)
dat.pt <- subset(dat.pt, dat.pt$Q147 != "Other (please specify)")
dat.pt <- subset(dat.pt, dat.pt$Q147 != "Next year")
dat.pt <- subset(dat.pt, dat.pt$Q147 != "Thursday next week")
# DB
table(dat.pt$Q180)
dat.pt <- subset(dat.pt, dat.pt$Q180 != "Other (please specify)")
dat.pt <- subset(dat.pt, dat.pt$Q180 != "June 30th next year")
dat.pt <- subset(dat.pt, dat.pt$Q180 != "Next week")

# How well understood the language (1,2,3 out)
table(dat.pt$Q47)
dat.pt <- subset(dat.pt, dat.pt$Q47 != '3')
dat.pt <- subset(dat.pt, dat.pt$Q47 != '2')

# Questions without reading (the entire time out)
table(dat.pt$Q52)
dat.pt <- subset(dat.pt, dat.pt$Q52 != 'The entire time')

# Did you hurry? (empty and 4 out. Sometimes is middle category)
table(dat.pt$Q49)
dat.pt <- subset(dat.pt, dat.pt$Q49 != '4')

# How long did people take?
# On average people took about 8-9 minutes
summary(dat.pt$Duration..in.seconds.)
# exclude on the basis of rushing and lagging
dat.pt <- subset(dat.pt, dat.pt$Duration..in.seconds. < quantile(dat.pt$Duration..in.seconds., 0.9))
dat.pt <- subset(dat.pt, dat.pt$Duration..in.seconds. > quantile(dat.pt$Duration..in.seconds., 0.1))

# exclude who took the survey more than once (4 cases)
table(table(dat.pt$Q178) > 1)
dat.pt$Q178[table(dat.pt$Q178) > 1]
dat.pt <- subset(dat.pt, table(dat.pt$Q178) == 1)

# Exclude who guessed the specific hypothesis
dat.pt <- subset(dat.pt, dat.pt$Q56 != "Yo ser if long vs short term thinking influences how you perceive tasks (literally vs overall purpose)")
dat.pt <- subset(dat.pt, dat.pt$Q56 != "Maybe something in relation to whether how we view things differs based on whether it’s something in the near or distant future e.g whether we see the task as an action or more of a holistic event")
dat.pt <- subset(dat.pt, dat.pt$Q56 != "Depending on how far in the future we were supposed to imagine, does this change our interpretation of events as being literal or more emotive and goal oriented?")



# -- Description of Sample -----------------------------------------------------
# age
summary(dat.pt$Q171)
sd(dat.pt$Q171, na.rm = TRUE)

# gender
table(dat.pt$Q173)

# where do they come from?
lon <- dat.pt$LocationLongitude
lat <- dat.pt$LocationLatitude
# draw on map 
df <- data.frame(longitude = lon, 
                 latitude = lat)
df <- na.omit(df)
coordinates(df) <- ~longitude+latitude
leaflet(df) %>% addMarkers() %>% addTiles()

# counting by hand from map: 
# 17 from Nort America 
# 14 Africa 
# 1 South Corea
# 9 Australia and NewZealand
# rest Europe
dim(dat.pt)[1] - (17+14+1+9)


# -- Aggregate BIF Scores ------------------------------------------------------
dat.dn.d <- dat.pt[,grepl('dn.d', names(dat.pt))]
scores.dn.d <- apply(dat.dn.d, 1, sum, na.rm=TRUE)

dat.db.d <- dat.pt[,grepl('db.d', names(dat.pt))]
scores.db.d <- apply(dat.db.d, 1, sum, na.rm=TRUE)

dat.cn.d <- dat.pt[,grepl('cn.d', names(dat.pt))]
scores.cn.d <- apply(dat.cn.d, 1, sum, na.rm=TRUE)

dat.cb.d <- dat.pt[,grepl('cb.d', names(dat.pt))]
scores.cb.d <- apply(dat.cb.d, 1, sum, na.rm=TRUE)

dat.pt$bif.score <- scores.dn.d + scores.db.d + scores.cn.d + scores.cb.d



# -- Analysis ------------------------------------------------------------------
# defining colours
v.colors <- viridis_pal(option = 'plasma')(5)

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red=curcoldata[1], green=curcoldata[2],
        blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

v.colors.t <- makeTransparent(v.colors, 80)


# plot BIF scores
pdf('../out/BIF_time_treatments.pdf', width = 7, height = 5)
par(mar = c(4,6,1,1))
cn.scores <- dat.pt$bif.score[dat.pt$treatment == 'cn']
cb.scores <- dat.pt$bif.score[dat.pt$treatment == 'cb']
dn.scores <- dat.pt$bif.score[dat.pt$treatment == 'dn']
db.scores <- dat.pt$bif.score[dat.pt$treatment == 'db']

boxplot(cn.scores, cb.scores, dn.scores,  db.scores,
        ylim = c(0,25),
        xlab = 'Abstract Answers per Respondent', ylab = '',
        # horizontal = TRUE,axes = FALSE, col = c('grey30','grey30',
        #                                         'grey80', 'grey80'))
        horizontal = TRUE,axes = FALSE, col = c(v.colors[4],v.colors[4],
                                                v.colors[1], v.colors[1]))
axis(2, col = "white", las = 1, at = c(1,2,3,4),
     lab =c("Next Week\nThursday",
            "Next Week", "Next Year\nJune 30th", "Next Year"))
axis(1)
dev.off()


# Reporting Descriptives
mean(dat.pt$bif.score[dat.pt$treatment == 'cn'], na.rm = TRUE)
mean(dat.pt$bif.score[dat.pt$treatment == 'cb'], na.rm = TRUE)
mean(dat.pt$bif.score[dat.pt$treatment == 'dn'], na.rm = TRUE)
mean(dat.pt$bif.score[dat.pt$treatment == 'db'], na.rm = TRUE)

sd(dat.pt$bif.score[dat.pt$treatment == 'cn'], na.rm = TRUE)
sd(dat.pt$bif.score[dat.pt$treatment == 'cb'], na.rm = TRUE)
sd(dat.pt$bif.score[dat.pt$treatment == 'dn'], na.rm = TRUE)
sd(dat.pt$bif.score[dat.pt$treatment == 'db'], na.rm = TRUE)

median(dat.pt$bif.score[dat.pt$treatment == 'cn'], na.rm = TRUE)
median(dat.pt$bif.score[dat.pt$treatment == 'cb'], na.rm = TRUE)
median(dat.pt$bif.score[dat.pt$treatment == 'dn'], na.rm = TRUE)
median(dat.pt$bif.score[dat.pt$treatment == 'db'], na.rm = TRUE)


# Reporting Inference
# Temporal Distance
t.test(dat.pt$bif.score ~ dat.pt$temp.distance.long)
sd(dat.pt$bif.score[dat.pt$temp.distance.long == TRUE])
sd(dat.pt$bif.score[dat.pt$temp.distance.long == FALSE])

# Temporal Resolution
t.test(dat.pt$bif.score ~ dat.pt$temp.resolution.broad)
sd(dat.pt$bif.score[dat.pt$temp.resolution.broad == TRUE])
sd(dat.pt$bif.score[dat.pt$temp.resolution.broad == FALSE])

# Linear model because of imbalanced samples
lm.m <- lm(bif.score ~  temp.distance.long + temp.resolution.broad , 
           data = dat.pt)
summary(lm.m)

