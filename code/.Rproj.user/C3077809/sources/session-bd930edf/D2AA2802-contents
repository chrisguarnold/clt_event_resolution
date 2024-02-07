# ------------------------------------------------------------------------------
# Studying the effects from the other studies 
# 
# Perceiving Distances and Construing Targets Based on Communication
# Chris Arnold, Cardiff University
# June 2023
# ------------------------------------------------------------------------------

# -- Housekeeping --------------------------------------------------------------
cat("Running the Metastudy: Studying Time Treatments in CLT Literature \n")

# -- Read Data
dat <- data.frame(
  read_excel('../data/Time_Experiments_Annotated.xlsx', 
                  sheet = 'Direct Effect Studies')
)

# -- Explore Data --------------------------------------------------------------
# Count the treatment pairs
dat$treat.CN.DB <- !is.na(dat$Treatment.CN) & !is.na(dat$Treatment.DB)
dat$treat.CN.DN <- !is.na(dat$Treatment.CN) & !is.na(dat$Treatment.DN)
dat$treat.CN.CB <- !is.na(dat$Treatment.CN) & !is.na(dat$Treatment.CB)
dat$treat.CB.DN <- !is.na(dat$Treatment.CB) & !is.na(dat$Treatment.DN)
dat$treat.CB.DB <- !is.na(dat$Treatment.CB) & !is.na(dat$Treatment.DB)
dat$treat.DN.DB <- !is.na(dat$Treatment.DN) & !is.na(dat$Treatment.DB)

nr.treat.CN.DB <- table(dat$treat.CN.DB)[2]
nr.treat.CN.DN <- table(dat$treat.CN.DN)[2]
nr.treat.CN.CB <- table(dat$treat.CN.CB)[2]
nr.treat.CB.DN <- table(dat$treat.CB.DN)[2]
nr.treat.CB.DB <- table(dat$treat.CB.DB)[2]
nr.treat.DN.DB <- table(dat$treat.DN.DB)[2]

nr.treat.CN.DB[is.na(nr.treat.CN.DB)] <- 0
nr.treat.CN.DN[is.na(nr.treat.CN.DN)] <- 0
nr.treat.CN.CB[is.na(nr.treat.CN.CB)] <- 0
nr.treat.CB.DN[is.na(nr.treat.CB.DN)] <- 0
nr.treat.CB.DB[is.na(nr.treat.CB.DB)] <- 0
nr.treat.DN.DB[is.na(nr.treat.DN.DB)] <- 0

dat$treat.resolution.ok <- dat$treat.CN.DN == TRUE | dat$treat.CB.DB == TRUE
table(dat$treat.resolution.ok)

# plot
pdf('../out/meta_study.pdf', width = 9, height = 3)
par(mar = c(2,15,1,1))
barplot(table(dat$treat.resolution.ok), 
        las = 1, border = NA, col = v.colors[1], 
        xlim = c(0, 100), 
        names = c('Jointly Manipulating Distance \n and  Mesurement Unit',
                'Manipulating Distance Controlling \n for Measurement Unit'),
        horiz = TRUE
)
dev.off()
