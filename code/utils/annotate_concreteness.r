# ------------------------------------------------------------------------------
# Helpers for annotating concreteness
# 
# Perceiving Distances and Construing Targets Based on Communication
# Chris Arnold, Cardiff University
# July 2021
# ------------------------------------------------------------------------------


# Concreteness per article
# Function to calculate it 
calc.concreteness <- function(article, brysbaert.dat = brysbaert.dat){
  conc.article <- 0 
  nr.words.in.dict <- 0
  word.vals.in.dict <- 0
  # chop up article in words
  article.lower <- tolower(article)
  article.no.punct <- gsub("[[:punct:][:blank:]]+", " ", article.lower) 
  article.cleaned <- strsplit(article.no.punct, ' ')[[1]]
  for (i in seq(1,length(article.cleaned))){
    # check wether a word is in the dictionary. If so sum up.
    try(  # To be save against empty words
      if (sum(brysbaert.dat$Word == article.cleaned[i]) == 1){
        word.conc.value <- brysbaert.dat$Conc.M[brysbaert.dat$Word == article.cleaned[i]]
        # summing up
        conc.article <- conc.article + word.conc.value
        # counter
        nr.words.in.dict <- nr.words.in.dict + sum(brysbaert.dat$Word == article.cleaned[i])  
        # collect all values for the words in a sentence 
        word.vals.in.dict <- c(word.vals.in.dict, word.conc.value)
      }
    )
  }
  word.vals.in.dict <- word.vals.in.dict[-1]
  article.cleaned.sd <- sd(word.vals.in.dict)
  article.cleaned.median <- median(word.vals.in.dict)
  article.cleaned.mean <- mean(word.vals.in.dict)
  return(c(conc.article, nr.words.in.dict,
           article.cleaned.sd, article.cleaned.median, article.cleaned.mean))
}


run.loop <- FALSE
if (run.loop){
  # Loops over each article
  one.percent <- round(length(NYT.corpus)/100)
  ten.percent <- 10 * one.percent
  
  concreteness.values <- data.frame(0,0,0,0,0)
  names(concreteness.values) <- c('conc.article', "nr.words.in.dict",
                                  "article.cleaned.sd", "article.cleaned.median",
                                  "article.cleaned.mean")
  start_time <- Sys.time()
  for (j in seq(1, length(NYT.corpus))){
    # quick progress bar with a '+' all ten percents
    if ((j %% one.percent) == 0) cat('-')
    if ((j %% ten.percent) == 0) {
      end_time <- Sys.time()
      cat('\n already running for')
      cat(round(end_time- start_time), 2)
      cat('minutes \n')
    }
    concreteness.values <- rbind(concreteness.values,
                                 calc.concreteness(NYT.corpus[[j]]$article.text))
  }
  concreteness.values <- concreteness.values[-1,]
}
















# 
# 
# # Concreteness per article
# # Function to calculate it 
# calc.concreteness <- function(article, brysbaert.dat = brysbaert.dat){
#   conc.article <- 0 
#   nr.words.in.dict <- 0
#   word.vals.in.dict <- 0
#   # chop up article in words
#   article.lower <- tolower(article)
#   article.no.punct <- gsub("[[:punct:][:blank:]]+", " ", article.lower) 
#   article.cleaned <- strsplit(article.no.punct, ' ')[[1]]
#   for (i in seq(1,length(article.cleaned))){
#     # check wether a word is in the dictionary. If so sum up.
#     try(  # To be save against empty words
#       if (sum(brysbaert.dat$Word == article.cleaned[i]) == 1){
#         word.conc.value <- brysbaert.dat$Conc.M[brysbaert.dat$Word == article.cleaned[i]]
#         # summing up
#         conc.article <- conc.article + word.conc.value
#         # counter
#         nr.words.in.dict <- nr.words.in.dict + sum(brysbaert.dat$Word == article.cleaned[i])  
#         # collect all values for the words in a sentence 
#         word.vals.in.dict <- c(word.vals.in.dict, word.conc.value)
#       }
#     )
#   }
#   word.vals.in.dict <- word.vals.in.dict[-1]
#   article.cleaned.sd <- sd(word.vals.in.dict)
#   article.cleaned.median <- median(word.vals.in.dict)
#   article.cleaned.mean <- mean(word.vals.in.dict)
#   return(c(conc.article, nr.words.in.dict,
#            article.cleaned.sd, article.cleaned.median, article.cleaned.mean))
# }
# 
# 
# # run <- FALSE
# # 
# # if (run){
# #   # Loops over each article
# #   one.percent <- round(length(NYT.corpus)/100)
# #   ten.percent <- 10 * one.percent
# #   
# #   concreteness.values <- data.frame(0,0,0,0,0)
# #   names(concreteness.values) <- c('conc.article', "nr.words.in.dict",
# #                                   "article.cleaned.sd", "article.cleaned.median",
# #                                   "article.cleaned.mean")
# #   start_time <- Sys.time()
# #   for (j in seq(1, length(NYT.corpus))){
# #     # quick progress bar with a '+' all ten percents
# #     if ((j %% one.percent) == 0) cat('-')
# #     if ((j %% ten.percent) == 0) {
# #       end_time <- Sys.time()
# #       cat('\n already running for')
# #       cat(round(end_time- start_time), 2)
# #       cat('minutes \n')
# #     }
# #     concreteness.values <- rbind(concreteness.values,
# #                                  calc.concreteness(NYT.corpus[[j]]$article.text))
# #   }
# #   concreteness.values <- concreteness.values[-1,]
# # }
# # 
# # # dim(concreteness.values)
# # 
# # # save(concreteness.values, file = '../../data/NYT_corpus_concreteness_values.RData')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
