# ------------------------------------------------------------------------------
# Studying the Construal of Articles in the NYT Corpus
# Helpers for Reading Data
# Chris Arnold, Cardiff University
# July 2021
# ------------------------------------------------------------------------------

NYT.data.extractor <- function(one.file){
  # Generate a list with the necessary data 
  dat <- read_xml(paste(xml.dat.location, one.file, sep = ''), version = '1.0')
  # Find the date -----
  # get head
  head.children <- xml_children( xml_find_all(dat, '//head') ) 
  # get last element
  last.element <- xml_attrs(head.children)[[length(xml_attrs(head.children))]]
  # get date 
  pub.date <- last.element[names(last.element)=='date.publication']
  # convert into date 
  pub.date <- as.Date(pub.date, format = '%Y%m%d')
  # define election day ----
  election.year <- substr(one.file, 1, 4)
  if (election.year == 1988) election.date <- as.Date('1988-11-08')
  else if (election.year == 1992) election.date <- as.Date('1992-11-03')
  else if (election.year == 1996) election.date <- as.Date('1996-11-05')
  else if (election.year == 2000) election.date <- as.Date('2000-11-07')
  else if (election.year == 2004) election.date <- as.Date('2004-11-07')
  # Calculate temporal horizon 
  temp.horizon <- pub.date - election.date
  # all TIMEX3 annotations -----
  TIMEX3 <- xml_attrs(xml_find_all(dat, '//TIMEX3'))
  # find the full body text of the article ----
  body.content.children <- xml_children( xml_find_all(dat, '//body//body.content') ) 
  article.text <- xml_text(body.content.children)[2]
  # add into output 
  return.obj <- list(pub.date, election.date, temp.horizon, TIMEX3, article.text)
  names(return.obj) <- c("pub.date", "election.date", "temp.horizon", 
                         "TIMEX3", "article.text")
  return(return.obj)
}

