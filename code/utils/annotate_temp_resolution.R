# ------------------------------------------------------------------------------
# Studying the Construal of Articles in the NYT Corpus
# Annotating Temporal Resolution from TIMEX 3 Annotations
# Chris Arnold, Cardiff University
# July 2021
# ------------------------------------------------------------------------------



# Function to annotate one temporal resolution
annotate.one.temp.resolution <- function(one.TIMEX3.annotation, counters.list){
  # find the type and value
  type.mask <- names(one.TIMEX3.annotation) =='type'
  value.mask <- names(one.TIMEX3.annotation) =='value'
  # get the type and value
  type <- one.TIMEX3.annotation[type.mask]
  value <- one.TIMEX3.annotation[value.mask]
  # Select from DATE
  if (type == 'DATE' & length(value)!=0){
    # Years
    #check whether year in format '2001'
    if (grepl("^(1|2)(9|0)\\d{2}$", value)) {
      counters.list$mentions.counter.1.y <- counters.list$mentions.counter.1.y +1
    }
    # Months
    #check whether month in format '1989-01'
    if (grepl("^(1|2)(9|0)\\d{2}-(0|1)\\d$", value)) {
      counters.list$mentions.counter.1.m <- counters.list$mentions.counter.1.m +1
    }
    # Weeks
    #check whether week in format '1989-W01'
    if (grepl("^(1|2)(0|1)\\d{2}-W\\d{2}$", value)) {
      counters.list$mentions.counter.1.w <- counters.list$mentions.counter.1.w +1
    }
    # Days
    #check whether day in format '1987-01-01'
    if (grepl('^(1|2)(9|0)\\d{2}-(0|1)\\d-(0|1|2|3)\\d$', value)) {
      counters.list$mentions.counter.1.d <- counters.list$mentions.counter.1.d +1
    }
    # checks any future ref
    if (value == 'FUTURE_REF') {
      counters.list$future.ref.counter <- counters.list$future.ref.counter +1
    }
    # checks any past ref
    if (value == 'FUTURE_REF') {
      counters.list$past.ref.counter <- counters.list$past.ref.counter +1
    }
    # References to Spring, Summer, Autumn and Winter
    #check whether month in format '1989-01'
    if (grepl("^(1|2)(9|0)\\d{2}-[S|F|W][P|U|A|I]$", value)) {
      counters.list$mentions.counter.3.m <- counters.list$mentions.counter.3.m +1
    }
  }
  # Select from Duration
  if (type == 'DURATION' & length(value)!=0){ 
    # Years checks whether year in format 'PX+Y', so P1Y or P30Y or P300Y etc.
    if (grepl('^P\\d+Y$', value)) {
      # extract the number of units
      temp.horizon.count <- gsub("P(\\d+)Y", '\\1',value)
      # extract which unit, i.e. year, month, week etc.
      temp.horizon.unit <- tolower(gsub("P\\d+([A-Z])", '\\1',value))
      # generate a variable name
      varname <- paste('mentions.counter',temp.horizon.count,temp.horizon.unit, sep = '.')
      # check if the X year is still missing 
      if (!is.element(varname, names(counters.list))){
        # if not there, create the variable 
        counters.list <- append(counters.list, 0)
        names(counters.list)[length(counters.list)] <- varname
      }
      counters.list[[varname]] <- counters.list[[varname]] + 1
    }
    # Months 
    # Weeks 
    # Days
  }
  # annotate all explicit time mentions as 'smaller than a day'
  value <- '1987-01-01T1'
  if (type == 'TIME' & length(value)!=0){
    if (grepl('(1|2)(9|0)\\d{2}-(0|1)\\d-(0|1|2|3)\\d', value)) {
      counters.list$mentions.counter.smaller.1.d <- counters.list$mentions.counter.smaller.1.d +1
    }
  }
  return(counters.list)
}



# Function to annotate all temp expressions of one document
annotate.temp.resolution <- function(one.doc.TIMEX3){
  # Variable creation
  # define return object
  counters.list <- list()
  counters.list$mentions.counter.1000.y <- 0
  counters.list$mentions.counter.100.y <- 0
  counters.list$mentions.counter.10.y <- 0
  counters.list$mentions.counter.9.y <- 0
  counters.list$mentions.counter.8.y <- 0
  counters.list$mentions.counter.7.y <- 0
  counters.list$mentions.counter.6.y <- 0
  counters.list$mentions.counter.5.y <- 0
  counters.list$mentions.counter.4.y <- 0
  counters.list$mentions.counter.3.y <- 0
  counters.list$mentions.counter.2.y <- 0
  counters.list$mentions.counter.1.y <- 0
  counters.list$mentions.counter.1.m <- 0
  counters.list$mentions.counter.1.w <- 0
  counters.list$mentions.counter.1.d <- 0
  counters.list$future.ref.counter <- 0
  counters.list$past.ref.counter <- 0
  counters.list$mentions.counter.3.m <- 0
  counters.list$mentions.counter.smaller.1.d <- 0
  # Apply 
  for (i in seq(1, length(one.doc.TIMEX3))){
    if (length(one.doc.TIMEX3)>0){
      counters.list <- annotate.one.temp.resolution(one.doc.TIMEX3[[i]],
                                                    counters.list)  
    }
  }
  return(counters.list)
}


# Wrap them all
tempres.counters.list.all.docs <- list()
for (i in seq(1, length(NYT.corpus))){
  #  quick progress bar
  if (i==1) cat(strrep('=', floor(length(NYT.corpus)/1000)), '\n')
  if ((i %% 1000) == 0) cat('*') 
  tempres.counters.list.all.docs[[i]] <- annotate.temp.resolution(NYT.corpus[[i]]$TIMEX3)
}



















