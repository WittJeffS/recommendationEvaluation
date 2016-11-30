######################################################################
######################################################################

# loads px.ls object R-drive and merges px.ls and rec.ls by ticker and
# save merged list object as rec.ls in working directory

######################################################################
######################################################################

library(plyr)

# load required list objects
load('R:/David/R/Recommendation Evaluation/px.ls.RData')
load('rec.ls.RData')

# reformat and save px.ls element names
nm <- tolower(names(px.ls))
names(px.ls) <- nm

# format date column of each element of px.ls as %Y%m%d
f <- function(tick){
        dt <- px.ls[[tick]]$date
        dt <- format(dt, format = '%Y%m%d')
        dt <- as.character(dt)
        
        px.ls[[tick]]$date <- dt
        
        return(px.ls[[tick]])
}

px.ls <- lapply(names(px.ls), f)
names(px.ls) <- nm

# for each ticker merge associated px.ls data.frame and rec.ls 
# data.frame by date column
g <- function(tick){
        df1 <- px.ls[[tick]]
        df2 <- rec.ls[[tick]]
        
        df <- merge(x=df1, y = df2, by = 'date', all = TRUE)
        
        return(df)
}

nm <- names(rec.ls)
rec.ls <- lapply(names(rec.ls), g)
names(rec.ls) <- nm

# save rec.ls to working directory
save(rec.ls, file = 'rec.ls.RData')

