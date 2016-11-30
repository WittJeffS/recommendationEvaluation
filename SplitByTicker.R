######################################################################
######################################################################

# transforms a list object, portrev.ls, which is organized by date 
# into a list object, rec.ls, which is organized by ticker. Saves 
# rec.ls to working directory.

######################################################################
######################################################################

library(data.table)

load('portrev.ls.RData')

# add a date column to each data.frame in portrev.ls
AddDateVariable <- function(i){
        dat <- portrev.ls[[i]]
        dat$date <- rep(i, length(dat$tick))
        
        return(dat)}

list.names <- names(portrev.ls)
portrev.ls <- lapply(list.names, function(i) AddDateVariable(i))
names(portrev.ls) <- list.names

# combine each element of portrev.ls, which are data.frames organized
# by date, into a single data.frame
portrev.df <- rbindlist(portrev.ls)

# split portrev.df into a list object organized by ticker symbol
rec.ls <- split(portrev.df, portrev.df$tick)
names(rec.ls) <- tolower(names(rec.ls))

# save rec.ls to working directory
save(rec.ls, file = 'rec.ls.RData')
