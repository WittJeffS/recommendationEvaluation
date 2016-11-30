######################################################################
######################################################################

# a function used during a call to GetNewFilesInDir(). ImportPortrev()
# loads .csv files in R:/David/portrev/csv by date as a data.frame 
# object and then prunes the data.frame to just the columns required
# for future plotting.

######################################################################
######################################################################

test123

ImportPortRev <- function(x){
        # read .csv file corresponding to date argument
        file.path <- 'R:/David/portRev/csv/'
        file.name <- paste0(file.path, x, '.csv')
        dat <- read.csv(file.name, stringsAsFactors = F,
                        na.strings = c('', ' '))
        
        # trim data frame to desired columns and rows
        dat <- dat[,c(3,   # company name
                      4,   # ticker symbol
                      6,   # recommendation
                      9,   # pam fair value estimate
                      13,  # lower eval level
                      14,  # lower end of buy range
                      16,  # upper end of buy range
                      17)] # upper eval level
        
        last.row <- as.numeric(row.names(dat[is.na(dat),])[2]) - 1
        dat <- dat[3:last.row,]
        
        # format data.frame appropriately
        names(dat) <- c('name','tick','rec','pt','eval.low', 
                        'buy.low', 'buy.high','eval.high')
        
        row.names(dat) <- tolower(dat$tick)
        
        dat$pt <- as.numeric(gsub('\\$','',gsub(',','',dat$pt)))
        dat$eval.low  <- as.numeric(dat$eval.low)
        dat$buy.low   <- as.numeric(dat$buy.low)
        dat$buy.high  <- as.numeric(dat$buy.high)
        dat$eval.high <- as.numeric(dat$eval.high)
        
        return(dat)}