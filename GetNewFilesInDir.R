######################################################################
######################################################################

# a function that checks for new files in R:/David/portrev/csv, if any
# exist adds them to portrev.ls, and saves portrev.ls to the working
# directory

######################################################################
######################################################################

# load existing portrev list object organized by date
load(paste0('portrev.ls','.RData'))
ls <- portrev.ls
in.list <- names(ls)

# get list of portrev files in directory on R-drive
direct <- 'R:/David/portRev/csv'
files <- as.data.frame(dir(direct), stringsAsFactors = F)
in.dir <- substr(files[,1], 1,8)
in.dir <- as.Date(in.dir, format = '%m%d%Y')
in.dir <- format(in.dir, '%Y%m%d')

# compare directory to existing portrev list object
included <- in.dir %in% in.list

# add data to portrev list object from new files on R-drive if
#  necessary
if(sum(included) == length(included)){
        print('portrev.ls is up to date.')
        
} else {
        not.included <- in.dir[!included]
        not.included.Ymd <- as.Date(not.included, 
                                    format = '%Y%m%d')
        not.included.mdy <- format(not.included.Ymd, 
                                   format = '%m%d%Y')
        
        # create a list of data to be added
        source('ImportPortRev.R')
        new.elements <- lapply(not.included.mdy, ImportPortRev)
        names(new.elements) <- not.included
        
        # combine new and old lists, reorder
        ls <- c(new.elements, ls)
        ls.names <- as.numeric(names(ls))
        ls.order <- order(ls.names, decreasing = TRUE)
        portrev.ls <-ls[ls.order]
        
        save(portrev.ls, file = 'portrev.ls.RData')
        
        print('portrev.ls update complete')}
