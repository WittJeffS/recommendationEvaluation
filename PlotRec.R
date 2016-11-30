PlotRec <- function(tick){
        
        library(reshape2)
        library(ggplot2)
        
        load('rec.ls.RData')
        
        dat <- rec.ls[[tick]]

        dat$date <- as.Date(dat$date, format = '%Y%m%d')
        
        dat.rec <- data.frame(date = dat$date,
                              rec  = dat$rec)
        dat.rec$rec <- factor(dat.rec$rec,
                          levels = c('BUY','HOLD',
                                     'SOURCE','SELL','EVAL'),
                          labels = c('Buy','Hold',
                                     'Source','Sell', 'Eval'))
        
        ran <- data.frame(date      = dat$date,
                          eval.low  = dat$eval.low,
                          eval.high = dat$eval.high,
                          buy.low   = dat$buy.low,
                          buy.high  = dat$buy.high)
        
        ran.eval <- data.frame(date = ran$date, 
                               low  = ran$eval.low, 
                               high = ran$eval.high)
        ran.buy <- data.frame(date  = ran$date, 
                              low   = ran$buy.low, 
                              high  = ran$buy.high)
        
        ran.eval$type <- rep('Eval Range', length(ran.eval$date))
        ran.buy$type <- rep('Buy Range', length(ran.buy$date))
        
        ran <- rbind.data.frame(ran.eval, ran.buy)
        
        ran$type <- factor(ran$type, 
                           levels = c('Eval Range',
                                      'Buy Range'),
                           labels = c('Eval Range',
                                      'Buy Range'))
        
        dat.ln <- data.frame(date = dat$date,
                             pt   = dat$pt,
                             px   = dat$PX_LAST,
                             st   = dat$BEST_TARGET_PRICE)
        
        long.ln <- melt(dat.ln, id.vars = 'date')
        long.ln$variable <- factor(long.ln$variable,
                                   levels = c('st','pt','px'))
        
        g <- ggplot(dat, aes(x = date)) + 
                geom_ribbon(aes(ymax = eval.high, ymin = eval.low), 
                            fill = 'grey') +
                geom_ribbon(aes(ymax = buy.high, ymin = buy.low), 
                            fill = 'darkgrey',
                            color = 'black', 
                            size = 2) +
                geom_line(aes(y = pt), 
                          color = 'red') +
                geom_line(aes(y = PX_LAST), 
                          color = 'black',
                          size = 2) +
                geom_line(aes(y = BEST_TARGET_PRICE))
        
        xx <- ggplot_build(g)
        lo <- xx$layout$panel_ranges[[1]]$y.range[1]
        hi <- xx$layout$panel_ranges[[1]]$y.range[2]
        
        
        p <- ggplot() +
                geom_ribbon(data = dat.rec[!is.na(dat.rec$rec),],
                            aes(x = date,
                                ymax = hi,
                                ymin = lo,
                                fill = rec),
                            alpha = 1) +
                # geom_ribbon(data = ran[!is.na(ran$high),],
                #             aes(x     = date, 
                #                 ymax  = high, 
                #                 ymin  = low, 
                #                 fill  = type)) +
                scale_fill_manual(
                        values = c('darkolivegreen3','khaki',
                                   'red3','red4','lightblue3'),
                        guide =guide_legend(title = 'Recommendations'),
                        drop = FALSE) +
                # scale_fill_manual(
                #         values = c('grey','grey50'),
                #         guide =guide_legend(title = 'Ranges')) +
                geom_line(data =long.ln[!is.na(long.ln$value),],
                          aes(x     = date, 
                              y     = value, 
                              color = variable),
                          size = 2) +
                scale_color_manual(
                        values = c('#CBC3B7','#850237','black'),
                        labels = c('Consensus Estimate',
                                   'PAM Price Target',
                                   'Share Price'),
                        guide = guide_legend(title = 'Prices')) +
                theme_bw() +
                theme(text = element_text(family = 'Palatino'),
                      legend.margin = unit(.2,'lines'),
                      legend.position = 'bottom',
                      legend.box = 'horizontal',
                      legend.box.just = 'left',
                      legend.text = element_text(size = 9),
                      legend.title = element_text(face= 'bold',
                                                  size = 9),
                      legend.key = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank()) +
                ggtitle(paste(toupper(tick), 'Recommendation History'))
        
        p
        
        file.name <- paste0(tick,'.pdf')
        
        # ggsave(file.name, height = 8.25, width = 11)
        return(p)
}