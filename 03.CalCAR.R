library(tidyverse)
library(R.matlab)
library(ggplot2)


# Input data of a window ======
datdir <- '~/NutSync/MyData/QEAData/CH3/'
# Specifying trem information
Accprd <- as.Date('2017-09-30')
Pretype <- '6'
Markettype <- '21'
weekterm <- 'wekbind'

Stkcd <- paste(Accprd, Pretype, Markettype, weekterm, sep='_') %>% 
    paste0('.*?est_stkcd[.]csv$') %>% 
    dir(datdir, pattern = .) %>% 
    paste0(datdir, .) %>% 
    read_delim(delim=',', na = '')

stkest <- paste(Accprd, Pretype, Markettype, weekterm, sep='_') %>% 
    paste0('.*?est_TradStat[.]csv$') %>% 
    dir(datdir, pattern = .) %>% 
    paste0(datdir, .) %>% 
    read_delim(delim=',', na = '')

stkeve <- paste(Accprd, Pretype, Markettype, weekterm, sep='_') %>% 
    paste0('.*?eve_TradStat[.]csv$') %>% 
    dir(datdir, pattern = .) %>% 
    paste0(datdir, .) %>% 
    read_delim(delim=',', na = '') 


T <- 81L
N <- nrow(stkeve)/T
# we abandon part of event window for data visualization beauty
abdtau <- sapply(1:N, function(x) {seq(1, 20) + (x-1)*T}, simplify = TRUE) %>% as.vector()
stkeve <- stkeve[-abdtau,]
timeline <- c(-20:+40) # Note that the number, 20, at there 
# is calculated by 80/2 and then minus 20 setting above  


## Read the result from MATLAB with applying algorithm in PLS(su, 2016) ========
PLSpath <- "~/NutSync/QEA/Matlab_PLS"

# Group information
PLSclus <- readMat(file.path(PLSpath, paste('group', Accprd, Pretype, Markettype, weekterm, 
                                            'CH3', 'SMB', 'VMG', '3', sep = '_') %>% 
                                      paste0('.mat'))) %>% `[[`(1) 
colnames(PLSclus) <- c("group")
grpnum <- length(unique(PLSclus))
# grouped stock code
PLSgrp <- data.frame()
samnum <- c()
for (i in 1:grpnum) {
    sam <- Stkcd[PLSclus==i, 1]
    samn <- nrow(sam)
    sam <- cbind(sam,c(i))
    PLSgrp <- rbind(PLSgrp, sam)
    samnum <- c(samnum,samn)
}
rm(sam,samn,PLSclus)
colnames(PLSgrp) <- c('Stkcd', 'group')
PLSgrp$Stkcd <- as.character(PLSgrp$Stkcd)


# PLS coefficients
PLScoef <- readMat(file.path(PLSpath, paste('est_PLS',Accprd, Pretype, Markettype, weekterm, 
                                            'CH3', 'SMB', 'VMG', '3',sep = '_') %>% 
                                      paste0('.mat'))) %>% `[[`(1)
PLScolnam <- c()
for (i in 1:grpnum) {
    colnam <- paste0('g', i, '_', c('coef', 'sd', 't'))
    PLScolnam <- c(PLScolnam ,colnam)
}
colnames(PLScoef) <- PLScolnam; rm(colnam)


## decide Asset pricing model, CAPM, FF 3-factors or FF 5-factors? ====
if (nrow(PLScoef)==3) { # 3 factors
    stkff <- subset(stkest, select=c(Stkcd, Dret_rf, mkt_rf, SMB, VMG))
    modeltype <- 'FF3'
} else if (nrow(PLScoef)==5) {
    stkff <- subset(stkest, select=c(Stkcd, Dret_rf, mkt_rf, Five_SMB, Five_HML,
                                     Five_RMW, Five_CMA))
    modeltype <- 'FF5'
} else {
    stkff <- subset(stkest, select=c(Stkcd, Dret_rf, mkt_rf))
    modeltype <- 'CAPM'
}


    paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
          'PLScoef', 'CH3', 'SMB', 'VMG', '3',sep='_') %>%
      paste0(datdir, ., '.csv') %>% 
        write.csv(round(PLScoef,4), file=., quote=F, row.names = F)
    
    
    paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
          'group', 'CH3', 'SMB', 'VMG', '3', sep='_') %>%
      paste0(datdir, ., '.csv') %>% 
        write.csv(PLSgrp, file=., quote=F, row.names = F)



# Obtain the OLS estimate parameters =====
windlen <- 240L # The length of estimation window

# the correlation coefficient between explanation variables
rownum <- windlen * sample(1:length(unique(stkff$Stkcd)), size = 1)
cor(subset(stkff[c(1:windlen)+rownum,], select = -c(Stkcd, Dret_rf))); rm(rownum)


## personal function for getting the OLS estimator one by one
stkcoef <- function(x) {
    g <- data.frame()
    for(i in unique(x$Stkcd)) {
        q <- filter(x, Stkcd==i)[,-1]
        stkreg <- lm(Dret_rf ~ ., data = q)
        stkregcoef <- round(t(as.data.frame(coef(stkreg))), 6)
        k <- cbind('Stkcd' = i, stkregcoef)
        g <- rbind(g,k)
    }
    return(as.data.frame(g))
}
OLScoef <- stkcoef(stkff)


# Set colunm names and data extraction strings
if (ncol(OLScoef)==5) { # 5= 2(Stkcd+alpha) + 3 factors
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "VMG")
    coefcol <- c("MKT", "SMB", "VMG")
    evencol <- c("mkt_rf", "SMB", "VMG")
} else if (ncol(OLScoef)==7) {
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "VMG", "RMW", "CMA")
    coefcol <- c("MKT", "SMB", "HML", "RMW", "CMA")
    evencol <- c("RiskPrem", "Five_SMB", "Five_HML", "Five_RMW", "Five_CMA")
} else {
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT")
    coefcol <- c("MKT")
    evencol <- c("RiskPrem")
}


# solve the problem of data type (factor to numeric)
# and please assure the OLS estimator in R is same with the MATLAB's
cdcoef <- paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
                'OLScoef', 'CH3', 'SMB', 'VMG', '3',sep='_') %>%
          paste0(datdir, ., '.csv')
write.csv(OLScoef, file=cdcoef, quote=F, row.names = F)
OLScoef <- as.data.frame(read_delim(cdcoef, delim=',', na = '')); rm(cdcoef)
OLScoef[, 3:ncol(OLScoef)] <- round(OLScoef[, 3:ncol(OLScoef)], 6)


## Calculate AR & CAR ====
windlen <- length(timeline) # the length of event window

# calculate and output the abnormal returns
QEAabr <- data.frame('tau' = timeline)
for (z in 1:grpnum) {
    subsam <- filter(PLSgrp, group==z)$Stkcd
    stkabr <- c('tau' = timeline)
    for (i in subsam) {
        evedat <- filter(stkeve, Stkcd==i)
        w <- as.matrix(evedat[, evencol]) %*% diag(subset(OLScoef, Stkcd==i, select = coefcol))
        stkfit <- rowSums(w)
        AbRet <- subset(evedat, select = Dret_rf) - stkfit
        stkabr <- cbind(stkabr, AbRet)
    }
    colnames(stkabr) <- c('tau', subsam)
    QEAabr <- cbind(QEAabr, stkabr)
}
QEAabr <- as.data.frame(QEAabr)[,-c(1)] # Abnormal returns
rm(subsam, evedat, w, stkfit, AbRet, stkabr)

# output AR
for (i in 1:grpnum) {
    if (i == 1) {
        paste(Accprd, Pretype, Markettype, weekterm, modeltype, 'group', i, 'AR',sep='_') %>%
            paste0(datdir, ., '.csv') %>%
            write.csv(QEAabr[,c(1:(samnum[i]+1))], file=., quote=F, row.names = F)
    } else {
        paste(Accprd, Pretype, Markettype, weekterm, modeltype, 'group', i, 'AR',sep='_') %>%
            paste0(datdir, ., '.csv') %>%
            write.csv(QEAabr[,c((i+sum(samnum[1:(i-1)])) : (i+sum(samnum[1:i])))], 
                      file=., quote=F, row.names = F)
    }
}


# calculate and plot the abnormal returns
QEAcar <- as.data.frame(matrix(0,windlen,grpnum))
for (i in 1:grpnum) {
    ifelse(i==1,
           QEAabrmen <- rowMeans(QEAabr[,1+c(1:samnum[i])]),
           QEAabrmen <- rowMeans(QEAabr[,i+sum(samnum[1:(i-1)]) + c(1:samnum[i])]))
    for (t in 1:windlen) {
        ifelse(t==1,
               QEAcar[t,i] <- QEAabrmen[t],
               QEAcar[t,i] <- sum(QEAabrmen[1:t])) 
    }
}
colnames(QEAcar) <- c(paste0('g',1:grpnum,'_PLS')) 

# subtract columns of tau and add a column of mean CAR without cluster
if(grpnum == 2) {
  minstau <- c(1, samnum[1]+2)
} else if (grpnum == 3) {
  minstau <- c(1, samnum[1]+2, samnum[1]+samnum[2]+3)
} else {print('Error! The number of colums of tau isn\'t correct.')}
QEAcar <- mutate(QEAcar, 'unclassified'=rowMeans(QEAabr[ , - minstau]))
rm(minstau)

    # output CAR
    paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
          'CAR', 'CH3', 'SMB', 'VMG', '3',sep='_') %>%
      paste0(datdir, ., '.csv') %>%
        write.csv(QEAcar, file=., quote=F, row.names = F)


## Path of CAR =====
titchar <- paste0('Paths of grouped cumulative abnormal return (CAR) ', 
                  '\nattributed to quarterly earnings announcement ',
                  '\narround accounting period ', Accprd)
ggcar <- data.frame(matrix(0,windlen*(grpnum+1),3))
for (i in 1:(grpnum+1)) {
    ifelse(i==1, ggcar[(1:windlen),] <- cbind(timeline,QEAcar[,i],c(i)),
           ggcar[(i-1)*windlen + (1:windlen), ] <- cbind(timeline,QEAcar[,i],c(i)))
}
colnames(ggcar) <- c('timeline','CAR', 'group')
ggcar$group <- as.factor(ggcar$group)


pdf(paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
          'CAR', 'CH3', 'SMB', 'VMG', '3', sep='_') %>%
    paste0(datdir, ., '.pdf'))

if (grpnum+1==4) {
  ggplot(ggcar, aes(timeline, CAR, 
                    linetype = group, colour = group)) + 
    geom_line() + geom_point() +
    scale_linetype_manual(name='Group',values=c("solid", 'solid', 'solid', "dotted"),
                          labels=c('PLS_G1', 'PLS_G2', 'PLS_G3','Unclassified')) +
    scale_colour_manual(name="Group", values = c("blue", "red", 'green', "black"),
                        labels=c('PLS_G1', 'PLS_G2', 'PLS_G3','Unclassified')) +
    labs(title = titchar, x = "Time line", y = 'Cumulative Abnormal Return') + 
    theme(plot.title = element_text(size=11), 
          axis.ticks.y = element_blank(),
          plot.margin = margin(1.6,.8,1.6,.8, "cm")) 
  
} else if (grpnum+1==3) {
    ggplot(ggcar, aes(timeline, CAR, 
                      linetype = group, colour = group)) + 
        geom_line() + geom_point() + coord_fixed() +
        scale_linetype_manual(name='Group',values=c("solid", 'solid', "dotted"),
                              labels=c('PLS_G1', 'PLS_G2', 'Unclassified')) +
        scale_colour_manual(name="Group", values = c("blue", "red", "black"),
                        labels=c('PLS_G1', 'PLS_G2', 'Unclassified')) +
        labs(title = titchar, x = "Time line", y = 'Cumulative Abnormal Return') + 
        theme(plot.title = element_text(size=11), 
              axis.ticks.y = element_blank(),
              plot.margin = margin(1.6,.8,1.6,.8, "cm"))
} else print('Group number is error')

dev.off()



## Obtain the plots of OLS estimator used above QEA-FF data ====

require(grid)
library(latex2exp)

pdf(paste(Accprd, Pretype, Markettype, weekterm, modeltype, 
          'OLScoefdis', 'CH3', 'SMB', 'VMG', '3', sep='_') %>%
    paste0(datdir, ., '.pdf'))

grid.newpage()
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
if(ncol(OLScoef)==5) {
    {pushViewport(viewport(layout = grid.layout(2,2)))
        print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'),
                    ylab = "Count",bins=100), vp = vplayout(1,1:2))
        print(qplot(OLScoef$SMB, xlab=TeX('$\\beta_2$ of SMB'),
                    ylab = "Count",bins=50), vp = vplayout(2,1))       
        print(qplot(OLScoef$VMG, xlab=TeX('$\\beta_3$ of HML'),
                    ylab = "Count",bins=50), vp = vplayout(2,2))}
} else if (ncol(OLScoef)==7) {
    {pushViewport(viewport(layout = grid.layout(2,4)))
        print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'),
                    ylab = "Count",bins=100), vp = vplayout(1,1:4))
        print(qplot(OLScoef$SMB, xlab=TeX('$\\beta_2$ of SMB'),
                    ylab = "Count",bins=50), vp = vplayout(2,1))       
        print(qplot(OLScoef$HML, xlab=TeX('$\\beta_3$ of HML'),
                    ylab = "Count",bins=50), vp = vplayout(2,2))
        print(qplot(OLScoef$RMW, xlab=TeX('$\\beta_4$ of RMW'),
                    ylab = "Count",bins=50), vp = vplayout(2,3))
        print(qplot(OLScoef$CMA, xlab=TeX('$\\beta_5$ of CMA'),
                    ylab = "Count",bins=50), vp = vplayout(2,4))}
} else {
    pushViewport(viewport(layout = grid.layout(1,1)))
    print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'),
                ylab = "Count",bins=100), vp = vplayout(1,1))
}

dev.off()


