library(readxl)
dfforarticle <- read_excel('dataforarticle march-23.xlsx', col_names = T)

#boxplot for QoL data
QoL <- data.frame(GHLSBT=dfforarticle$`GHLS BT` ,GHLSDT=dfforarticle$`GHLS DT` ,MADRSBT=dfforarticle$`MADRS BT` , MADRSDT=dfforarticle$`MADRS DT` 
                  , MCScBT=dfforarticle$`MCSc BT` , MCScDT=dfforarticle$`MCSc DT` , PCScBT=dfforarticle$`PCSc BT` , PCScDT=dfforarticle$`PCSc DT` )
ID <- c(1:15)
QoL <- cbind(ID,QoL)
library(reshape2)
QoLlong <- melt(QoL, value.name = 'QoLpoints', id.vars = 'ID')
library(ggplot2)
library(ggpubr)
library(ggsignif)
p <- ggplot(QoLlong, aes(x=variable, y=QoLpoints)) + geom_boxplot() +
   theme(text=element_text(size=20), panel.grid.major=element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  stat_signif(comparisons = list(c('GHLSBT','GHLSDT')), test.args=list(var.equal=T, alternative="two.sided", paired=T), test="t.test", map_signif_level = T, col=2) +
  stat_signif(comparisons = list(c('MADRSBT','MADRSDT')), test.args=list(var.equal=T, alternative="two.sided", paired=T), test="t.test", map_signif_level = T, col=2) +
  stat_signif(comparisons = list(c('MCScBT','MCScDT')), test.args=list(var.equal=T, alternative="two.sided", paired=T), test="t.test", map_signif_level = T, col=2) +
  stat_signif(comparisons = list(c('PCScBT','PCScDT')), test.args=list(var.equal=T, alternative="two.sided", paired=T), test="t.test", map_signif_level = T, col=2)
p

#spearman correlation for ratio of fT4, delta Neurogranin and QoL data
QoLDTandDelta <- data.frame(Ngdelta=dfforarticle$diffNgPowerofTenTransf, fT4ratioDT=dfforarticle$`PowerodTen CSFfT4_sfT4 DT`, fT4ratioDelta=dfforarticle$DeltapowerofTenRatioCSF_sfT4,
                            MADRSDT=dfforarticle$`MADRS DT`, MADRSdelta=dfforarticle$diffMADRSpowerofTenTransf, GHLSDT=dfforarticle$`GHLS DT`,
                            GHLSdelta=dfforarticle$diffGHLSpowerofTenTransf, MCScDT=dfforarticle$`MCSc DT`, MCScDelta=dfforarticle$diffMCScpowerofTenTransf,
                            PCScDT=dfforarticle$`PCSc DT`, PCScdelta=dfforarticle$diffPCScpowerofTenTransf)
library(psych)
library(dplyr)
ratiocorrelation <- corr.test(QoLDTandDelta$fT4ratioDT, QoLDTandDelta, method = 'spearman')
pvalue <- t(round(ratiocorrelation$p,3))
rvalue <- t(round(ratiocorrelation$r,2))
ratiocorrelation <- data.frame(pvalue, rvalue)
ratiocorrelation %>% add_significance(p.col = 'pvalue') %>% View()
#and the correlation for NgDelta
Ngcorrelation <- corr.test(QoLDTandDelta$Ngdelta, QoLDTandDelta, method = 'spearman')
pvalue <- t(round(Ngcorrelation$p,3))
rvalue <- t(round(Ngcorrelation$r,2))
Ngcorrelation <- data.frame(pvalue, rvalue)
Ngcorrelation %>% add_significance(p.col = 'pvalue') %>% View()


