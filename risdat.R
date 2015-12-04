setwd("C:/Users/fh/Dropbox/Data/FA2015")
require(readxl)
require(dplyr)
require(magrittr)
require(ggplot2)

rdp<-read_excel("fa14risdat.xls") %>%
  mutate(turnout=100*afgivne/Stemmeberettigede,
         yshare=100*ja/afgivne)

ggplot(rdp,aes(x=turnout,y=yshare)) +
  theme_bw() +
  geom_point() +
  labs(x="Valgdeltagelse, pct.",y="Andel 'ja' til patentdomstolen") +
  geom_smooth(method="lm",color="red",alpha=0) +
  geom_hline(yintercept=50,linetype="dashed")

ggsave("patentturnout.png",width=7,height=7)
