setwd("~/Dropbox/Data/FA2015")
require(readxl)
eurefs<-read_excel("eurefres_xl.xls")

require(dplyr)
require(magrittr)
require(reshape2)
require(lubridate)
require(readr)

dates<-c(dmy("02-10-1972"),dmy("27-02-1986"),dmy("02-06-1992"),dmy("18-05-1993"),
         dmy("28-05-1998"),dmy("28-09-2000"),dmy("25-05-2014"),dmy("03-12-2015"))


er <- eurefs %>%
  select(shortunit,y72:y15) %>%
  filter(shortunit %in% c("cphdif","rkbdif")) %>%
  melt(.,id.vars="shortunit") %>%
  arrange(shortunit) %>%
  mutate(elecdate=rep(dates,2)) 

#%>%  filter(variable!="y15")

require(ggplot2)

ggplot(er,aes(x=elecdate,y=value,group=shortunit,color=shortunit)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0,linetype="dashed") +
  theme_bw() +
  labs(x="",y="Forskel ift. landsresultat") +
  scale_color_manual(values=c("red","blue")) +
  theme(legend.position="none") +
  annotate(geom="text",label="Ringkøbing",x=dmy("01-07-1994"),y=11,color="blue",size=4) +
  annotate(geom="text",label="København",x=dmy("01-07-1994"),y=-7,color="red",size=4)

setwd("~/GitHub/fa15data")
ggsave("rkbkbh.png",width=8,height=5)

#get patent data
pd<-read_csv("https://raw.githubusercontent.com/ok-dk/epvalg2014/master/patentdomstol.csv")
names(pd)[8:9]<-c("yvotes","nvotes")
pd <- pd %>%
  mutate(allvotes=yvotes+nvotes,
         yshare=yvotes/allvotes) %>%
  filter(yshare>.25)

ggplot(pd,aes(x=allvotes,y=yshare)) +
  geom_point() +
  geom_smooth(method="loess",color="red",alpha=0) +
  theme_bw()


