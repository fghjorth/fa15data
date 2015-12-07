setwd("~/Dropbox/Data/FA2015")
require(dplyr)
require(magrittr)
require(reshape2)
require(lubridate)
require(readr)
require(RCurl)
require(ggplot2)

# #preliminarily: get data from EK
# fa<-"http://emilkirkegaard.dk/da/wp-content/uploads/valgdata.csv" %>%
#   getURL() %>%
#   textConnection() %>%
#   read.csv()
# 
# ggplot(fa,aes(x=Turnout/Eligable,y=Yes)) +
#   geom_point(aes(size=Eligable*2),alpha=.6) +
#   theme_bw() +
#   theme(legend.position="none")

#try to scrape patent data
require(haven)
require(rvest)
skurls<-paste("http://www.dst.dk/valg/Valg1664255/valgopgmid/valgopgStor",10:19,".htm",sep="")
#"http://www.dst.dk/valg/Valg1664255/valgopgmid/valgopgStor10.htm"

opkurls<-list("") #create empty list

for (i in 1:length(skurls)){
  sklinks<-skurls[i] %>% #scrape out urls
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")
  
  sklinks<-sklinks[grep("valgopgOpst",sklinks)] #get only relevant links
  
  opkurls_sk<-paste("http://www.dst.dk/valg/Valg1664255/valgopg/",sklinks,sep="") #make url list
  
  opkurls<-append(opkurls,opkurls_sk) #append urls to full list
}

afsturls<-list("")

for (i in 2:length(opkurls)){
  opklinks<-opkurls %>%
    extract(i) %>%
    extract2(1) %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("16",.,value=T) %>% #note this number should correspond to first two digits of election code
    gsub("../","",.,fixed=T)
  
  afsturls_op<-paste("http://www.dst.dk/valg/Valg1664255/",opklinks,sep="") #make url list
  
  afsturls<-append(afsturls,afsturls_op)
}

#cleanup
afsturls<-afsturls %>%
  unlist() %>%
  extract(-grep("valgopgStor",.)) %>%
  unique() %>%
  extract(-1)

#prep data frame
afstdat<-data.frame(landsdel=rep(NA,length(afsturls)),storkreds=NA,opstkreds=NA,afst=NA,ja=NA,nej=NA,gyldige=NA)

#loop over all polling places
for (i in 1:length(afsturls)){
  tryCatch({
    #get polling place info
    afstinfo<-afsturls[i] %>%
      read_html() %>%
      html_nodes("div") %>%
      html_text() %>%
      extract(5) %>%
      strsplit(.,split=" - ") %>%
      extract2(1)
    
    #get polling place data
    afstres<-afsturls[i] %>%
      read_html() %>%
      html_table(.,fill=T,dec=",") %>%
      extract2(5) %>%
      extract(5:7,) %>%
      extract(,2) %>%
      gsub(".","",.,fixed=T) %>%
      as.numeric()
    
    #fill in
    afstdat[i,]<-c(afstinfo[3:6],afstres)
    
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#save data
saveRDS(afstdat,"tilvalg_afst.rds")
afstdat<-readRDS("tilvalg_afst.rds")
head(afstdat)
str(afstdat)
class(afstdat[,5])<-"numeric"
class(afstdat[,6])<-"numeric"
class(afstdat[,7])<-"numeric"


#rename for merging
fa15<-afstdat

#get fa14 data
fa14<-readRDS("patent_afst.rds")
str(fa14)
class(fa14[,5])<-"numeric"
class(fa14[,6])<-"numeric"
class(fa14[,7])<-"numeric"
names(fa14)[5:7]<-c("ja_14","nej_14","gyldige_14")

#rename fa15 vars
names(fa15)[5:7]<-c("ja_15","nej_15","gyldige_15")

#merge on all four id variables
allfa<-full_join(fa14,fa15,by=c("landsdel","storkreds","opstkreds","afst"))

allfa<-allfa %>%
  mutate(yshare_14=ja_14/gyldige_14,
         yshare_15=ja_15/gyldige_15)

#run ols of 15 share on 14 share
summary(lagmod<-lm(yshare_15~yshare_14,data=allfa))
str(lagmod)

#merge in residual
allfa$lagmodresid<-NA
allfa$lagmodresid[-as.numeric(lagmod$na.action)]<-lagmod$residuals  

hist(allfa$lagmodresid)

#save full data
saveRDS(allfa,"allfa.rds")


  
