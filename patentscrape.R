setwd("~/Dropbox/Data/FA2015")
require(dplyr)
require(magrittr)
require(reshape2)
require(lubridate)
require(readr)

#try to scrape patent data
require(haven)
require(rvest)
skurls<-paste("http://www.dst.dk/valg/Valg1475796/valgopg/valgopgStor",10:19,".htm",sep="")

opkurls<-list("") #create empty list

for (i in 1:length(skurls)){
  sklinks<-skurls[i] %>% #scrape out urls
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")
  
  sklinks<-sklinks[grep("valgopgOpst",sklinks)] #get only relevant links
  
  opkurls_sk<-paste("http://www.dst.dk/valg/Valg1475796/valgopg/",sklinks,sep="") #make url list
  
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
    grep("14",.,value=T) %>%
    gsub("../","",.,fixed=T)
  
  afsturls_op<-paste("http://www.dst.dk/valg/Valg1475796/",opklinks,sep="") #make url list
  
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
saveRDS(afstdat,"patent_afst.rds")
afstdat<-readRDS("patent_afst.rds")
head(afstdat)
str(afstdat)
class(afstdat[,5])<-"numeric"
class(afstdat[,6])<-"numeric"
class(afstdat[,7])<-"numeric"

afstdat<-afstdat %>%
  mutate(yshare=ja/gyldige)

afstdat %>%
  arrange(gyldige) %>%
  filter(gyldige<100)
