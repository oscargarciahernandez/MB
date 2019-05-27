library(here)
source('libraries.R')


library(RCurl)
bdown=function(url, file){
  
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

url_WRF_http<- "http://ems3.comet.ucar.edu/scums/releases/"
release_WRF<- url_WRF_http %>% GET() %>% htmlParse() %>% xpathSApply(., "//a/@href") %>% .[length(.)]



url_WRF_lastrelease<- paste0(url_WRF_http, release_WRF)

TBZs<-url_WRF_lastrelease %>% GET() %>% htmlParse() %>% xpathSApply(., "//a/@href") %>% 
  as.character() %>% .[str_detect(., ".tbz")]

path_wrf<- here::here(paste0('/usr1/repository/', release_WRF))
if(!dir.exists(path_wrf)){dir.create(path_wrf, recursive = T)}


urls_descarga<- paste0(url_WRF_lastrelease, TBZs)

for (i in 1:length(urls_descarga)) {
  bdown(urls_descarga[i], file = paste0(path_wrf, TBZs[i]))
  
}

