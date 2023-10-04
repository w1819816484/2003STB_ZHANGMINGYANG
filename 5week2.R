searchUrl <- "https://openapi.naver.com/v1/search/news.xml"

Client_ID <- "XSgTAqY22SfEv6H9ivQ6"

Client_Secret <- "LSJujZa8Zq"

query <- URLencode(iconv("HUAWEI", "euc-kr", "UTF-8"))

url <- paste(searchUrl, "?query=", query, "&display=25", sep="")

doc <- getURL(url, 
              httpheader = c('Content-Type' = "apllication/xml",
                             'X-Naver-CLient-Id' = Client_ID, 
                             'X-Naver-CLient-Secret' = Client_Secret))
doc

xmlFile <- xmlParse(doc)

df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))

str(df)

description <- df[,4]
description

description2 <- gsub("\\d|<b>|</b>|&quot;","",description)
description2

nouns <- nouns(iconv(description2, "utf-8"))
nouns

nouns.all <- unlist(nouns, use.names=F)
nouns.all

nouns.all1 <- nouns.all[nchar(nouns.all) <=1]
nouns.all1

nouns.all2 <- nouns.all[nchar(nouns.all) >=2]
nouns.all2

nouns.freq <- table(nouns.all2)
nouns.freq

nouns.df <- data.frame(nouns.freq, stringsAsFactors=F)
nouns.df

nouns.df.sort <- nouns.df[order(-nouns.df$Freq),]
nouns.df.sort
wordcloud(nouns.df.sort[,1],
          freq=nouns.df.sort[,2],
          min.freq=1,
          scale=c(3,0.7),
          rot.per=0.25,
          random.order=F,
          random.color=T,
          colors=rainbow(11))