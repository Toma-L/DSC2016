library(xml2)
#install.packages("xmlview")
library(xmlview)

url <- "http://www.appledaily.com.tw/appledaily/article/headline/20160704/37293645"
download.file(url, "data/test.html") #讀網頁

url   <- "http://search.appledaily.com.tw/charity/projlist/Page"
doc   <- read_html(url)

xpath <- "//*[@id='inquiry3']/table//tr[4]/td[1]" #選節點

#XPath
## / 選取根節點
## // 選取任何節點
## @ 選取屬性
## * 選取所有節點
## | OR


#xml_find_all(doc, xpath) %>% xml_text()
xml_text(xml_find_all(doc, xpath)) #抓資訊
#xml_view(doc, add_filter = T)

#xml2
##讀取網頁：read_html; read_xml
##選擇節點：xml_find_all; xml_find_one
##擷取資訊：xml_text; xml_attrs

HeadLine = 0
url <- "http://search.appledaily.com.tw/charity/projlist/Page/6"
doc <- read_html(url)
for (i in 8:14){
        xpath = paste("//*[@id='inquiry3']/table//tr[", i, "]/td[2]/a", sep = "")
        HeadLine[i-7] = xml_text(xml_find_all(doc, xpath))
}
HeadLine

ArticleNum <- 0
for (i in 8:14){
        xpath = paste("//*[@id='inquiry3']/table//tr[", i, "]/td[1]", sep = "")
        ArticleNum[i-7] = xml_text(xml_find_all(doc, xpath))
}
ArticleNum

