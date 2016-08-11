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


##ExA-01====================

HeadLine = 0
url <- "http://search.appledaily.com.tw/charity/projlist/Page/6"
doc <- read_html(url)
for (i in 9:15){
        xpath = paste("//*[@id='inquiry3']/table//tr[", i, "]/td[2]/a", sep = "")
        HeadLine[i-8] = xml_text(xml_find_all(doc, xpath))
}
HeadLine

ArticleNum <- 0
for (i in 9:15){
        xpath = paste("//*[@id='inquiry3']/table//tr[", i, "]/td[1]", sep = "")
        ArticleNum[i-8] = xml_text(xml_find_all(doc, xpath))
}
ArticleNum

xpath <- "//*[@id='inquiry3']//tr/td[6]/a" #a
xml_attrs(xml_find_all(doc, xpath), "href") #用xml_attrs()

xpath <- "//*[@id='charity_day']"
x <- xml_text(xml_find_all(doc, xpath))
unlist(strsplit(x, " "))[4] #共191頁


##ExA-02====================

web.url <- "http://search.appledaily.com.tw"
base.url <- paste0(web.url, "/charity/projlist/Page/")
doc <- read_html(base.url)

###抓出總篇數與總頁數
tmp <- xml_text(xml_find_all(doc, "//*[@id = 'charity_day']"))
info <- unlist(strsplit(tmp, " "))[c(4, 2)]

n.page <- as.integer(info[1])

set.seed(412)
page <- sample(1:n.page, 1)

url <- paste0(base.url, page) #隨機挑某一頁的所有文章
doc <- read_html(url)

###抓出文章連結
t1 <- xml_attr(xml_find_all(doc, "//*[@id = 'inquiry3']//a"), "href")
t1 <- data.frame(matrix(t1, ncol = 2, byrow = TRUE))
head(t1)
colnames(t1) <- c("url.article", "url.detail")
t1$url.detail <- paste0(web.url, t1$url.detail)

###取得表格資訊
base.url
t2 <- xml_text(xml_find_all(doc, "//*[@id = 'inquiry3']/table/tr/td"))
t2 <- data.frame(matrix(t2, ncol = 6, byrow = TRUE))
head(t2)
colnames(t2) <- c("aid", "title", "date.published", "case.closed", "donation", "details")
t2 <- t2[, -which(colnames(t2) == "details")]
?stopifnot()
stopifnot(ncol(t2) == 5, nrow(t2) == 20)

###結合兩個dataframe
t1 <- cbind(t2, t1)
###(optional)
t1$case.closed <- ifelse(t1$case.closed == "未結案", 0, 1)
View(t1)
#?View()

write.csv(t1, "df_article_raw.csv", row.names = FALSE)


##ExA-03====================

###抓捐款資料

article.list <- read.csv("df_article_raw.csv", stringsAsFactors = FALSE)
head(article.list)
detail.list.index <- which(article.list$case.closed == 1)

out.dir <- "db_donation_txt"
dir.create(out.dir)

install.packages("base")
library(base)
?base
?trimws() #R跟base版本要夠新才有

url <- article.list$url.detail[1]
doc <- read_html(url)
t <- trimws(xml_text(xml_find_all(doc, "//*[@id = 'inquiry3']//*[@id = 'wordcenter']")))
t <- data.frame(matrix(t, ncol = 4, byrow = TRUE))
n <- nrow(t)
t <- t[c(-1, -n), ]
colnames(t) <- c("aid", "donor.name", "donation", "date.donate")
t$aid <- article.list$aid[1]
head(t)


detail.list <- lapply(detail.list.index, function(i, article.list){
        url <- article.list$url.detail[i]
        doc <- read_html(url)
        t <- trimws(xml_text(xml_find_all(doc, "//*[@id = 'inquiry3']//*[@id = 'wordcenter']")))
        t <- data.frame(matrix(t, ncol = 4, byrow = TRUE))
        n <- nrow(t)
        if (n > 2) {
                t <- t[c(-1, -n), ] #頭尾都非資料
                colnames(t) <- c("aid", "donor.name", "donation", "date.donate")
                t$aid <- article.list$aid[i]
                t
        } else {
                t <- t[-n, ]
                colnames(t) <- c("aid", "donor.name", "donation", "date.donate")
                t$aid <- article.list$aid[i]
                t[, c("donor.name", "donation", "date.donate")] <- NA
                t
        }
        out.fnm <- sprintf("%s/aid.txt", out.dir)
        write.csv(t, out.fnm, row.names = FALSE)
}, article.list)


###抓文章內容

fb <- "http://api.facebook.com/restserver.php?method=links.getStats&urls="
n.row <- nrow(article.list)
out.dir <- "db_articles_txt"
article.info <- sapply(1:n.row, xxx) 
out.dir <- "data/db_articles_txt"

function(i, article.list) {
        aid <- article.list$aid[i]
        url <- article.list$url.article[i]
        doc <- read_html(url)
        
        write.file <- FALSE
        out.fnm <- sprintf("%s/%s.txt", out.dir, aid)
        if(write.file){
                title <- xml_text(xml_find_all(doc, "//article//hgroup/h1"))
                art <- xml_text(xml_find_all(doc, "//*[@class='articulum trans']/p | //*[@class='articulum trans']/h2 | //*[@class='articulum trans']/introid"))
                art <- c(title, art)
                out.fnm <- sprintf("%s/%s.txt", out.dir, aid)
                writeLines(art, out.fnm)
        }
        n.word <- sum(nchar(art))
        fb.api <- paste0(fb, url)
        n.fb.like <- read_html(fb.api) %>%
                xml_find_all("//share_count|//like_count|//comment_count|//total_count|//click_count") %>%
                xml_text()
        n.image <- length(xml_find_all(doc, "//*[@class='articulum trans']/figure")) %>%
                length()
        
        journalist <- xml_find_all(doc, "//*[@class='articulum trans']//*[@id='introid']") %>%
                xml_text() %>%
                strsplit("攝影╱") %>%
                unlist() %>%
                "["(2)
        
        c(aid, n.word, n.image, n.fb.like, journalist)
}, article.list)
        
article.info <- t(article.info)
colnames(article.info) <- c("aid", "n.word", "n.image", "share_count", " like_count", 
                            "comment_count", "total_count", "click_count", "journalist")