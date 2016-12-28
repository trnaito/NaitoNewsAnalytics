
rm(list=ls())

#install.packages("plotrix")
library(plotrix)

#install.packages("RMeCab", repos="http://rmecab.jp/R")
library(RMeCab)

# 単語感情極性対応表の取得
pndic <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic",
                    sep=":",
                    col.names = c("term","kana","pos","value"),
                    colClasses=c("character","character","factor","numeric"),
                    fileEncoding = "SJIS")
head(pndic)
# 名詞＋品詞で複数の候補がある場合は平均値を採用
pndic2 <- aggregate(value ~ term + pos,pndic,mean)

# pndic2$value <- pndic2$value + 0.35
pndic2$value <- pndic2$value + 0.50

getwd()
setwd("GitHub/NaitoNewsAnalytics")
oil <- RMeCabFreq("oil_20161201_1.txt")

# pndicに登録されている単語(term)のみ抽出
oil <- subset(oil,Term %in% pndic2$term)

# 単語感情極性スコア付与
oil <- merge(oil,pndic2,by.x=c("Term","Info1"),by.y=c("term","pos"))
oil

# #単語の出現回数にスコアをかけて総和をとる
scoreOil <- oil[4:(ncol(oil)-1)] * oil$value

oil <-c(sum(scoreOil > 0),sum(scoreOil < 0))

lbls <- c("positive", "negative")
lbls <- paste(lbls, oil, sep="：")
pie3D(oil, radius=0.9, labels=lbls, explode=0.1, main="Naito Analyticsによるポジネガ(原油)ヽ('A`)ﾉ ")

#---------------------------------
nky <- RMeCabFreq("nky_20161201_1.txt")
nky <- subset(nky,Term %in% pndic2$term)
nky
nky <- merge(nky,pndic2,by.x=c("Term","Info1"),by.y=c("term","pos"))

scoreNky <- nky[4:(ncol(nky)-1)]*nky$value
head(scoreNky)

nky <-c(sum(scoreNky > 0),sum(scoreNky < 0))


lbls <- c("positive", "negative")
lbls <- paste(lbls, nky, sep="：")
pie3D(nky, radius=0.9, labels=lbls, explode=0.1, main="Naito Analyticsによるポジネガ(日経)ヽ('A`)ﾉ ")

#---------------------------------
jgb <- RMeCabFreq("jgb_20161201_1.txt")
jgb <- subset(jgb,Term %in% pndic2$term)
jgb <- merge(jgb,pndic2,by.x=c("Term","Info1"),by.y=c("term","pos"))
scoreJgb <- jgb[4:(ncol(jgb)-1)]*jgb$value
jgb <-c(sum(scoreJgb > 0),sum(scoreJgb< 0))

lbls <- c("positive", "negative")
lbls <- paste(lbls, jgb, sep="：")
pie3D(jgb, radius=0.9, labels=lbls, explode=0.1, main="Naito Analyticsによるポジネガ(国債)ヽ('A`)ﾉ ")

#----------------------------------
aus <- RMeCabFreq("aus_20161201_1.txt")
aus <- subset(aus,Term %in% pndic2$term)
aus <- merge(aus,pndic2,by.x=c("Term","Info1"),by.y=c("term","pos"))
aus
scoreAus <- aus[4:(ncol(aus)-1)]*aus$value
aus <-c(sum(scoreAus > 0),sum(scoreAus< 0))

lbls <- c("positive", "negative")
lbls <- paste(lbls, aus, sep="：")
pie3D(aus, radius=0.9, labels=lbls, explode=0.1, main="Naito Analyticsによるポジネガ(オーストラリア)ヽ('A`)ﾉ ")

