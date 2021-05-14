library(arules)
library(arulesViz)

#import the data
df_shopping_cart_raw <- read.csv2(sep = ";","data.csv",header = TRUE,skipNul = FALSE,na=c("x","","NA"),stringsAsFactors = FALSE)
head(df_shopping_cart_raw)

#Analysis for the Channel Web
df_shopping_cart_web<-df_shopping_cart_raw[df_shopping_cart_raw$channel=="Web",]

#Columns article_name and basket_id are taken
df_shopping_cart_web<-df_shopping_cart_web[c(4,6)]
head(df_shopping_cart_web)

#Check NA and remove NA
sum(is.na(df_shopping_cart_web$article_name))
df_shopping_cart_web <- na.omit(df_shopping_cart_web)

#Split data based on basket ID and put items in the same basket as list of itemsets
df_shopping_cart_web_split<-split(df_shopping_cart_web$article_name,df_shopping_cart_web$basket_id)

#convert the dataframe to transactions
retail_transactions_web <- as(df_shopping_cart_web_split, "transactions")

#Analyse data and itemsets
summary(retail_transactions_web)
inspect(retail_transactions_web[1:10])

#top itemsets are observed for analysis
itemFrequencyPlot(retail_transactions_web, topN = 5,support = 0.00008)

# apriori algorithm is applied to the dataset
cartrules <- apriori(retail_transactions_web,parameter = list(support = 0.00008, conf = 0.6, minlen =3, maxlen =5))

#Top rules are found based on high lift values
CartSubRules<-cartrules[quality(cartrules)$confidence>0.6]
top10subRules <- head(CartSubRules, n = 10, by = "lift")
top10subRules=as(top10subRules,"data.frame")
top10subRules


#the rules are filtered only for confidence>0.5 and plot of the rules are done
cartrules<-cartrules[quality(cartrules)$confidence>0.5]
plot(cartrules)
plot(cartrules, method = "grouped", control = list(k=10))

#plot of item deep plates rules and item hooks are done
gborules <- subset(cartrules, subset = rhs %in% "deep plates")
#gborules <- subset(cartrules, subset = rhs %in% "hooks")
subRules <- gborules[quality(gborules)$confidence>0.6]
inspect(head(sort(gborules, by = "lift"),4))
plot(sort(subRules, by ="lift"), method = "graph", shading = "lift")


#Analysis for the Channel Branch
df_shopping_cart_Bra<-df_shopping_cart_raw[df_shopping_cart_raw$channel=="Bra",]

#Columns article_name and basket_id are taken
df_shopping_cart_Bra<-df_shopping_cart_Bra[c(4,6)]
head(df_shopping_cart_Bra)

#Check NA and remove NA
sum(is.na(df_shopping_cart_Bra$article_name))
df_shopping_cart_Bra <- na.omit(df_shopping_cart_Bra)

#Split data based on basket ID and put items in the same basket as list of itemsets
df_shopping_cart_Bra_split<-split(df_shopping_cart_Bra$article_name,df_shopping_cart_Bra$basket_id)

#convert the dataframe to transactions
retail_transactions_Bra <- as(df_shopping_cart_Bra_split, "transactions")

#Analyse data and itemsets
summary(retail_transactions_Bra)
inspect(retail_transactions_Bra[1:10])

#top itemsets are observed for analysis
itemFrequencyPlot(retail_transactions_Bra, topN = 5,support = 0.00009)

# apriori algorithm is applied to the dataset
cartrules <- apriori(retail_transactions_Bra,parameter = list(support = 0.000065, conf = 0.45, minlen= 4, maxlen =5))

#Top rules are found based on high lift values
SubRules<-cartrules[quality(cartrules)$confidence>0.6]
top10subRules <- head(SubRules, n = 10, by = "lift")
top10subRules=as(top10subRules,"data.frame")
top10subRules$rules

#the rules are filtered only for confidence>0.5 and plot of the rules are done
cartrules<-cartrules[quality(cartrules)$confidence>0.5]
plot(cartrules)
plot(cartrules, method = "grouped", control = list(k=10))

#plot of item tweezers with magnifying glass are done
gborules <- subset(cartrules, subset = rhs %in% "tweezers with magnifying glass")
subRules <- gborules[quality(gborules)$confidence>0.6]
inspect(head(sort(gborules, by = "lift"),4))
plot(sort(subRules, by ="lift"), method = "graph", shading = "lift")