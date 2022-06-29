#Association Rule Mining 


# 1 read the file into RStudio
marketbasket <-read.csv("marketbasket.csv",header=T, colClasses="factor")

# 2 Inspect the DataSet

names(marketbasket) 
head(marketbasket) 
tail(marketbasket) 
summary(marketbasket)
str(marketbasket)

    
# 3 Check the dimension of the "marketbasket" dataset

dim(marketbasket)

#  dim(marketbasket)
# [1] 1000   14
# the output shows 1000 rows and 14 columns

# 4 Plot and explore the "marketbasket" dataset with barplot() function

yes <- colSums(marketbasket == "Yes")
yes

no <-colSums(marketbasket=="No")
no

purchased <- rbind(yes,no) # combining yes and no
purchased

barplot(purchased,legend=rownames(purchased)) #Plot 1
barplot(purchased, beside=T,legend=rownames(purchased))# Plot 2


install.packages("arules") 
library(arules) # activate "arules" package

#arules package is a powerful tool for mining associative rules in transactional databases.

rules <- apriori(marketbasket)

# get a summary of the rules
summary(rules)
#The result tells you that there was 3 rules with 1 item 
#and 17062 rules with 6 items, etc..

#inspect function prints the internal representation of an R object
inspect(rules)
# as there are too many rules we  reduce them using parameters  
rules <- apriori(marketbasket,
                 parameter =list(minlen=2,maxlen=3, conf = 0.95))
summary(rules)

inspect(rules)

barplot(purchased, beside=T,legend=rownames(purchased))
rules <- apriori(marketbasket,
                 parameter = list(minlen=2, maxlen=3,conf = 0.95), appearance= list(rhs=c("cosmetics=Yes"),default="lhs"))


inspect(rules)
#there are no rules for this parameter values
# hence we try another parameter and change the confidence parameter to 70%

rules <- apriori(marketbasket,
                 parameter = list(minlen=2, maxlen=3,conf = 0.70), appearance= list(rhs=c("cosmetics=Yes"),default="lhs"))
inspect(rules)
# 16 rules available for given parameters

library(arulesViz)
#arulesViz package provides various visualization techniques for association rules and itemsets.

plot(rules, method ="grouped")

plot(rules@quality)
#Thiscode displays a scatterplot matrix to compare the support, confidence, and lift


rules3 <- apriori(marketbasket,
                  parameter = list(minlen=2,maxlen=4, conf = 0.60),
                  appearance =list(rhs=c("banana=Yes","apples=Yes","avocado=Yes")
                                   ,default="lhs") )
plotly_arules(rules3, measure = c("support", "lift"), shading = "confidence")


rules2 <- apriori(marketbasket,
                  parameter = list(minlen=2, maxlen=3,conf = 0.7),
                  appearance =list(rhs=c("cosmetics=Yes"),
                                   lhs=c("apples=Yes",
                                         "banana=Yes",
                                         "coke=Yes",
                                         "turkey=Yes",
                                         "bourbon=Yes",
                                         "ice_cream=Yes",
                                         "baguette=Yes",
                                         "soda=Yes",
                                         "choclate=Yes",
                                         "cracker=Yes",
                                         "avocado=Yes",
                                         "sardines=Yes"),
                                   default="none"))


inspect(rules2)


# > inspect(rules2)
# lhs                           rhs             support confidence
# [1] {avocado=Yes}              => {cosmetics=Yes} 0.356   0.7265306 
# [2] {choclate=Yes,avocado=Yes} => {cosmetics=Yes} 0.130   0.7182320 
# [3] {cracker=Yes,avocado=Yes}  => {cosmetics=Yes} 0.146   0.7263682 
# coverage lift     count
# [1] 0.490    1.355468 356  
# [2] 0.181    1.339985 130  
# [3] 0.201    1.355164 146 


