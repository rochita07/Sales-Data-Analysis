# market basket analysis

transaction_df <- cleaned_data[, c("Order.ID", "Product.Name")]
transaction_df
length(unique(Product.Name))
length(unique(Order.ID))
#How many unique levels of InvoiceNo and Description of the product are there
str(transaction_df)
#save transaction id and commodities in one file for future reference
 write.csv(transaction_df,'E:/Internship/HEB_Presentation/transaction_df.csv', row.names = FALSE)

#creating a itemList from the Description column of the data.
#for each InvoiceNo,description of all the products brought together are written together

itemList <- plyr :: ddply(transaction_df, c("Order.ID"), 
                          function(transaction_df)paste(transaction_df$Product.Name, 
                                                        collapse = ","))
 # itemList
# dim(itemList) #Unique invoice No
# dim(retail_cleaned)
# names(itemList)
#deleting the InvoiceNO from the itemList data as this is not required anymore
itemList$Order.ID <- NULL
itemList
#Write out the itemlist per transaction in a csv file
write.csv(itemList,'E:/Internship/HEB_Presentation/market_basket_tr.csv', row.names = FALSE)

#Read the csv in 'basket' format
#rm.duplicates removes duplicate items in a particular transaction.
transaction <- read.transactions('E:/Internship/HEB_Presentation/market_basket_tr.csv', format = 'basket', quote = "", cols = NULL, sep=',', skip = 1, rm.duplicates = T)
transaction
 #summary(transaction)

# Make a frequency plot of the transactions with a support of 0.05 or greater.
# This shows the the most popular gift items sold.
# x11()
# itemFrequencyPlot(transaction, support = 0.05, col = rainbow(4))
# itemFrequencyPlot(transaction, topN = 10, type = 'absolute')

# create association rules with a minimum support value ,where support indicates appearance of 
#commodity A and B together out of total transactions of all items.
rules <- apriori(transaction, parameter = list(supp = 0.001, conf = 0.5, minlen = 2))
#options(digits=2)


# Finding redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="confidence")
inspect(rules.pruned)
top10rules <-rules.pruned[1:10]
inspect(top10rules)

plot(rules.pruned[1:10], method = "graph", engine = "interactive")#, main = "Top 10rules by Confidence")

#if A => B is the rule, confidence shows the proportion of transactions having both A and B,
#out of total transactions having A.

#sort the rules by decreasing confidence and show top 10 rules
rules_by_confidence <- sort(rules, by ='confidence', decreasing = TRUE)
#summary(rules_by_confidence)
toprules_by_confidence <- rules_by_confidence[1:10]
#options(digits=2)
inspect(toprules_by_confidence)

plot(toprules_by_confidence, method="graph",engine = 'interactive',shading = NA)

#Lift is the factor by which, the co-occurence of A and B exceeds the expected
#probability of A and B co-occuring, had they been independent. So, higher the
#lift, higher the chance of A and B occurring together.
# sort the rules by decreasing lift and show top 10 rules


#________________________________________________________________
redundant <- is.redundant(rules, measure="lift")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)
top10rules <-rules.pruned[1:10]
inspect(top10rules)

plot(rules.pruned[1:10], method = "graph", engine = "interactive")#, main = "Top 10rules by Confidence")
#______________________________________________________________________________

rules_by_lift <- sort(rules, by='lift', decreasing = TRUE)
#summary(rules_by_lift)
toprules_by_lift <- rules_by_lift[1:10]
inspect(toprules_by_lift)

plot(toprules_by_lift, method="graph",engine = 'interactive',shading = NA)

t = table(Product.Name)
which.max(t)

#Since Staple envelope  is the most popular item, we are
#interested in the items bought with it.
rules_lhs_Stapl_envelope  <- apriori(data=transaction, parameter=list(supp=0.0001,conf = 0.01, minlen = 2), 
                                                      appearance = list(default="rhs",lhs="Staple envelope"),
                                                      control = list(verbose=F))
rules_lhs_Stapl_envelope <- sort(rules_lhs_Stapl_envelope, decreasing=TRUE,by="confidence")

redundant <- is.redundant(rules_lhs_Stapl_envelope, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

inspect(rules_lhs_Stapl_envelope)
gifts_with_Stapl_envelope <- rules_lhs_Stapl_envelope[1:10]

plot(gifts_with_Stapl_envelope, method="graph",engine = 'interactive',shading = NA)


