################################################################
# GMMA 831 - Marketing Analytics
# R Codes for Team Executive Challenge
# Team Melbourne
#################################################################

##### Step 1. Install and load libraries & packages

#install.packages("stringr", dependencies = TRUE)
#install.packages("gpairs")
#install.packages("RColorBrewer")  # install color package of R

library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)
library(anytime)
library(ggplot2)
library(gpairs)

library(arules)
library(arulesViz)
library(knitr)
library(plyr)
library(janitor)
library(RColorBrewer)

library(stringr)

##### Step 2. Import data

## load data from the given Excel file, "Bigbasket Exhibits", from the computer by browsing

# Set up the original dataframe from data imported fron the Excel file
bigbasket_original.df <- read_excel(file.choose(), sheet=2)  

# Create a working copy version of the origibal dataframe for Data Preprocessing & Model Building
bigbasket.df <- bigbasket_original.df

# Display the structure of the R object in both, working copy and original data
str(bigbasket_original.df)
str(bigbasket.df)

# get column names of the working data
colnames(bigbasket.df)

##### Step 3. Data Preprocessing
## All the work is carried forward in the working copy data

# Rename the column, "Created On", to "Created_On"
names(bigbasket.df)[names(bigbasket.df) == "Created On"] <- "Created_On"
colnames(bigbasket.df)  ## confirm the name change

# Format inconsistent data entries of date and time for the variable, "Created_On"
bigbasket.df$date <- parse_date_time(bigbasket.df$Created_On, orders = c('mdy_HM', 'dmy_HM'))
bigbasket.df$date2 <- excel_numeric_to_date(as.numeric(bigbasket.df$Created_On),include_time = TRUE)

bigbasket.df$date[is.na(bigbasket.df$date)] <- bigbasket.df$date2[is.na(bigbasket.df$date)]
str(bigbasket.df) # confirm the changes

# Dplyr remove "date2" and "Created_On" columns by their names
bigbasket.df = select(bigbasket.df, -date2)
bigbasket.df = select(bigbasket.df, -Created_On)
str(bigbasket.df) # confirm the changes

# Create two duplicate variables, "Created_On," to have separate variables for date and time
bigbasket.df$Date <- bigbasket.df$date
bigbasket.df$Time <- bigbasket.df$date

# Rename the column, "date", to "Created_On"
names(bigbasket.df)[names(bigbasket.df) == "date"] <- "Created_On"

bigbasket.df <- bigbasket.df[,c(1,2,3,5, 6, 7, 4)]  # rearrange variables
str(bigbasket.df)

# Change the data type of "Date" and "Time" variables to date and time only, respectively

bigbasket.df$Date = as.Date(bigbasket.df$Date,"%Y-%m-%d")
bigbasket.df$Time = as.hms(bigbasket.df$Time, "%H:%M", tz = "GMT")

str(bigbasket.df)

## Remove five objects with multiple items starting with "cfls..." from the variable, "Description"

# Add a new column, "Length", to compute the length of characters in "Description" variable
bigbasket.df$Length <- nchar(bigbasket.df$Description)
str(bigbasket.df) # check the addition of the new column, "Length"

# Keep observations in "bigbasket" dataframe
#  only if the length of the "Description" character is less than 100.
#  This removes any observations with "CFLS.cfls...." strings in "Description" column.
bigbasket.df <- filter(bigbasket.df, Length < 100)

# Remove the "Length" column from "bigbasket" dataframe
bigbasket.df = select(bigbasket.df, -Length)

## Replace "," with "&" in the observations for the variable, "Description" 
# Assign the number of the values for "Description" variable
num <- length(bigbasket.df$Description)  

# For Loop for replacing "," with "&" for each value in "Description" variable
for(i in 1:num) {
  bigbasket.df$Description[i] <- str_replace_all(bigbasket.df$Description[i], ","," &")
  i = i + 1
}

## Create a new variable, "product", that combines "Description" and "SKU
bigbasket.df$Product <- paste(bigbasket.df$Description, bigbasket.df$SKU, sep = "_")

str(bigbasket.df)  # check the changes

# Keep the data preprocessed dataframe in a separate copy of dataframe, "bigbasket_clean.df"
bigbasket_clean.df <- bigbasket.df
str(bigbasket_clean.df)  # check the new dataframe

# Export the data preprocessed dataframe to csv file
write.csv(bigbasket_clean.df, "C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/bigbasket_preprocessed.csv")


###### Step 4. Preliminary Data Inspection

# check for missing values, there are none
sum(is.na(bigbasket.df))

# basic descriptive statistics 
summary(bigbasket.df)


###### Step 5. Market Basket Analysis

glimpse(bigbasket.df)

n_distinct(bigbasket.df$Description)
n_distinct(bigbasket.df$SKU)
n_distinct(bigbasket.df$Product)

table(bigbasket.df$Product)

##convert POS data to transactional form, one line per order#
bb_transact <- ddply(bigbasket.df,c("Member", "Order", "Created_On"),
                     function(bigbasket.df)paste(bigbasket.df$Product,
                                                 collapse = ","))


# Rename the column, "V1", to "items"
names(bb_transact)[names(bb_transact) == "V1"] <- "items"

summary(bb_transact)

bb_transact <- as.data.frame(bb_transact)

bb_transact_items <- select(bb_transact,items)

#create transactions object
dats <- strsplit(as.character(bb_transact_items$items),',',fixed=T)
tr <- as(dats,"transactions")

summary(tr)

#write.csv(bb_transact_items, "C:\Users\busol\OneDrive\Desktop\Queens\GMMA 831 - Marketing Analytics\Exec Challenge\write files\bb_transact_items.csv", row.names = FALSE, quote = FALSE)

#######
#tr <- read.transactions('C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/bb_transact_items.csv', format = 'basket', header = TRUE, sep=',')

tr

inspect(tr[1:10])

summary(tr)
str(tr)

## Create an item frequency plot for the top 20 items

# Plotting bar graph of the most popular items
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), horiz=TRUE, main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), horiz=TRUE,main="Relative Item Frequency Plot")

# Plotting bar graph of the least popular items
par(mar=c(2,16,2,2), mfrow=c(1,1))
barplot(sort(table(unlist(LIST(tr))))[1:40], horiz=TRUE, las=1, col=brewer.pal(8,'Pastel2'), main="Plot-Least Popular Items")

## Density of bigbasket
# Plot a sample of 1000 transactions to see the density of 'tr' transactional data in itemMatrix
image(sample(tr,1000))  # density of 0.004290383 from summary(tr)

### Frequent items with apriori
## Extract the most frequent items
itemsets = apriori(tr, parameter = list(support=0.1, target='frequent'))

# Inspect the most frequent items
inspect(sort(itemsets, by='support', decreasing = TRUE)[1:10])


## Extract the set of most frequent items
Itemsets_freq = apriori(tr, parameter = list(supp=0.01, minlen=2, target='frequent'))

# Sorting and inspecting the frequent items
inspect(head(sort(Itemsets_freq, by="support")))  # most frequent items
inspect(tail(sort(Itemsets_freq, by="support")))  # least frequent items

## Rules with apriori
# set the rule
rules = apriori(tr, parameter = list(supp=0.001, conf=0.9, minlen=2, target='rules'))

# Inspect
inspect(head(sort(rules, by="confidence")))


### Picking the right bigbasket parameters

# Set of confidence level
confidenceLevels = seq(from=0.95, to=0.5, by=-0.05)

# Create an empty vector
rules_sup01 = NULL  # support = 0.1%
rules_sup02 = NULL  # support = 0.3%

## Calculate the number of extracted rules with minimum support 0.1% & 0.3%
# Apriori algorithm with a support level of 0.1%
for (i in 1:length(confidenceLevels)) {
  rules_sup01[i] = 
    length(apriori(tr,
                   parameter=list(sup=0.001, 
                                  conf=confidenceLevels[i],
                                  target="rules")))
}

# Apriori algorithm with a support level of 0.3%
for (i in 1:length(confidenceLevels)) {
  rules_sup02[i] = 
    length(apriori(tr,
                   parameter=list(sup=0.003, 
                                  conf=confidenceLevels[i],
                                  target="rules")))
}


# Number of rules found with a support level of 0.1%
qplot(confidenceLevels, rules_sup01, 
      geom=c("point", "line"),xlab="Confidence level",
      ylab="Number of rules found", 
      main="Apriori with a support level of 0.1%") +
  theme_bw()


# Create data frame with all metrics to be plotted
nb_rules = data.frame(rules_sup02, rules_sup01,
                      x = confidenceLevels)

# Number of rules found with a support level of 0.1% and 0.3%
ggplot(data=nb_rules, aes(x=confidenceLevels)) +
  # Lines and points for rules_sup02
  geom_line(aes(y=rules_sup02, colour="Support level of 0.3%")) + 
  geom_point(aes(y=rules_sup02,colour="Support level of 0.3%")) +
  # Lines and points for rules_sup01
  geom_line(aes(y=rules_sup01, colour="Support level of 0.1%")) +
  geom_point(aes(y=rules_sup01,colour="Support level of 0.1%")) + 
  # Polishing the graph
  theme_bw() + ylab("") +
  ggtitle("Number of extracted rules with apriori")


### Extracting the bigbasket set rules
## Summarize the set of extracted rules
## - with minimum support of 1% and minimum confidence of 90%.

# Extract rules with the apriori
rules_basket = apriori(tr,
                       parameter = list(supp = 0.0025,
                                        conf = 0.8,
                                        minlen = 2, 
                                        target = "rules"))

# Summary of extracted rules
summary(rules_basket)

# Inspect
inspect(head(sort(rules_basket, by="confidence")))

## Find a set of redundant rules
# Create redudant rules and filter from extracted rules
rules_redundant = is.redundant(rules_basket)

# Figure out a set of non-reduntant rules
rules.pruned = rules_basket[!rules_redundant]

# Inspect the non-redundant rules with highest confidence
inspect(head(sort(rules.pruned, by="confidence")))

rules_bigbasket <- rules.pruned # Assign non-redunant rules

summary(rules.pruned)
summary(rules_bigbasket)

# Inspect non-redundant rules
inspect(sort(rules_bigbasket, by="confidence", decreasing = TRUE)[1:10])


### Interactive Visualizations & Tables

## Interactive Table - to inspect the rules
inspectDT(rules_bigbasket)

### Visualize the rules
## Plot rules as scatterplot
# Plot 1 - less detailed
plot(rules_bigbasket, method = "scatterplot", engine="html")

# Plot 2 - more detailed
plot(rules_bigbasket,
     measure = c("confidence", "lift"),
     shading = "support",
     jitter = 1,
     engine = "htmlwidget")

# Interactive matrix-based plot
plot(rules_bigbasket, method = "matrix",
     shading ="support",
     engine = "html"
)

# Grouped matrix plot of rules
plot(rules_bigbasket, 
     method = "grouped",
     measure = "lift",
     shading = "confidence")

# Parallel coordinate plots with confidence as color coding
plot(rules_bigbasket, 
     method = "paracoord", 
     shading = "confidence")


### Favorite bigbasket as Graph

# Plot rules as an interactive graph
plot(rules_bigbasket,
     method = "graph",
     engine = "htmlwidget")


# Retrieve the top 20 rules with highest confidence
top20_rules_bigbasket = head(sort(rules_bigbasket,
                               by = "confidence"), 20)

plot(top20_rules_bigbasket, method = "graph", engine = "htmlwidget")


# Retrieve the top 20 rules with highest lift
top20_rules_bigbasket_lift = head(sort(rules_bigbasket,
                                  by = "lift"), 20)

plot(top20_rules_bigbasket_lift, method = "graph", engine = "htmlwidget")

# Inspect non-redundant rules - top 20
inspect(sort(rules_bigbasket, by="lift", decreasing = TRUE)[1:20])

# Inspect non-redundant rules - all 194 rules
inspect(sort(rules_bigbasket, by="lift", decreasing = TRUE)[1:194])


##############################################
## Analysis by product: product insights

### What influenced the specific purchase?
# Extract rules with "Beans" on the right side
specific_rules_rhs = apriori(tr, parameter = list(supp = 0.0025, conf = 0.8), 
                                appearance = list(default = "lhs", rhs = "Beans_15668468")) 

# Inspect the first rules
inspect(head(specific_rules_rhs))

# Find rules with highest lift
inspect(head(sort(specific_rules_rhs, by="lift"), 10)) # sort by lift
summary(specific_rules_rhs)

## what purchases influended purchase of other veg?
### ANS= {Beans_15668468, Exotic Vegetables_15668594, Root Vegetables_15668465, Root Vegetables_15668688}
otherVeg381_rules <- apriori(tr, parameter = list(supp = 0.0025, conf = 0.8), 
                             appearance = list(default = "lhs", rhs = 'Other Vegetables_15668381')) 

otherVeg381_rules <- sort(otherVeg381_rules, by = "lift", decreasing = TRUE) ## sort by lift
inspect(otherVeg381_rules[1:10])
summary(otherVeg381_rules)


otherVeg381_rules <- sort(otherVeg381_rules, by = "confidence", decreasing = TRUE) ## sort by confidence
inspect(otherVeg381_rules[1:10])
summary(otherVeg381_rules)


## what products influenced purchase of product X, and what products did purchase of X influence?
# Extract rules with "Beans" on the left side
specific_rules_lhs = apriori(tr, parameter = list(supp = 0.0025, conf = 0.8),
                                appearance = list(default = "rhs", lhs = "Beans_15668468"))

# Summary of extracted rules
summary(specific_rules_lhs)

# Inspect the first rules
inspect(head(specific_rules_lhs))


### what purchases did Other Veg influence?
otherVeg381_rules2 <- apriori(tr, parameter = list(supp = 0.0025, conf = 0.8), 
                              appearance = list(default = "rhs", lhs = 'Other Vegetables_15668381')) 

otherVeg381_rules2 <- sort(otherVeg381_rules2, by = "confidence", decreasing = TRUE)
inspect(otherVeg381_rules2)
summary(otherVeg381_rules2)


## Inspect the first few rules with the item "Beans"
##   as a antecedent item and at least two items for the rules.
# Extract rules with "Beans" on the left side
specific_setrules_lhs = apriori(tr, parameter = list(supp = 0.0025, conf = 0.8, minlen = 2), 
                                appearance = list(default = "rhs", lhs = "Beans_15668468")) 

# Inspect the first rules
inspect(head(specific_setrules_lhs))
summary(specific_setrules_lhs)


##############################################
## Analysis by member
## member with most orders : M36432, filter bb_sku_transact for member id M36432
mem_M36432 <- bb_transact[bb_transact$Member == 'M36432',]
mem_M36432_sku_items <- select(mem_M36432,items)

# File Path (Busola): C:/Users/busol/OneDrive/Desktop/Queens/GMMA 831 - Marketing Analytics/mem_M36432_sku_items.csv
# File Path (Nicole): C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/mem_M36432_sku_items.csv
write.csv(mem_M36432_sku_items, "C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/mem_M36432_sku_items.csv", row.names = FALSE, quote = FALSE)

#######

# File Path (Busola): C:/Users/busol/OneDrive/Desktop/Queens/GMMA 831 - Marketing Analytics/mem_M36432_sku_items.csv
# File Path (Nicole): C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/mem_M36432_sku_items.csv
mem_M36432_tr <- read.transactions('C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 831 Marketing Analytics/Team Executive Challenge/mem_M36432_sku_items.csv', format = 'basket', header = TRUE, sep=',')

mem_M36432_tr

inspect(mem_M36432_tr[1:10])

summary(mem_M36432_tr)
str(mem_M36432_tr)


# Plotting bar graph of the most popular items
itemFrequencyPlot(mem_M36432_tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), horiz=TRUE, main="Absolute Item Frequency Plot - mem_M36432")
itemFrequencyPlot(mem_M36432_tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), horiz=TRUE,main="Relative Item Frequency Plot - mem_M36432")


## Rules with apriori
# set the rule
mem_M36432_rules = apriori(mem_M36432_tr, parameter = list(supp=0.0025, conf=0.8, minlen=2, target='rules'))
summary(mem_M36432_rules)

## inspect member rules: first sort by confidence
mem_M36432_rules <- sort(rules, by = 'confidence', decreasing = TRUE)
inspect(mem_M36432_rules[1:10])


## inspect member rules: first sort by suport
mem_M36432_rules <- sort(rules, by = 'support', decreasing = TRUE)
inspect(mem_M36432_rules[1:10])



#####################recommender labs
## code adapted from https://rstudio-pubs-static.s3.amazonaws.com/577445_5d1a56fe1aa34045b5e23771d5d364b5.html


##install.packages("recommenderlab")
library(recommenderlab)

######
### Convert bigbasket.df to Data Matrix format
######


### split bigbasket.df to set aside a validation set for use later

library(stringr)
## identify orders with 2 or more items in basket
#multi_item.df <- bb_transact %>%
#  filter(str_count(bb_transact$items, ',') >= 1)

## subset bigbasket.df using multi_items.df
#multi_bigbasket.df <- bigbasket.df %>%
#  filter(bigbasket.df$Order %in% multi_item.df$Order)

## split out bottom 20% (last 1563 items) from multi_item.df for validation
#multi_item_val <- tail(multi_item.df,n=1563)

#'%notin%' <- Negate('%in%')

## subset multi_bigbasket.df to exclude validation set
#multi_bigbasket_train <- multi_bigbasket.df %>% 
#  filter(multi_bigbasket.df$Order %notin% multi_item_val$Order)

##arrange purchase history in a rating matrix format accepted by recommenderlab

ratings_matrix <- bigbasket.df %>%
  
  ## Select only needed variables
  select(Order, Product) %>% 
  
  ## Add a column of 1s
  mutate(value = 1) %>%
  
  ## Spread into user-item format
  spread(Product, value, fill = 0) %>%
  select(-Order) %>%
  
  ## Convert to matrix
  as.matrix()


## Convert to recommenderlab class 'binaryRatingsMatrix'
ratings_matrix <- as(ratings_matrix, "binaryRatingMatrix")


##Model Evaluation Scheme and Validation

scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross-validation",
                   k      = 5, 
                   #train  = 0.8,  
                   given  = -1)
scheme


#### create Algorithms List

algorithms <- list(
  "association rules" = list(name  = "AR", 
                             param = list(supp = 0.0025, conf = 0.8)), ###SUPPORT MATCHES TEAM
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", 
                             param = list(method = "Cosine", nn = 500))
)


#### Estimate the Models
#####   pass the scheme and algorithms to the evaluatefunction >
#####   select type = topNList to evaluate a Top N List of product recommendations >
######  and specify how many recommendations to calculate with the parameter n = c(1, 3, 5)

results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5))

### display results
results

#########
### Visualizing and Evaluating Our Models
#########
## Results as a list of confusion matrix information per model 

avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

## Using map() to iterate function across all models
results_tbl <- results %>%
  map(avg_conf_matr) %>% 
  
  ## Turning into an unnested tibble
  enframe() %>%
  
  ## Unnesting to have all variables on same level
  unnest()

results_tbl


###### ROC 
results_tbl %>%
  ggplot(aes(FPR, TPR, 
             colour = fct_reorder2(as.factor(name), 
                                   FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves", colour = "Model") +
  theme_grey(base_size = 14)

### Precision - Recall
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  
                                   precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves", colour = "Model") +
  theme_grey(base_size = 14)


## select best model based on ROC and precision-recall
## item based and association rules are the top 2
## proceed with item based

## test prediction using a made-up order: customer_order

############
#### DID YOU  FORGET test scenario 1 : random order
customer_order <- c("Other Sauces_34993740",
                    "Cashews_15669800",
                    "Moong Dal_21409002",
                    "Beans_15668468",
                    "Banana_15668478")

## Put string in a format that recommenderlab accepts.
new_order_rat_matrix <- bigbasket.df %>% 
  select(Product) %>%
  
  ## Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Product %in% customer_order)) %>% 
  
  ## Add a 'value' column
  spread(key = Product, value = value) %>% 
  
  ## Spread into sparse matrix format
  as.matrix() 

## Change to a matrix
new_order_rat_matrix <-   as(new_order_rat_matrix, "binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


## Create a `Recommender`
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",   
                      param = list(k = 5))


## Pass the `Recommender` and the made-up order to the `predict` function to create 
## A top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrix, 
                n       = 5)


## Inspect this pediction as a list
as(pred, 'list')

############
##### DID YOU  FORGET  test scenario 2 : missing Paneer Butter Masala recipe item
customer_order2 <- c("Organic Masalas & Spices_7587687",
                    "Banana_15668478")

## Put string in a format that recommenderlab accepts.
new_order_rat_matrix2 <- bigbasket.df %>% 
  select(Product) %>%
  
  ## Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Product %in% customer_order2)) %>% 
  
  ## Add a 'value' column
  spread(key = Product, value = value) %>% 
  
  ## Spread into sparse matrix format
  as.matrix() 

## Change to a matrix
new_order_rat_matrix2 <-   as(new_order_rat_matrix2, "binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


## Create a `Recommender`
#recomm <- Recommender(getData(scheme, 'train'), 
#                      method = "IBCF",   
#                      param = list(k = 5))


## Pass the `Recommender` and the test order to the `predict` function to create 
## A top n recommendation list for the new customer.
pred2 <- predict(recomm, 
                newdata = new_order_rat_matrix2, 
                n       = 5)


## Inspect this pediction as a list
as(pred2, 'list')

############
#####DID YOU  FORGET test scenario 3 : missing personal care item
customer_order3 <- c("Face Wash_7633009",
                     "Face Cream_7737419")

## Put string in a format that recommenderlab accepts.
new_order_rat_matrix3 <- bigbasket.df %>% 
  select(Product) %>%
  
  ## Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Product %in% customer_order3)) %>% 
  
  ## Add a 'value' column
  spread(key = Product, value = value) %>% 
  
  ## Spread into sparse matrix format
  as.matrix() 

## Change to a matrix
new_order_rat_matrix3 <-   as(new_order_rat_matrix3, "binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


## Create a `Recommender`
#recomm <- Recommender(getData(scheme, 'train'), 
#                      method = "IBCF",   
#                      param = list(k = 5))


## Pass the `Recommender` and the test order to the `predict` function to create 
## A top n recommendation list for the new customer.
pred3 <- predict(recomm, 
                 newdata = new_order_rat_matrix3, 
                 n       = 5)


## Inspect this pediction as a list
as(pred3, 'list')
