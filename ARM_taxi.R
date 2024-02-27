library(arules)
library(RColorBrewer)

# Load the transactions dataset from your uploaded file
transactions <- read.transactions("D:\\CSCI-5622-872 Machine Learning\\taxi_ARM.csv",
                                  rm.duplicates = FALSE, 
                                  format = "basket",
                                  sep=",",
                                  cols=NULL)

# Perform association rule mining with specified support and confidence levels
rules <- apriori(transactions, parameter = list(support=0.3, confidence=0.5, minlen=2))
inspect(rules)
# Plot the most frequent items
itemFrequencyPlot(transactions, topN = 20,
                  col = brewer.pal(8, 'Pastel2'),
                  main = 'Relative Item Frequency Plot',
                  type = "relative",
                  ylab = "Item Frequency (Relative)")

# Sort rules by confidence
sortedRules_confidence <- sort(rules, by="confidence", decreasing=TRUE)

# Inspect the top 10 sorted rules
inspect(sortedRules_confidence[1:15])

# Sort rules by support
sortedRules_support <- sort(rules, by="support", decreasing=TRUE)

# Inspect the top 10 sorted rules
inspect(sortedRules_support[1:15])

# Sort rules by lift
sortedRules_support <- sort(rules, by="lift", decreasing=TRUE)

# Inspect the top 10 sorted rules
inspect(sortedRules_support[1:15])

# Summary of the sorted rules
summary(sortedRules)

# scatter plot
library(arulesViz)
library(ggplot2)

# Convert rules to a data frame for ggplot
rules_df <- as(rules, "data.frame")

# Scatter plot with ggplot2
ggplot(rules_df, aes(x = lift, y = confidence, color = support)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Rules", x = "Lift", y = "Confidence")

# Network Graph of Rules

plot(rules, method="graph", control=list(type="items"))

plot(rules, method = "graph", engine = "htmlwidget")


# grouped matrix

plot(rules, method="grouped")









