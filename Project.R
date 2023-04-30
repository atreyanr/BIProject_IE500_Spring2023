Invistico_Airline <- read.csv("~/Library/CloudStorage/Box-Box/UB/Spring 2023/IE500 BI/Project/Invistico_Airline.csv")
dim(Invistico_Airline)


Categorical_columns <- Invistico_Airline %>%
  select_if(is.character)

Numerical_columns <- Invistico_Airline %>%
  select_if(is.numeric)


t(summary(Numerical_columns))
t(summary(Categorical_columns))


# Calculate the percentage of missing values for each column
missing <- data.frame(colnames(Invistico_Airline), round(colMeans(is.na(Invistico_Airline)) * 100, 2))
# Sort the columns by percentage of missing values
missing <- missing[order(missing[, 2], decreasing = TRUE),]
# Rename the columns
names(missing) <- c("index", "Average")
# Show the top rows of the resulting data frame
head(missing)

colnames(Invistico_Airline)

sum(duplicated(Invistico_Airline))


IA_numeric <- apply(Invistico_Airline, 2, function(x) as.numeric(as.character(Invistico_Airline)))
# Compute correlation matrix
corr <- cor(IA_numeric)


any(is.na(corr))
any(is.nan(corr))
any(!is.finite(corr))

corr[!is.finite(corr)] <- 0

d <- dist(1 - corr)
fit <- hclust(d, method = "ward.D2")

# Create a heatmap of the correlation matrix
corrplot(corr, method="color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black",
         tl.srt=45, tl.pos="lt", addCoef.col="black", number.cex=0.7,
         tl.cex=0.7, cl.cex=0.7, cl.pos="n")




# Count the number of satisfied and dissatisfied values
satisfaction_counts <- table(Invistico_Airline$satisfaction)

# Create the pie chart
fig <- plot_ly(values = satisfaction_counts, labels = c("Satisfied", "Dissatisfied"), type = "pie",
               hole = 0.6, textinfo = "label+percent", textfont = list(size = 20),
               marker = list(colors = c("#7FC97F", "#BEAED4"), line = list(color = "white", width = 2)))

# Customize the layout
fig <- fig %>% layout(title = list(text = "<b>Satisfied and Dissatisfied Ratio", font = list(size = 30)),
                      font = list(size = 20), showlegend = FALSE)

fig






