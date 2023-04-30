Invistico_Airline <- read.csv("~/Library/CloudStorage/Box-Box/UB/Spring 2023/IE500 BI/Project/Invistico_Airline.csv")


fit <- rpart(Invistico_Airline$satisfaction ~.,data = Invistico_Airline, method = "class",
             control=rpart.control(minsplit=15, minbucket=10, maxdepth=5))
rpart.plot(x = fit,extra = "auto",box.palette = "auto")



