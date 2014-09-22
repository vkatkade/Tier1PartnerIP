#setwd("/Users/vkatkade/workspace/shiny");

library(shiny)
library(reshape)
library(plyr)
library(ggplot2)

options(scipen=12, digits=2);

df <- read.csv("/Users/vkatkade/workspace/shiny/partnerAnalysis/Access Switching.csv")
rules <- read.csv("/Users/vkatkade/workspace/shiny/partnerAnalysis/VIP24_UA_rules.csv");
pricing <- read.csv("/Users/vkatkade/workspace/shiny/partnerAnalysis/VIP24_UA_data.csv");
pf <- list("C4500", "C3850", "C3650", "C3750X", "C3560X", "C3750E", "C3560E","C3750","C3560","C2960S","C2960P","C2960","C2960XR","C2960X")

drops <- c("Sales.Level.1", "Sales.Level.2", "Country", "Sold.To.Global.Ultimate.Name", "End.Customer.Global.Ultimate.Name", "End.Customer.Global.Ultimate.VM.Top", "ERP.Sales.Order.Number");
df <- df[,!(names(df) %in% drops)]
rm(drops)

# Filter out negative bookings data
df <- df[df$Product.Bookings.Net > 0,]

# Filter for Product Families of interest
df <- df[df$Product.Family %in% pf,]

partnerAnalysis <- function(partnerName) {
  
  rm(df.partner);
  rm(df.final);
  rm(df.m);
  
  # Work with this one partner for now
  df.partner <- df[df$Partner == partnerName,];
  
  # Only look for PIDs where we have some rules associated
  df.partner <- df.partner[df.partner$Product.ID %in% rules$Original.PID, ];
  
  
  df.partner <- merge(df.partner, pricing, by.x="Product.ID", by.y="PID", all.x=TRUE)
  df.partner <- rename(df.partner, c("VIP"="VIP.Current", "Price"="Price.Current"))
  df.partner <- merge(df.partner, rules, by.x="Product.ID", by.y="Original.PID", all.x=TRUE)
  df.partner <- merge(df.partner, pricing, by.x="Upsell", by.y="PID", all.x=TRUE)
  df.partner <- rename(df.partner, c("VIP"="VIP.Upsell", "Price"="Price.Upsell"))
  df.partner <- merge(df.partner, pricing, by.x="Migration", by.y="PID", all.x=TRUE)
  df.partner <- rename(df.partner, c("VIP"="VIP.Migration", "Price"="Price.Migration"))
  
  
  df.partner <- within(df.partner, "Incremental.Current" <- df.partner$VIP.Current * df.partner$Price.Current)
  df.partner <- within(df.partner, "Incremental.Upsell" <- df.partner$VIP.Upsell * df.partner$Price.Upsell)
  df.partner <- within(df.partner, "Incremental.Migrate" <- df.partner$VIP.Migration * df.partner$Price.Migration)
  
  df.partner$Incremental.Current[is.na(df.partner$Incremental.Current)] <- 0
  df.partner$Incremental.Upsell[is.na(df.partner$Incremental.Upsell)] <- 0
  df.partner$Incremental.Migrate[is.na(df.partner$Incremental.Migrate)] <- 0
  
  df.partner <- within(df.partner, "Incremental.Total" <- ifelse(df.partner$Incremental.Upsell > df.partner$Incremental.Migrate, df.partner$Incremental.Upsell, df.partner$Incremental.Migrate ))
  
  df.final <- data.frame(credit.type = factor(c("Current VIP Credits","Upsell VIP Credits", "Migrate VIP Credits")),
                         credit.value = c(sum(df.partner$Incremental.Current), sum(df.partner$Incremental.Upsell), sum(df.partner$Incremental.Migrate)));
  
  df.final <- cbind(as.data.frame(c("Switching", "Switching", "Switching")), df.final)
  colnames(df.final)[1] <- "technology"
  df.m <- melt(df.final, id.vars = c("technology", "credit.type"), measure=c("credit.value"))
  ggplot(df.m, aes(x=technology, y=value, fill=credit.type)) + geom_bar(stat='identity', color='white')  
}
 





# m.df <- melt(df2, id=c("Product.Family", "Product.ID"), measure="Product.Bookings.Net")
# c.df <- cast(m.df, Product.Family + Product.ID ~ variable, sum)