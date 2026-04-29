#load the required packages

library(lpSolve)
library(ggplot2)
library(readr)
library(scales)   



getwd()
setwd("C:/Users/anii0/OneDrive/Desktop/optimization project")
getwd()

# Load dataset
data <- read_csv("electricity_bill_dataset.csv")

# Ensure numeric columns
data$TariffRate <- as.numeric(data$TariffRate)
data$ElectricityBill <- as.numeric(data$ElectricityBill)
data$Month <- as.numeric(data$Month)

# Appliance columns
appliance_names <- c("Fan","Refrigerator","AirConditioner","Television","Monitor","MotorPump")

# Requirements
requirement <- colSums(data[appliance_names], na.rm=TRUE)

# Rates
peak_rate <- mean(data$TariffRate, na.rm=TRUE) * 1.2
offpeak_rate <- mean(data$TariffRate, na.rm=TRUE) * 0.8
objective <- rep(offpeak_rate, length(appliance_names))

# Constraints
const.mat <- diag(length(appliance_names))
const.dir <- rep(">=", length(appliance_names))
const.rhs <- requirement

# Solve LP
result <- lp("min", objective, const.mat, const.dir, const.rhs)

# Summary Table
before_cost <- sum(requirement * peak_rate)
after_cost <- result$objval
savings <- before_cost - after_cost
eff_ratio <- round(savings / sum(requirement), 2)

summary_table <- data.frame(
  Metric = c("Before Optimization Cost","After Optimization Cost","Total Savings","Efficiency Ratio"),
  Value = c(before_cost, after_cost, savings, eff_ratio)
)
print(summary_table)

# Graph 1: Optimal allocation
df <- data.frame(Appliance=appliance_names, Allocation=result$solution)
ggplot(df, aes(x=Appliance, y=Allocation, fill=Appliance)) +
  geom_bar(stat="identity") +
  ggtitle("Optimized Appliance Allocation") +
  ylab("kWh") + theme_minimal(base_size=14)

# Graph 2: Cost comparison
df_cost <- data.frame(Type=c("Before","After"), Cost=c(before_cost, after_cost))
ggplot(df_cost, aes(x=Type, y=Cost, fill=Type)) +
  geom_bar(stat="identity", width=0.6) +
  scale_fill_manual(values=c("red","green")) +
  labs(title="Electricity Bill Comparison", y="Cost (Rs.)") +
  scale_y_continuous(labels=comma) + theme_minimal(base_size=14)

# Graph 3: Savings pie chart
pie(c(after_cost, savings),
    labels=c(paste0("Optimized (",round(100*after_cost/(before_cost),1),"%)"),
             paste0("Savings (",round(100*savings/(before_cost),1),"%)")),
    col=c("skyblue","orange"), main="Savings Contribution")

# Efficiency Table 
efficiency_table <- data.frame(
  Metric = c(
    "Total Demand (all appliances)",
    "Total Savings",
    "Efficiency Ratio (E = S / total demand)",
    "Peak Tariff Rate",
    "Off-Peak Tariff Rate",
    "Tariff Ratio (peak / off-peak)"
  ),
  Value = c(
    paste0(format(sum(requirement), big.mark=","), " kWh"),
    paste0("Rs. ", format(round(savings), big.mark=",")),
    paste0(round(savings / sum(requirement), 2), " Rs. per kWh"),
    paste0(round(peak_rate, 2), " Rs./kWh"),
    paste0(round(offpeak_rate, 2), " Rs./kWh"),
    paste0(round(peak_rate / offpeak_rate, 2), "x")
  )
)

print(efficiency_table)

# Graph 5: Monthly Load Curve (numeric y-axis)
before_curve <- aggregate(as.numeric(data$ElectricityBill),
                          by=list(Month=as.numeric(data$Month)),
                          FUN=sum, na.rm=TRUE)
colnames(before_curve) <- c("Month","Consumption")
before_curve$Consumption <- as.numeric(before_curve$Consumption)

optimized_curve <- before_curve
optimized_curve$Consumption <- optimized_curve$Consumption * 0.85

ggplot() +
  geom_line(data=before_curve, aes(x=Month, y=Consumption, color="Before"), linewidth=1.2) +
  geom_line(data=optimized_curve, aes(x=Month, y=Consumption, color="After"), linewidth=1.2) +
  labs(title="Monthly Load Curve", x="Month", y="Consumption (Rs.)") +
  scale_color_manual(values=c("red","green")) +
  scale_y_continuous(labels=comma) + theme_minimal(base_size=14)

# Graph 6: Monthly Savings Trend
df_monthly <- data.frame(
  Month = before_curve$Month,
  Before = before_curve$Consumption,
  After = optimized_curve$Consumption,
  Savings = before_curve$Consumption - optimized_curve$Consumption
)

ggplot(df_monthly, aes(x=Month, y=Savings)) +
  geom_line(color="blue", linewidth=1.2) +
  geom_point(color="darkblue", size=2) +
  labs(title="Monthly Savings Trend", x="Month", y="Savings (Rs.)") +
  scale_y_continuous(labels=comma) + theme_minimal(base_size=14)

