library(XLConnect)
library(lubridate)
library(psych)
library(TTR)

df <- read.csv("X:\\Personal Development\\Data Science\\HFC\\kenya_violence\\ACLED-All-Africa-File_20170101-to-20170923_csv.csv")

# Subset to appropriate data
algeria_data <- df[which(df$COUNTRY == 'Algeria' & df$YEAR == 2017 & (df$EVENT_TYPE %in% c('Battle-No change of territory','Battle-Non-state actor overtakes territory','Battle-Government regains territory'))), ]

algeria_data$MONTH <- month(as.Date(algeria_data$EVENT_DATE, format = "%m/%d/%Y"))

# get summary statistics by month
describeBy(algeria_data$FATALITIES, algeria_data$MONTH)

aggdata <- aggregate(algeria_data$FATALITIES, by=list(algeria_data$MONTH), FUN = sum)

hist(algeria_data$FATALITIES)

algeria_data$DATE <- as.Date(algeria_data$EVENT_DATE, format = "%m/%d/%Y")

# create a full date range 
FullRange <- data.frame(DATE = seq(as.Date('2017/1/1'), as.Date('2017/8/31'), 'days'))

daily_data <- merge(x = FullRange, y = algeria_data, by = "DATE", all.x = TRUE)

daily_data$FATALITIES[is.na(daily_data$FATALITIES)] <- 0

aggdata <- aggregate(daily_data$FATALITIES, by=list(daily_data$DATE), FUN = sum)

# --------------------------------------------------------------------------------
# Distribution estimates

current_toll <- 0

end_date <- as.Date('2017-09-30')

current_date <- Sys.Date()

remaining_days <- as.numeric(end_date) - as.numeric(current_date) + 1 

moving_averages <- SMA(daily_data$FATALITIES, n = remaining_days) 

moving_averages <- moving_averages[!is.na(moving_averages)]


# Group 1: <5 deaths
group1_max_death <- 4

group1_max_rate <- (group1_max_death - current_toll)/remaining_days



# Group 2: 5 <= deaths <= 10
group2_max_death <- 10

group2_max_rate <- (group2_max_death - current_toll)/remaining_days

# Group 3: >10 deaths

group3_min_rate <- group2_max_rate

# --------------------------------------------------------------------
# Moving averages


# Divide the number of periods with rates <= the group 1 rate by the total number of periods
group1_prob <- length(moving_averages[moving_averages <= group1_max_rate])/length(moving_averages)

# Divide the number of periods with rates <= the group 2 rate by the total number of periods minus the group 1 prob
group2_prob <- length(moving_averages[moving_averages <= group2_max_rate])/length(moving_averages) - group1_prob

# Divide the number of periods with rates > the group 2 rate by the total number of periods
group3_prob <- length(moving_averages[moving_averages > group2_max_rate])/length(moving_averages)

# Sanity check, should equal 1 
group1_prob + group2_prob + group3_prob


# ------------------------------------------------------------------
# Expected value of events 
freqs <- prop.table(table(daily_data$FATALITIES))

expectation_data <- data.frame(freqs)

expectation_data$Var1 <- sort(unique(daily_data$FATALITIES))

expectation_data$expected_value <- expectation_data$Var1 * expectation_data$Freq

sum(expectation_data$expected_value)

# Multiply by the number of remaining days

sum(expectation_data$expected_value) * remaining_days

plot(aggdata$x ~ aggdata$Group.1)
