## WHAT NIGHT WOULD BE BEST TO RUN A MARKETING PROMOTION TO INCREASE ATTENDANCE?

df <- read.csv("Week3/dodgers.csv", header=TRUE)
head(df)
tail(df)

summary(df)
table(df$opponent)  # Number of games by opponent
length(unique(df$opponent))  # Number of unique opponents
barplot(table(df$opponent))

ggplot(df, aes(x=as.factor(opponent))) + geom_bar() + 
  coord_flip() + scale_y_continuous(n.breaks=8)

## BOXPLOTS ##
# boxplot(df, ylab="Y label", xlab="X label", 
#         main="Main title")

# bymonth <- df
# bymonth$month <- factor(bymonth$month, 
#                         levels=c("APR", "MAY", "JUN", "JUL", 
#                                  "AUG", "SEP", "OCT"))
# boxplot(attend ~ month, bymonth)

df$month <- factor(df$month, levels=c("APR", "MAY", "JUN", "JUL", 
                                      "AUG", "SEP", "OCT"))
boxplot(attend ~ month, df)
?boxplot
# OCT is lowest max and lowest median

boxplot(attend ~ day, df)
cor(df$attend, df$day)
print("test") 
print(cor(df$attend, df$day))
# very low correlation w/day number (no surprise)

# byDoW <- bymonth
# byDoW$day_of_week <- factor(byDoW$day_of_week, 
#                             levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
#                                      "Thursday", "Friday", "Saturday"))
# boxplot(attend ~ day_of_week, byDoW)

df$day_of_week <- factor(df$day_of_week, 
                         levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                  "Thursday", "Friday", "Saturday"))
boxplot(attend ~ day_of_week, df)
# Monday is the lowest overall by most metrics (then Wednesdays)

head(df)
boxplot(attend ~ opponent, df)
# Come back to opponents later...

boxplot(attend ~ temp, df)
# prolly better as a SCATTER PLOT

boxplot(attend ~ skies, df)
# Attendance is lower when cloudy, but not a lot really

boxplot(attend ~ day_night, df)
# Basically the same; more variation in the night games (lower half)

boxplot(attend ~ cap, df)
# slightly lower on cap nights (small number of cap nights?)

boxplot(attend ~ shirt, df)
# higher shirt nights, for sho (shirts because higher attendance night?)

boxplot(attend ~ fireworks, df)
# roughly the same, but more variation on non-fireworks nights

boxplot(attend ~ bobblehead, df)
# much better on bobblehead nights (again, bobbleheads because busier?)


head(df)
## SCATTER PLOTS ##
library(ggplot2)
ggplot(df, aes(x=day, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=day_of_week, y=attend, color=df$day_night)) + geom_point() + geom_jitter(width=0.1)
ggplot(df, aes(x=opponent, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=temp, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=skies, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=day_night, y=attend, color=day_of_week)) + geom_point() + geom_jitter(width=0.1)
ggplot(df, aes(x=cap, y=attend)) + geom_point() + geom_smooth(method="lm") + geom_jitter(width=0.1)
ggplot(df, aes(x=shirt, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=fireworks, y=attend)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=bobblehead, y=attend)) + geom_point() + geom_smooth(method="lm")

ggplot(df, aes(x=opponent, y=attend)) + geom_jitter(width=0.1)
ggplot(df, aes(x=opponent, y=attend)) + geom_violin() # Kind of like this one

ggplot(df, aes(x=opponent, y=attend)) + geom_violin() + 
  labs(x="Opponent", y="Attendance", title="Attendance by Opponent") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df, aes(x=opponent, y=attend)) + geom_boxplot()

ggplot(df, aes(x=day, y=attend)) + geom_point() + geom_smooth(method="lm") + 
  labs(x="Day of Month", y="Attendance", title="Attendance by Day of Month") + 
  scale_x_continuous(n.breaks=20)

ggplot(df, aes(x=day_of_week, y=attend, color=df$day_night)) + geom_point() +
  labs(x="Day of Week", y="Attendance", title="Weekday Attendance by Time") + 
  geom_jitter(width=0.1) + guides(color=guide_legend(title="Time of Day"))


## REGRESSION
# lm(formula = attend ~ month + day_of_week + bobblehead, data = df)
relation <- lm(formula = attend ~ month + day_of_week + bobblehead, data = df)
summary(relation)

# mayMon <- data.frame(month="MAY", day_of_week="Monday", bobblehead="NO")
# predict(relation, mayMon)
# 
# mayWed <- data.frame(month="MAY", day_of_week="Wednesday", bobblehead="NO")
# predict(relation, mayWed)
# 
# octMon <- data.frame(month="OCT", day_of_week="Monday", bobblehead="NO")
# predict(relation, octMon)
# 
# octWed <- data.frame(month="OCT", day_of_week="Wednesday", bobblehead="NO")
# predict(relation, octWed)

# Prediction
noBobble <- data.frame(month=c("MAY", "MAY", "MAY", "OCT", "OCT", "OCT"), 
                   day_of_week=c("Monday", "Wednesday", "Thursday"),
                   bobblehead="NO")
noBobble
predict(relation, noBobble)

yesBobble <- data.frame(month=c("MAY", "MAY", "MAY", "OCT", "OCT", "OCT"),  
                   day_of_week=c("Monday", "Wednesday"),
                   bobblehead="YES")
predict(relation, yesBobble)

mean(df$attend)
median(df$attend)
hist(df$attend, xlab="Attendance", main="Histogram of Game Attendance")
?hist
hist(df$attend, breaks=25)

test <- noBobble
test
test$prediction <- predict(relation, noBobble)
test
