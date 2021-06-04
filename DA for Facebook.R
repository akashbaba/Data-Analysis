install.packages("mice")
library('mice')
install.packages('tidyverse')
library('tidyverse')
install.packages('corrplot')
library(corrplot)


#Replace the null values (NA) of gender column with its mode or median and explain
#why mode/median used to replace NA values

interview=read.csv('C:/Users/v-rakul/Desktop/facebook_user_data.csv',header = TRUE)
is.na(interview$gender)
which(is.na(interview$gender))
f_dta <- interview$gender
## Convert to `as.character`
f_dta <- as.character(f_dta)
## Replace Na
f_dta <- f_dta %>%
  replace_na("Unknown")
## Convert Back to Factor
f_dta <- as.factor(f_dta)
## Now we can place it back in the data set
interview$gender <- f_dta
fb_data <- subset(interview,select = - gender)
df_data <- as.data.frame(fb_data)




##Replace the null values (NA) of tenure column (numerical variable) with its median,
##and explain why mode/median used to replace NA values

which(is.na(interview$tenure))
interviewt<-(is.na(interview$tenure))
mydata<- as_tibble(interview)
mydatafemale<-mydata %>% filter(gender == "female") 
meanfemale<-mean(mydatafemale$tenure, na.rm = TRUE)
interview[is.na(interview$tenure),]$tenure <- meanfemale
interview$tenure[which(is.na(interview$tenure))] <- mean(interview$tenure,na.rm = TRUE)


##correlation matrix on all the columns.

cor(df_data)



#What is composition of male and female users?
install.packages('janitor')
library('janitor')
tabyl(interview$gender)



##Which category of gender has more friends?
frc=read.csv('C:/Users/v-rakul/Desktop/Count_friend_ gender.csv',header = TRUE)

##the data above is extracted from power bi

ggplot(frc, aes(factor(frc$gender),frc$ï..Count.of.friend_count)) + 
  geom_bar(stat = "identity", width = 0.2, position = "dodge") +
  labs(list(x = "x", y = "count",fill = "group"))




##Which category of gender initiated more friendships?
df_plot2 <- data.frame(interview$friendships_initiated, 
                      interview$gender,
                      interview$friend_count
                      )
ggplot(df_plot2, aes(factor(df_plot2$interview.friendships_initiated), df_plot2$interview.friend_count, fill = df_plot2$interview.gender)) + 
  geom_bar(stat = "identity", width = 0.2, position = "dodge") +
  labs(list(x = "x", y = "count",fill = "group"))



##What is the distribution of tenure across different categories of gender?
ten=read.csv('C:/Users/v-rakul/Desktop/Count_of_tenure.csv',header = TRUE)

##the data above is extracted from power bi

ggplot(ten, aes(factor(ten$ï..gender),ten$Count.of.tenure)) + 
  geom_bar(stat = "identity", width = 0.2, position = "dodge") +
  labs(list(x = "x", y = "count",fill = "group"))




##How many users have no friends?

nofriends<- interview$friend_count
tabyl(nofriends)


##How many users did not like any posts?

likes<-interview$likes
tabyl(likes)



##How many users did not receive any likes?

reclike<-interview$likes_received
tabyl(reclike)


##What is the average number of posts liked by users (based on gender) through web vs.
##mobile devices?

##please refer ppt document
##you calculate using pivot table fuction but i found it hard so used power bi to find



##What is the average number of likes received by users (based on gender) through web
##vs. mobile devices

##please refer ppt document
##you calculate using pivot table fuction but i found it hard so used power bi to find



