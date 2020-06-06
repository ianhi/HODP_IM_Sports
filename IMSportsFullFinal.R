## Set working directory
setwd("~/Desktop/HODP/HODP IM sports")
## Read in the points by season from csv
## load tidyverse
library(tidyverse)
library(openxlsx)
library(stargazer)
library(xtable)

##import data, set up modifications for bball, add year ranks, remove mean
setupdata <- read.csv("sportbysportfinal.csv")

sports <- c("Football.Fall", "Soccer.Fall","Frisbee.Fall","Tennis.Fall","VBFA",
            "VBFBC","VBSBC","Squash","BBA","BBB","BBC","BBForfeits","Hockey",
            "Broomball","Softball","Relay","Water.Polo","Futsal","Badminton",
            "Spike.Ball","X3v3WBB","Climbing","Dodgeball","Foosball","Swim.Meet",
            "Table.Tennis","MCA","MCB","WCA","WCB","Kickball","Billiards",
            "W.Handball","C.Handball","Fencing","Inner.Tube","VBSA","VBSB","Soccer.Spring","Football.Spring","Frisbee.Spring",
            "Tennis.Spring","VBFBP","VBSBP", "River.Run.Fall","River.Run.Spring","HOTM","Meetings","Forfeit.points.returned")

setupdata <- setupdata %>% mutate(totalBB=BBA+BBB+BBC+BBForfeits)

setupdata <- setupdata %>% 
  group_by(Year) %>%
  mutate_at(sports, scale, scale=FALSE)

setupdata <- setupdata %>% 
  group_by(Year) %>%
  mutate(year_rank=rank(-TOTAL)) %>%
  arrange(year_rank, .by_group=TRUE)

setupdata <- setupdata[-c(52:56)]
view(setupdata)



## set up dataset for first regression
## mod1: regression with mean removed and no other correction
mod1_data_seasons <- setupdata %>% filter(Year < 2019)
mod1_data_2019 <- setupdata %>% filter(Year==2019)
mod1_data_2019 <- as.data.frame(c(mod1_data_2019))

## run regression with mean removed data and no correction
mod1 = lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+VBFBP+
            River.Run.Fall+Futsal+Badminton+HOTM+Spike.Ball+X3v3WBB+Squash+
            BBA+BBB+BBC+BBForfeits+Broomball+Dodgeball+Table.Tennis+
            Meetings+Inner.Tube, data=mod1_data_seasons)
summary(mod1)


## do prediction for mod1
mod1_results <- predict(mod1, mod1_data_2019)
t(mod1_results)
mod1_results <- as.data.frame(mod1_results)
mod1_results$House <- mod1_data_2019$House
mod1_results <- mod1_results %>%
  arrange(mod1_results)
View(mod1_results)

## add in House fixed effects - see last graphs as to why this is a bad idea
mod2 = lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+VBFBP+
            River.Run.Fall+Futsal+Badminton+HOTM+Spike.Ball+X3v3WBB+Squash+
            BBA+BBB+BBC+BBForfeits+Broomball+Dodgeball+Table.Tennis+
            Meetings+Inner.Tube+House-1, data=mod1_data_seasons)
summary(mod2)

mod2_results <- predict(mod2, mod1_data_2019)
t(mod2_results)
mod2_results <- as.data.frame(mod2_results)
mod2_results$House <- mod1_data_2019$House
mod2_results <- mod2_results %>%
  arrange(mod2_results)
View(mod2_results)



##regression with only complete data + edit to basketball
mod3 <- lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+
             River.Run.Fall+HOTM+Squash+
             totalBB+Dodgeball+Table.Tennis+
             Meetings, data=mod1_data_seasons)
summary(mod3)

mod3_results <- predict(mod3, mod1_data_2019)
t(mod3_results)
mod3_results <- as.data.frame(mod3_results)
mod3_results$House <- mod1_data_2019$House
mod3_results <- mod3_results %>%
  arrange(mod3_results)
View(mod3_results)

house_effects_mod3 <- NULL
house_effects_mod3$residuals <- residuals(mod3)
house_effects_mod3 <- as.data.frame(house_effects_mod3)
house_effects_mod3$House <- mod1_data_seasons$House
house_effects_mod3$Year <- mod1_data_seasons$Year
View(house_effects_mod3)


ggplot(house_effects_mod3, aes(x=Year, y=residuals, group=House, color=House)) +
  geom_point()



## main regression with basketball adjustment
mod4 = lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+VBFBP+
            River.Run.Fall+Futsal+Badminton+HOTM+Spike.Ball+X3v3WBB+Squash+
            totalBB+Broomball+Dodgeball+Table.Tennis+
            Meetings+Inner.Tube, data=mod1_data_seasons)
summary(mod4)
stargazer(mod4, title="Linear Fit Through All Sports", dep.var.caption = "Year Rank", single.row = TRUE)

mod4_results <- predict(mod4, mod1_data_2019)
t(mod4_results)
mod4_results <- as.data.frame(mod4_results)
mod4_results$House <- mod1_data_2019$House
mod4_results <- mod4_results %>%
  arrange(mod4_results)
xtable(mod4_results)
View(mod4_results)

house_effects_mod4 <- NULL
house_effects_mod4$predicted <- predict(mod4)
house_effects_mod4$residuals <- residuals(mod4)
house_effects_mod4 <- as.data.frame(house_effects_mod4)
house_effects_mod4$House <- mod1_data_seasons$House
house_effects_mod4$Year <- mod1_data_seasons$Year
View(house_effects_mod4)

png("mod4_residual_plot.png")
mod4_residual_plot <- ggplot(house_effects_mod4, aes(x=Year, y=residuals, group=House, color=House)) +
  geom_point() + xlab("Year") + ylab("Rank - Predicted Rank") +
  ylim(-3,4) +
  theme(legend.position="bottom") + ggtitle("Residuals over time for Houses")+theme_hodp()+ theme(plot.title = element_text(size=15))
print(mod4_residual_plot)
dev.off()

mod4_residual_plot

house_effects_limited_mod4 <- house_effects_mod4 %>% 
                            group_by(House) %>% filter(House == "Mather" | House== "Kirkland" | House == "Lowell" | House == "Currier")


png("mod4_limited_residual_plot.png")
mod4_limited_residual_plot <- ggplot(house_effects_limited_mod4, aes(x=Year, y=residuals, group=House, color=House)) +
        geom_line() +geom_point() + ylab("Rank - Predicted Rank") +
        ylim(-3,4) + theme(legend.position="bottom") + ggtitle("Residuals over time for top 4 houses in 2019-2020")+theme_hodp()+
        scale_color_manual(values=primary)+ theme(plot.title = element_text(size=15))
print(mod4_limited_residual_plot)
dev.off()

mod4_limited_residual_plot

## set up for the lagged regressions
lagregressionsetup_lag1 <- setupdata %>%
  group_by(House) %>%
  mutate(lagged_ranks1 = lag(year_rank, n=1)) %>%
  filter(Year > 2012)

lagregressionsetup_lag1_2019 <- lagregressionsetup_lag1 %>%
  filter(Year == 2019)

lagregressionsetup_lag1_seasons <-lagregressionsetup_lag1 %>%
  filter(Year < 2019)

lagregressionsetup_lag2 <- setupdata %>%
  group_by(House) %>%
  mutate(lagged_ranks1 = lag(year_rank, n=1)) %>%
  mutate(lagged_ranks2 = lag(year_rank, n=2)) %>%
  filter(Year > 2013)

lagregressionsetup_lag2_2019 <- lagregressionsetup_lag2 %>%
  filter(Year == 2019)

lagregressionsetup_lag2_seasons <-lagregressionsetup_lag2 %>%
  filter(Year < 2019)

## run lagged prediction 1
mod5 = lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+VBFBP+
            River.Run.Fall+Futsal+Badminton+HOTM+Spike.Ball+X3v3WBB+Squash+
            totalBB+Broomball+Dodgeball+Table.Tennis+
            Meetings+Inner.Tube+lagged_ranks1, data=lagregressionsetup_lag1_seasons)
summary(mod5)

mod5_results <- predict(mod5, lagregressionsetup_lag1_2019)
t(mod5_results)
mod5_results <- as.data.frame(mod5_results)
mod5_results$House <- lagregressionsetup_lag1_2019$House
mod5_results <- mod5_results %>%
  arrange(mod5_results)
View(mod5_results)

## run lagged prediction 2
mod6 = lm(year_rank ~ Soccer.Fall+Frisbee.Fall+Tennis.Fall+VBFA+VBFBP+
            River.Run.Fall+Futsal+Badminton+HOTM+Spike.Ball+X3v3WBB+Squash+
            totalBB+Broomball+Dodgeball+Table.Tennis+
            Meetings+Inner.Tube+lagged_ranks1+lagged_ranks2, data=lagregressionsetup_lag2_seasons)
summary(mod6)
stargazer(mod6, title="Linear Fit Through All Sports and Previous Rankings",single.row = TRUE)

mod6_results <- predict(mod6, lagregressionsetup_lag2_2019)
t(mod6_results)
mod6_results <- as.data.frame(mod6_results)
mod6_results$House <- lagregressionsetup_lag2_2019$House
mod6_results <- mod6_results %>%
  arrange(mod6_results)
View(mod6_results)
xtable(mod6_results)


## set up results with the prediction intervals
mod4_results_predictions <- predict(mod4, mod1_data_2019, interval = "prediction")
mod4_results_predictions <- as.data.frame(mod4_results_predictions)
mod4_results_predictions <- mod4_results_predictions %>% 
                            mutate(House=mod1_data_2019$House) %>%
                            arrange(fit)
View(mod4_results_predictions)
xtable(mod4_results_predictions)

mod6_results_predictions <- predict(mod6, lagregressionsetup_lag2_2019, interval = "prediction")
mod6_results_predictions <- as.data.frame(mod6_results_predictions)
mod6_results_predictions <- mod6_results_predictions %>% 
  mutate(House=lagregressionsetup_lag2_2019$House) %>%
  arrange(fit)
View(mod6_results_predictions)
xtable(mod6_results_predictions)







## descriptive statistics
## percentages
percent_data <- read.csv("scoresbyseasonfinal.csv")

percent_data <- percent_data %>% mutate(springpercent = Spring/TOTAL) %>% 
                mutate(fallpercent = Fall/TOTAL) %>%
                mutate(winterpercent = Winter/TOTAL) %>%
                group_by(Year) %>%
                mutate(year_rank=rank(-TOTAL)) %>%
                arrange(year_rank, .by_group=TRUE) %>%
                filter(Year < 2019)
View(percent_data)

avg_spring_percent <- mean(percent_data$springpercent)
avg_winter_percent <- mean(percent_data$winterpercent)
avg_fall_percent <- mean(percent_data$fallpercent)

avg_spring_percent_ranks <- aggregate(percent_data$springpercent, list(percent_data$year_rank), mean)
View(avg_spring_percent_ranks)
xtable(avg_spring_percent_ranks)


## zscores by category and total
zscore_setup <- read.csv("sportbysportfinal.csv")
winloss <- c("Football.Fall", "Soccer.Fall","Frisbee.Fall","Tennis.Fall","VBFA",
             "VBFBC","Squash","BBA","BBB","BBC","BBForfeits","Hockey",
             "Broomball","Softball", "VBSA", "VBSBC", "Tennis.Spring")
tournament <- c("Relay","Water.Polo","Futsal","Badminton",
                "Spike.Ball","X3v3WBB","Climbing","Dodgeball","Foosball","Swim.Meet",
                "Table.Tennis","MCA","MCB","WCA","WCB","Kickball","Billiards",
                "W.Handball","C.Handball","Fencing","Inner.Tube","Soccer.Spring",
                "Football.Spring","Frisbee.Spring")
participation <- c("VBFBP","VBSBP", "River.Run.Fall","River.Run.Spring","HOTM")
other <- c("Meetings","Forfeit.points.returned")

zscore_setup$winlosssum <- rowSums(zscore_setup[,winloss])
zscore_setup$tournamentsum <- rowSums(zscore_setup[,tournament])
zscore_setup$participationsum <- rowSums(zscore_setup[,participation])
zscore_setup$othersum <- rowSums(zscore_setup[,other])

zscore_setup <- zscore_setup %>%
                group_by(Year) %>%
                mutate(year_rank=rank(-TOTAL)) %>%
                arrange(year_rank, .by_group=TRUE)

zscoresummaryidx <- c("House","Year","TOTAL","year_rank","winlosssum","tournamentsum","participationsum","othersum")

zscorers <- c("winlosssum","tournamentsum",
              "participationsum","othersum","TOTAL")

zscoresummary <- zscore_setup[,zscoresummaryidx]
zscoresummary <- zscoresummary %>% filter(Year < 2019)
zscoresummary <- zscoresummary %>% group_by(Year) %>% 
  mutate_at(zscorers, scale)

## average z score per rank
summary_zscore_mean <- aggregate(zscoresummary[,c(3,5:8)], list(zscoresummary$year_rank), mean)
View(summary_zscore_mean)
xtable(summary_zscore_mean)



## one final basic regression
basic_reg_setup <- read.csv("scoresbyseasonfinal.csv")

View(basic_reg_setup)

basic_reg_setup <- basic_reg_setup %>%
                    mutate(fallwintertotal = Fall+Winter) %>%
                    group_by(Year) %>%
                     mutate(year_rank=rank(-TOTAL)) %>%
                    mutate(year_rank_partial=rank(-fallwintertotal)) %>%
                    arrange(year_rank, .by_group=TRUE)
View(basic_reg_setup)

basic_reg_seasons <- basic_reg_setup %>% filter(Year <2019)
basic_reg_2019 <- basic_reg_setup %>% filter(Year > 2018)

mod_basic <- lm(year_rank ~ year_rank_partial, basic_reg_seasons)
summary(mod_basic)
basic_mod_results_intervals <- predict(mod_basic, basic_reg_2019, interval="prediction")
basic_mod_results_intervals <- as.data.frame(basic_mod_results_intervals)
basic_mod_results_intervals <- basic_mod_results_intervals %>%mutate(House=basic_reg_2019$House)
View(basic_mod_results_intervals)

basic_mod_results <- as.data.frame(predict(mod_basic, basic_reg_2019)) %>% mutate(House=basic_reg_2019$House)
xtable(basic_mod_results)
xtable(basic_mod_results_intervals)
stargazer(mod_basic, dep.var.caption = "Year Rank", single.row = TRUE)


primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB')
sidebysidebarplot <- c("#ef3e3e", "#2c3e50")
theme_hodp <- function () { 
  theme_classic(base_size=12, base_family="Helvetica") %+replace%
    theme(
      panel.background  = element_rect(fill="#F2F2F2", colour=NA),
      plot.background = element_rect(fill="#F2F2F2", colour="#d3d3d3"),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle = element_text(size=18,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.caption = element_text(size=8,  family="Helvetica", hjust = 1),
      axis.text.x =element_text(size=10,  family="Helvetica"),
      axis.title.x =element_text(size=14, family="Helvetica", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, family="Helvetica", angle=90, face ='bold'),
      legend.title=element_text(size=10, family="Helvetica"), 
      legend.text=element_text(size=10, family="Helvetica"),
      legend.position = "bottom",
      axis.ticks = element_blank()
    )
}
