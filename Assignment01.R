###########################################
# ASSIGNMENT 1 IN COMPUTING FOR ANALYTICS #
# PREPARED BY: ARTEMIO R. AMBAS JR.       #
###########################################

#- Problem number 1

#d.
w<-read.csv("who.csv")
print(lmin <- min(w$LiteracyRate, na.rm = TRUE))
print(country<-subset(w, LiteracyRate == min(w$LiteracyRate, na.rm =TRUE))$Country)

#e.
print(max_gni_eur <- max(subset(w, Region == 'Europe')$GNI, na.rm = TRUE))
print(subset(w, GNI == max_gni_eur)$Country)

#f
print(mean_gni_afr <- mean(subset(w, Region == 'Africa')$LifeExpectancy, na.rm = TRUE))

#g
print(NROW(coun_pop_10k <- subset(w, Population > 10000)$Country))

#h
x<-subset(w,Region == "Americas")
print(head(x[order(x$ChildMortality, decreasing = FALSE),],5)$Country)
#- end of number 1

#- Problem number 2

# a.
library('xlsx')
nba<-read.xlsx("Historical NBA Performance.xlsx","Sheet1")
print(bulls_highest <- max(subset(nba,  Team == 'Bulls')$Winning.Percentage, na.rm = TRUE))
print(team_name <- subset(nba, Winning.Percentage == bulls_highest)$Team)

# b.
print(even_team <- subset(nba, Winning.Percentage == 0.5)$Winning.Percentage)
print(even_team <- subset(nba, Winning.Percentage == 0.5)$Team)
#- end of number 2

#- Problem number 3

players_stat <- read.csv('Seasons_Stats.csv')
stat.anova(players_stat)
str(players_stat)

# a.
top_x3p <- max(players_stat$X3P, na.rm = TRUE)
top_x3p_player<-subset(players_stat, X3P == top_x3p)
print(top_x3p)
print(top_x3p_player$Player)

# b.
top_ftr <- max(players_stat$FTr, na.rm = TRUE)
top_ftr_player<-subset(players_stat, FTr == top_ftr)
print(top_ftr)
print(top_ftr_player$Player)

# c.
lebron_stat <-subset(players_stat, Player == 'LeBron James')
lebron_highest <- max(lebron_stat$FG)
lebron_highest_season<-subset(lebron_stat, FG == lebron_highest)
print(lebron_highest_season$FG)
print(lebron_highest_season$Year)

# d.
jordan_stat <-subset(players_stat, Player == 'Michael Jordan*')
jordan_highest <- max(jordan_stat$FG)
jordan_highest_season<-subset(jordan_stat, FG == jordan_highest)
print(jordan_highest_season$Player)
print(jordan_highest_season$FG)
print(jordan_highest_season$Year)

# e.
kobe_stat <-subset(players_stat, Player == 'Kobe Bryant')
kobe_lowest <- min(kobe_stat$MP)
kobe_lowest_season <-subset(kobe_stat, MP == kobe_lowest)
print(kobe_lowest_season$Player)
print(kobe_lowest_season$Year)
print(kobe_lowest_season$MP)
print(kobe_lowest_season$PER)
#- end of number 3

#- Problem number 4

# a.
university_stat<- read.csv('National Universities Rankings.csv')
university_stat$Undergrad.Enrollment<-gsub('\\$',"",university_stat$Undergrad.Enrollment)
university_stat$Undergrad.Enrollment<-gsub('\\"',"",university_stat$Undergrad.Enrollment)
university_stat$Undergrad.Enrollment<-gsub('\\,',"",university_stat$Undergrad.Enrollment)
university_stat$Undergrad.Enrollment<-as.numeric(university_stat$Undergrad.Enrollment)
print(university_stat[which.max(university_stat$Undergrad.Enrollment),]$Undergrad.Enrollment)
print(university_stat[which.max(university_stat$Undergrad.Enrollment),]$Name)

# b.
university_stat$Tuition.and.fees<-gsub('\\$',"",university_stat$Tuition.and.fees)
university_stat$Tuition.and.fees<-gsub('\\"',"",university_stat$Tuition.and.fees)
university_stat$Tuition.and.fees<-gsub('\\,',"",university_stat$Tuition.and.fees)
university_stat$Tuition.and.fees<-as.numeric(university_stat$Tuition.and.fees)
university_ranking<-university_stat[order(university_stat$Rank,decreasing = FALSE),]
university_ranking<-university_ranking[1:10,]
print(mean(university_ranking$Tuition.and.fees))
print(university_ranking$Name)
#- end of number 4