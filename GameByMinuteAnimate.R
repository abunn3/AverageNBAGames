library(ggplot2)
library(gganimate)
library(transformr)
library(dplyr)
 
dir <- '***\\By Minute Chart' #set path
setwd(dir)

df1 <- read.csv('AverageGamebyMinute2015.csv')
df2 <- read.csv('AverageGamebyMinute2016.csv')
df3 <- read.csv('AverageGamebyMinute2017.csv')
df4 <- read.csv('AverageGamebyMinute2018.csv')
df5 <- read.csv('AverageGamebyMinute2019.csv')
all <- rbind(df1,df2,df3,df4,df5)

year <-c(rep(2015, 1500), rep(2016, 1500), rep(2017, 1500), rep(2018, 1500), rep(2019, 1500))
year <- as.data.frame(year)
all <- cbind(all, year)
all_sort <-arrange(all,Team)

facet_order <- c("MIL","GSW","TOR","DEN","BOS","UTA","POR","IND", "PHI", "OKC", "HOU", "SAS", "LAC", "MIA", "DET", "ORL",
                 "BKN", "NOP", "SAC", "CHA", "MIN", "DAL", "LAL", "MEM", "WAS", "ATL", "CHI", "CLE", "NYK", "PHX")

all_sort$Team <- factor(all_sort$Team, levels = facet_order)


#bar plot
r <- ggplot(all_sort, aes(x = Minute, y = Margin, color = Margin, fill = Margin)) + 
  geom_bar(stat = "identity")+
  scale_color_gradient(low = "#3582b9", high = "#f5ebff")+
  scale_fill_gradient(low = "#3582b9", high = "#f5ebff")+
  scale_x_continuous(limits = c(0, 49), breaks = c(0, 12, 24, 36, 48))
r <- r + facet_wrap(~Team, scales = "fixed", nrow = 6)+theme_minimal()

r <- r + geom_vline(xintercept = 12, aes(alpha = 0.5)) + 
  geom_vline(xintercept = 24) + 
  geom_vline(xintercept = 36)+
  geom_vline(xintercept = 48)+
  geom_hline(yintercept = 0)

r <- r + labs(title = "Average game in the NBA: {closest_state}", 
              subtitle = "Calculated mean of the score margin for each minute of every game")+
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = '#ecf3e7', colour = '#ecf3e7'))+
  theme(plot.title = element_text(size=28, face="bold", margin = margin(10, 0, 10, 0), color="#04295e"))+
  theme(axis.title.x = element_text(color="#04295e", vjust=-0.35),
        axis.title.y = element_text(color="#04295e" , vjust=0.35),
        strip.text = element_text(size=14, face = "bold", color = "#04295e")
  )
# r
anim <- r + 
  transition_states(year,
                    transition_length = 10,
                    state_length = 20)+
  ease_aes('cubic-in-out') # Slow start and end for a smoother look

anim <- anim + enter_fade() + exit_shrink()
animate(anim, nframes = 150, width= 1200, height=960)
anim_save("AvgGame2015_2019.gif")
