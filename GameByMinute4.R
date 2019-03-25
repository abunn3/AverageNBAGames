#Create a vector that gives the score at the end of each minute
library(nbastatR)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape2)

delay <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 
}

allTeams <- as.data.frame(rep(0,50))

#list of all games
gl<-game_logs(seasons = 2018, result_types = "Team")
teams <- unique(gl$slugTeam)

for (j in c(1:30)) {
  
  
  #Selected team
  gl2 <- gl[c(gl$slugTeam == teams[j]),]
  games <- as.data.frame(rep(0,50))
  gameIDs <- gl2$idGame
  for (i in c(1:length(gameIDs))){
    #Example game is Jazz @ Suns Mar 13, 2019
    p<-play_by_play(game_ids = gameIDs[i], nest_data = F, return_message = T)
    
    #grab the game time and score columns of a game play by play (4 columns total)
    TScols <- p[c(12,16:18)]
    TScols$marginScore <- TScols$scoreAway - TScols$scoreHome
    
    #Pre-allocate data frame to populate
    # minutes <- TScols[nrow(TScols),1]+1
    minuteGame <- c(0:49)
    TS_red <- as.data.frame(minuteGame)
    
    col_headings <- c("minuteGame", "scoreHome", "scoreAway", "marginScore")
    
    gameEnd <- as.data.frame(cbind(49,TScols$scoreHome[nrow(TScols)], TScols$scoreAway[nrow(TScols)], TScols$marginScore[nrow(TScols)]))
    names(gameEnd) <- col_headings
    TScols <- rbind(TScols, gameEnd)
    
    time_round <- ceiling(TScols$minuteGame) 
    TScols$minuteGame <- time_round
    
    # TScols$marginScore <- TScols$scoreAway - TScols$scoreHome #replace margin column with one that includes a 0 where the score is tied
    
    dr1 <- TScols[complete.cases(TScols$marginScore),] #removes rows where there is no score change
    dr2 <- dr1[order(dr1$minuteGame, decreasing=TRUE),] #sort from end of game
    dr3 <- dr2[!duplicated(dr2$minuteGame),] #remove duplicates based on minuteGame
    dr4 <- dr3[order(dr3$minuteGame, decreasing = FALSE),] #reorder from 0-48 for game time
    dr5 <- merge(TS_red,dr4, all.x = TRUE) #merge with list of 0-48 to account for minutes where no points are scored
    dr6 <-na.locf(dr5, fromLast = TRUE) #duplicate from last for na items
    if (gl2[i,13] == "A") {
      dr6[4] <- dr6[4] *-1
    }
    games[i]<-dr6[4]
    # dr6 <- dr6 %>% mutate(mycolor = ifelse(marginScore>0, "type1", "type2")) #add color sorting column for +/-
    delay(.5)
  }
  names(games) <- gameIDs
  games <- cbind(dr6[1],games)
  
  seasonMargin <- rowSums(games[2:ncol(games)]) / length(gameIDs)
  allTeams[j] <- seasonMargin
  
  
  # games <- cbind(games, seasonMargin)
  # games <- games %>% mutate(mycolor = ifelse(seasonMargin>0, "type1", "type2")) #add color sorting column for +/-
}

allTeams <- cbind(dr6[1],allTeams)
names(allTeams) <- c("minuteGame", teams)
allTeamsLong <- melt(allTeams, id.vars = c("minuteGame"))
names(allTeamsLong) <- c("Minute", "Team", "Margin")

facet_order <- c("MIL","GSW","TOR","DEN","BOS","UTA","POR","IND", "PHI", "OKC", "HOU", "SAS", "LAC", "MIA", "DET", "ORL",
                 "BKN", "NOP", "SAC", "CHA", "MIN", "DAL", "LAL", "MEM", "WAS", "ATL", "CHI", "CLE", "NYK", "PHX")

allTeamsLong$Team <- factor(allTeamsLong$Team, levels = facet_order)

#bar plot
r <- ggplot(allTeamsLong, aes(x = Minute, y = Margin, color = Margin, fill = Margin)) + 
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
          
r <- r + labs(title = "An Average game for each team in the NBA", 
              subtitle = "Calculated mean of the game margin for each minute of every game so far (3/22/2019)")+
              theme(legend.position = "none")+
              theme(panel.background = element_rect(fill = '#ecf3e7', colour = '#ecf3e7'))+
              theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0), color="#04295e"))+
              theme(axis.title.x = element_text(color="#04295e", vjust=-0.35),
                    axis.title.y = element_text(color="#04295e" , vjust=0.35)   
              )

r
