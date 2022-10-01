### Premier League World Cup

# Last updated: 1st October 2022

# The purpose of this R script is to test how often the best team wins a tournament structured like a World Cup or
# European Championships. Or, to put it another way, how much luck has to play in deciding the winner. This model
# assumes that the results of a Premier League season are an accurate way of determining 'the best team' (i.e. the
# Premier League final table is the gold standard measure of 'best'). It then generates a structure where the same
# Premier League season would be replayed (with the same results), but on a much smaller, World Cup style basis.
# Randomness is introduced in three ways: 
#   1. Formation of the group stage teams / routes to the final; 
#   2. Decisions on who progresses in knock-out rounds that result in draws;
#   3. Selection of which of the two games between each team in the Premier League season are used in the tournament.

# The tournament consists of 16 teams split into four round robin groups of four. The top two teams in each group
# progress to the quarter finals. The structure is basically the same as the 1970 World Cup. Groups are formed randomly
# with the exception of seeding for the top four teams, which is based on the Premier League table from the 
# previous season. The competing teams are the top 16 teams from the previous season.

### Note: to change the season, update code in line 27 and 175


## Load data
# The datasets are all downloaded from https://datahub.io/sports-data/english-premier-league

dir    <- "C:\\Users\\peterds\\Desktop\\TEMPORARY"
dbname <- "season-1516.csv"

# Choice from "season-1819.csv", "season-1617.csv", "season-1516.csv"

setwd(dir)
db <- read.csv(dbname)

# Percentage likelihood that higher rank team wins in event of draw in knockout stage
koperc <- 0.6

# Set number of tournaments to simulate
num <- 10000

## Functions
win <- function(team, home, away, homescore, awayscore) {
  if(team == home) {
    ifelse(homescore > awayscore, 1, 0)
  } else {
    if (team == away) {
      ifelse(awayscore > homescore, 1, 0)
    }
    else {
      0
    }
  } 
}

draw <- function(team, home, away, homescore, awayscore) {
  if(team == home | team == away) {
    ifelse(homescore == awayscore, 1, 0)
  } else {
    0
  }
}

lose <- function(team, home, away, homescore, awayscore) {
  if(team == home) {
    ifelse(homescore < awayscore, 1, 0)
  } else {
    if (team == away) {
      ifelse(awayscore < homescore, 1, 0)
    }
    else {
      0
    }
  } 
}

goalsfor <- function(team, home, away, homescore, awayscore) {
  if(team == home) {
    homescore
  } else {
    if (team == away) {
      awayscore
    }
    else {
      0
    }
  } 
}

goalsagainst <- function(team, home, away, homescore, awayscore) {
  if(team == home) {
    awayscore
  } else {
    if (team == away) {
      homescore
    }
    else {
      0
    }
  } 
}

kowinner <- function(home, away, homescore, awayscore, homerank, awayrank) {
  if (homescore > awayscore) {
    return(1) 
  } else {
    if (awayscore > homescore) {
      return(0)
    } else {
      if (homerank > awayrank) {
        rbinom(1,1,koperc)
      } else {
        rbinom(1,1,1-koperc)
      }
    }
  }
}

idwinner <- function(homewin, HomeID, AwayID) {
  homewin*HomeID + (1-homewin)*AwayID
}

## Select teams for the tournament

teams1819 <- c("Man City",    # NB: This is based on position at end of 2017/18 season
               "Man United",
               "Tottenham",
               "Liverpool",
               "Chelsea",
               "Arsenal",
               "Burnley",
               "Everton",
               "Leicester",
               "Newcastle",
               "Crystal Palace",
               "Bournemouth",
               "West Ham",
               "Watford",
               "Brighton",
               "Huddersfield")

teams1617 <- c("Leicester",  # NB: This is based on position at end of 2015/16 season
               "Arsenal",
               "Tottenham",
               "Man City",
               "Man United",
               "Southampton",
               "West Ham",
               "Liverpool",
               "Stoke",
               "Chelsea",
               "Everton",
               "Swansea",
               "Watford",
               "West Brom",
               "Crystal Palace",
               "Bournemouth")

teams1516 <- c("Chelsea",      # NB: This is based on position at end of 2014/15 season
               "Man City",
               "Arsenal",
               "Man United",
               "Tottenham",
               "Liverpool",
               "Southampton",
               "Swansea",
               "Stoke",
               "Crystal Palace",
               "Everton",
               "West Ham",
               "West Brom",
               "Leicester",  
               "Newcastle",
               "Sunderland")

tab <- data.frame(position = 1:16,
                  team = teams1516)

# NB: Choice above is teams1819, teams1617, teams1516

# Generate ID numbers for each game in the results database
db$team <- db$HomeTeam

dbresults <- merge(db, tab, by = "team")

dbresults <- data.frame(HomeTeam = dbresults$HomeTeam,
                        AwayTeam = dbresults$AwayTeam,
                        FTHG = dbresults$FTHG,
                        FTAG = dbresults$FTAG,
                        HomeID = dbresults$position,
                        team = dbresults$AwayTeam)

dbresults <- merge(dbresults, tab, by = "team")

dbresults <- data.frame(HomeTeam = dbresults$HomeTeam,
                        AwayTeam = dbresults$AwayTeam,
                        FTHG = dbresults$FTHG,
                        FTAG = dbresults$FTAG,
                        HomeID = dbresults$HomeID,
                        AwayID = dbresults$position)

dbresults$gameID = dbresults$HomeID*100 + dbresults$AwayID

# Loop around the results begins here
WCresults <- data.frame(it = numeric(),
                        winner = character())

for (j in 1:num){
  
  # Generate four random groups. Only top 4 teams are seeded, then all other teams positioned randomly
  pool <- sample(5:16, size = 12) # This randomly orders teams 5 to 16
  
  # Group 1
  group1teams <- c(tab$team[1],
                   tab$team[pool[1:3]])
  group1 <- data.frame(ID = c(1,pool[1:3]),
                       team = group1teams)
  
  # Create subset of group games
  g1 <- c(sample(c(1*100+pool[1], pool[1]*100+1),1),
          sample(c(1*100+pool[2], pool[2]*100+1),1),
          sample(c(1*100+pool[3], pool[3]*100+1),1),
          sample(c(pool[1]*100+pool[2], pool[2]*100+pool[1]),1),
          sample(c(pool[1]*100+pool[3], pool[3]*100+pool[1]),1),
          sample(c(pool[2]*100+pool[3], pool[3]*100+pool[2]),1))
  group1games <- dbresults[dbresults$gameID%in%g1,]
  
  ## Build group 1 results table
  # First generate results for each single team then combine
  groupbuild <- NULL
  
  for (i in c(1,pool[1:3])){
    group1games$wins <- mapply(win,
                               team = tab$team[i],
                               home = group1games$HomeTeam,
                               away = group1games$AwayTeam,
                               homescore = group1games$FTHG,
                               awayscore = group1games$FTAG)
    
    group1games$draws <- mapply(draw,
                               team = tab$team[i],
                               home = group1games$HomeTeam,
                               away = group1games$AwayTeam,
                               homescore = group1games$FTHG,
                               awayscore = group1games$FTAG)
    
    group1games$loss <- mapply(lose,
                                team = tab$team[i],
                                home = group1games$HomeTeam,
                                away = group1games$AwayTeam,
                                homescore = group1games$FTHG,
                                awayscore = group1games$FTAG)
    
    group1games$goalsfor <- mapply(goalsfor,
                               team = tab$team[i],
                               home = group1games$HomeTeam,
                               away = group1games$AwayTeam,
                               homescore = group1games$FTHG,
                               awayscore = group1games$FTAG)
    
    group1games$goalsagainst <- mapply(goalsagainst,
                                   team = tab$team[i],
                                   home = group1games$HomeTeam,
                                   away = group1games$AwayTeam,
                                   homescore = group1games$FTHG,
                                   awayscore = group1games$FTAG)
    
    # Summarise for each team
    teamresults <- c(i,
                     tab$team[i],
                     sum(group1games$wins),
                     sum(group1games$draws),
                     sum(group1games$loss),
                     sum(group1games$goalsfor),
                     sum(group1games$goalsagainst))
    
    groupbuild <- rbind(groupbuild,teamresults)
  
  }
  
  group1$W <- as.numeric(groupbuild[,3])
  group1$D <- as.numeric(groupbuild[,4])
  group1$L <- as.numeric(groupbuild[,5])
  group1$GF <- as.numeric(groupbuild[,6])
  group1$GA <- as.numeric(groupbuild[,7])
  group1$GD <- group1$GF - group1$GA
  group1$Pts <- 3*group1$W + group1$D
  
  # Add a column with a random number, only used in the case of ties of points, GD and GF
  group1$rand <- rnorm(4,0,1)
  
  # Finally, rank the group
  group1 <- group1[order(-group1$Pts,
                         -group1$GD,
                         -group1$GF,
                         -group1$rand),]
  group1$rank <- 1:nrow(group1)
  
  # Group 2
  group2teams <- c(tab$team[2],
                   tab$team[pool[4:6]])
  group2 <- data.frame(ID = c(2,pool[4:6]),
                       team = group2teams)
  
  # Create subset of group games
  g2 <- c(sample(c(2*100+pool[4], pool[4]*100+1),2),
          sample(c(2*100+pool[5], pool[5]*100+1),2),
          sample(c(2*100+pool[6], pool[6]*100+1),2),
          sample(c(pool[4]*100+pool[5], pool[5]*100+pool[4]),1),
          sample(c(pool[4]*100+pool[6], pool[6]*100+pool[4]),1),
          sample(c(pool[5]*100+pool[6], pool[6]*100+pool[5]),1))
  group2games <- dbresults[dbresults$gameID%in%g2,]
  
  ## Build group 1 results table
  # First generate results for each single team then combine
  groupbuild <- NULL
  
  for (i in c(2,pool[4:6])){
    group2games$wins <- mapply(win,
                               team = tab$team[i],
                               home = group2games$HomeTeam,
                               away = group2games$AwayTeam,
                               homescore = group2games$FTHG,
                               awayscore = group2games$FTAG)
    
    group2games$draws <- mapply(draw,
                                team = tab$team[i],
                                home = group2games$HomeTeam,
                                away = group2games$AwayTeam,
                                homescore = group2games$FTHG,
                                awayscore = group2games$FTAG)
    
    group2games$loss <- mapply(lose,
                               team = tab$team[i],
                               home = group2games$HomeTeam,
                               away = group2games$AwayTeam,
                               homescore = group2games$FTHG,
                               awayscore = group2games$FTAG)
    
    group2games$goalsfor <- mapply(goalsfor,
                                   team = tab$team[i],
                                   home = group2games$HomeTeam,
                                   away = group2games$AwayTeam,
                                   homescore = group2games$FTHG,
                                   awayscore = group2games$FTAG)
    
    group2games$goalsagainst <- mapply(goalsagainst,
                                       team = tab$team[i],
                                       home = group2games$HomeTeam,
                                       away = group2games$AwayTeam,
                                       homescore = group2games$FTHG,
                                       awayscore = group2games$FTAG)
    
    # Summarise for each team
    teamresults <- c(i,
                     tab$team[i],
                     sum(group2games$wins),
                     sum(group2games$draws),
                     sum(group2games$loss),
                     sum(group2games$goalsfor),
                     sum(group2games$goalsagainst))
    
    groupbuild <- rbind(groupbuild,teamresults)
    
  }
  
  group2$W <- as.numeric(groupbuild[,3])
  group2$D <- as.numeric(groupbuild[,4])
  group2$L <- as.numeric(groupbuild[,5])
  group2$GF <- as.numeric(groupbuild[,6])
  group2$GA <- as.numeric(groupbuild[,7])
  group2$GD <- group2$GF - group2$GA
  group2$Pts <- 3*group2$W + group2$D
  
  # Add a column with a random number, only used in the case of ties of points, GD and GF
  group2$rand <- rnorm(4,0,1)
  
  # Finally, rank the group
  group2 <- group2[order(-group2$Pts,
                         -group2$GD,
                         -group2$GF,
                         -group2$rand),]
  group2$rank <- 1:nrow(group2)
  
  
  
  # Group 3
  group3teams <- c(tab$team[3],
                   tab$team[pool[7:9]])
  group3 <- data.frame(ID = c(3,pool[7:9]),
                       team = group3teams)
  
  # Create subset of group games
  g3 <- c(sample(c(3*100+pool[7], pool[7]*100+3),1),
          sample(c(3*100+pool[8], pool[8]*100+3),1),
          sample(c(3*100+pool[9], pool[9]*100+3),1),
          sample(c(pool[7]*100+pool[8], pool[8]*100+pool[7]),1),
          sample(c(pool[7]*100+pool[9], pool[9]*100+pool[7]),1),
          sample(c(pool[8]*100+pool[9], pool[9]*100+pool[8]),1))
  group3games <- dbresults[dbresults$gameID%in%g3,]
  
  ## Build group 1 results table
  # First generate results for each single team then combine
  groupbuild <- NULL
  
  for (i in c(3,pool[7:9])){
    group3games$wins <- mapply(win,
                               team = tab$team[i],
                               home = group3games$HomeTeam,
                               away = group3games$AwayTeam,
                               homescore = group3games$FTHG,
                               awayscore = group3games$FTAG)
    
    group3games$draws <- mapply(draw,
                                team = tab$team[i],
                                home = group3games$HomeTeam,
                                away = group3games$AwayTeam,
                                homescore = group3games$FTHG,
                                awayscore = group3games$FTAG)
    
    group3games$loss <- mapply(lose,
                               team = tab$team[i],
                               home = group3games$HomeTeam,
                               away = group3games$AwayTeam,
                               homescore = group3games$FTHG,
                               awayscore = group3games$FTAG)
    
    group3games$goalsfor <- mapply(goalsfor,
                                   team = tab$team[i],
                                   home = group3games$HomeTeam,
                                   away = group3games$AwayTeam,
                                   homescore = group3games$FTHG,
                                   awayscore = group3games$FTAG)
    
    group3games$goalsagainst <- mapply(goalsagainst,
                                       team = tab$team[i],
                                       home = group3games$HomeTeam,
                                       away = group3games$AwayTeam,
                                       homescore = group3games$FTHG,
                                       awayscore = group3games$FTAG)
    
    # Summarise for each team
    teamresults <- c(i,
                     tab$team[i],
                     sum(group3games$wins),
                     sum(group3games$draws),
                     sum(group3games$loss),
                     sum(group3games$goalsfor),
                     sum(group3games$goalsagainst))
    
    groupbuild <- rbind(groupbuild,teamresults)
    
  }
  
  group3$W <- as.numeric(groupbuild[,3])
  group3$D <- as.numeric(groupbuild[,4])
  group3$L <- as.numeric(groupbuild[,5])
  group3$GF <- as.numeric(groupbuild[,6])
  group3$GA <- as.numeric(groupbuild[,7])
  group3$GD <- group3$GF - group3$GA
  group3$Pts <- 3*group3$W + group3$D
  
  # Add a column with a random number, only used in the case of ties of points, GD and GF
  group3$rand <- rnorm(4,0,1)
  
  # Finally, rank the group
  group3 <- group3[order(-group3$Pts,
                         -group3$GD,
                         -group3$GF,
                         -group3$rand),]
  group3$rank <- 1:nrow(group3)
  
  
  
  # Group 4
  group4teams <- c(tab$team[4],
                   tab$team[pool[10:12]])
  group4 <- data.frame(ID = c(4,pool[10:12]),
                       team = group4teams)
  
  # Create subset of group games
  g4 <- c(sample(c(4*100+pool[10], pool[10]*100+4),1),
          sample(c(4*100+pool[11], pool[11]*100+4),1),
          sample(c(4*100+pool[12], pool[12]*100+4),1),
          sample(c(pool[10]*100+pool[11], pool[11]*100+pool[10]),1),
          sample(c(pool[10]*100+pool[12], pool[12]*100+pool[10]),1),
          sample(c(pool[11]*100+pool[12], pool[12]*100+pool[11]),1))
  group4games <- dbresults[dbresults$gameID%in%g4,]
  
  ## Build group 1 results table
  # First generate results for each single team then combine
  groupbuild <- NULL
  
  for (i in c(4,pool[10:12])){
    group4games$wins <- mapply(win,
                               team = tab$team[i],
                               home = group4games$HomeTeam,
                               away = group4games$AwayTeam,
                               homescore = group4games$FTHG,
                               awayscore = group4games$FTAG)
    
    group4games$draws <- mapply(draw,
                                team = tab$team[i],
                                home = group4games$HomeTeam,
                                away = group4games$AwayTeam,
                                homescore = group4games$FTHG,
                                awayscore = group4games$FTAG)
    
    group4games$loss <- mapply(lose,
                               team = tab$team[i],
                               home = group4games$HomeTeam,
                               away = group4games$AwayTeam,
                               homescore = group4games$FTHG,
                               awayscore = group4games$FTAG)
    
    group4games$goalsfor <- mapply(goalsfor,
                                   team = tab$team[i],
                                   home = group4games$HomeTeam,
                                   away = group4games$AwayTeam,
                                   homescore = group4games$FTHG,
                                   awayscore = group4games$FTAG)
    
    group4games$goalsagainst <- mapply(goalsagainst,
                                       team = tab$team[i],
                                       home = group4games$HomeTeam,
                                       away = group4games$AwayTeam,
                                       homescore = group4games$FTHG,
                                       awayscore = group4games$FTAG)
    
    # Summarise for each team
    teamresults <- c(i,
                     tab$team[i],
                     sum(group4games$wins),
                     sum(group4games$draws),
                     sum(group4games$loss),
                     sum(group4games$goalsfor),
                     sum(group4games$goalsagainst))
    
    groupbuild <- rbind(groupbuild,teamresults)
    
  }
  
  group4$W <- as.numeric(groupbuild[,3])
  group4$D <- as.numeric(groupbuild[,4])
  group4$L <- as.numeric(groupbuild[,5])
  group4$GF <- as.numeric(groupbuild[,6])
  group4$GA <- as.numeric(groupbuild[,7])
  group4$GD <- group4$GF - group4$GA
  group4$Pts <- 3*group4$W + group4$D
  
  # Add a column with a random number, only used in the case of ties of points, GD and GF
  group4$rand <- rnorm(4,0,1)
  
  # Finally, rank the group
  group4 <- group4[order(-group4$Pts,
                         -group4$GD,
                         -group4$GF,
                         -group4$rand),]
  group4$rank <- 1:nrow(group4)
  
  
  ### THE QUARTER FINALS
  # At this stage Teams 1 and 2 from groups 1 and 2 play each other, and the same from groups 3 and 4.
  
  # Create subset of QF games
  qf <- c(sample(c(group1$ID[group1$rank==1]*100+group2$ID[group2$rank==2], 
                   group2$ID[group2$rank==2]*100+group1$ID[group1$rank==1]), 1),
          sample(c(group1$ID[group1$rank==2]*100+group2$ID[group2$rank==1], 
                   group2$ID[group2$rank==1]*100+group1$ID[group1$rank==2]),1),
          sample(c(group3$ID[group1$rank==1]*100+group4$ID[group2$rank==2], 
                   group4$ID[group2$rank==2]*100+group3$ID[group1$rank==1]),1),
          sample(c(group3$ID[group1$rank==2]*100+group4$ID[group2$rank==1], 
                   group4$ID[group2$rank==1]*100+group3$ID[group1$rank==2]),1))
          
  qfgames <- dbresults[dbresults$gameID%in%qf,]
  
  
  # Extract winners
  qfgames$homewin <- mapply(kowinner,
                            home = qfgames$HomeTeam,
                            away = qfgames$AwayTeam,
                            homescore = qfgames$FTHG,
                            awayscore = qfgames$FTAG,
                            homerank = qfgames$HomeID,
                            awayrank = qfgames$AwayID)
  
  qfgames$idwin <- mapply(idwinner,
                          homewin = qfgames$homewin,
                          HomeID = qfgames$HomeID,
                          AwayID = qfgames$AwayID)
  
  ### THE SEMI FINALS
  # At this stage, QF1 winner plays QF3 winner, and QF2 winner plays QF4 winner
  
  # Create subset of games
  sf <- c(sample(c(qfgames$idwin[1]*100 + qfgames$idwin[3],
                   qfgames$idwin[3]*100 + qfgames$idwin[1]),1),
          sample(c(qfgames$idwin[2]*100 + qfgames$idwin[4],
                   qfgames$idwin[4]*100 + qfgames$idwin[2]),1))
  
  sfgames <- dbresults[dbresults$gameID%in%sf,]
  
  # Extract winners
  sfgames$homewin <- mapply(kowinner,
                            home = sfgames$HomeTeam,
                            away = sfgames$AwayTeam,
                            homescore = sfgames$FTHG,
                            awayscore = sfgames$FTAG,
                            homerank = sfgames$HomeID,
                            awayrank = sfgames$AwayID)
  
  sfgames$idwin <- mapply(idwinner,
                          homewin = sfgames$homewin,
                          HomeID = sfgames$HomeID,
                          AwayID = sfgames$AwayID)
  
  ### THE FINAL
  # Randomly draw game for the final
  final <- c(sample(c(sfgames$idwin[1]*100 + sfgames$idwin[2],
                      sfgames$idwin[2]*100 + sfgames$idwin[1]),1))
  
  finalgame <- dbresults[dbresults$gameID%in%final,]
  
  # Extract winners
  finalgame$homewin <- mapply(kowinner,
                              home = finalgame$HomeTeam,
                              away = finalgame$AwayTeam,
                              homescore = finalgame$FTHG,
                              awayscore = finalgame$FTAG,
                              homerank = finalgame$HomeID,
                              awayrank = finalgame$AwayID)
  
  finalgame$idwin <- mapply(idwinner,
                            homewin = finalgame$homewin,
                            HomeID = finalgame$HomeID,
                            AwayID = finalgame$AwayID)

  winner <- c(j, ifelse(finalgame$homewin==1,finalgame$HomeTeam,finalgame$AwayTeam))
  WCresults <- rbind(WCresults,winner)
  
}

colnames(WCresults) <- c("it", "winner")

# Collate the winners
Nwin <- as.data.frame(table(WCresults$winner))
Nwin <- Nwin[order(Nwin$Freq, decreasing = FALSE),]

# Display results
par(mar = c(3, 7, 2, 2))
barplot(Nwin$Freq, 
        names.arg = Nwin$Var1,
        horiz = TRUE,
        xlim = c(0,5000),
#        xlab = "Number of simulated tournament wins",
        cex.names = 0.8,
        las = 1)
