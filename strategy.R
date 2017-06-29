rm(list=ls())
args<-commandArgs(TRUE)

# Set the tennis-data path to find the extracted dataset
# tennisdatapath<-'/home/enrico/Desktop/sds/extracted/Tennis-Data/'
tennisdatapath<-paste(args[1],'/Tennis-Data/',sep='')
# Set the betfair data path to find the extracted dataset
# betfairdatapath<-'/home/enrico/Desktop/sds/extracted/Betfair/'
betfairdatapath<-paste(args[1],'/Betfair/',sep='')

# Year to investigate
# year<-2016
year<-args[2]

bankroll<<-100
wins<<-0
losses<<-0
# successivebets<<-0
fixedamount<<-5
betamount<<-fixedamount


# -----------------------------------------------------------------------------------------------
# Function declaration
# -----------------------------------------------------------------------------------------------
# Convert from probability to logit 
#  prob2logit<-function(prob){
#   logit<-log(prob/(1-prob))
#   return(logit)
# }
# Convert from logit to probability
#  logit2prob<-function(logit){
#   odds<-exp(logit)
#   prob<-odds/(1+odds)
#   return(prob)
# }
# Get summary of statistics
# summary.list = function(x)list(
#   N.with.NA.removed= length(x[!is.na(x)]),
#   Count.of.NA= length(x[is.na(x)]),
#   Mean=mean(x, na.rm=TRUE),
#   Median=median(x, na.rm=TRUE),
#   Max.Min=range(x, na.rm=TRUE),
#   Range=max(Data$ Fish, na.rm=TRUE) - min(Data$ Fish, na.rm=TRUE),
#   Variance=var(x, na.rm=TRUE),
#   Std.Dev=sd(x, na.rm=TRUE),
#   Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
#   Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
#   Quantile=quantile(x, na.rm=TRUE)
# )
# Get the tennis-data and process it
gettennisdata<-function(year){
  # read the tennis data information 
  # Please format the tennis-data files as AUS followed by the year(ex. AUS2016.csv)
  rawtennisdata<-read.csv(paste(tennisdatapath,'AUS',year,'.csv',sep=""),header=TRUE,sep=',')  
  # Get the match data which with a comment of 'Completed'
  rawtennisdata<-subset(rawtennisdata,rawtennisdata$Comment=='Completed')
  
  # Get the player 1's last name by splitting with ' ' and getting the first part
  player1<-data.frame(Winner=apply(data.frame(rawtennisdata$Winner),c(1),function(x) strsplit(x,' ')[[1]][1]))
  # Get the player 2's last name by splitting with ' ' and getting the first part
  player2<-data.frame(Loser=apply(data.frame(rawtennisdata$Loser),c(1),function(x) strsplit(x,' ')[[1]][1]))
  # Get the matches' dates and match types and aggregate with the winners and losers
  matchinfo<-data.frame(DatePlayed=rawtennisdata$Date,Type=rawtennisdata$Round,Winner=player1,Loser=player2)
  
  # win odds 
  # winlogitodds<-apply(tdwinnerodds,c(1,2),function(x) prob2logit(1/x)) 
  # winaggregated<-data.frame(winlogitodds,data.frame(MeanLogOdds=apply(winlogitodds,1,sum)/4))
  # meanwinBCMpercent=data.frame(apply(winaggregated[5],c(2),function(x) logit2prob(x)*100))
  # names(meanwinBCMpercent)<-'BCMWinPrediction'
  # winaggregated<-data.frame(winaggregated,meanwinBCMpercent)
  
  # Get Player 1 odds from Bet365,Expekt,Ladbrokes,Pinnacles
  tdp1odds<-data.frame(WO1=rawtennisdata$B365W,WO2=rawtennisdata$EXW,WO3=rawtennisdata$LBW,WO4=rawtennisdata$PSW)
  # Compute the probabilities for Player 1
  p1probabilities<-data.frame(apply(tdp1odds,c(1,2),function(x) 100/x))
  # Compute Player 1's mean odds over the bookmakers' odds
  meanp1oddspercentages<-data.frame(WinnerConfidence=apply(p1probabilities,1,sum)/4)
  # Aggregate the player 1s' mean odds and percentage of winning
  p1averageodds<-data.frame(tdp1odds,meanp1oddspercentages,
                            apply(data.frame(P1Odds=meanp1oddspercentages),c(1,2),function(x)100/x))
  # ---------------------------------------------------------------------------------------------
  # lose odds 
  # loselogitodds<-apply(tdloserodds,c(1,2),function(x) prob2logit(1/x)) 
  # loseaggregated<-data.frame(loselogitodds,data.frame(MeanLogOdds=apply(loselogitodds,1,sum)/4))
  # meanloseBCMpercent=data.frame(apply(loseaggregated[5],c(2),function(x) logit2prob(x)*100))
  # names(meanloseBCMpercent)<-'BCMLosePrediction'
  # loseaggregated<-data.frame(loseaggregated,meanloseBCMpercent)
  
  # Get Player 2 odds from Bet365,Expekt,Ladbrokes,Pinnacles
  tdp2odds<-data.frame(LO1=rawtennisdata$B365L,LO2=rawtennisdata$EXL,LO3=rawtennisdata$LBL,LO4=rawtennisdata$PSL)
  # Compute the probabilities for Player 2
  p2probabilities<-data.frame(apply(tdp2odds,c(1,2),function(x) 100/x))
  # Compute Player 2's mean odds over the bookmakers' odds
  meanp2oddspercentages<-data.frame(LoserConfidence=apply(p2probabilities,1,sum)/4)
  # Aggregate the player 2s' mean odds and percentage of winning
  p2averageodds<-data.frame(tdp2odds,meanp2oddspercentages,
                            apply(data.frame(P1Odds=meanp2oddspercentages),c(1,2),function(x)100/x))
  # Set a global variable with the processed tennis-data matches
  tennisdata<<-data.frame(matchinfo,P1MeanOdds=p1averageodds$WinnerConfidence.1,
                         P2MeanOdds=p2averageodds$LoserConfidence.1,
                         tdp1odds,tdp2odds,
                         P1Confidence=p1averageodds$WinnerConfidence,
                         P2Confidence=p2averageodds$LoserConfidence)
  # Clean memory
  # rm(winaggregated)
  # rm(loseaggregated)
  # rm(loselogitodds)
  # rm(meanloseBCMpercent)
  # rm(meanwinBCMpercent)
  # rm(tdloserodds)
  # rm(tdwinnerodds)
  # # rm(winlogitodds)
  rm(matchinfo)
  rm(rawtennisdata)
  rm(player1)
  rm(player2)
  rm(p1probabilities)
  rm(p2probabilities)
  rm(meanp2oddspercentages)
  rm(meanp1oddspercentages)
  rm(p1averageodds)
  rm(p2averageodds)
  rm(tdp2odds)
  rm(tdp1odds)
}
# Get the betfair data and process it
getbetfairdata<-function(year){
  # read the raw betfair file
  # Please format the betfair extracted files as AUS followed by the year(ex. AUS2016.csv)
  rawbetfairdata<-read.csv(paste(betfairdatapath,'AUS', year,'.csv', sep=""), header=TRUE, sep=',')
  # Get the Match Odds bets and where the match actually took place
  matchodds<-subset(rawbetfairdata, rawbetfairdata$raw.EVENT=='Match Odds' & rawbetfairdata$raw.DT.ACTUAL_OFF!='')
  # Order the data by the first bet taken, latest taken and selection
  selected<-matchodds[with(matchodds,order(raw.FIRST_TAKEN,raw.LATEST_TAKEN,raw.SELECTION)),]
  # I'm removing the first redundant part from Group A to First Round to leave the match type(First Round) and the players
  # The length from 'Group A' to 'First Round' is 46. This is standard for all AUS matches 
  # 'Group A/Australian Open 2013/Mens Tournament/First Round Matches/Djokovic v Mathieu' 
  description<-data.frame(apply(data.frame(FullDescription=selected$raw.FULL_DESCRIPTION), c(2), 
                                function(x) substr(x,46,nchar(x))))
  
  # Join the description with the index of the slash which separates the match type and the players
  temp<-data.frame(description, apply(description,c(2),function(x) regexpr('/',x)))
  # Substring and get the match type played
  matchtype<-apply(data.frame(MatchType=temp$FullDescription),c(2),
                   function(x) substring(x,1,temp$FullDescription.1-1))
  # These were all made so I can match with the tennis-data matches info
  # Remove 'Matches' keyword
  matchtype<-apply(matchtype,c(2),function(x) gsub("Matches","",x))
  # Replace 'First' with '1st'
  matchtype<-apply(matchtype,c(2),function(x) gsub("First","1st",x))
  # Replace 'Second' with '2nd'
  matchtype<-apply(matchtype,c(2),function(x) gsub("Second","2nd",x))
  # Replace 'Third' with '3rd'
  matchtype<-apply(matchtype,c(2),function(x) gsub("Third","3rd",x))
  # Replace 'Fourth' with '4th'
  matchtype<-apply(matchtype,c(2),function(x) gsub("Fourth","4th",x))
  # Replace 'Quarter Final' with 'Quarterfinals'
  matchtype<-apply(matchtype,c(2),function(x) gsub("Quarter Final","Quarterfinals",x))
  # Replace 'Semi Final' with 'Semifinals'
  matchtype<-apply(matchtype,c(2),function(x) gsub("Semi Final","Semifinals",x))
  # Adding the match type to full description
  relevantinfo<-data.frame(FullDescription=temp$FullDescription,matchtype)
  # Adding both players to the frame
  relevantinfo<-data.frame(FullDescription=relevantinfo$FullDescription,
                           MatchType=relevantinfo$MatchType,
                           apply(data.frame(BothPlayers=temp$FullDescription),c(2),
                                 function(x) substring(x,temp$FullDescription.1+1,nchar(x))))
  # We add the index for the separator(' v ') which separates both match players
  bothplayers<-data.frame(BothPlayers=relevantinfo$BothPlayers,
                          apply(data.frame(BothPlayers=relevantinfo$BothPlayers),c(2),
                                function(x) regexpr(' v ',x)))
  # Get the first player by taking the first substring
  player1<-apply(data.frame(Player1=bothplayers$BothPlayers),c(2),
                 function(x) substr(x,1,bothplayers$BothPlayers.1-1))  
  # Get the first player's last name by splitting by ' ' and getting the last part
  player1<-apply(data.frame(player1),c(1,2),
                 function(x) 
                   if(length(strsplit(x,' ')[[1]]) ==1){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Jr'){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Agut'){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Galung'){strsplit(x,' ')[[1]][1]}
                 else{strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]})
  # Adding the extracted player 1 to my frame
  relevantinfo<-data.frame(relevantinfo,player1)
  # -----------------------------------------------------------------------------------
  # Do the same for player 2
  # Get the second player by taking the second substring
  # I do (bothplayers$BothPlayers.1+3) to start from the second player's name
  player2<-apply(data.frame(Player2=bothplayers$BothPlayers),c(2),
                 function(x) substr(x,bothplayers$BothPlayers.1+3,nchar(x)))
  # I extract the second player's last name
  player2<-apply(data.frame(player2),c(1,2),
                 function(x) 
                   if(length(strsplit(x,' ')[[1]])==1){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Jr'){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Agut'){strsplit(x,' ')[[1]][1]}
                 else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Galung'){strsplit(x,' ')[[1]][1]}
                 else{strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]})
  # Add the extracted second player to my relevant info frame
  relevantinfo<-data.frame(relevantinfo,player2)
  
  # Getting bet selection last name by doing a similar process on the selection
  selection<-data.frame(apply(data.frame(Selection=selected$raw.SELECTION),c(1,2),
                              function(x) 
                                if(length(strsplit(x,' ')[[1]])==1){strsplit(x,' ')[[1]][1]}
                              else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Jr' & strsplit(x,' ')[[1]][1]=='Alex'){strsplit(x,' ')[[1]][2]}
                              else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Agut'){strsplit(x,' ')[[1]][1]}
                              else if(strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]=='Galung'){strsplit(x,' ')[[1]][1]}
                              else{strsplit(x,' ')[[1]][length(strsplit(x,' ')[[1]])]}))
  # Get the datetime the match took place
  actualoffdates<-data.frame(selected$raw.DT.ACTUAL_OFF)
  # Extract the date by splitting the time and date separately
  actualoffdates<-data.frame(ActualOff=apply(actualoffdates,c(1),function(x) strsplit(x,' ')[[1]][1]))
  # Make global the variable holding the extracted, cleaned information from betfair
  betfair<<-data.frame(Type=relevantinfo$MatchType,P1=relevantinfo$Player1,
                      P2=relevantinfo$Player2,Selection=selection,
                      Odds=selected$raw.ODDS,BetNumber=selected$raw.NUMBER_BETS,
                      Volume=selected$raw.VOLUME_MATCHED,FirstTaken=selected$raw.FIRST_TAKEN,
                      LastTaken=selected$raw.LATEST_TAKEN,Actual_Off=actualoffdates)
  
  # Clean up memory
  rm(description)
  rm(temp)
  rm(bothplayers)
  rm(selected)
  rm(matchodds)
  rm(relevantinfo)
  rm(rawbetfairdata)
  rm(selection)
  rm(player1)
  rm(player2)
  rm(actualoffdates)
  rm(matchtype)
}
# Initialise placeholder to store the matches' information we bet on
playedmatchesinfo<-function(n){
  # selection = who we bet on
  # besttdodds = choose out of all odds shown in tennis-data
  # bestbfodds = choose best odds from betfair
  # betamount = the stake to bet
  # bankroll = the resulting bankroll amount
  playedmatches<-data.frame(actualwinner=character(n),selection=character(n),
                            besttdodds=numeric(n),bestbfodds=numeric(n),
                            betamount=numeric(n),bankroll=numeric(n),stringsAsFactors = FALSE)
  for(i in 1:n){
    playedmatches$actualwinner[i]<-toString(i)
    playedmatches$selection[i]<-toString(i)
    playedmatches$besttdodds[i]<-i
    playedmatches$bestbfodds[i]<-i
    playedmatches$betamount[i]<-i
    playedmatches$bankroll[i]<-i
  }
  return(playedmatches)
}
# First strategy
firststrategy<-function(){
  # Choosing a set of data upon which we will place our bets
  bettingsubset<<-subset(tennisdata, (tennisdata$P1MeanOdds>=1.70 & tennisdata$P2MeanOdds>=1.70))
  # Initialise a summary data frame which will hold info about our played bets
  matchesplayed<<-playedmatchesinfo(nrow(bettingsubset))
  # Loop through our betting set
  for(i in 1:nrow(bettingsubset))
  {
    # Get both players' mean odds computed from tennis-data
    p1meanodds<-bettingsubset[i, "P1MeanOdds"]
    p2meanodds<-bettingsubset[i, "P2MeanOdds"]
    
    # Get match date and format appropriately
    matchdate<-matrix(as.matrix(bettingsubset[i,][1]))[1,]
    matchdate<-paste(substr(gsub("/","-",matchdate),1,6),'20',substr(gsub("/","-",matchdate),7,9),sep='')
    # Retrieve the match type
    matchtype<-matrix(as.matrix(bettingsubset[i,][2]))[1,]
    # Check which player's odds are the lowest
    if(p1meanodds<p2meanodds){
      # Get the favorite player's(player 1) name and the other player's as well
      favoriteplayer<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
      if(length(strsplit(favoriteplayer,'-')[[1]])>1){favoriteplayer<-strsplit(favoriteplayer,'-')[[1]][2]}
      otherplayer<-matrix(as.matrix(bettingsubset[i,][4]))[1,]
      if(length(strsplit(otherplayer,'-')[[1]])>1){otherplayer<-strsplit(otherplayer,'-')[[1]][2]}
      # Get player 1's(favorite) best odds offered from tennis-data
      besttdodds<-max(bettingsubset[i,7],bettingsubset[i,8],bettingsubset[i,9],bettingsubset[i,10])
    }else{
      # Get the favorite player's(player 2) name and the other player's as well
      favoriteplayer<-matrix(as.matrix(bettingsubset[i,][4]))[1,]
      if(length(strsplit(favoriteplayer,'-')[[1]])>1){favoriteplayer<-strsplit(favoriteplayer,'-')[[1]][2]}
      otherplayer<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
      if(length(strsplit(otherplayer,'-')[[1]])>1){otherplayer<-strsplit(otherplayer,'-')[[1]][2]}
      # Get player 2's(favorite) best odds offered from tennis-data
      besttdodds<-max(bettingsubset[i,11],bettingsubset[i,12],bettingsubset[i,13],bettingsubset[i,14])
    }
    # Check if selection has '-' and select player's last name
    if(regexpr('-',favoriteplayer)>-1){
      # Split player's name by '-' and get last part
      surname<-strsplit(favoriteplayer,'-')[[1]][2]
      
      # Get best odds from Betfair for our selection in the match we're dealing with
      bfmatchodds<-data.frame(subset(betfair, 
                                     (((P1==favoriteplayer & P2==otherplayer) | 
                                         (P1==otherplayer & P2==favoriteplayer)) &
                                        Selection==surname &
                                        ActualOff==matchdate &
                                        Type==paste(matchtype,''))))
    }else{
      # Get best odds from Betfair for our selection in the match we're dealing with
      bfmatchodds<-data.frame(subset(betfair, 
                                     (((P1==favoriteplayer & P2==otherplayer) | 
                                         (P1==otherplayer & P2==favoriteplayer)) &
                                        Selection==favoriteplayer &
                                        ActualOff==matchdate &
                                        Type==paste(matchtype,''))))
    }
    # Initialise Betfair odds to 0, incase we don't find Betfair odds
    bestbfodds<-0
    # If there are Betfair odds for our match, get the best odds
    if(nrow(bfmatchodds)>0){
      bestbfodds<-max(data.frame(bfmatchodds$Odds))
    }
    # Set my odds to Betfair's odds or Tennis-Data's odds
    if(bestbfodds>besttdodds){
      mychosenodds<-bestbfodds
    }else{
      mychosenodds<-besttdodds
    }
    # Get the actual winner of the match under investigation
    actualwinner<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
    # Check if we've won the match
    if(actualwinner==favoriteplayer){
      # if((successivebets%%3)==0  & successivebets>0){betamount<<-betamount*fixedamount}
      totalreturns<-mychosenodds*betamount
      bankroll<<-bankroll+totalreturns
      wins<<-wins+1
      # successivebets<<-successivebets+1
      rm(totalreturns)
    }else{
      bankroll<<-bankroll-betamount
      losses<<-losses+1
      # successivebets<<-0
      betamount<<-fixedamount
    }
    # Place information about our current bet
    matchesplayed[i,]<<-c(actualwinner,favoriteplayer,besttdodds,bestbfodds,betamount,bankroll)  
    # Clean unnecessary variables
    rm(bestbfodds)
    rm(besttdodds)
    rm(p1meanodds)
    rm(p2meanodds)
    rm(matchdate)
    rm(matchtype)
    rm(bfmatchodds)
    rm(actualwinner)
    rm(favoriteplayer)
    rm(otherplayer)
  }
}
secondstrategy<-function(){
  # Choosing a set of data upon which we will place our bets
  bettingsubset<<-subset(tennisdata, 
                         (tennisdata$P1MeanOdds>=1.45 & tennisdata$P2MeanOdds>=1.45) &
                         (tennisdata$P1MeanOdds-tennisdata$P2MeanOdds>=0.8 | tennisdata$P2MeanOdds-tennisdata$P1MeanOdds>=0.8 ))
  # Initialise a summary data frame which will hold info about our played bets
  matchesplayed<<-playedmatchesinfo(nrow(bettingsubset))
  # Loop through our betting set
  for(i in 1:nrow(bettingsubset))
  {
    # Get both players' mean odds computed from tennis-data
    p1meanodds<-bettingsubset[i, "P1MeanOdds"]
    p2meanodds<-bettingsubset[i, "P2MeanOdds"]
    
    # Get match date and format appropriately
    matchdate<-matrix(as.matrix(bettingsubset[i,][1]))[1,]
    matchdate<-paste(substr(gsub("/","-",matchdate),1,6),'20',substr(gsub("/","-",matchdate),7,9),sep='')
    # Retrieve the match type
    matchtype<-matrix(as.matrix(bettingsubset[i,][2]))[1,]
    # Check which player's odds are the lowest
    if(p1meanodds<p2meanodds){
      # Get the favorite player's(player 1) name and the other player's as well
      favoriteplayer<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
      if(length(strsplit(favoriteplayer,'-')[[1]])>1){favoriteplayer<-strsplit(favoriteplayer,'-')[[1]][2]}
      otherplayer<-matrix(as.matrix(bettingsubset[i,][4]))[1,]
      if(length(strsplit(otherplayer,'-')[[1]])>1){otherplayer<-strsplit(otherplayer,'-')[[1]][2]}
      # Get player 1's(favorite) best odds offered from tennis-data
      besttdodds<-max(bettingsubset[i,7],bettingsubset[i,8],bettingsubset[i,9],bettingsubset[i,10])
    }else{
      # Get the favorite player's(player 2) name and the other player's as well
      favoriteplayer<-matrix(as.matrix(bettingsubset[i,][4]))[1,]
      if(length(strsplit(favoriteplayer,'-')[[1]])>1){favoriteplayer<-strsplit(favoriteplayer,'-')[[1]][2]}
      otherplayer<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
      if(length(strsplit(otherplayer,'-')[[1]])>1){otherplayer<-strsplit(otherplayer,'-')[[1]][2]}
      # Get player 2's(favorite) best odds offered from tennis-data
      besttdodds<-max(bettingsubset[i,11],bettingsubset[i,12],bettingsubset[i,13],bettingsubset[i,14])
    }
    # Check if selection has '-' and select player's last name
    if(regexpr('-',favoriteplayer)>-1){
      # Split player's name by '-' and get last part
      surname<-strsplit(favoriteplayer,'-')[[1]][2]
      
      # Get best odds from Betfair for our selection in the match we're dealing with
      bfmatchodds<-data.frame(subset(betfair, 
                                     (((P1==favoriteplayer & P2==otherplayer) | 
                                         (P1==otherplayer & P2==favoriteplayer)) &
                                        Selection==surname &
                                        ActualOff==matchdate &
                                        Type==paste(matchtype,'') &
                                        BetNumber>=5)))
    }else{
      # Get best odds from Betfair for our selection in the match we're dealing with
      bfmatchodds<-data.frame(subset(betfair, 
                                     (((P1==favoriteplayer & P2==otherplayer) | 
                                         (P1==otherplayer & P2==favoriteplayer)) &
                                        Selection==favoriteplayer &
                                        ActualOff==matchdate &
                                        Type==paste(matchtype,''))&
                                        BetNumber>=5))
    }
    # Initialise Betfair odds to 0, incase we don't find Betfair odds
    bestbfodds<-0
    # If there are Betfair odds for our match, get the best odds
    if(nrow(bfmatchodds)>0){
      bestbfodds<-max(data.frame(bfmatchodds$Odds))
    }
    # Set my odds to Betfair's odds or Tennis-Data's odds
    if(bestbfodds>besttdodds){
      mychosenodds<-bestbfodds
    }else{
      mychosenodds<-besttdodds
    }
    # Get the actual winner of the match under investigation
    actualwinner<-matrix(as.matrix(bettingsubset[i,][3]))[1,]
    # Check if we've won the match
    if(actualwinner==favoriteplayer){
      # if((successivebets%%3)==0  & successivebets>0){betamount<<-betamount*fixedamount}
      totalreturns<-mychosenodds*betamount
      bankroll<<-bankroll+totalreturns
      wins<<-wins+1
      # successivebets<<-successivebets+1
      rm(totalreturns)
    }else{
      bankroll<<-bankroll-betamount
      losses<<-losses+1
      # successivebets<<-0
      betamount<<-fixedamount
    }
    # Place information about our current bet
    matchesplayed[i,]<<-c(actualwinner,favoriteplayer,besttdodds,bestbfodds,betamount,bankroll)  
    # Clean unnecessary variables
    rm(bestbfodds)
    rm(besttdodds)
    rm(p1meanodds)
    rm(p2meanodds)
    rm(matchdate)
    rm(matchtype)
    rm(bfmatchodds)
    rm(actualwinner)
    rm(favoriteplayer)
    rm(otherplayer)
  }
}
# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------
# Extract and process data from tennis-data.co.uk for the year under investigation
gettennisdata(year)
# -----------------------------------------------------------------------------------------------
# Extract and process data from betfair for the year under investigation
getbetfairdata(year)
# -----------------------------------------------------------------------------------------------
# Executing the first strategy
firststrategy()
# secondstrategy()
# -----------------------------------------------------------------------------------------------

# rm(favoriteplayer)
# rm(otherplayer)
# rm(matchdate)
# rm(matchtype)


# statistics
# ct<-data.frame(TennisData=matchesplayed$besttdodds,Betfair=matchesplayed$bestbfodds)
# max(apply(data.frame(ct$Betfair), c(2), function(x) {(as.numeric(x))}))
# min(apply(data.frame(ct$Betfair), c(2), function(x) {(as.numeric(x))}))
# median(apply(data.frame(ct$Betfair), c(2), function(x) {(as.numeric(x))}))
# var(ct$Betfair)
# sd(ct$Betfair)
# sd(ct$Betfair)/sqrt(length(ct$Betfair[!is.na(sd(ct$Betfair))]))


# frame<-(apply(data.frame(ct$Betfair), c(2), function(x) {(as.numeric(x))}))
# qqnorm(frame)
# qqline(frame, col='red')
# 
# qqnorm(betfair$Odds)
# tennisframe<-apply(data.frame(ct$TennisData), c(2), function(x) {(as.numeric(x))})
# betfairframe<-apply(data.frame(ct$Betfair), c(2), function(x) {(as.numeric(x))})
# t.test(tennisframe,betfairframe)








