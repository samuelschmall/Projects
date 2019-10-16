UCLA <- read.csv("UCLA Lineup Logs_Conf.csv", header=T) ### just to have raw data stored as well in R

time <- read.table('times_Conf.txt', header = T, stringsAsFactors = F)
gtime <- as.matrix(time)
gtime <- as.vector(gtime)
gtime <- strptime(gtime, "%M:%S")
length(gtime)
x <- NULL
for(i in 1:length(gtime)-1){x[i] <- difftime(gtime[i], gtime[i+1], units="secs")}
x
xpos <- 1200 + x[which(x<0)]
x[which(x<0)] <- xpos
x
seconds <- append(x, 91)
seconds
length(seconds)
minutes <- round(seconds/60, 5)
Season <- UCLA[UCLA$Time!="1H" & UCLA$Time!="2H" & UCLA$Time!='"+/-"' & UCLA$Time!="OT",]
dim(Season)
Season[,2] <- time
Season <- cbind(Season, seconds, minutes)
rm(minutes, seconds, time)

PtsPerMin <- round(Season$UCLA.Margin/Season$minutes, 3)
Season <- cbind(Season, PtsPerMin)
rm(PtsPerMin)
attach(Season)
dim(Season)


#################################PAIRS###########################################
### Paired combinations of 13 players, 78 combinations in total
Twos <- matrix(NA, nrow=78, ncol=5)
Twos <- as.data.frame(Twos)
colnames(Twos) <- c("Player 1", "Player 2", "+/-", "Minutes", "EPG")
combo2 <- combn(seq(1,13), 2)
combo2 <- t(combo2)
for (i in 1:78)
{
  matched <- rep(0, dim(Season)[1])
  for (j in 1:dim(Season)[1])
    {
      matched[j] = identical(as.numeric(Season[j, (combo2[i,]+3)]), c(1,1))
    } # the +3 is to get past the Game/Time/Half columns
    subset <- Season[which(matched==1), "UCLA.Margin"]
    Twos[i,3] <- sum(subset) # +/-
}
for (i in 1:78)
{
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
   {
     matched[j] = identical(as.numeric(Season[j, (combo2[i,]+3)]), c(1,1))
   } # the +3 is to get past the Game/Time/Half columns
   subset <- Season[which(matched==1), "minutes"]
   Twos[i,4] <- round(sum(subset), 2) # minutes
}
combo2 <- as.data.frame(combo2)
combo2[combo2==1] <- "Kris Wilkes"
combo2[combo2==2] <- "Jaylen Hands"
combo2[combo2==3] <- "Moses Brown"
combo2[combo2==4] <- "Prince Ali"
combo2[combo2==5] <- "Jalen Hill"
combo2[combo2==6] <- "Cody Riley"
combo2[combo2==7] <- "Chris Smith"
combo2[combo2==8] <- "David Singleton"
combo2[combo2==9] <- "Jules Bernard"
combo2[combo2==10] <- "Kenneth Nwuba"
combo2[combo2==11] <- "Armani Dodson"
combo2[combo2==12] <- "Alex Olesinski"
combo2[combo2==13] <- "Russell Stong"
Twos[,1:2] <- combo2
Twos$EPG <- round(Twos[,3]*40/Twos[,4], 2)
 
## Pairs of 11 Players ##
Pairs <- matrix(NA, nrow=13, ncol=13)
rownames(Pairs) <- c("Kris Wilkes", "Jaylen Hands", "Moses Brown", "Prince Ali", "Jalen Hill", "Cody Riley",
                         "Chris Smith", "David Singleton", "Jules Bernard", "Kenneth Nwuba", "Armani Dodson", "Alex Olseinski", "Russell Stong")
colnames(Pairs) <- c("Kris Wilkes", "Jaylen Hands", "Moses Brown", "Prince Ali", "Jalen Hill", "Cody Riley",
                        "Chris Smith", "David Singleton", "Jules Bernard", "Kenneth Nwuba", "Armani Dodson", "Alex Olesinski", "Russell Stong")
Pairs[1,2:13] <- Twos[1:12,3]
Pairs[2,3:13] <- Twos[13:23,3]
Pairs[3,4:13] <- Twos[24:33,3]
Pairs[4,5:13] <- Twos[34:42,3]
Pairs[5,6:13] <- Twos[43:50,3]
Pairs[6,7:13] <- Twos[51:57,3]
Pairs[7,8:13] <- Twos[58:63,3]
Pairs[8,9:13] <- Twos[64:68,3]
Pairs[9,10:13] <- Twos[69:72,3]
Pairs[10,11:13] <- Twos[73:75,3]
Pairs[11,12:13] <- Twos[76:77,3]
Pairs[12,13] <- Twos[78,3]
### normalized paired data per 40 minutes played together
NPairs <- Pairs
Pairs[2:13,1] <- Twos[1:12,4]
Pairs[3:13,2] <- Twos[13:23,4]
Pairs[4:13,3] <- Twos[24:33,4]
Pairs[5:13,4] <- Twos[34:42,4]
Pairs[6:13,5] <- Twos[43:50,4]
Pairs[7:13,6] <- Twos[51:57,4]
Pairs[8:13,7] <- Twos[58:63,4]
Pairs[9:13,8] <- Twos[64:68,4]
Pairs[10:13,9] <- Twos[69:72,4]
Pairs[11:13,10] <- Twos[73:75,4]
Pairs[12:13,11] <- Twos[76:77,4]
Pairs[13,12] <- Twos[78,4]
NPairs[2:13,1] <- Twos[1:12,5]
NPairs[3:13,2] <- Twos[13:23,5]
NPairs[4:13,3] <- Twos[24:33,5]
NPairs[5:13,4] <- Twos[34:42,5]
NPairs[6:13,5] <- Twos[43:50,5]
NPairs[7:13,6] <- Twos[51:57,5]
NPairs[8:13,7] <- Twos[58:63,5]
NPairs[9:13,8] <- Twos[64:68,5]
NPairs[10:13,9] <- Twos[69:72,5]
NPairs[11:13,10] <- Twos[73:75,5]
NPairs[12:13,11] <- Twos[76:77,5]
NPairs[13,12] <- Twos[78,5]
Pairs
NPairs


#################################TRIOS###############################################
### Triple combinations of 13 players, 286 combinations in total
Triples <- matrix(NA, nrow=286, ncol=6)
Triples <- as.data.frame(Triples)
colnames(Triples) <- c("Player 1", "Player 2", "Player 3", "+/-", "Minutes", "EPG")
combo3 <- combn(seq(1,13), 3)
combo3 <- t(combo3)
for (i in 1:286)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     {
     matched[j] = identical(as.numeric(Season[j, (combo3[i,]+3)]), c(1,1,1))
     } # the +3 is to get past the Game/Time/Half columns
   subset <- Season[which(matched==1), "UCLA.Margin"]
   Triples[i,4] <- sum(subset) # +/-
   }
for (i in 1:286)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     {
     matched[j] = identical(as.numeric(Season[j, (combo3[i,]+3)]), c(1,1,1))
     } # the +3 is to get past the Game/Time/Half columns
   subset <- Season[which(matched==1), "minutes"]
   Triples[i,5] <- round(sum(subset), 2) # minutes
   }
combo3 <- as.data.frame(combo3)
combo3[combo3==1] <- "Kris Wilkes"
combo3[combo3==2] <- "Jaylen Hands"
combo3[combo3==3] <- "Moses Brown"
combo3[combo3==4] <- "Prince Ali"
combo3[combo3==5] <- "Jalen Hill"
combo3[combo3==6] <- "Cody Riley"
combo3[combo3==7] <- "Chris Smith"
combo3[combo3==8] <- "David Singleton"
combo3[combo3==9] <- "Jules Bernard"
combo3[combo3==10] <- "Kenneth Nwuba"
combo3[combo3==11] <- "Armani Dodson"
combo3[combo3==12] <- "Alex Olesinski"
combo3[combo3==13] <- "Russell Stong"
Triples[,1:3] <- combo3
Triples$EPG <- round(Triples[,4]*40/Triples[,5], 2)
TriplesSorted <- Triples[order(Triples$EPG, decreasing=TRUE),] ### Triples will always have all combinations
TriplesSorted <- TriplesSorted[TriplesSorted$Minutes>10,]
dim(TriplesSorted)
TriplesSorted

####################################QUADS#########################################
### Quadruple combinations of 13 players, 715 combinations in total
Quads <- matrix(NA, nrow=715, ncol=7)
Quads <- as.data.frame(Quads)
colnames(Quads) <- c("Player 1", "Player 2", "Player 3", "Player 4", "+/-", "Minutes", "EPG")
combo4 <- combn(seq(1,13), 4)
combo4 <- t(combo4)
for (i in 1:715)
 {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     {
     matched[j] = identical(as.numeric(Season[j, (combo4[i,]+3)]), c(1,1,1,1))
     } # the +3 is to get past the Game/Time/Half columns
   subset <- Season[which(matched==1), "UCLA.Margin"]
   Quads[i,5] <- sum(subset) # +/-
   }
for (i in 1:715)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     {
     matched[j] = identical(as.numeric(Season[j, (combo4[i,]+3)]), c(1,1,1,1))
     } # the +3 is to get past the Game/Time/Half columns
   subset <- Season[which(matched==1), "minutes"]
   Quads[i,6] <- round(sum(subset), 2) # minutes
   }
combo4 <- as.data.frame(combo4)
combo4[combo4==1] <- "Kris Wilkes"
combo4[combo4==2] <- "Jaylen Hands"
combo4[combo4==3] <- "Moses Brown"
combo4[combo4==4] <- "Prince Ali"
combo4[combo4==5] <- "Jalen Hill"
combo4[combo4==6] <- "Cody Riley"
combo4[combo4==7] <- "Chris Smith"
combo4[combo4==8] <- "David Singleton"
combo4[combo4==9] <- "Jules Bernard"
combo4[combo4==10] <- "Kenneth Nwuba"
combo4[combo4==11] <- "Armani Dodson"
combo4[combo4==12] <- "Alex Olesinski"
combo4[combo4==13] <- "Russell Stong"
Quads[,1:4] <- combo4
Quads$EPG <- round(Quads[,5]*40/Quads[,6], 2)
QuadsSorted <- Quads[order(Quads$EPG, decreasing=TRUE),] ### Quads will always have all combinations
QuadsSorted <- QuadsSorted[QuadsSorted$Minutes>5,]
dim(QuadsSorted)

QuadsSorted



#####################################FIVES#########################################
### Five combinations of 13 players, 1287 combinations in total
FivesPlus <- matrix(NA, nrow=1287, ncol=36)
FivesPlus <- as.data.frame(FivesPlus)
colnames(FivesPlus) <- c("Player 1", "Player 2", "Player 3", "Player 4", "Player 5", "+/-", "Minutes", "EPG", "G2FG", "A2FG", "2FG%", "G3FG", "A3FG", "3FG%",
  "GFG", "AFG", "FG%", "OpG2FG", "OpA2FG", "Op2FG%", "OpG3FG", "OpA3FG", "Op3FG%", "OpGFG", "OpAFG", "OpFG%", "TO", "TO/min.", "OpTO", "OpTO/min.",
  "DReb", "OpOReb", "%DReb", "OReb", "OpDReb", "%OReb")
combo5 <- combn(seq(1,13), 5)
combo5 <- t(combo5)
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "UCLA.Margin"]
   FivesPlus[i,6] <- sum(subset) # +/-
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "minutes"]
   FivesPlus[i,7] <- round(sum(subset), 2) # minutes
   }
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "G2FG"]
   FivesPlus[i,9] <- sum(subset) # made 2-pointers
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset1 <- Season[which(matched==1), "G2FG"]
   subset2 <- Season[which(matched==1), "M2FG"]
   FivesPlus[i,10] <- sum(subset1) + sum(subset2) # 2-pointers attempted
   }
FivesPlus[,11] <- round(FivesPlus[,9]/FivesPlus[,10], 3) # 2-point FG percentage
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "G3FG"]
   FivesPlus[i,12] <- sum(subset) # made 3-pointers
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset1 <- Season[which(matched==1), "G3FG"]
   subset2 <- Season[which(matched==1), "M3FG"]
   FivesPlus[i,13] <- sum(subset1) + sum(subset2) # 3-pointers attempted
   }
FivesPlus[,14] <- round(FivesPlus[,12]/FivesPlus[,13], 3) # 3-point FG percentage
 
FivesPlus[,15] <- FivesPlus[,9] + FivesPlus[,12] # overall made shots
FivesPlus[,16] <- FivesPlus[,10] + FivesPlus[,13] # overall attempted shots
FivesPlus[,17] <- round(FivesPlus[,15]/FivesPlus[,16], 3) # overall FG percentage
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "OpG2FG"]
   FivesPlus[i,18] <- sum(subset) # opp made 2-pointers
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset1 <- Season[which(matched==1), "OpG2FG"]
   subset2 <- Season[which(matched==1), "OpM2FG"]
   FivesPlus[i,19] <- sum(subset1) + sum(subset2) # opp 2-pointers attempted
   }
FivesPlus[,20] <- round(FivesPlus[,18]/FivesPlus[,19], 3) # opp 2-point FG percentage
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
   }
   subset <- Season[which(matched==1), "OpG3FG"]
   FivesPlus[i,21] <- sum(subset) # opp made 3-pointers
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset1 <- Season[which(matched==1), "OpG3FG"]
   subset2 <- Season[which(matched==1), "OpM3FG"]
   FivesPlus[i,22] <- sum(subset1) + sum(subset2) # opp 3-pointers attempted
   }
FivesPlus[,23] <- round(FivesPlus[,21]/FivesPlus[,22], 3) # opp 3-point FG percentage
 
FivesPlus[,24] <- FivesPlus[,18] + FivesPlus[,21] # opp overall made shots
FivesPlus[,25] <- FivesPlus[,19] + FivesPlus[,22] # opp overall attempted shots
FivesPlus[,26] <- round(FivesPlus[,24]/FivesPlus[,25], 3) # opp overall FG percentage
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "TO"]
   FivesPlus[i,27] <- sum(subset) # TO
   }
FivesPlus[,28] <- round(FivesPlus[,27]/FivesPlus[,7], 3) # TO per min.
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "OpTO"]
   FivesPlus[i,29] <- sum(subset) # opp TO
   }
FivesPlus[,30] <- round(FivesPlus[,29]/FivesPlus[,7], 3) # opp TO per min.

for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "RebD"]
   FivesPlus[i,31] <- sum(subset) # D boards
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "ROppO"]
   FivesPlus[i,32] <- sum(subset) # opp O boards
   }
FivesPlus[,33] <- round(FivesPlus[,31]/(FivesPlus[,31] + FivesPlus[,32]), 3) # percent D boards grabbed
 
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "RebO"]
   FivesPlus[i,34] <- sum(subset) # O boards
   }
for (i in 1:1287)
   {
   matched <- rep(0, dim(Season)[1])
   for (j in 1:dim(Season)[1])
     { 
     matched[j] = identical(as.numeric(Season[j, (combo5[i,]+3)]), c(1,1,1,1,1))
     }
   subset <- Season[which(matched==1), "ROppD"]
   FivesPlus[i,35] <- sum(subset) # opp D boards
   }
FivesPlus[,36] <- round(FivesPlus[,34]/(FivesPlus[,34] + FivesPlus[,35]), 3) # percent O boards grabbed
 
combo5 <- as.data.frame(combo5)
combo5[combo5==1] <- "Kris Wilkes"
combo5[combo5==2] <- "Jaylen Hands"
combo5[combo5==3] <- "Moses Brown"
combo5[combo5==4] <- "Prince Ali"
combo5[combo5==5] <- "Jalen Hill"
combo5[combo5==6] <- "Cody Riley"
combo5[combo5==7] <- "Chris Smith"
combo5[combo5==8] <- "David Singleton"
combo5[combo5==9] <- "Jules Bernard"
combo5[combo5==10] <- "Kenneth Nwuba"
combo5[combo5==11] <- "Armani Dodson"
combo5[combo5==12] <- "Alex Olesinski"
combo5[combo5==13] <- "Russell Stong"
FivesPlus[,1:5] <- combo5
FivesPlus$EPG <- round(FivesPlus[,6]*40/FivesPlus[,7], 2)
FivesPlus[,7] <- round(FivesPlus[,7], 2)
FivesPlusSorted <- FivesPlus[order(FivesPlus$Minutes, decreasing=TRUE),] ### FivesPlus will always have all combinations
FivesPlusSorted <- FivesPlusSorted[FivesPlusSorted$Minutes>2,]
dim(FivesPlusSorted)
FivesPlusSorted



#########################################################################################
write.csv(round(Pairs, 2), file="UCLAPairs.csv")
write.csv(round(NPairs, 2), file="UCLANPairs.csv")
write.csv(TriplesSorted, file="UCLATriplesSorted.csv")
write.csv(QuadsSorted, file="UCLAQuadsSorted.csv")
write.csv(FivesPlusSorted, file="UCLAFivesPlusSorted.csv")
