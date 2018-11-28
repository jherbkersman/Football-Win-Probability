
## DO NOT USE THE LIVE PROBABILITES FOR LAST 5 MIN OF GAME YET,
## You SHOULD use the generic probabilities given by these
## formulas as a guide for your prediction of what a coach
## will decide to do as the clock runs low.

# PART 1: For probabilities before a game starts.

# type in team name you are examining
# replace Falcons but keep team name the "quotes", ex:
# "Falcons"

team <- "teamName" #(change me)

# type in the vegas line multiplied by negative 1;
# ex: if they are favored by four enter 4
# if the opponent is favored by 4 enter -4
# no "quotes" here

# i.e. line = points favored by
line <- negVegasLine #(change me)


################################################################
sd <- 13.4
n <- 10000

sim <- rnorm(n, mean = line, sd = sd)

hist(sim, freq = F, breaks = 15, xlab = paste(team, "Win Margin"), ylab = "Probability", main = paste("Histogram of n =", n, "Simulated Games"))

lines(density(sim, adjust = 5), lty="dotted", col="darkgreen", lwd=2)

abline(v = -1, lty=3); abline(v = 1, lty=3); abline(v = line, lty = 2, col='blue')

lose.prob <- pnorm(q = -1, mean = line, sd = sd)
win.prob <- 1 - pnorm(q = 1, mean = line, sd = sd)

ot.win <- win.prob * (pnorm(1, line, sd) - pnorm(-1, line, sd))

legend("topright", paste(team, "Win Prob. =", round((win.prob + ot.win) * 100, digits = 2), "%"))
box()
#dev.copy(pdf, 'win_pred.pdf')
#dev.off()
################################################################

# If you want to know the probability that a team wins by at
# least x or loses by at least y, use the following code:

win.by <- x #(change me)
lose.by <- y #(change me)

#######
win.by.prob <- 1 - pnorm(q = win.by, mean = line, sd = sd)
lose.by.prob <- pnorm(q = -lose.by, mean = line, sd = sd)
print(paste("Probability of Winning By", win.by, "or more =", win.by.prob))
print(paste("Probability of Losing By", lose.by, "or more =", lose.by.prob))
#######


# PART 2: For probabilities after a game starts.

# input minutes REMAINING
min.remain <- min #(change me)

# input the new live line
live.line <- liveLine #(change me)

################################################################

live.sd <- (sd / sqrt((60 / min.remain)))

n <- 10000

live.sim <- rnorm(n, mean = live.line, sd = live.sd)

hist(live.sim, freq = F, breaks = 15, xlab = paste(team, "Win Margin"), ylab = "Probability", main = paste("Histogram of n =", n, "Simulated Games with", min.remain, "Minutes Left"))

lines(density(live.sim, adjust = 5), lty="dotted", col="darkgreen", lwd=2)

abline(v = -1, lty=3); abline(v = 1, lty=3)

live.lose.prob <- pnorm(q = -1, mean = live.line, sd = live.sd)
live.win.prob <- 1 - pnorm(q = 1, mean = live.line, sd = live.sd)

live.ot.win <- live.win.prob * (pnorm(1, live.line, live.sd) - pnorm(-1, live.line, live.sd))

legend("topright", paste(team, "Win Prob. =", round((live.win.prob + live.ot.win) * 100, digits = 2), "%"))
box()
#dev.copy(pdf, 'live_win_pred.pdf')
#dev.off()
################################################################

# If you want to know the probability that a team wins by at
# least x or loses by at least y, use the following code:

live.win.by <- x #(change me)
live.lose.by <- y #(change me)

#######
live.win.by.prob <- 1 - pnorm(q = live.win.by, mean = live.line, sd = live.sd)
live.lose.by.prob <- pnorm(q = -live.lose.by, mean = live.line, sd = live.sd)
print(paste("Probability of Winning By", live.win.by, "or more =", live.win.by.prob))
print(paste("Probability of Losing By", live.lose.by, "or more =", live.lose.by.prob))
#######
