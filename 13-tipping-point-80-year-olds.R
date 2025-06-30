#Tipping point for OAC medication main script
#by: Aleksi Kristian Winsten
#contact: alkrwi@utu.fi
#date: 7.06.2025
#University of Turku, Finland


load("data/main-analysis-80-year-olds.RData")



##################################################################
##                   Tipping Point Analysis                    ##
##################################################################

##################################################################
##        Calculate mean diff values and tipping point         ##
##################################################################

mean_diff_values <- vector(mode = "numeric", length = 101)
for (i in 1:101) {
  mean_diff_values[i] <- mean(sapply(simulated_cumqalys_with_noac[[i]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[i]], function(x) x$QALY))
}

# Calculate the tipping point
qaly.tipping.point <- (min(which(mean_diff_values >= 0)) -1 ) / 10 - 0.05
sprintf( "the QALY tipping point is: %.2f", qaly.tipping.point)
