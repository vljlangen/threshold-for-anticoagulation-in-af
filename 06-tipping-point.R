# QALY Tipping Point Calculation Script

# Load simulation data (adjust path if needed)
load("data/main-analysis-70-year-olds.RData")

# Calculate mean QALY difference for each risk level
mean_diff_values <- vector(mode = "numeric", length = 101)
for (i in 1:101) {
  mean_diff_values[i] <- mean(sapply(simulated_cumqalys_with_noac[[i]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[i]], function(x) x$QALY))
}

# Find the tipping point
qaly.tipping.point <- (min(which(mean_diff_values >= 0)) -1 ) / 10 - 0.05
sprintf( "the QALY tipping point is: %.2f", qaly.tipping.point)

# Standard error function
default.std.err <- function(x) sd(x)/sqrt(length(x))

# Calculate means and confidence intervals for both groups
eckmann_with_noac_df <- do.call(rbind,
  lapply(simulated_cumqalys_with_noac, 
         function(isRisk.list) {
           x <- sapply(isRisk.list, function(x) x$QALY)
           c(mean(x), mean(x) - 1.96*default.std.err(x), mean(x) + 1.96*default.std.err(x))
         }
  )
)
colnames(eckmann_with_noac_df) <- c("mean.with", "lower.with", "upper.with")

eckmann_without_noac_df <- do.call(rbind,
  lapply(simulated_cumqalys_without_noac, 
         function(isRisk.list) {
           x <- sapply(isRisk.list, function(x) x$QALY)
           c(mean(x), mean(x) - 1.96*default.std.err(x), mean(x) + 1.96*default.std.err(x))
         }
  )
)
colnames(eckmann_without_noac_df) <- c("mean.without", "lower.without", "upper.without")

df <- cbind(ISrisk = seq(0,10,by=0.1), eckmann_with_noac_df)
df <- cbind(df, eckmann_without_noac_df)
df <- as.data.frame(df) 
