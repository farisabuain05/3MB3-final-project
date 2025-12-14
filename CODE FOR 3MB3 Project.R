# Parameters

S0    <- 100     # initial dose 
c     <- 0.6     # clearance rate
ka    <- 1.2     # absorption rate
t_end <- 12      # hours
time  <- seq(0, t_end, by = 0.01)

S <- function(t, S0, ka) {
  S0 * exp(-ka * t)
}

A_ext <- function(t, S0, ka, c) {
  (ka * S0 / (c - ka)) * (exp(-ka * t) - exp(-c * t))
}

A_base <- function(t, S0, c) {
  S0 * exp(-c * t)
}

# Figure 1: S(t) and A(t)

plot(time, S(time, S0, ka), type = "l", lwd = 2, col = "blue",
     xlab = "Time (hr)", ylab = "Drug amount (mg)",
     main = "Extended model: S(t) and A(t)")
lines(time, A_ext(time, S0, ka, c), lwd = 2, col = "red")

legend("topright",
       legend = c("S(t) (gut)", "A(t) (bloodstream)"),
       col = c("blue", "red"),
       lty = 1, lwd = 2, bty = "n")

# Figure 2: Effect of k_a

ka_values <- c(0.2, 0.5, 1.0, 2.0)
cols <- c("darkorange", "goldenrod", "forestgreen", "purple")

plot(time, A_ext(time, S0, ka_values[1], c), type = "l", lwd = 2,
     col = cols[1],
     xlab = "Time (hr)", ylab = "A(t) (mg)",
     main = "Extended model: effect of absorption rate k_a on A(t)",
     ylim = c(0, max(A_ext(time, S0, ka_values, c))))

for (i in seq_along(ka_values)) {
  lines(time, A_ext(time, S0, ka_values[i], c),
        lwd = 2, col = cols[i])
}

legend("topright",
       legend = paste("k_a =", ka_values),
       col = cols,
       lty = 1, lwd = 2, bty = "n")

# Figure 3: Base vs Extended

plot(time, A_base(time, S0, c), type = "l", lwd = 2, col = "black",
     xlab = "Time (hr)", ylab = "A(t) (mg)",
     main = "Base vs Extended model")
lines(time, A_ext(time, S0, ka, c), lwd = 2, col = "red")

legend("topright",
       legend = c("Base model", "Extended model"),
       col = c("black", "red"),
       lty = 1, lwd = 2, bty = "n")

