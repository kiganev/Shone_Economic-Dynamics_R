# Clear workspace
rm(list = ls())

# Clear plots
check_dev <- dev.list()
if(!is.null(check_dev)){
  dev.off(dev.list()["RStudioGD"])  
}

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc, check_dev)

# Set locale to English
Sys.setlocale("LC_ALL","en_US.utf8")

library(ggplot2)
library(dplyr)
library(deSolve)

### Example 2.12 (p. 44)

# Define the slope function
f <- function(x, y) {
  2*x - y
}

# Direction field grid
x_vals <- seq(-2, 2, length.out = 20)
y_vals <- seq(-2, 2, length.out = 20)
grid <- expand.grid(x = x_vals, y = y_vals)

# Compute direction vectors
grid <- grid %>%
  rowwise() %>%
  mutate(
    slope = f(x, y),
    angle = atan(slope),
    dx = 0.4 * cos(angle),
    dy = 0.4 * sin(angle)
  )

# Define the ODE in deSolve format
ode_func <- function(t, state, parameters) {
  with(as.list(state), {
    dy <- 2*x - y
    list(c(dx = 1, dy = dy))
  })
}

# Function to generate a solution trajectory
solve_trajectory <- function(x0, y0) {
  times <- seq(-5, 5, by = 0.1)
  out <- ode(y = c(x = x0, y = y0), times = times, func = ode_func, parms = NULL)
  as.data.frame(out)
}

# Generate solution curves for a few initial y values at x = 0
sol1 <- solve_trajectory(-2, 20)
sol2 <- solve_trajectory(-2, 10)
sol3 <- solve_trajectory(-2, 5)
sol4 <- solve_trajectory(-2, 2)
sol5 <- solve_trajectory(-2, -2)


# Combine all into one data frame
sol_df <- bind_rows(
  mutate(sol1, group = "y0 = 20"),
  mutate(sol2, group = "y0 = 10"),
  mutate(sol3, group = "y0 = 5"),
  mutate(sol4, group = "y0 = 2"),
  mutate(sol5, group = "y0 = -2")
)

# Plot the direction field with solution curves
plot_fig2_8 <- ggplot() +
  xlim(-2,2) +
  ylim(-2,2) + 
  # Direction field
  geom_segment(data = grid, aes(x = x, y = y, xend = x + dx, yend = y + dy),
               arrow = arrow(length = unit(0.1, "cm")), lineend = "round") +
  # Solution curves
  geom_path(data = sol_df, aes(x = x, y = y, color = group), linewidth = 1) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Direction Field with Solution Curves",
       x = "x", y = "y", color = "Initial condition")

plot_fig2_8

ggsave("fig2_8.pdf", plot_fig2_8)
