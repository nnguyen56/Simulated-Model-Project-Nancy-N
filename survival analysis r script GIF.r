# Install and load required packages
install.packages(c("survival", "survminer", "gganimate", "gifski"))
library(survival)
library(survminer)
library(gganimate)
library(gifski)
library(ggplot2)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Simulate data
n <- 200  # Number of individuals
data <- data.frame(
  id = 1:n,
  trauma_severity = runif(n, 1, 10),  # Trauma severity on a scale of 1-10
  time_to_event = rexp(n, rate = 0.1),  # Time to event (e.g., PTSD diagnosis)
  event = rbinom(n, 1, 0.7)  # Whether the event occurred (1 = event, 0 = censored)
)

# Inspect the data
head(data)

# Create a survival object
surv_obj <- Surv(time = data$time_to_event, event = data$event)

# Fit a Cox proportional hazards model
cox_model <- coxph(surv_obj ~ trauma_severity, data = data)

# Summary of the model
summary(cox_model)

# Create groups based on trauma severity for visualization
data$trauma_group <- cut(data$trauma_severity, breaks = 3, labels = c("Low", "Medium", "High"))

# Fit Kaplan-Meier curves by trauma group
km_fit <- survfit(Surv(time_to_event, event) ~ trauma_group, data = data)

# Extract survival information into a data frame for plotting
km_data <- broom::tidy(km_fit) %>%
  left_join(data %>% distinct(trauma_group), by = c("strata" = "trauma_group")) %>%
  mutate(trauma_group = case_when(
    grepl("Low", strata) ~ "Low",
    grepl("Medium", strata) ~ "Medium",
    grepl("High", strata) ~ "High"
  ))

# Create animated plot of Kaplan-Meier curves by trauma group
animated_plot <- ggplot(km_data, aes(x = time, y = estimate, color = trauma_group, group = trauma_group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = trauma_group), alpha = 0.2) +
  labs(
    title = 'Survival Curves by Trauma Severity: Time = {frame_time}',
    x = 'Time to Event',
    y = 'Survival Probability',
    color = 'Trauma Severity',
    fill = 'Trauma Severity'
  ) +
  theme_minimal() +
  transition_reveal(time)

# Display the animated plot as a gif
animate(animated_plot, renderer = gifski_renderer(), nframes = 100)
