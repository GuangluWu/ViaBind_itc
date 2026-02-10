# 需在项目根目录运行：setwd("path/to/ITCprocessor") 后 source("test_window.R")
source("R/integration.R")

# Mock data: 100 points
time <- 1:100
corrected_power <- rep(1, 100)
injection_indices <- c(10, 50, 90)

# Scenario A: Check if window limit works
# Start Offset 0, Window 20.
# Expect End = 10 + 20 = 30.
res_a <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 20, start_offset = 0)
print("Scenario A (Offset 0, Window 20):")
print(paste("Start:", res_a$StartTime_s[1], "End:", res_a$EndTime_s[1]))

# Scenario B: Check decoupling
# Start Offset +5, Window 20.
# Start = 15.
# End should be 10 + 20 = 30. (Fixed end point relative to injection marker)
res_b <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 20, start_offset = 5)
print("Scenario B (Offset +5, Window 20):")
print(paste("Start:", res_b$StartTime_s[1], "End:", res_b$EndTime_s[1]))

# Scenario C: Check user's issue "Fixed width"
# If I change Window to 30.
# End should be 10 + 30 = 40.
res_c <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 30, start_offset = 5)
print("Scenario C (Offset +5, Window 30):")
print(paste("Start:", res_c$StartTime_s[1], "End:", res_c$EndTime_s[1]))
