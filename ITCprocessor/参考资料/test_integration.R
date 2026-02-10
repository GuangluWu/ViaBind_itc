# 需在项目根目录运行：setwd("path/to/ITCprocessor") 后 source("test_integration.R")
source("R/integration.R")

# Mock data: 100 points
time <- 1:100
corrected_power <- rep(1, 100)
injection_indices <- c(10, 50, 90)

# Test 1: Window Width 10 points, Offset 0
# Start 1: 10. End 1: 10 + 10 = 20? Or 19?
# Limit: Next is 50. Limit 49.
# Expected End: 20 (or 19/21 depending on interpretation)
res1 <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 10, start_offset = 0)
print("Test 1 (Window 10, Offset 0):")
print(paste("Start:", res1$StartTime_s[1], "End:", res1$EndTime_s[1]))

# Test 2: Window Width 100 points (Exceeds gap), Offset 0
# Start 1: 10. Gap to 50 is 40 points.
# Limit: 49.
# Expected End: 49.
res2 <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 100, start_offset = 0)
print("Test 2 (Window 100, Offset 0):")
print(paste("Start:", res2$StartTime_s[1], "End:", res2$EndTime_s[1]))

# Test 3: Window Width 10 points, Offset +5
# Start 1: 15.
# Limit: Next 50+5 = 55. Limit 54.
# Target End: 15 + 10 = 25.
# Expected End: 25.
res3 <- integrate_peaks(time, corrected_power, injection_indices, integration_window = 10, start_offset = 5)
print("Test 3 (Window 10, Offset +5):")
print(paste("Start:", res3$StartTime_s[1], "End:", res3$EndTime_s[1]))
