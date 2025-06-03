# Load necessary libraries (they were installed by postCreateCommand)
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 1. Read the CSV file
# The file is now in the same directory as your script within the Codespace
file_path <- "Satellite Database - refined.csv" # Relative path

tryCatch({
  sat_data <- read_csv(file_path, show_col_types = FALSE)
}, error = function(e) {
  stop(paste("Error reading CSV file:", e$message))
})

# 2. Clean and prepare the data
orbit_counts <- sat_data %>%
  rename(Class_of_Orbit = `Class of Orbit`) %>%
  filter(Class_of_Orbit %in% c("LEO", "MEO", "GEO", "Elliptical")) %>%
  count(Class_of_Orbit, name = "Count") %>%
  arrange(desc(Class_of_Orbit))

desired_orbits <- c("LEO", "MEO", "GEO", "Elliptical")
for (orb in desired_orbits) {
  if (!orb %in% orbit_counts$Class_of_Orbit) {
    orbit_counts <- orbit_counts %>% add_row(Class_of_Orbit = orb, Count = 0)
  }
}
orbit_counts <- orbit_counts %>% filter(Class_of_Orbit %in% desired_orbits)

# 3. Calculate percentages for labels
orbit_counts <- orbit_counts %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = paste0(Class_of_Orbit, "\n", Count, " (", sprintf("%.1f%%", Percentage), ")")
  )

# 4. Create the pie chart using ggplot2
pie_chart <- ggplot(orbit_counts, aes(x = "", y = Count, fill = Class_of_Orbit)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  labs(
    title = "Distribution of Satellites by Class of Orbit",
    fill = "Class of Orbit",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_brewer(palette = "Pastel1")

# 5. Display the chart
print(pie_chart)

# To save the chart (it will save within your Codespace workspace):
# ggsave("satellite_orbit_pie_chart.png", plot = pie_chart, width = 8, height = 7, dpi = 300)
