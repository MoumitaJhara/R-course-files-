install.packages("sf")
library(tidyverse)
library(sf)
library(ggplot2)

# 1. Load shapefile and dengue data
bangladesh_shp <- st_read("C:/Users/Windows 10 22H2/Downloads/gadm41_BGD_shp/gadm41_BGD_2.shp")
dengue_data <- read_csv("C:/Users/Windows 10 22H2/Downloads/Cleaned_Dengue_Data_by_Year.csv")

# 2. Clean names
dengue_data$District <- str_trim(dengue_data$District)
bangladesh_shp$NAME_2 <- str_trim(bangladesh_shp$NAME_2)

Filter out districts with NA dengue category
plot_data <- merged_data %>% 
  filter(!is.na(Cases_Category))

# Create centroids for labeling
plot_data <- plot_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# Plot map with district labels
ggplot(plot_data) +
  geom_sf(aes(fill = Cases_Category), color = "gray80", size = 0.2) +
  scale_fill_manual(
    name = "Dengue Cases 2023",
    values = map_colors,
    drop = FALSE
  ) +
  geom_text(
    aes(label = NAME_2),
    size = 2.2, color = "black", fontface = "bold", check_overlap = TRUE
  ) +
  labs(
    title = "District Based Spatial Distribution of Dengue Cases in Bangladesh 2023",
    caption = "Source: DGHS, MIS, Health Emergency Operation Center & Control Room"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# 8. Show it
print(map_plot)

#For 2022

# 3. Define bins and colors
bin_levels <- c("1–500", "501–1500", "1501–3000", "3001–6000", "6001–18000", "18001–120000")
map_colors <- c(
  "1–500" = "#ffffb2",
  "501–1500" = "#fecc5c",
  "1501–3000" = "#fd8d3c",
  "3001–6000" = "#f03b20",
  "6001–18000" = "#bd0026",
  "18001–120000" = "#800026"
)

# 4. Merge data and fix NAs
merged <- bangladesh_shp %>%
  left_join(dengue_data, by = c("NAME_2" = "District")) %>%
  mutate(
    Cases_2022 = ifelse(is.na(Cases_2022) | Cases_2022 == 0, 1, Cases_2022),
    Cases_Category = factor(
      cut(Cases_2022,
          breaks = c(0, 500, 1500, 3000, 6000, 18000, 120000),
          labels = bin_levels,
          include.lowest = TRUE
      ),
      levels = bin_levels
    )
  )

# 5. Add dummy rows to force full legend
dummy <- st_as_sf(
  tibble(
    Cases_Category = factor(bin_levels, levels = bin_levels),
    lon = 90 + 0.1 * 1:6,
    lat = rep(20, 6)
  ),
  coords = c("lon", "lat"),
  crs = st_crs(bangladesh_shp)
)

# 6. Compute label centroids
merged <- merged %>%
  mutate(centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

# 7. Plot
map_plot <- ggplot() +
  geom_sf(data = merged, aes(fill = Cases_Category), color = "gray80", size = 0.2) +
  geom_sf(data = dummy, aes(fill = Cases_Category), alpha = 0) +
  geom_text(data = merged, aes(x = lon, y = lat, label = NAME_2),
            size = 2.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  scale_fill_manual(name = "Dengue Cases 2022", values = map_colors, drop = FALSE) +
  labs(
    title = "District Based Spatial Distribution of Dengue Cases in Bangladesh 2022",
    caption = "Source: DGHS, MIS, Health Emergency Operation Center & Control Room\n31st December 2022"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 10, 10, 10)
  )

# 8. Show it
print(map_plot)


#For 2021

# 1. Load shapefile and dengue data
bangladesh_shp <- st_read("C:/Users/Windows 10 22H2/Downloads/gadm41_BGD_shp/gadm41_BGD_2.shp")
dengue_data <- read_csv("C:/Users/Windows 10 22H2/Downloads/Cleaned_Dengue_Data_by_Year.csv")

# Define breaks and labels
breaks_2021 <- c(-1, 0, 500, 1500, 3000, 6000, 18000, 40000)
labels_2021 <- c("0", "1–500", "501–1500", "1501–3000", "3001–6000", "6001–18000", "18001–40000")

# Categorize 2021 cases
dengue_data <- dengue_data %>%
  mutate(
    Cases_Category_2021 = cut(
      Cases_2021,
      breaks = breaks_2021,
      labels = labels_2021,
      right = TRUE
    )
  )

# Merge with shapefile and assign "1–500" to NA after join
merged_data_2021 <- bangladesh_shp %>%
  left_join(dengue_data, by = c("NAME_2" = "District")) %>%
  mutate(
    Cases_Category_2021 = as.character(Cases_Category_2021),
    Cases_Category_2021 = ifelse(is.na(Cases_Category_2021), "1–500", Cases_Category_2021),
    Cases_Category_2021 = factor(Cases_Category_2021, levels = labels_2021)
  )

# Define color palette
map_colors <- c(
  "0" = "#a1d99b",              # green
  "1–500" = "#ffffcc",          # pale yellow
  "501–1500" = "#ffeda0",       # light yellow
  "1501–3000" = "#feb24c",      # orange
  "3001–6000" = "#fd8d3c",      # red-orange
  "6001–18000" = "#e31a1c",     # dark red
  "18001–40000" = "#800026"     # maroon
)

# Plot the map
ggplot(merged_data_2021) +
  geom_sf(aes(fill = Cases_Category_2021), color = "gray60", size = 0.2) +
  geom_sf_text(aes(label = NAME_2), size = 2.5, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    name = "Dengue 2021",
    values = map_colors,
    drop = FALSE
  ) +
  labs(
    title = "District Based Spatial Distribution of Dengue Cases in Bangladesh 2021",
    caption = "Data Source: DGHS, MIS, Health Emergency Operation Center & Control Room"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # smaller centered title
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 20, 20),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# 8. Show it
print(map_plot)



#for 2020
# Define breaks and labels
breaks_2020 <- c(-1, 0, 500, 1500, 3000, 6000, 18000, 40000)
labels_2020 <- c("0", "1–500", "501–1500", "1501–3000", "3001–6000", "6001–18000", "18001–40000")

# Categorize 2020 cases
dengue_data <- dengue_data %>%
  mutate(
    Cases_Category_2020 = cut(
      Cases_2020,
      breaks = breaks_2020,
      labels = labels_2020,
      right = TRUE
    )
  )

# Merge with shapefile and assign "1–500" to NA after join
merged_data_2020 <- bangladesh_shp %>%
  left_join(dengue_data, by = c("NAME_2" = "District")) %>%
  mutate(
    Cases_Category_2020 = as.character(Cases_Category_2020),
    Cases_Category_2020 = ifelse(is.na(Cases_Category_2020), "1–500", Cases_Category_2020),
    Cases_Category_2020 = factor(Cases_Category_2020, levels = labels_2020)
  )

# Define color palette
map_colors <- c(
  "0" = "#a1d99b",              # green
  "1–500" = "#ffffcc",          # pale yellow
  "501–1500" = "#ffeda0",       # light yellow
  "1501–3000" = "#feb24c",      # orange
  "3001–6000" = "#fd8d3c",      # red-orange
  "6001–18000" = "#e31a1c",     # dark red
  "18001–40000" = "#800026"     # maroon
)

# Plot the map
ggplot(merged_data_2020) +
  geom_sf(aes(fill = Cases_Category_2020), color = "gray60", size = 0.2) +
  geom_sf_text(aes(label = NAME_2), size = 2.5, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    name = "Dengue 2020",
    values = map_colors,
    drop = FALSE
  ) +
  labs(
    title = "District Based Spatial Distribution of Dengue Cases in Bangladesh 2020",
    caption = "Data Source: DGHS, MIS, Health Emergency Operation Center & Control Room"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # smaller centered title
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 20, 20),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# 8. Show it
print(map_plot)


# Temporal analysis of dengue 2023
library(ggplot2)
library(dplyr)

# Load the data
file_path <- "C:/Users/Windows 10 22H2/Downloads/Temporal analysis of dengue 2023.xlsx"
dengue_data <- read_excel(file_path, sheet = "Sheet1", skip = 1)

# Scaling
cfr_scale_factor <- 45000
death_scale_factor <- 10

# Reshape for stacked legend handling
bar_data <- dengue_data %>%
  tidyr::pivot_longer(cols = c("Cases", "Deaths"), names_to = "Metric", values_to = "Value") %>%
  mutate(Scaled_Value = ifelse(Metric == "Deaths", Value * death_scale_factor, Value),
         Fill_Label = ifelse(Metric == "Deaths", "Death", "Case"))

# Plot
ggplot() +
  # Bar layers with legend
  geom_bar(data = bar_data, aes(x = Month, y = Scaled_Value, fill = Fill_Label),
           stat = "identity", position = "identity", width = 0.5) +
  
  # CFR line and points with legend
  geom_line(data = dengue_data, aes(x = Month, y = CFR * cfr_scale_factor, color = "CFR (%)", group = 1),
            size = 1.3) +
  geom_point(data = dengue_data, aes(x = Month, y = CFR * cfr_scale_factor, color = "CFR (%)"), size = 2) +
  
  # CFR value labels
  geom_text(data = dengue_data, aes(x = Month, y = CFR * cfr_scale_factor, label = CFR),
            vjust = -1, color = "red", size = 4, fontface = "bold") +
  
  # Death value labels
  geom_text(data = dengue_data, aes(x = Month, y = Deaths * death_scale_factor, label = Deaths),
            vjust = 1.5, color = "black", size = 4) +
  
  # Axes
  scale_y_continuous(
    name = "CASE AND DEATH",
    sec.axis = sec_axis(~ . / cfr_scale_factor, name = "CFR (%)"),
    limits = c(0, 90000)
  ) +
  
  # Manual legends for fill (bars) and color (line)
  scale_fill_manual(name = "", values = c("Case" = "skyblue", "Death" = "yellow")) +
  scale_color_manual(name = "", values = c("CFR (%)" = "red")) +
  
  # Labels and Theme
  labs(title = "Monthly Dengue Cases, Deaths, and CFR in 2023") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y.left = element_text(color = "black", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13),
    axis.text.y.right = element_text(color = "red", size = 10),
    axis.text.y.left = element_text(color = "black", size = 10),
    axis.text.x = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )
# Age- specific grouped bar chart 
library(ggplot2)
library(dplyr)
library(tidyr)

# Data
df <- data.frame(
  Age_Group = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
                "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-80", ">80"),
  Cases = c(16019, 16209, 21929, 38795, 45343, 41284, 31060, 27884,
            20517, 17932, 13859, 12065, 8161, 5316, 2283, 1466, 1057),
  Deaths = c(66, 58, 44, 100, 125, 148, 155, 165,
             133, 139, 122, 134, 118, 87, 49, 26, 36)
)

# Scaling
death_scale_factor <- 350

# Reshape data
df_long <- df %>%
  mutate(Deaths_scaled = Deaths * death_scale_factor) %>%
  pivot_longer(cols = c("Cases", "Deaths_scaled"), names_to = "Metric", values_to = "Count") %>%
  mutate(Metric = recode(Metric, "Deaths_scaled" = "Deaths"))

# Plot with all text set to black
ggplot(df_long, aes(x = Age_Group, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(
    name = "Number of Cases",
    breaks = seq(0, 60000, 10000),
    sec.axis = sec_axis(~ . / death_scale_factor, name = "Number of Deaths")
  ) +
  scale_fill_manual(values = c("Cases" = "green", "Deaths" = "yellow")) +
  labs(
    title = "Dengue Cases and Deaths by Age Group (2023)",
    x = "Age Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Sex-specific analysis 
library(ggplot2)
library(dplyr)

# Prepare case and death data
cases <- data.frame(
  Sex = c("Female", "Male"),
  Count = c(128569, 192610),
  Type = "Cases"
)

deaths <- data.frame(
  Sex = c("Female", "Male"),
  Count = c(970, 735),
  Type = "Deaths"
)

# Combine data
df <- bind_rows(cases, deaths)

# Compute percentages for labeling
df <- df %>%
  group_by(Type) %>%
  mutate(
    Fraction = Count / sum(Count),
    Label = paste0(Sex, "\n", round(Fraction * 100), "%"),
    ymax = cumsum(Fraction),
    ymin = c(0, head(ymax, -1))
  )

# Donut chart using geom_rect + coord_polar
ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Sex)) +
  geom_rect() +
  facet_wrap(~Type, nrow = 1) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("Female" = "hotpink", "Male" = "dodgerblue")) +
  theme_void() +
  geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Label), color = "black", size = 4.5) +
  ggtitle("Donut Charts: Gender Distribution of Dengue Cases and Deaths (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#Chi-square test
# Create contingency table for cases
cases <- matrix(c(128569, 192610), nrow = 2, byrow = TRUE,
                dimnames = list(Gender = c("Female", "Male"),
                                Status = c("Case")))

# Create contingency table for deaths
deaths <- matrix(c(970, 735), nrow = 2, byrow = TRUE,
                 dimnames = list(Gender = c("Female", "Male"),
                                 Status = c("Death")))

# Add "non-case" and "non-death" assuming same base population
# For illustrative purpose, use the total as sum of cases + non-cases
# This step allows 2x2 Chi-sq test instead of 1-row table

# Estimate: Let's say total observed population = 128569 + 192610 + 500000 (add nonspecific group)
total_pop <- 500000 + 128569 + 192610
non_cases_female <- (total_pop / 2) - 128569
non_cases_male   <- (total_pop / 2) - 192610

non_deaths_female <- 128569 - 970  # deaths are from cases
non_deaths_male   <- 192610 - 735

# 2x2 table for cases
cases_tbl <- matrix(c(128569, non_cases_female,
                      192610, non_cases_male),
                    nrow = 2, byrow = TRUE,
                    dimnames = list(Gender = c("Female", "Male"),
                                    Status = c("Case", "No Case")))

# 2x2 table for deaths
deaths_tbl <- matrix(c(970, non_deaths_female,
                       735, non_deaths_male),
                     nrow = 2, byrow = TRUE,
                     dimnames = list(Gender = c("Female", "Male"),
                                     Status = c("Death", "Survived")))

# Run chi-squared tests
chisq.test(cases_tbl)
chisq.test(deaths_tbl)

