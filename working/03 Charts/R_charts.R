library(readr)

R_charts <- read_csv("jupyter/R_charts.csv", col_types = cols(
  ...1 = col_skip(),
  `2005` = col_double(),
  `2006` = col_double(),
  `2008` = col_double(), 
  `2009` = col_double(),
  `2011` = col_double(), 
  `2012` = col_double(),
  `2013` = col_double(), 
  `2014` = col_double(),
  `2016` = col_double(), 
  `2017` = col_double(),
  `2018` = col_double()))

# Load necessary library
library(ggplot2)
library(tidyr)
library(dplyr) # For data manipulation
library(zoo) #For interpolation
library(showtext) #For fonts
library(scales) #For extended breaks
library(patchwork) #For combined plots

# Load fonts for plotting
font_add_google(name = "Bitter", family = "bitter")
showtext_auto()

# Filter the dataset for the required neighbourhoods
filtered_data_FH <- R_charts %>%
  filter(Neighbourhood %in% c("Cricklewood", "Forest Hill"))
filtered_data_AR <- R_charts %>%
  filter(Neighbourhood %in% c("Wembley Central", "Abbey Road"))
filtered_data_IW <- R_charts %>%
  filter(Neighbourhood %in% c("Harlesden", "Imperial Wharf"))

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)

# List of datasets, x-intercept years, colors, and dynamic titles
datasets <- list(
  FH = list(
    data = filtered_data_FH,
    xintercept = 2010,
    colors = c("Cricklewood" = "royalblue", "Forest Hill" = "orange")
  ),
  AR = list(
    data = filtered_data_AR,
    xintercept = 2011,
    colors = c("Wembley Central" = "gray", "Abbey Road" = "gold")
  ),
  IW = list(
    data = filtered_data_IW,
    xintercept = 2009,
    colors = c("Harlesden" = "lightblue", "Imperial Wharf" = "forestgreen")
  )
)

# Loop through datasets and process each one
plots <- list() # To store the resulting plots

for (name in names(datasets)) {
  
  # Access the current dataset, x-intercept, colors, and generate dynamic title
  data <- datasets[[name]]$data
  xint <- datasets[[name]]$xintercept
  color_values <- datasets[[name]]$colors
  neighborhoods <- names(color_values) # Extract neighborhood names for the title
  dynamic_title <- paste(neighborhoods[1], "(control) vs.", neighborhoods[2], "(treatment)")
  
  # Convert from wide to long format
  data_long <- data %>%
    pivot_longer(cols = -Neighbourhood, # All columns except "Neighbourhood"
                 names_to = "Year",
                 values_to = "IMD_Rank")
  
  # Ensure "Year" is numeric
  data_long$Year <- as.numeric(data_long$Year)
  
  # Interpolate null values
  data_long <- data_long %>%
    group_by(Neighbourhood) %>% # Ensure interpolation is by group
    mutate(IMD_Rank = na.approx(IMD_Rank, na.rm = FALSE)) # Linear interpolation
  
  # Create the plot for the current dataset
  plot <- ggplot(data_long, aes(x = Year, y = IMD_Rank, color = Neighbourhood)) +
    geom_line(linewidth = 1) + # Line plot with specified line thickness
    scale_color_manual(values = color_values) + # Use dynamically provided colors
    labs(
      title = dynamic_title, # Use dynamically generated title
      x = "IMD year",
      y = "Population-weighted IMD rank",
      color = "Neighbourhood"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "bitter", size = 30), # Use Bitter font
      panel.grid.major.x = element_blank(), # Remove vertical grid lines
      panel.grid.major.y = element_line(color = "grey", size = 0.5), # Thin grey horizontal grid lines
      panel.grid.minor = element_blank() # Remove minor grid lines
    ) +
    geom_vline(xintercept = xint, linetype = "dotted", size = 1.5, color = "black") + # Add dynamic vertical dotted line
    scale_x_continuous(breaks = c(2004, 2007, 2010, 2015, 2019)) + # Specify the exact years for x-axis labels
    scale_y_continuous(breaks = extended_breaks(n = 5)) # Dynamic calculation for roughly 4 y gridlines
  # Store the plot in the list
  plots[[name]] <- plot
}

# Access individual plots
plots$FH # Plot for filtered_data_FH
plots$AR # Plot for filtered_data_AR
plots$IW # Plot for filtered_data_IW

# Combine plots into a 3x1 column
combined_plot <- plots$FH / plots$AR / plots$IW + 
  plot_layout(
    ncol = 1,               # 1 column, 3 rows
    heights = c(1, 0.1, 1)  # Adjust heights to control spacing between plots
  )
combined_plot

# Save the combined plot as a single PNG file
ggsave(
  filename = "combined_plot.png",
  plot = combined_plot,
  width = 12,  # Width of the combined plot in inches
  height = 18, # Height in inches (adjust for 3x1 layout)
  dpi = 300    # High resolution for export
)

