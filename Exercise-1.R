#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

library(tidyverse)

install.packages("socviz")
library(socviz)


# Create a new table called rel_by_region

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above has taked the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

p1 <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  theme(legend.position = "top") +
  theme_classic()

p1


p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)

p2

# Install Color Brewer library to change palette

install.packages("RColorBrewer")
library(RColorBrewer)

# Create plot 2 with title, sorted bars, no gridlines, and color changes

p2 <- ggplot(rel_by_region, aes(x = reorder(religion, pct), y = pct, fill = religion)) +
  geom_col(aes(y = pct), position = position_stack(reverse = TRUE)) +
  labs(x = NULL, y = "Percent", title = "Most religions across US regions", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2")

p2

# Remove NA data from data set

data_complete <- rel_by_region[complete.cases(rel_by_region), ]
data_complete

p2 <- ggplot(data_complete, aes(x = reorder(religion, pct), y = pct, fill = religion)) +
  geom_col(aes(y = pct), position = position_stack(reverse = TRUE)) +
  labs(x = NULL, y = "Percent", title = "Protestant is the most ubiquitous religion in all but one US region", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2")

p2

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

# Once you're happy with your changes, save your plot:
ggsave("plot1.png",
  plot = last_plot(),
  dpi = 300)
