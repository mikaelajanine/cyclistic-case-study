---
title: "One Ride Isn’t Enough: How Cyclistic Can Win Over Casual Riders"
author: "Mikaela Janine"
date: "2025-06-09"
output: 
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    number_sections: false
    df_print: paged
    code_folding: hide
---

---

**Let’s talk bikes**. Specifically, bike-share bikes. If you’ve ever rolled through the city with Cyclistic, your ride might tell a bigger story than you think.

Every trip belongs to either a **casual rider** or a **member** — at least that’s how the system sorts it. But what really separates the two? Are casual riders just in it for the fun? Are members all about the daily grind? Before we assume too much, we need to dig into the data.

This analysis takes a closer look at Cyclistic’s 2019 ride data using R. It’s part of my capstone for the Google Data Analytics Professional Certificate. The company might be fictional, but the insights come from real-world public data, which makes it the perfect sandbox to explore what drives rider behavior — and how marketing can turn occasional riders into loyal ones.

Whether you’re into marketing, curious about data, or just wondering why people ride when they do, you’re in the right place.

Let’s dive in.

---

## Behind the Insights

If you're just here for the takeaways, feel free to skip the behind-the-scenes. But I’ve included my full data analysis process to showcase how I worked through it in R. If you're curious, go ahead and dive in. And if you want to see the code, just hit "Show." I’d also love to hear your thoughts—whether it's feedback on the methods I used or something you think I could improve. Be my critic. I'm all ears. 

---

## Data Cleaning

First things first, I loaded the necessary packages and brought in the data.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(grid)
library(shiny)
library(patchwork)
library(reactable)
library(knitr)
library(kableExtra)
```

I worked with four separate datasets—one for each quarter of 2019. But things got a little messy with the second quarter. Its column names didn’t match the other three. Same data, just different labels. So, I renamed the Q2 columns to match the others so I could combine everything into one clean dataset for the full year.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
q2 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4 <- read_csv("Divvy_Trips_2019_Q4.csv")
names(q2) <- c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear")
```

Once that was done, I removed rows with missing values. I know deleting data sounds a bit harsh, but I checked. The missing values were random and didn’t seem critical, so it felt safe to drop them to keep the dataset clean and analysis-ready.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
data <- bind_rows(q1, q2, q3, q4)
df <- na.omit(data)
```

Next, I wanted to double-check the trip IDs. They should all be 8-digit numbers, but some came up short—literally. After a quick summary, I found 4 trip IDs that only had 7 digits. I looked into it, but without a way to verify or correct them, I chose to exclude those entries. Not ideal, but for clean, reliable analysis, I figured it was the best call.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
# Inspect and filter trip_id lengths
df_summary <- df %>%
  mutate(
    trip_id_length = str_length(trip_id),
    has_space = str_detect(trip_id, "\\s")
  )

# Display unique lengths, space presence, and filter trip_id by length
trip_id_check <- df_summary %>%
  summarise(
    unique_lengths = paste(sort(unique(trip_id_length)), collapse = ", "),
    any_spaces = any(has_space)
  )

# Display the number of trip IDs with each length
length_distribution <- df_summary %>%
  count(trip_id_length)

# Show 7-digit trip IDs (if any)
trip_ids_7_digits <- df_summary %>%
  filter(trip_id_length == 7)

# Keep only 8-digit trip IDs in the main dataset
df <- df_summary %>%
  filter(trip_id_length == 8) %>%
  select(-trip_id_length, -has_space)

# Output summaries
length_distribution
trip_ids_7_digits
```

Now let’s take a closer look at the data. Specifically, the number of unique values in each column.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
sapply(df, function(x) length(unique(x)))
```

I noticed something off right away. The number of unique entries in from_station_id didn’t match from_station_name — same with to_station_id and to_station_name. Ideally, each station ID should link to just one station name, but that wasn’t the case here.

So what’s going on?

Some station names had extra characters like asterisks, spaces, or notes in parentheses (like “(temp)”), which made the same station appear under different names. To clean that up, I stripped out the extra spaces, anything inside parentheses, and any asterisks.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df <- df %>%
  mutate(from_station_name = str_trim(str_remove(from_station_name, "\\s*\\(.*\\)|\\*")))
```

I review the remaining from_station_id with more than 1 unique names

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  group_by(from_station_id) %>%
  summarise(unique_names = n_distinct(from_station_name)) %>%
  filter(unique_names > 1)
```
When I looked closer, I saw they referred to the same place, just written differently.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  filter(from_station_id == 645) %>%
  distinct(from_station_name)
```
So I checked which name appeared more often and used that as the standard.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  filter(from_station_id == 645) %>%
  count(from_station_name, sort = TRUE) %>%
  slice(1)
```
Then I replaced the less common variations to keep things consistent.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
corrections <- data.frame(
  from_station_id = c(19, 217, 238, 286, 645),
  incorrect_name = c("Loomis St & Taylor St", "Racine Ave & Fulton St", "Wolcott", "Franklin St & Adams St", "Archer"),
  correct_name = c("Throop St & Taylor St", "Elizabeth St & Fulton St", "Wolcott Ave & Montrose Ave", "Franklin St & Quincy St", "Archer Ave & 37th St")
)
df <- df %>%
  left_join(corrections, by = c("from_station_id", "from_station_name" = "incorrect_name")) %>%
  mutate(
    from_station_name = ifelse(is.na(correct_name), from_station_name, correct_name)
  ) %>%
  select(-correct_name)
```

After the cleanup, every from_station_id matched exactly one from_station_name. Much better.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
sapply(df, function(x) length(unique(x)))
```
Next, I did the same cleanup for to_station_id and to_station_name. That’s sorted now too.

```{r}
# Clean, identify inconsistencies, apply corrections, and check uniqueness
df <- df %>%
  mutate(to_station_name = str_trim(str_remove(to_station_name, "\\s*\\(.*\\)|\\*"))) %>%
  left_join(
    data.frame(
      to_station_id = c(19, 217, 238, 286, 645),
      incorrect_name = c("Loomis St & Taylor St", "Racine Ave & Fulton St", "Wolcott", 
                         "Franklin St & Adams St", "Archer"),
      correct_name = c("Throop St & Taylor St", "Elizabeth St & Fulton St", 
                       "Wolcott Ave & Montrose Ave", "Franklin St & Quincy St", 
                       "Archer Ave & 37th St")
    ),
    by = c("to_station_id", "to_station_name" = "incorrect_name")
  ) %>%
  mutate(
    to_station_name = ifelse(is.na(correct_name), to_station_name, correct_name)
  ) %>%
  select(-correct_name)

# Check station IDs with multiple names and output unique value counts per column
df %>%
  group_by(to_station_id) %>%
  summarise(unique_names = n_distinct(to_station_name)) %>%
  filter(unique_names > 1)

sapply(df, function(x) length(unique(x)))
```
Then I moved on to review the birthyear column. The minimum value was 1759 and the maximum was 2014 — which clearly doesn’t add up. No one riding in 2019 was born in the 1700s. That 1759 entry looked like a typo, so I assumed it was meant to be 1959.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  summarise(
    min_birthyear = min(birthyear, na.rm = TRUE),
    max_birthyear = max(birthyear, na.rm = TRUE)
  )
```

Looking further, I also found a 1790 in the mix. Again, likely a typo, so I adjusted it to 1990.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df <- df %>%
  mutate(birthyear = case_when(
    birthyear == 1759 ~ 1959,
    birthyear == 1790 ~ 1990,
    TRUE ~ birthyear
  ))
```

With those fixes, the data looks much cleaner and more realistic. Ready to move forward.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  summarise(
    min_birthyear = min(birthyear, na.rm = TRUE),
    max_birthyear = max(birthyear, na.rm = TRUE)
  )
```

___

## Data Transformation

To make the analysis more insightful, I added a few extra columns. First, I created an age column by subtracting the birth year from 2019 (since the dataset is from that year). Then, I pulled the month, hour, and day of the week from the start_time column, which follows the format YYYY-MM-DD hh:mm:ss. I also converted the trip duration into minutes for easier interpretation.

Next, I grouped some of the values into categories:

**Age groups**

* 18 and below → Children/Teens
* 19 to 35 → Young Adults
* 36 to 50 → Middle-Aged Adults
* 51 to 65 → Older Adults
* 66 and above → Seniors

**Time of day**

* 5 AM to before 12 PM → Morning
* 12 PM to before 5 PM → Afternoon
* 5 PM to before 9 PM → Evening
* 9 PM to before 5 AM → Night

These added layers help us better understand who’s riding and when.

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df <- df %>%
  mutate(
    age = 2019 - birthyear,
    age_group = cut(age, breaks = c(0, 18, 35, 50, 65, Inf),
                    labels = c("0–18", "19–35", "36–50", "51–65", "66+")),
    age_label = case_when(
      age <= 18 ~ "Children/Teens",
      age <= 35 ~ "Young Adults",
      age <= 50 ~ "Middle-Aged Adults",
      age <= 65 ~ "Older Adults",
      age > 65 ~ "Seniors"
    ),
    month = month(start_time, label = TRUE, abbr = FALSE),
    hour = hour(start_time),
    weekday = wday(start_time, label = TRUE, abbr = FALSE),
    time_label = case_when(
      hour >= 5 & hour < 12  ~ "Morning",
      hour >= 12 & hour < 17 ~ "Afternoon",
      hour >= 17 & hour < 21 ~ "Evening",
      hour >= 21 | hour < 5  ~ "Night"
    ),
    trip_duration_min = tripduration / 60
  )
```

___

## Data Analysis

A quick glance at the numbers shows Cyclistic is driven mostly by its loyal members. They account for a huge 89.4% of all rides, while casual riders make up just 10.6%. That gap naturally raises a few questions: What sets these two groups apart? And more importantly, what would it take to turn more casual riders into long-term members?

```{r}
# Prepare percentage data
user_counts <- df %>%
  count(usertype) %>%
  mutate(
    percent = n / sum(n) * 100,
    label = paste0(usertype, "\n", round(percent, 1), "%")
  )

# Donut chart with consistent theme
ggplot(user_counts, aes(x = 2, y = percent, fill = usertype)) +
  geom_col(width = 1, color = "gray30", alpha = 0.5) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5  # Smaller text size (default was 5)
  ) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  xlim(0.5, 2.5) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color = "gray30"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "User Type Distribution",
    subtitle = "Percentage of Customers and Subscribers"
  )
```

***

### Riding Seasons and Weekly Habits

To start answering that, let’s look at when people are riding. August sees the most action for both groups—no surprise, it’s summer and the city’s buzzing. But patterns shift once we break it down by days of the week. Members are more active during weekdays, suggesting a steady, regular habit. Casual riders, on the other hand, come alive on weekends, when the pace is slower and the vibe is more relaxed.

```{r monthly-weekday-usage-dashboard, fig.width=14, fig.height=10, warning=FALSE, message=FALSE}

# Shared theme
base_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold", color = "gray20"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "gray20"),
    axis.text.y = element_text(color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray97"),
    panel.grid.major.x = element_line(color = "gray97")
  )

# Monthly usage by user type
monthly_usertype <- df %>%
  count(month, usertype) %>%
  ggplot(aes(x = month, y = n, color = usertype, group = usertype)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Monthly Usage by User Type",
    subtitle = "Trends in ride activity across months",
    x = "Month", y = "Number of Rides", color = NULL
  ) +
  base_theme +
  theme(legend.position = "top", legend.text = element_text(color = "gray30"), legend.title = element_blank())

# Monthly usage by Customers
monthly_customers <- df %>%
  filter(usertype == "Customer") %>%
  count(month, usertype) %>%
  ggplot(aes(x = month, y = n, group = 1)) +
  geom_line(color = "#E69F00", size = 1, alpha = 0.5) +
  geom_point(color = "#E69F00", size = 1.5, alpha = 0.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Monthly Usage by Customers",
    subtitle = "Trends in ride activity across months (non-subscribers only)",
    x = "Month", y = "Number of Rides"
  ) +
  base_theme +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

# Usage by weekday (all users)
weekday_all <- ggplot(df, aes(x = weekday, fill = usertype)) +
  geom_bar(position = "stack", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Day of the Week",
    subtitle = "Ride counts differentiated by user type",
    x = "Weekday", y = "Count", fill = NULL
  ) +
  base_theme +
  theme(legend.position = "top", legend.title = element_text(face = "bold"), panel.grid.major.x = element_blank())

# Usage by weekday (Customers)
weekday_customers <- df %>%
  filter(usertype == "Customer") %>%
  ggplot(aes(x = weekday)) +
  geom_bar(fill = "#E69F00", alpha = 0.5, color = "gray30", size = 0.4, width = 0.7) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Day of the Week",
    subtitle = "Ride counts for Customers (non-subscribers)",
    x = "Weekday", y = "Count"
  ) +
  base_theme +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

# Combine plots using patchwork
(monthly_usertype + monthly_customers) /
(weekday_all + weekday_customers) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

```

***

### When People Ride

That difference becomes even clearer when we check the time of day. Members hop on mostly in the morning, but usage stays solid throughout the day. Casual riders tend to peak in the afternoon, probably when the weather’s nicer and the day feels more open. Interestingly, both groups hit their highest point around 5 PM—just as the workday ends and the city transitions into evening.

```{r fig.width=14, fig.height=10, warning=FALSE, message=FALSE}
base_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold", color = "gray20"),
    axis.text.x = element_text(color = "gray20"),
    axis.text.y = element_text(color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray97"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(color = "gray30")
  )

# Ensure time_label factor levels
df$time_label <- factor(df$time_label, levels = c("Morning", "Afternoon", "Evening", "Night"))

# Usage by time of day (all users)
usage_time_usertype <- ggplot(df, aes(x = time_label, fill = usertype)) +
  geom_bar(position = "stack", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Time of Day",
    subtitle = "Ride counts by user type and time category",
    x = "Time of Day",
    y = "Count",
    fill = NULL
  ) +
  base_theme

# Usage by time of day (Customers only)
usage_time_customers <- df %>%
  filter(usertype == "Customer") %>%
  ggplot(aes(x = time_label)) +
  geom_bar(fill = "#E69F00", alpha = 0.5, color = "gray30", size = 0.4, width = 0.7) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Time of Day",
    subtitle = "Ride counts for Customers (non-subscribers)",
    x = "Time of Day",
    y = "Count"
  ) +
  base_theme +
  theme(legend.position = "none")

# Extract hour from start_time (if not done yet)
if (!"hour" %in% colnames(df)) {
  df$hour <- hour(df$start_time)
}

# Ride Volume by Hour of Day (Customers)
hourly_summary_customer <- df %>%
  filter(usertype == "Customer") %>%
  group_by(hour) %>%
  summarise(count = n(), .groups = "drop")

ride_volume_hour_customers <- ggplot(hourly_summary_customer, aes(x = hour, y = count)) +
  geom_line(color = "#E69F00", size = 1, alpha = 0.5) +
  geom_point(color = "#E69F00", size = 1.5, alpha = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 23),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Ride Volume by Hour of Day (Customers)",
    subtitle = "Hourly ride count for Customers (aggregated across all dates)",
    x = "Hour of Day",
    y = "Ride Count"
  ) +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

# Ride Volume by Hour of Day (All users)
hourly_summary <- df %>%
  group_by(usertype, hour) %>%
  summarise(count = n(), .groups = "drop")

ride_volume_hour_all <- ggplot(hourly_summary, aes(x = hour, y = count)) +
  geom_line(aes(color = usertype), size = 0.8, alpha = 0.5) +
  geom_point(aes(color = usertype), size = 1, alpha = 0.8) +
  scale_color_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 23),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Ride Volume by Hour of Day",
    subtitle = "Hourly ride count by user type (aggregated across all dates)",
    x = "Hour of Day",
    y = "Ride Count",
    color = "User Type"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_line(color = "gray97"))

# Combine all four plots
(usage_time_usertype + usage_time_customers) / (ride_volume_hour_all + ride_volume_hour_customers) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")
```

***

### Who’s Actually Riding

Now, let’s talk about who's on these bikes. Young adults lead the way across both rider types, which makes sense in a city that moves fast. But there’s a gender gap worth noting—male riders significantly outnumber female riders. That imbalance points to a broader opportunity: how might Cyclistic create a riding experience that feels safer, more welcoming, and more inclusive?

```{r fig.width=14, fig.height=10, warning=FALSE, message=FALSE}
base_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20"),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, lineheight = 0.9, color = "gray20"),
    axis.text.y = element_text(color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray97"),
    panel.grid.major.x = element_blank()
  )

# Factor levels for age
df$age_label <- factor(df$age_label, levels = c(
  "Children/Teens", 
  "Young Adults", 
  "Middle-Aged Adults", 
  "Older Adults", 
  "Seniors"
))

# Custom x-axis labels
age_labels <- c(
  "Children/Teens\n(0–19)",
  "Young Adults\n(20–35)",
  "Middle-Aged Adults\n(36–50)",
  "Older Adults\n(51–64)",
  "Seniors\n(65+)"
)
names(age_labels) <- levels(df$age_label)

# Usage by Age Group (all users)
age_all <- ggplot(df, aes(x = age_label, fill = usertype)) +
  geom_bar(position = "stack", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(labels = age_labels) +
  labs(
    title = "Usage by Age Group",
    x = "Age Group", y = "Count", fill = NULL
  ) +
  base_theme +
  theme(legend.position = "top", legend.text = element_text(color = "gray30"))

# Usage by Age Group (Customers)
age_customers <- df %>%
  filter(usertype == "Customer") %>%
  ggplot(aes(x = age_label)) +
  geom_bar(fill = "#E69F00", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(labels = age_labels) +
  labs(
    title = "Usage by Age Group (Customers)",
    x = "Age Group", y = "Count"
  ) +
  base_theme +
  theme(legend.position = "none")

# Usage by Gender (all users)
gender_all <- ggplot(df, aes(x = gender, fill = usertype)) +
  geom_bar(position = "stack", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Gender",
    x = "Gender", y = "Count", fill = NULL
  ) +
  base_theme +
  theme(legend.position = "top", legend.text = element_text(color = "gray30"))

# Usage by Gender (Customers)
gender_customers <- df %>%
  filter(usertype == "Customer") %>%
  ggplot(aes(x = gender)) +
  geom_bar(fill = "#E69F00", alpha = 0.5, color = "gray30", size = 0.4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Usage by Gender (Customers)",
    x = "Gender", y = "Count"
  ) +
  base_theme +
  theme(legend.position = "none")

# Combine all 4 plots
(age_all + age_customers) / 
(gender_all + gender_customers) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "top")

```

***

### How Long the Rides Last

The length of each trip also reveals a lot. Members typically keep it short and focused, averaging around 5 minutes per ride. That signals purpose—get on, get there, get off. Casual riders, though, average about 10 minutes. They’re clearly not in a rush. These are the rides where people slow down and take in the sights.

```{r trip-duration-density-dashboard, fig.width=14, fig.height=5, warning=FALSE, message=FALSE}
# Filtered trip durations
df_filtered <- df %>%
  filter(trip_duration_min >= 1, trip_duration_min <= 180)

# Shared aesthetics
trip_duration_breaks <- seq(0, 180, by = 30)
trip_duration_labels <- scales::label_number()

# Density of Trip Duration by User Type
trip_density_all <- ggplot(df_filtered, aes(x = trip_duration_min, fill = usertype, color = usertype)) +
  geom_density(alpha = 0.5, size = 0.5, show.legend = TRUE) +
  scale_x_continuous(breaks = trip_duration_breaks, labels = trip_duration_labels) +
  scale_fill_manual(values = c("Customer" = "#E69F00", "Subscriber" = "#009E73")) +
  scale_color_manual(values = c("Customer" = "gray50", "Subscriber" = "gray50")) +
  labs(
    title = "Density of Trip Duration by User Type",
    x = "Trip Duration (minutes)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "gray20"),
    axis.text = element_text(size = 12, color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# Density of Trip Duration (Customers Only)
trip_density_customers <- df_filtered %>%
  filter(usertype == "Customer") %>%
  ggplot(aes(x = trip_duration_min)) +
  geom_density(
    fill = "#E69F00",
    color = "gray30",
    alpha = 0.5,
    size = 0.5
  ) +
  scale_x_continuous(breaks = trip_duration_breaks, labels = trip_duration_labels) +
  labs(
    title = "Density of Trip Duration (Customers Only)",
    x = "Trip Duration (minutes)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "gray20"),
    axis.text = element_text(size = 12, color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20"),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# Patch together
trip_density_all + trip_density_customers +
  plot_layout(ncol = 2, widths = c(1, 1)) &
  theme(plot.margin = margin(10, 15, 10, 10))
```

***

### Where They’re Headed

Even the routes people take reinforce that difference. Members mostly travel between business-heavy spots like Canal St & Adams St to Michigan Ave & Washington St. It's likely part of their usual routine. Meanwhile, casual riders gravitate toward scenic paths—think Lake Shore Dr & Monroe St to Streeter Dr & Grand Ave—close to tourist areas, waterfronts, and museums. The ride itself may be part of the experience.

```{r}
# Top From → To Stations (Customers)
top_pairs_customers <- df %>%
  filter(usertype == "Customer") %>%
  count(from_station_name, to_station_name, sort = TRUE) %>%
  slice_head(n = 3) %>%
  rename("From Station" = from_station_name,
         "To Station" = to_station_name,
         "Trips" = n)

kable(top_pairs_customers, align = 'c') %>%
  add_header_above(c("Top From → To Stations (Customers)" = 3)) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover"))

# Top From → To Stations (Subscribers)
top_pairs_subscribers <- df %>%
filter(usertype == "Subscriber") %>%
  count(from_station_name, to_station_name, sort = TRUE) %>%
  slice_head(n = 3) %>%
  rename("From Station" = from_station_name,
         "To Station" = to_station_name,
         "Trips" = n)

kable(top_pairs_subscribers, align = 'c') %>%
  add_header_above(c("Top From → To Stations (Subscribers)" = 3)) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover"))
```

***

### A Station to Watch

And speaking of scenic routes, one station stands out: Streeter Dr & Grand Ave. It’s the go-to spot for casual riders, whether they’re starting or wrapping up their trip. That makes it a key location for targeted marketing, service improvements, or even testing new ideas like pop-up bike events or rental promos.

```{r}
# Replace this with your actual data import if needed
# df <- read_csv("your_cleaned_data.csv")

# Filter for Customers only
df_customers <- df %>%
  filter(usertype == "Customer")

# Count number of rides that start from each station
from_counts <- df_customers %>%
  count(from_station_name, name = "from_count") %>%
  rename(station = from_station_name)

# Count number of rides that end at each station
to_counts <- df_customers %>%
  count(to_station_name, name = "to_count") %>%
  rename(station = to_station_name)

# Join and summarize both counts
combined_counts <- full_join(from_counts, to_counts, by = "station") %>%
  mutate(
    from_count = replace_na(from_count, 0),
    to_count = replace_na(to_count, 0),
    total_count = from_count + to_count
  ) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 5)

# Display as a formatted table
combined_counts %>%
  rename(
    `Station Name` = station,
    `Trips From` = from_count,
    `Trips To` = to_count,
    `Total Trips` = total_count
  ) %>%
  kable() %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  ) %>%
  add_header_above(c(" " = 1, "Top 5 Stations Used by Customers" = 3))
```

---

## The Bottom Line

Cyclistic isn’t just moving bikes — it’s moving people in two very different ways. Members ride with routine and purpose. Casual riders show up for the experience. That gap matters. It’s the difference between stable, long-term growth and short bursts of unpredictable traffic.

But here’s the good news: casual riders aren’t a mystery. The data shows us when they ride, where they go, and how long they stay in the saddle. And that gives Cyclistic a clear path forward. With smart, behavior-based strategies — like better weekend offers, scenic route promotions, or targeted campaigns at hotspots like Streeter Dr & Grand Ave — there’s a real opportunity to turn occasional riders into everyday ones.

At the end of the day, this isn’t just about boosting membership. It’s about understanding people, meeting them where they are, and giving them a reason to keep coming back.

---

## Recommendations

So how can we turn casual riders into lifelong cyclists? Here are three ways to do just that:

**1. Sell the Weekend Vibe**

Casual riders live for sunny afternoons and weekend getaways on two wheels. So meet them there. Create weekend-focused promos that speak to their pace — think “Sunday Cruise” passes, “Scenic Saturday” discounts, or 2-for-1 ride deals that make exploring more fun with a friend. Use social media to highlight beautiful routes and share real stories from other casual riders. You’re not just selling a bike — you’re selling a moment.

**2.Own the Hotspots**

That one station near the waterfront? It’s your marketing goldmine. It’s where casual riders show up the most — and where they’re most likely to be open to trying more. Use signage, QR codes, or geotargeted ads around that area to pitch low-commitment trials, monthly passes, or “first month on us” deals. If they’re already loving the ride, all they need is the right nudge.

**3. Make Membership Feel Like a Lifestyle**

Let’s face it — the word “membership” can sound like a commitment trap. But what if it felt like a backstage pass to more adventures? Use digital campaigns to reframe what being a member means: priority access on busy days, exclusive routes or guides, maybe even partnerships with coffee shops or museums. Highlight the flexibility and perks, not just the savings. A lifestyle sells better than a contract.

**The Big Idea?**

Casual riders don’t need a hard sell. They just need the right story. And digital media — from Instagram reels to targeted ads — is your chance to tell it in real-time, right where they’re already looking.

---

## Writer’s Note

Digging into this data was both a challenge and a joy. I’m genuinely proud of how it came together. I know I’ve still got a lot to learn, but this project reminded me why I love working with data in the first place. If anything here sparked your curiosity, taught you something new, or even nudged you to explore data analysis yourself — then that means a lot. Thanks for reading.

---
