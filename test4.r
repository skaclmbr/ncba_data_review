library("ggplot2")
library("ggiraph")

data(mtcars)
category_colors <- c(
  "Confirmed" = "#6a51a3",
  "Probable" = "#9e9ac8",
  "Possible" = "#cbc9e2"
)

df <- data.frame(
  "cyl" = c(4, 6, 8),
  "BREEDING_CATEGORY" = c("Confirmed", "Probable", "Possible")
)
df2 <- data.frame(
  "am" = c(1, 0),
  "atm" = c("Automatic", "Manual")
)

mc_df <- mtcars %>%
  merge(
    df,
    by = "cyl"
  ) %>%
  mutate(
    auto = ifelse(am == 1, "Automatic", "Manual")
  )

cyl_cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
am_cols <- c("1" = "yellow", "0" = "red")
p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point_interactive(
    aes(
      colour = factor(cyl),
      fill = factor(am),
      size = 2,
      stroke = 2
    ),
    shape = 21
  ) +
scale_color_manual(values = cyl_cols) +
scale_fill_manual(values = am_cols) +
theme_minimal()

p
