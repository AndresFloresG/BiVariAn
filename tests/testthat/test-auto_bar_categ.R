# Sample data for tests
df <- data.frame(
  group = rep(c("A", "B"), each = 5),
  category1 = rep(c("X", "Y"), times = 5),
  category2 = rep(c("M", "N"), times = 5)
)
df$group <- as.factor(df$group)
df$category1 <- as.factor(df$category1)
df$category2 <- as.factor(df$category2)

test_that("auto_bar_categ errors for non-logical showpercent", {
  expect_error(
    auto_bar_categ(df, groupvar = "group", showpercent = "yes"),
    "showpercent must be a logical argument"
    )
  expect_error(
    auto_bar_categ(df, groupvar = "group", showpercent = NA),
    "showpercent must be a logical argument"
  )
})

test_that("auto_bar_categ English labels branch", {
  plots_en <- auto_bar_categ(
    data = df,
    groupvar = "group",
    lang_labs = "EN",
    theme_func = theme_minimal,
    showpercent = FALSE
  )
  p1 <- plots_en$category1
  expect_equal(p1$labels$title, "Distribution of category1")
  expect_equal(p1$labels$y, "Frequency")
})

test_that("auto_bar_categ without grouping removes legend and uses default Spanish labels", {
  plots_ng <- auto_bar_categ(
    data = df,
    groupvar = NULL,
    lang_labs = NULL,
    theme_func = theme_minimal,
    showpercent = FALSE
  )
  p2 <- plots_ng$category2
  # Legend hidden
  expect_equal(p2$theme$legend.position, "none")
  # Title in Spanish
  expect_equal(p2$labels$title, "Distribución de category2")
})

test_that("auto_bar_categ omits text layer when showpercent = FALSE", {
  plots <- auto_bar_categ(
    data = df,
    groupvar = "group",
    theme_func = theme_minimal,
    showpercent = FALSE
  )
  p <- plots$category1
  # Only the geom_bar layer should be present
  expect_length(p$layers, 1)
})

test_that("auto_bar_categ uses default bar_args parameters", {
  plots <- auto_bar_categ(
    data = df,
    groupvar = "group",
    theme_func = theme_minimal,
    showpercent = FALSE
  )
  p <- plots$category1
  layer <- p$layers[[1]]
  expect_true(inherits(layer$position, "PositionDodge"))
  aes_params <- layer$aes_params
  expect_identical(aes_params$colour, "black")
  expect_identical(aes_params$linewidth, 0.9)
  expect_identical(aes_params$alpha, 0.5)

})

test_that("auto_bar_categ merges custom bar_args with defaults", {
  custom_args <- list(colour = "red", linewidth = 2)
  plots <- auto_bar_categ(
    data = df,
    groupvar = "group",
    bar_args = custom_args,
    theme_func = theme_minimal,
    showpercent = FALSE
  )
  p <- plots$category1
  layer <- p$layers[[1]]
  aes_params <- layer$aes_params

  # Custom args override defaults
  expect_identical(aes_params$colour, "red")
  expect_identical(aes_params$linewidth, 2)

  # Unspecified defaults remain
  expect_true(inherits(layer$position, "PositionDodge"))
  expect_identical(aes_params$alpha, 0.5)

})
