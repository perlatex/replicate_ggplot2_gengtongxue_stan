library(tidyverse)
library(minpack.lm)

d <- readxl::read_excel("./data/fig1c.xlsx") %>% 
  mutate(
    Val  = as.numeric(Val),
    cpm  = as.numeric(cpm),
    low  = as.numeric(low),
    high = as.numeric(high),
    
    ymin = pmin(cpm, low, high, na.rm = TRUE),
    ymax = pmax(cpm, low, high, na.rm = TRUE)
  )



# 四参数剂量-反应曲线
# y = bottom + (top - bottom) / (1 + (x / Kd)^hill)

fit <- nlsLM(
  cpm ~ bottom + (top - bottom) / (1 + (Val / Kd)^hill),
  data = d,
  start = list(
    bottom = 100,
    top    = 21000,
    Kd     = 1.95,
    hill   = 1
  ),
  lower = c(
    bottom = -1000,
    top    = 10000,
    Kd     = 0.0001,
    hill   = 0.1
  )
)

coef(fit)


# 生成平滑曲线
newd <- tibble(
  Val = 10^seq(-3, 5, length.out = 500)
)

newd$cpm_hat <- predict(fit, newdata = newd)



# 作图
d |> 
  ggplot(aes(x = Val, y = cpm)) +
  geom_line(
    data = newd,
    aes(x = Val, y = cpm_hat),
    linewidth = 0.9,
    color = "red"
  ) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.12,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    size = 4.2,
    color = "black"
  ) +
  scale_x_log10(
    limits = c(1e-3, 1e5),
    breaks = c(1e-3, 1e-1, 1e1, 1e3, 1e5),
    labels = c("0.001", "0.1", "10", "1,000", "100,000")
  ) +
  scale_y_continuous(
    limits = c(-2000, 30000),
    breaks = c(0, 10000, 20000, 30000),
    labels = scales::comma
  ) +
  annotate(
    "text",
    x = 0.03,
    y = 28500,
    label = expression(K[d] == 1.95~mu*M),
    parse = TRUE,
    size = 8
  ) +
  labs(
    x = "Valine (μM)",
    y = expression("["^3*"H]Val bound (c.p.m.)")
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.background   = element_rect(fill = "grey94", color = NA),
    panel.background  = element_rect(fill = "grey94", color = NA),
    axis.line         = element_line(linewidth = 1.1, color = "black"),
    axis.ticks        = element_line(linewidth = 1.1, color = "black"),
    axis.ticks.length = unit(0.28, "cm"),
    axis.text         = element_text(color = "black", size = 18),
    axis.title        = element_text(color = "black", size = 22),
    axis.title.x      = element_text(margin = margin(t = 18)),
    axis.title.y      = element_text(margin = margin(r = 18)),
    plot.margin       = margin(15, 30, 20, 20)
  )




