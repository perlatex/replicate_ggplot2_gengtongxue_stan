library(tidyverse)
library(readxl)
library(latex2exp)


#########################################################################
d <- readxl::read_excel("./data/fig1c.xlsx") %>% 
  mutate(
    Val  = as.numeric(Val),
    cpm  = as.numeric(cpm),
    low  = as.numeric(low),
    high = as.numeric(high),
    ymin = pmin(cpm, low, high, na.rm = TRUE),  # 原始数据有误，所以选择最小的那个当 ymin
    ymax = pmax(cpm, low, high, na.rm = TRUE)   # 同理
  )
#########################################################################





#########################################################################
# 用 gam 去测试
ggplot(data = d, aes(x = Val, y = cpm)) +
  geom_point(size = 4) +
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    linewidth = 1.3,
    width = 0.2
  ) +
  scale_y_continuous(
    limits = c(0, 30000)
  ) +
  scale_x_log10(
    limits = c(0.001, 100000),
    breaks = c(0.001, 0.1, 10, 1000, 100000),
    labels = c("0.001", "0.1", "10", "1,000", "100,000")
  ) +
  annotate(
    geom = "text", x = 0.01, y = 30000, 
    label = TeX("$K_d = 1.95 \\mu M$")
  ) +
  theme_classic()
#########################################################################





#########################################################################
# 四参数剂量-反应曲线
# 数学表达式
# y = bottom + (top - bottom) / (1 + (x / Kd)^hill)

x_grid <- 10^seq(-3, 5, length.out = 500)

ggplot(data = d, aes(x = Val, y = cpm)) +
  geom_point(size = 4) +
  geom_smooth(
    method = "nls",
    formula = y ~ bottom + (top - bottom) / (1 + (x / Kd)^hill),
    method.args = list(
      start = list(
        bottom = 100,
        top    = 21000,
        Kd     = 1.95,
        hill   = 1
      )
    ),
    se = FALSE,
    linewidth = 0.9,
    color = "black",
    xseq = x_grid
  ) +
  scale_x_log10()   # 先把 x 变成 log10(x)，再拟合模型



ggplot(data = d, aes(x = Val, y = cpm)) +
  geom_point(size = 4) +
  geom_smooth(
    method = "nls",
    formula = y ~ bottom + (top - bottom) / (1 + (x / Kd)^hill),
    method.args = list(
      start = list(
        bottom = 100,
        top    = 21000,
        Kd     = 1.95,
        hill   = 1
      )
    ),
    se = FALSE,
    linewidth = 0.9,
    color = "black",
    xseq = x_grid
  ) +
  coord_transform(x = "log10") # 先在原始 x 上拟合模型，再把坐标轴显示成 log10 尺度。
#########################################################################



#########################################################################
# 如果 nls 偶尔不收敛，可以换成更稳的 nlsLM()
library(minpack.lm)

ggplot(data = d, aes(x = Val, y = cpm)) +
  geom_point(size = 4) +
  geom_smooth(
    method = minpack.lm::nlsLM,
    formula = y ~ bottom + (top - bottom) / (1 + (x / Kd)^hill),
    method.args = list(
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
    ),
    se = FALSE,
    xseq = x_grid
  ) +
  coord_transform(x = "log10")
#########################################################################






#########################################################################
# 完整版
# 四参数剂量-反应曲线
# 数学表达式 y = bottom + (top - bottom) / (1 + (x / Kd)^hill)
#########################################################################
library(tidyverse)

d <- readxl::read_excel("./data/fig1c.xlsx") %>% 
  mutate(
    Val  = as.numeric(Val),
    cpm  = as.numeric(cpm),
    low  = as.numeric(low),
    high = as.numeric(high),
    ymin = pmin(cpm, low, high, na.rm = TRUE),  # 原始数据有误，所以选择最小的那个当 ymin
    ymax = pmax(cpm, low, high, na.rm = TRUE)   # 同理
  )


x_grid <- 10^seq(-3, 5, length.out = 500)


d %>%  
  ggplot(aes(x = Val, y = cpm)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(size = 4.2, color = "black") +
  geom_smooth(
    method = "nls",
    formula = y ~ bottom + (top - bottom) / (1 + (x / Kd)^hill),
    method.args = list(
      start = list(
        bottom = 100,
        top    = 21000,
        Kd     = 1.95,
        hill   = 1
      )
    ),
    se        = FALSE,
    linewidth = 0.9,
    color     = "red",
    xseq      = x_grid
  ) +
  scale_x_continuous(
    limits = c(0.001, 100000),
    breaks = c(0.001, 0.1, 10, 1000, 100000),
    labels = c("0.001", "0.1", "10", "1,000", "100,000")
  ) +
  scale_y_continuous(
    limits = c(-2000, 30000),
    breaks = c(0, 10000, 20000, 30000),
    labels = scales::comma
  ) +
  coord_transform(x = "log10") + 
  labs(
    x = "Valine (μM)",
    y = expression("["^3*"H]Val bound (c.p.m.)")
  ) +
  annotate(
    "text",
    x = 0.02,
    y = 28500,
    label = "italic(K)[d] == 1.95 ~ mu*M",
    parse = TRUE,
    size = 6
  ) +
  theme_classic(base_size = 18)

#######################################################################################
# 说明
# scale_x_log10()               # 先把 x 变成 log10(x)，再拟合模型
# coord_transform(x = "log10")  # 先在原始 x 上拟合模型，再把坐标轴显示成 log10 尺度
# 如果不在ggplot2内部建模（比如nls和Stan是外部建模），即不用geom_smooth()，两者效果一样
#######################################################################################



