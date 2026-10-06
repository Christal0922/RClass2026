# ---- PPT7 图形资源生成（与 PPT6 / 指导书配色完全一致）----
suppressPackageStartupMessages({
  library(tidyverse)
  library(HistData)
})

outdir <- "D:/Project/Rproject/RClass2026/lessons/lesson02/ppt7_assets"

nightingale <- HistData::Nightingale

nightingale_long <- nightingale %>%
  mutate(阶段 = factor(rep(c("改革前", "改革后"), each = 12),
                       levels = c("改革前", "改革后"))) %>%
  rename(Disease.count = Disease, Wounds.count = Wounds, Other.count = Other) %>%
  pivot_longer(
    cols = c(Disease.count, Wounds.count, Other.count,
             Disease.rate, Wounds.rate, Other.rate),
    names_to = c("死亡原因", ".value"), names_sep = "\\."
  ) %>%
  rename(日期 = Date, 年份 = Year, 军队人数 = Army,
         死亡人数 = count, 死亡率 = rate) %>%
  mutate(
    月份 = factor(as.integer(format(日期, "%m")),
                  levels = c(4:12, 1:3),
                  labels = paste0(c(4:12, 1:3), "月")),
    死亡原因 = factor(
      recode(死亡原因, "Disease" = "可预防疾病",
             "Wounds" = "战伤", "Other" = "其他原因"),
      levels = c("可预防疾病", "战伤", "其他原因"))
  ) %>%
  select(日期, 月份, 年份, 军队人数, 阶段, 死亡原因, 死亡人数, 死亡率)

before <- nightingale_long %>% filter(阶段 == "改革前")

cause_colors <- c("可预防疾病" = "#BCC7C9", "战伤" = "#E6BDB8", "其他原因" = "#777777")
my_colors <- c("可预防疾病" = "#3B7EA1", "战伤" = "#C4622D", "其他原因" = "#6E6E6E")

cause_summary <- before %>%
  group_by(死亡原因) %>%
  summarise(累计死亡人数 = sum(死亡人数),
            平均死亡率 = mean(死亡率), .groups = "drop") %>%
  mutate(占比 = 100 * 累计死亡人数 / sum(累计死亡人数))

stage_summary <- nightingale_long %>%
  group_by(阶段, 死亡原因) %>%
  summarise(平均死亡率 = mean(死亡率), .groups = "drop") %>%
  pivot_wider(names_from = 阶段, values_from = 平均死亡率) %>%
  mutate(变化幅度 = (改革后 - 改革前) / 改革前 * 100)

CARD_BG <- "#FFFFFF"
CARD_TEXT <- "#1F2A33"
CARD_MUTE <- "#5B6B78"
CARD_GRID <- "#DCE3E8"
CARD_STRIP <- "#EDF2F5"
fam <- "Microsoft YaHei"

theme_card <- theme_minimal(base_size = 30, base_family = fam) +
  theme(
    text = element_text(color = CARD_TEXT, size = 30, family = fam),
    axis.text = element_text(color = CARD_MUTE, size = 27, family = fam),
    axis.title = element_text(color = CARD_TEXT, size = 29, face = "bold", family = fam),
    plot.title = element_text(color = CARD_TEXT, face = "bold", size = 32,
                              family = fam, lineheight = 1.05, margin = margin(b = 6)),
    plot.subtitle = element_text(color = CARD_MUTE, size = 26,
                                 family = fam, lineheight = 1.05, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin = margin(t = 12, r = 20, b = 10, l = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(color = CARD_TEXT, size = 27, family = fam),
    legend.title = element_text(color = CARD_TEXT, size = 27, face = "bold", family = fam),
    legend.margin = margin(0, 0, 0, 0),
    legend.background = element_rect(fill = CARD_BG, color = NA),
    legend.key = element_rect(fill = CARD_BG, color = NA),
    panel.grid.major = element_line(color = CARD_GRID, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = CARD_BG, color = NA),
    plot.background = element_rect(fill = CARD_BG, color = NA),
    strip.background = element_rect(fill = CARD_STRIP, color = NA),
    strip.text = element_text(color = CARD_TEXT, face = "bold", size = 28, family = fam)
  )

save_fig <- function(p, name, w, h) {
  ggsave(file.path(outdir, name), plot = p, width = w, height = h, dpi = 150,
         bg = CARD_BG, device = ragg::agg_png)
  cat("saved", name, "\n")
}

# ---- p1 累计死亡人数对比 ----
p1 <- ggplot(cause_summary, aes(x = 死亡原因, y = 累计死亡人数, fill = 死亡原因)) +
  geom_col(width = 0.62) +
  scale_fill_manual(values = cause_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "累计死亡人数（人）", title = "改革前 12 个月三类死因的累计死亡人数") +
  guides(fill = "none") +
  theme_card
save_fig(p1, "fig_p1.png", 11.6, 5.0)

p1_labeled <- ggplot(cause_summary, aes(x = 死亡原因, y = 累计死亡人数, fill = 死亡原因)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = paste0(累计死亡人数, "\n(", sprintf("%.1f", 占比), "%)")),
            vjust = -0.28, color = CARD_TEXT, size = 10, family = fam, lineheight = 0.95) +
  scale_fill_manual(values = cause_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(x = NULL, y = "累计死亡人数（人）", title = "加上数值标签，结论可以直接被核对") +
  guides(fill = "none") +
  theme_card
save_fig(p1_labeled, "fig_p1_labeled.png", 11.6, 5.0)

# ---- p2 月度堆叠柱形图 ----
p2 <- ggplot(before, aes(x = 月份, y = 死亡率, fill = 死亡原因)) +
  geom_col(width = 0.84) +
  scale_fill_manual(values = cause_colors) +
  labs(x = NULL, y = "年化死亡率（每千人年）",
       title = "改革前各月三类死因的死亡风险", fill = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_card +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 26, family = fam))
save_fig(p2, "fig_p2.png", 11.6, 5.0)

p2_facet <- ggplot(before, aes(x = 月份, y = 死亡率, fill = 死亡原因)) +
  geom_col(width = 0.84) +
  facet_wrap(~死亡原因, ncol = 3, scales = "fixed") +
  scale_fill_manual(values = cause_colors) +
  labs(x = NULL, y = "年化死亡率（每千人年）",
       title = "三类死因分开看：峰值不在同一个月", fill = NULL) +
  guides(fill = "none") +
  theme_card +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 22, family = fam))
save_fig(p2_facet, "fig_p2_facet.png", 11.6, 5.0)

p2_custom <- p2 + scale_fill_manual(values = my_colors)
save_fig(p2_custom, "fig_p2_custom.png", 11.6, 5.0)

# ---- p3 分面对比 ----
p3 <- ggplot(nightingale_long, aes(x = 月份, y = 死亡率, fill = 死亡原因)) +
  geom_col(width = 0.84) +
  facet_wrap(~阶段, ncol = 2, scales = "fixed") +
  scale_fill_manual(values = cause_colors) +
  labs(x = NULL, y = "年化死亡率（每千人年）",
       title = "改革前后各月死亡风险的对比", fill = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_card +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 23, family = fam))
save_fig(p3, "fig_p3.png", 11.6, 5.0)

# ---- 阶段平均死亡率对比 ----
stage_long <- stage_summary %>%
  select(死亡原因, 改革前, 改革后) %>%
  pivot_longer(c(改革前, 改革后), names_to = "阶段", values_to = "平均死亡率") %>%
  mutate(阶段 = factor(阶段, levels = c("改革前", "改革后")))

p_stage <- ggplot(stage_long, aes(x = 死亡原因, y = 平均死亡率, fill = 阶段)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", 平均死亡率)),
            position = position_dodge(width = 0.7), vjust = -0.45,
            color = CARD_TEXT, size = 8.5, family = fam) +
  scale_fill_manual(values = c("改革前" = "#8E9AA3", "改革后" = "#6FA9C9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "平均年化死亡率（每千人年）",
       title = "两个阶段的平均死亡风险：可预防疾病降幅最大", fill = NULL) +
  theme_card
save_fig(p_stage, "fig_stage.png", 11.6, 5.0)

# ---- p4_base 柱形表达 ----
p4_base <- ggplot(nightingale_long, aes(x = 月份, y = 死亡率, fill = 死亡原因)) +
  geom_col(width = 1, colour = CARD_BG, linewidth = 0.3) +
  facet_wrap(~阶段, ncol = 2, scales = "fixed") +
  scale_fill_manual(values = cause_colors) +
  labs(x = NULL, y = "年化死亡率（每千人年）",
       title = "柱形表达：高度直接对应数值", fill = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_card +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 22, family = fam))
save_fig(p4_base, "fig_p4_base.png", 11.6, 5.0)

# ---- p4_rose 玫瑰图（经典复刻）----
p4_rose <- p4_base +
  scale_y_sqrt(expand = expansion(mult = c(0, 0.03))) +
  coord_polar() +
  labs(title = "玫瑰图表达：扇形面积对应数值", y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 24, family = fam),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )
save_fig(p4_rose, "fig_rose.png", 11.6, 5.0)

cat("ALL DONE\n")
print(cause_summary)
print(stage_summary)
