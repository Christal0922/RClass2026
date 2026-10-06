# 加载包
library(tidyverse)
library(HistData)

# 读取南丁格尔原始数据
nightingale <- HistData::Nightingale

library(tidyverse)


#2. 计算月死亡率（每千人），便于跨月比较，同时分别输出卫生改革前数据nightingale_before，卫生改革后数据nightingale_after

nightingale_beore <- nightingale_long %>%
  filter(阶段 == "第1阶段") %>% # 筛选生改革前的数据
  mutate(
    月死亡 = 死亡人数 / 军队人数 * 1000
  ) #计算每月死亡率（每千人年化）
nightingale_after <
  nightingale_long %>%
    ilter(阶段 == "第2阶段") %>% # 筛选出卫生改革后据
    mutate(
      月死亡率 = 亡人数 / 军队人数 * 1000
    ) #计算每月死亡率（每千人年化）

#3. 卫生改革前死亡人数汇总

n_summary_before <- nightingale_before %>%
  group_by(死亡原因) %>%
  summarise(
    总死亡人数 = sum(死亡人数),
    平均年化死亡率 = mean(年化死亡率)
  )
