library(readr)
library(dplyr)
library(ggplot2)

cl_2014 <- "~/teaching/STAT545A/STAT545-UBC.github.io/course-admin/history/STAT545-registered-students-all-time-anonymized.csv"
cl_2015 <- "~/teaching/STAT545A/STAT545_InstructorsOnly/course-lists/classlist-merged.tsv"

d_2014 <- read_csv(cl_2014)
d_2015 <- read_tsv(cl_2015)
d_2015 <- d_2015 %>%
  select(Session, Gender, Program, Spec1_Subject) %>%
  rename(year = Session, Degree = Program, Program = Spec1_Subject)

df <- bind_rows(d_2014, d_2015) %>%
  mutate(STAT = factor(ifelse(Program %in% "Statistics", "Statistics", "Other"),
                       levels = c("Statistics", "Other")))

nrow(df) #250
n_distinct(df$Program) #54

sp_dat <- df %>%
  group_by(Program) %>%
  tally() %>%
  mutate(Program = reorder(factor(Program), n)) %>%
  arrange(desc(Program))
sp_dat_filt <- sp_dat %>%
  filter(n > 1, Program != "")
nrow(sp_dat_filt) #25

p <- ggplot(sp_dat_filt, aes(x = Program, y = n)) +
  geom_bar(stat = "identity") + coord_flip() +
  ylim(c(0, max(sp_dat_filt$n) + 5))
p + geom_text(aes(label = n), hjust = -0.2) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave("reg-bars.png", p, width = 6, height = 4)

df_by_year_STAT <- df %>%
  group_by(year, STAT) %>%
  tally()
df_by_year <- df_by_year_STAT %>%
  group_by(year) %>%
  summarize(n = sum(n))
j_cols <- c(Statistics = "grey33", Other = "grey67")
p <- ggplot(df_by_year_STAT, aes(x = as.factor(year), y = n, fill = STAT)) +
  geom_bar(stat = "identity", width = 0.8) +
  ylim(c(0, max(df_by_year$n) + 3)) +
  scale_fill_manual(values = j_cols) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  labs(x = "year", y = "") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank())
p <- p + with(year_dat, annotate("text", x = as.factor(year), y = n, label = n,
                                 vjust = -1))
ggsave("reg-bars-stat-vs-other.png", p, width = 6, height = 4)

stat_dat <- df %>%
  group_by(STAT) %>%
  tally()
pie_labels <-
  data_frame(freq = stat_dat$n,
             rel_freq = freq / sum(freq),
             pos = (cumsum(c(0, freq)) + c(freq, 0)/2)[1:2],
             label = paste(stat_dat$STAT, round(rel_freq, 2), sep = "\n"))
p <- ggplot(stat_dat, aes(x = factor(1), y = n, fill = STAT)) +
  geom_bar(width = 1, stat = "identity") +
  annotate("text", x = 1, y = pie_labels$pos,
           label = pie_labels$label) +
  coord_polar(theta = "y") +
  theme_bw() + guides(fill = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
