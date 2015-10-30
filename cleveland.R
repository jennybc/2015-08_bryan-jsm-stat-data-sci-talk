library(ggplot2)
library(dplyr)

cleveland <- data_frame(foci = c("Multidisciplinary investigations",
                                 "Models and Methods for Data",
                                 "Computing with Data",
                                 "Pedagogy",
                                 "Tool Eval",
                                 "Theory"),
                        allocation = c(0.25, 0.2, 0.15, 0.15, 0.05, 0.2))
#all.equal(sum(cleveland$allocation), 1)

p <- cleveland %>%
  mutate(foci = reorder(foci, allocation)) %>%
  ggplot(aes(foci, allocation)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = foci, y = 0), colour = "white",
            hjust = "bottom", nudge_y = 0.008) +
  scale_x_discrete(breaks=NULL) +
  coord_flip() +
  labs(x = "", y = "Suggested allocation of resources")
p
ggsave("cleveland-allocation.png", p, width = 6, height = 4)
