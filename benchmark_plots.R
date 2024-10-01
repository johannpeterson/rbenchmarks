require(ggplot2)
require(ggpubr)

ggplot(timings, aes(x=size, y=time_total, colour=n_cores)) + geom_point()

ggplot(timings, aes(x=elements/(time_total*n_cores), y=n_cores, colour=drop_fraction)) + geom_point()

ggplot(timings, aes(x=elements, y=time_total*n_cores, colour=arch)) + geom_point()

ggplot(timings, aes(x=n_cores, y=elements/time_total, colour=arch)) + geom_point()

ggplot(timings, aes(x=n_cores, y=elements/time_total, colour=NA_count/elements)) + geom_point()

ggplot(timings[timings$n_cores==8,], aes(x=arch, y=elements/time_total)) + 
  geom_boxplot() + ggtitle("8 Cores")
ggplot(timings[timings$n_cores==4,], aes(x=arch, y=elements/time_total)) + 
  geom_boxplot() + ggtitle("4 Cores")
ggplot(timings[timings$n_cores==1,], aes(x=arch, y=elements/time_total)) + 
  geom_boxplot() + ggtitle("1 Core")

ggboxplot(timings[timings$n_cores==8,], 
          x="arch", 
          y="elements/time_total", 
          color="arch", 
          add = "jitter") + 
  stat_compare_means(method = "t.test", label.x = 1.5) +
  ggtitle("8 Cores")

ggpaired(timings[timings$n_cores==8,], 
         x="arch", 
         y="elements/time_total", 
         color="arch", 
         line.color = "gray",
         line.size = 0.4) + 
  stat_compare_means(paired = TRUE, label.x = 1.5) +
  ggtitle("8 Cores") +
  xlab("CPU architecture") +
  ylab("elements/sec")
