install.packages("ggplot2")

library(ggplot2)

head(faithful)

str(faithful)

ggplot(data = faithful,
       mapping = aes(x = eruptions,
                     y = waiting)) +
  geom_point()

#colour

ggplot(faithful) +
  geom_point(aes(x = eruptions,
                 y = waiting,
                 colour = eruptions < 3))

ggplot(faithful) +
  geom_point(aes(x = eruptions,
                 y = waiting),
             colour = 'steelblue')

#layering
ggplot(faithful) +
  geom_histogram(aes(x = eruptions))

ggplot(faithful,
       aes(x = eruptions, y = waiting)) +
  geom_density_2d() +
  geom_point()