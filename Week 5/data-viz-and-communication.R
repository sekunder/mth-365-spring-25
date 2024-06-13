## ----echo=FALSE, message=FALSE, warning = FALSE----------------------------------------------
library(tidyverse)
library(knitr)

knitr::knit_hooks$set(purl = knitr::hook_purl)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = xfun::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})


## ----echo=FALSE, fig.align='center', out.width="70%"-----------------------------------------
knitr::include_graphics("../Week 5/images/unclear-graph.png")

## ----echo=FALSE, fig.align='center'----------------------------------------------------------
knitr::include_graphics("../Week 5/images/better-graph.png")

## ----fig.align="center", out.height="60%", out.width="60%", echo = FALSE---------------------
data <- data.frame(expand.grid(x = 1:6, y = 1:6), color = factor(sample(c(1, 2), 36, replace = TRUE)))
data$x <- data$x + rnorm(36, 0, .25)
data$y <- data$y + rnorm(36, 0, .25)
data$shape <- factor(c(rep(2, 15), 1, rep(2,20)))

ggplot(data, aes(x, y)) + geom_point(aes(shape = shape), size = 5, colour = "#1B9E77") + theme_void() + theme(legend.position = "none")

## ----fig.align="center", out.height="60%", out.width="60%", echo = FALSE---------------------
data$shape <- factor(c(rep(2, 25), 1, rep(2, 10)))

ggplot(data, aes(x, y)) + geom_point(aes(colour = shape), size = 5, shape = I(19)) + theme_void() + theme(legend.position = "none") + scale_colour_brewer(palette="Dark2")

## ----diamonds1, echo = FALSE, fig.align='center'---------------------------------------------

diamonds_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(Percent = n()/nrow(.) * 100)

ggplot(diamonds, aes(x = "", fill = cut)) + 
  geom_bar() +
  coord_polar(theta = "y") + ggtitle("Pie Chart")




## ----diamonds2, echo = FALSE, fig.align='center'---------------------------------------------
diamonds %>% ggplot() + geom_bar(aes(x=cut,fill = cut)) + ggtitle("Bar Chart")


## ----echo=FALSE, fig.align='center', out.width="60%"-----------------------------------------
knitr::include_graphics("../Week 5/images/color-spectrum.png")

## ----echo=FALSE, fig.width=3, fig.height=1, out.height="35%", out.width="35%", warning=FALSE----
data <- data.frame(x = 1:7, 
                   blues = brewer.pal(7, "Blues"), 
                   set1 = brewer.pal(7, "Set1"), 
                   diverge = brewer.pal(7,"RdBu"))

qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = set1) + 
    scale_fill_identity() + 
    ylab("") + 
    xlab("") + 
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          rect = element_blank()) + 
    coord_fixed(ratio = 1) + 
    theme_void()

## ----echo=FALSE, fig.width=3, fig.height=1, out.height="35%",  out.width="35%", warning=FALSE----
qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = blues) + 
    scale_fill_identity() + 
    ylab("") + 
    xlab("") + 
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          rect = element_blank()) + 
    coord_fixed(ratio = 1) + 
    theme_void()

## ----echo=FALSE, fig.width=3, fig.height=1, out.height="35%", out.width="35%", warning=FALSE----
qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = diverge) + 
    scale_fill_identity() + 
    ylab("") + 
    xlab("") + 
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          rect = element_blank()) + 
    coord_fixed(ratio = 1) + 
    theme_void()

## --------------------------------------------------------------------------------------------
data(HairEyeColor)
HairEyeColor[,,1] #Male

## --------------------------------------------------------------------------------------------
data(HairEyeColor)
HairEyeColor[,,2] #Female

## --------------------------------------------------------------------------------------------
newdata_male = as.data.frame(HairEyeColor[,,1]) 
newdata_female = as.data.frame(HairEyeColor[,,2]) 
newdata = rbind(newdata_male, newdata_female)
newdata = newdata %>% 
  mutate(Gender = rep(c("Male", "Female"), each  = 16))

## --------------------------------------------------------------------------------------------
head(newdata, n = 2)

## --------------------------------------------------------------------------------------------
hairData = newdata %>% 
  group_by(Hair) %>%
  summarise(Freq = sum(Freq))
hairData

## --------------------------------------------------------------------------------------------
hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) + 
  geom_col(aes(color = Hair))

## --------------------------------------------------------------------------------------------
hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) +  
  geom_col(aes(fill = Hair))

## --------------------------------------------------------------------------------------------
hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) + 
  geom_col(aes(color = Hair)) #<<

## --------------------------------------------------------------------------------------------
hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) +  
  geom_col(aes(fill = Hair)) #<<

## ----fig.align='center', fig.height=5, fig.width=8-------------------------------------------

ggplot(hairData, aes(x = Hair, y = Freq)) + 
  geom_col(aes(fill = Hair)) +
  scale_fill_manual(breaks = c("Black", "Brown", "Red", "Blond"), #<<
                    values=c("black", "brown", "red", "yellow")) #<<

## ----fig.height=4, fig.width=8, fig.align='center'-------------------------------------------
cbPalette <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442")

ggplot(hairData, aes(x = Hair, y = Freq)) + 
  geom_col(aes(fill = Hair)) +
  scale_fill_manual(values=cbPalette)

## --------------------------------------------------------------------------------------------
data(iris)

## ----results='asis', echo=FALSE, fig.align='center'------------------------------------------

i1 <- img_modal(src = "./images/bad-pie.PNG", alt = "")

c(str_split(i1, "\\n", simplify = T)[1:2],
  str_split(i1, "\\n", simplify = T)[3:9]
  ) %>% paste(collapse = "\n") %>% cat()


## ----results='asis', echo=FALSE, fig.align='center'------------------------------------------

i2 <- img_modal(src = "./images/bad-line.PNG", alt = "")

c(str_split(i2, "\\n", simplify = T)[1:2],
  str_split(i2, "\\n", simplify = T)[3:9]
  ) %>% paste(collapse = "\n") %>% cat()


## ----fig.height=4, fig.width=8, fig.align='center'-------------------------------------------
x = -5:10
y = x^2
xy = data.frame(x,y)
ggplot(xy, aes(x = x, y=y)) +
  geom_point() +
  xlab("X") + ylab(expression(X^2))#<<

## ----fig.height=3.5, fig.width=8, fig.align='center'-----------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, group=Species)) + 
  geom_point(aes(color = Species, shape = Species)) + 
  scale_color_manual(values=cbPalette) +
  xlab("Sepal Length") + 
  ylab("Patal Length") +
  ggtitle("Relation between Sepal Length and Petal Length") #<<

## ----echo = FALSE----------------------------------------------------------------------------
knitr::include_graphics("../Week 5/images/fig-caption.png")

## ----fig1, fig.cap="Scatter plot between Iris sepal length and Iris petal length. Different species are indicated by different color and shapes. In general, the length of sepal and petal for Iris have a strong positive linear relationship.", echo=FALSE, fig.height=3.5, fig.width=8, fig.align='center'----
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, group=Species)) + 
  geom_point(aes(color = Species, shape = Species)) + 
  scale_color_manual(values=cbPalette) +
  xlab("Sepal Length") + 
  ylab("Patal Length") +
  ggtitle("Relation between Sepal Length and Petal Length")

## ----fig.cap="Scatter plot between Iris sepal length and Iris petal length. Different species are indicated by different color and shapes. In general, the length of sepal and petal for Iris have a strong positive linear relationship.", echo=FALSE, fig.height=3.5, fig.width=8, fig.align='center'----
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, group=Species)) + 
  geom_point(aes(color = Species, shape = Species)) + 
  scale_color_manual(values=cbPalette) +
  xlab("Sepal Length") + 
  ylab("Patal Length") +
  ggtitle("Relation between Sepal Length and Petal Length")

