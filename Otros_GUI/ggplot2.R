### show info about the data
head(diamonds)
head(mtcars)
### comparison qplot vs ggplot
# qplot histogram
qplot(clarity, data=diamonds, fill=cut, geom="bar")
# ggplot histogram -> same output
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
### how to use qplot
# scatterplot
qplot(wt, mpg, data=mtcars)
# transform input data with functions
qplot(log(wt), mpg - 10, data=mtcars)
# add aesthetic mapping (hint: how does mapping work)
qplot(wt, mpg, data=mtcars, color=qsec)
# change size of points (hint: color/colour, hint: set aesthetic/mapping)
qplot(wt, mpg, data=mtcars, color=qsec, size=3)
qplot(wt, mpg, data=mtcars, colour=qsec, size=I(3))
# use alpha blending
qplot(wt, mpg, data=mtcars, alpha=qsec)

# continuous scale vs. discrete scale
head(mtcars)
qplot(wt, mpg, data=mtcars, colour=cyl)
levels(mtcars$cyl)
qplot(wt, mpg, data=mtcars, colour=factor(cyl))
# use different aesthetic mappings
qplot(wt, mpg, data=mtcars, shape=factor(cyl))
qplot(wt, mpg, data=mtcars, size=qsec)
# combine mappings (hint: hollow points, geom-concept, legend combination)
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=I(1))
qplot(wt, mpg, data=mtcars, size=qsec, shape=factor(cyl), geom="point")
qplot(wt, mpg, data=mtcars, size=factor(cyl), geom="point")
# bar-plot
qplot(factor(cyl), data=mtcars, geom="bar")
# flip plot by 90°
qplot(factor(cyl), data=mtcars, geom="bar") + coord_flip()
# difference between fill/color bars
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))
qplot(factor(cyl), data=mtcars, geom="bar", colour=factor(cyl))
# fill by variable
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))
# use different display of bars (stacked, dodged, identity)
head(diamonds)
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="stack")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="dodge")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="fill")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="identity")
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="identity")
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="stack")

# using pre-calculated tables or weights (hint: usage of ddply in package plyr)
table(diamonds$cut)
t.table <- ddply(diamonds, c("clarity", "cut"), "nrow")
head(t.table)
qplot(cut, nrow, data=t.table, geom="bar")
qplot(cut, nrow, data=t.table, geom="bar", stat="identity")
qplot(cut, nrow, data=t.table, geom="bar", stat="identity", fill=clarity)
qplot(cut, data=diamonds, geom="bar", weight=carat)
qplot(cut, data=diamonds, geom="bar", weight=carat, ylab="carat")
### excursion ddply (split data.frame in subframes and apply functions)
ddply(diamonds, "cut", "nrow")
ddply(diamonds, c("cut", "clarity"), "nrow")
ddply(diamonds, "cut", mean)
ddply(diamonds, "cut", summarise, meanDepth = mean(depth))
ddply(diamonds, "cut", summarise, lower = quantile(depth, 0.25, na.rm=TRUE), median = median(depth, na.rm=TRUE),
      upper = quantile(depth, 0.75, na.rm=TRUE))
t.function <- function(x,y){
  z = sum(x) / sum(x+y)
  return(z)
}
ddply(diamonds, "cut", summarise, custom = t.function(depth, price))
ddply(diamonds, "cut", summarise, custom = sum(depth) / sum(depth + price))
### back to ggplot
# histogram
qplot(carat, data=diamonds, geom="histogram")
# change binwidth
qplot(carat, data=diamonds, geom="histogram", binwidth=0.1)
qplot(carat, data=diamonds, geom="histogram", binwidth=0.01)

# use geom to combine plots (hint: order of layers)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"))
qplot(wt, mpg, data=mtcars, geom=c("smooth", "point"))
qplot(wt, mpg, data=mtcars, color=factor(cyl), geom=c("point", "smooth"))



# tweeking the smooth plot ("loess"-method: polynomial surface using local fitting)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"))
# removing standard error
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), se=FALSE)
# making line more or less wiggly (span: 0-1)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), span=0.6)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), span=1)
# using linear modelling
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), method="lm")
# using a custom formula for fitting
library(splines)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), method="lm", formula = y ~ ns(x,5))
# illustrate flip versus changing of variable allocation
qplot(mpg, wt, data=mtcars, facets=cyl~., geom=c("point", "smooth"))
qplot(mpg, wt, data=mtcars, facets=cyl~., geom=c("point", "smooth")) + coord_flip()
qplot(wt, mpg, data=mtcars, facets=cyl~., geom=c("point", "smooth"))
# save plot in variable (hint: data is saved in plot, changes in data do not change plot-data)
p.tmp <- qplot(factor(cyl), wt, data=mtcars, geom="boxplot")
p.tmp
# save mtcars in tmp-var
t.mtcars <- mtcars
# change mtcars
mtcars <- transform(mtcars, wt=wt^2)
# draw plot without/with update of plot data
p.tmp
p.tmp %+% mtcars
# reset mtcars
mtcars <- t.mtcars
rm(t.mtcars)
# get information about plot
summary(p.tmp)


# save plot (with data included)

save(p.tmp, file="temp.rData")
# save image of plot on disk (hint: svg device must be installed)
ggsave(file="test.pdf")
ggsave(file="test.jpeg", dpi=72)
ggsave(file="test.svg", plot=p.tmp, width=10, height=5)
### going further with ggplot
# create basic plot (hint: can not be displayed, no layers yet)
p.tmp <- ggplot(mtcars, aes(mpg, wt, colour=factor(cyl)))
p.tmp
# using additional layers (hint: ggplot draws in layers)
p.tmp + layer(geom="point")
p.tmp + layer(geom="point") + layer(geom="line")
# using shortcuts -> geom_XXX(mapping, data, ..., geom, position)
p.tmp + geom_point()
# using ggplot-syntax with qplot (hint: qplot creates layers automatically)
qplot(mpg, wt, data=mtcars, color=factor(cyl), geom="point") + geom_line()
qplot(mpg, wt, data=mtcars, color=factor(cyl), geom=c("point","line"))
# add an additional layer with different mapping
p.tmp + geom_point()
p.tmp + geom_point() + geom_point(aes(y=disp))
# setting aesthetics instead of mapping
p.tmp + geom_point(color="darkblue")
p.tmp + geom_point(aes(color="darkblue"))
# dealing with overplotting (hollow points, pixel points, alpha[0-1] )
t.df <- data.frame(x=rnorm(2000), y=rnorm(2000))
p.norm <- ggplot(t.df, aes(x,y))
p.norm + geom_point()
p.norm + geom_point(shape=1)
p.norm + geom_point(shape=".")
p.norm + geom_point(colour=alpha("black", 1/2))
p.norm + geom_point(colour=alpha("blue", 1/10))

# using facets (hint: bug in margins -> doesn't work)
qplot(mpg, wt, data=mtcars, facets=.~cyl, geom="point")
qplot(mpg, wt, data=mtcars, facets=gear~cyl, geom="point")
# facet_wrap / facet_grid
qplot(mpg, wt, data=mtcars, facets=~cyl, geom="point")
p.tmp <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p.tmp + facet_wrap(~cyl)
p.tmp + facet_wrap(~cyl, ncol=3)
p.tmp + facet_grid(gear~cyl)
p.tmp + facet_wrap(~cyl+gear)
# controlling scales in facets (default: scales="fixed")
p.tmp + facet_wrap(~cyl, scales="free")
p.tmp + facet_wrap(~cyl, scales="free_x")
p.tmp + facet_wrap(~cyl, scales="fixed")
# contstraint on facet_grid (all rows,columns same scale)
p.tmp + facet_grid(gear~cyl, scales="free_x")
p.tmp + facet_grid(gear~cyl, scales="free", space="free")
# using scales (color palettes, manual colors, matching of colors to values)
p.tmp <- qplot(cut, data=diamonds, geom="bar", fill=cut)
p.tmp
p.tmp + scale_fill_brewer()
p.tmp + scale_fill_brewer(palette="Paired")
RColorBrewer::display.brewer.all()
p.tmp + scale_fill_manual(values=c("#7fc6bc","#083642","#b1df01","#cdef9c","#466b5d"))
p.tmp + scale_fill_manual("Color-Matching", c("Fair"="#78ac07", "Good"="#5b99d4",
                                              "Ideal"="#ff9900", "Very Good"="#5d6778", "Premium"="#da0027", "Not used"="#452354"))
# changing text (directly in qplot / additional shortcut)
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point", xlab="Descr. of x-axis", ylab="Descr. of y-axis", main="Our
Sample Plot")
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") + xlab("x-axis")
# changing name of legend (bug: in labs you must use "colour", "color" doesn't work)
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") + labs(colour="Legend-Name")
# removing legend
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") + scale_color_discrete(legend=FALSE)
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") + opts(legend.position="none")


# moving legend to another place
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") + opts(legend.position="left")
# changing labels on legend
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") +
  scale_colour_discrete(name="Legend for cyl", breaks=c("4","6","8"), labels=c("four", "six", "eight"))
# reordering breaks (values of legend)
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") +
  scale_colour_discrete(name="Legend for cyl", breaks=c("8","4","6"))
# dropping factors
mtcars2 <- transform(mtcars, cyl=factor(cyl))
levels(mtcars2$cyl)
qplot(mpg, wt, data=mtcars2, colour=cyl, geom="point") + scale_colour_discrete(limits=c("4", "8"))
# limits vs zooming in vs breaks
p.tmp <- qplot(wt, mpg, data=mtcars, geom=c("point",
                                            "smooth"), method="lm")
p.tmp
p.tmp + scale_x_continuous(limits=c(15,30))
p.tmp + coord_cartesian(xlim=c(15,30))
p.tmp
p.tmp + scale_x_continuous(breaks=c(15, 18, 27))
p.tmp + scale_x_continuous(breaks=c(15, 18, 27),
                           labels=c("low", "middle", "high"))
# using transformation
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point")
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") +
  scale_y_continuous(trans="log2")
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point") +
  scale_y_continuous(trans="log2") + scale_x_log10()
### themes
# use theme for plot only
qplot(mpg, wt, data=mtcars, geom="point")
qplot(mpg, wt, data=mtcars, geom="point") + theme_bw()
# change font-size for all labels (change base_size)
qplot(mpg, wt, data=mtcars, geom="point") + theme_bw(18)
# change theme for all future plots
theme_set(theme_bw())

# get current theme
theme_get()
# change specific options (hint: "color" does not work in theme_text() -> use colour)
qplot(mpg, wt, data=mtcars, geom="point", main="THIS IS A TEST-PLOT")
qplot(mpg, wt, data=mtcars, geom="point", main="THIS IS A TEST-PLOT") + opts(axis.line=theme_segment(),
                                                                             plot.title=theme_text(size=20, face="bold", colour="steelblue"), panel.grid.minor=theme_blank(),
                                                                             panel.background=theme_blank(), panel.grid.major=theme_line(linetype="dotted", colour="lightgrey", size=0.5),
                                                                             panel.grid.major=theme_blank())
### create barplot like lattice
# use combination of geoms and specific stat for bin calculation
qplot(x=factor(gear), ymax=..count.., ymin=0, ymax=..count.., label=..count..,
      data=mtcars, geom=c("pointrange", "text"), stat="bin", vjust=-0.5,
      color=I("blue")) + coord_flip() + theme_bw()
### create a pie-chart, radar-chart (hint: not recommended)
# map a barchart to a polar coordinate system
p.tmp <- ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + geom_bar(width=1)
p.tmp
p.tmp + coord_polar(theta="y")
p.tmp + coord_polar()
ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) + geom_bar(width=1) + coord_polar()



qplot(diamonds$cut, diamonds$carat)
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, colour=clarity)
qplot(carat, price, data = diamonds, geom=c("point", "smooth"), method=lm)

qplot(carat, data = diamonds,â???¨  geom="histogram")
qplot(carat, data = diamonds,â???¨  geom="histogram", binwidth = 1)
qplot(carat, data = diamonds,â???¨  geom="histogram", binwidth = 0.1)
qplot(carat, data = diamonds,â???¨  geom="histogram", binwidth = 0.01)

# using ggplot() -------------------------------------------------------------
d <- ggplot(diamonds, aes(x=carat, y=price))
d + geom_point()
d + geom_point(aes(colour = carat))
d + geom_point(aes(colour = carat)) + scale_colour_brewer()

ggplot(diamonds) + geom_histogram(aes(x=price))

# Separation of statistcs and geometric elements -----------------------------

p <- ggplot(diamonds, aes(x=price))

p + geom_histogram()
p + stat_bin(geom="area")
p + stat_bin(geom="point")
p + stat_bin(geom="line")

p + geom_histogram(aes(fill = clarity))
p + geom_histogram(aes(y = ..density..))

# Setting vs mapping ---------------------------------------------------------
p <- ggplot(diamonds, aes(x=carat,y=price))

# What will this do?
p + geom_point(aes(colour = "green"))
p + geom_point(colour = "green")
p + geom_point(colour = colour)
