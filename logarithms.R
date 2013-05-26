# Author: xerula xerula@gmail.com
# Created: May 26, 2013 5:23:17 PM
# using StatET for Eclipse 
##################################

theme_set(theme_wallcloud)

# plot logarithm function for different bases

bases <- c(1.05, 1.1, 1.3, 1.75, 5)
s <- seq(1e-16, 30, by=0.01)

plotMultiCurve(quote(log(s, bases)), s, bases, extend=function(x) 1/x) -> p

p + geom_path(aes(color=factor(color))) +
    coord_cartesian(ylim=c(-22,22), xlim=c(-1, 10)) +
    ylab(expression(log[b](x))) + scale_color_discrete(name="b & 1/b") + 
    labs(title=c(expression(f(x) == log[b](x)~~"for different b"))) 
