# Author: xerula xerula@gmail.com
# Created: May 26, 2013 3:11:02 PM
# using StatET for Eclipse 
##################################
# generate harmonic progression from a, d of arithmetic progression
harmonic_series <- function(a, d, n){
    d <- d/a
    a <- 1/a
    res <- a
    for(i in 1:(n-1)){
        res <- c(res, a / (1 + i*d))
        }
    res
    } 

to recover arithmetic progression, e.g.:
harmonic_series(1, 1, 10) -> p
1/p

# compare geometric progression with a=1 for different r
r <- c(-1.1, -1, -0.9, 0.8, 1, 1.2)
s <- 0:15

theme_set(theme_grey())

plotMultiCurve(quote(r^s), s, r) -> p

p + geom_path(aes(color=factor(color))) + 
        coord_cartesian(ylim=c(-5,10)) + 
        geom_point(aes(color=factor(color)), size=2) +
        ylab(expression(r^x)) + scale_color_discrete(name="r") +
        labs(title=c(expression(f(x) == r^x ~~"for different r"))) 


