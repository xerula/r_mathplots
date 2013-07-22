# Author: xerula xerula@gmail.com
# Created: May 26, 2013 4:54:14 PM
# using StatET for Eclipse 
##################################
theme_set(theme_wallcloud)

## showing affect on parabola of varying coefficients

# varying A
s1 <- seq(-10, 10, by=0.01)
As <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 100)

plotMultiCurve(quote(As * s1^2), s1, As, extend=function(x) -x) -> p

p + geom_path(aes(color=factor(color))) +
    coord_cartesian(ylim=c(-100,100), xlim=c(-10, 10)) +
    ylab(expression(A*x^2)) + scale_color_discrete(name="A & -A") + 
    labs(title=c(expression(f(x) == A*x^2 ~~"for different A"))) 

plotMultiCurve(quote(As * s1^2 + 2), s1, As) -> p

p + geom_path(aes(color=factor(color))) +
        coord_cartesian(ylim=c(-100,100), xlim=c(-10, 10)) +
        ylab(expression(A*x^2)) + scale_color_discrete(name="A & -A") + 
        labs(title=c(expression(f(x) == A*x^2 + 2 ~~"for different A"))) 

# varying B
s2 <- seq(-20, 20, by=0.01)
Bs <- c(0, 1, 2, 4, 6, 8, 10, 15, 25)

plotMultiCurve(quote(s2^2 + Bs*s2), s2, Bs, extend=function(x) -x) -> p

p + geom_path(aes(color=factor(color))) + 
    coord_cartesian(xlim=c(-20, 20)) + 
    ylab(expression(x^2 + B*x)) + 
    scale_color_discrete(name="B & -B") + 
    labs(title=c(expression(f(x) == x^2 + Bx ~~"for different B")))

Bs <- 0:6

plotMultiCurve(quote(s2^2 + Bs*s2 + 1), s2, Bs) -> p

p + geom_path(aes(color=factor(color))) +
        coord_cartesian(xlim=c(-20, 10), ylim=c(-20, 50)) +
        ylab=(expression(x^2 + B*x)) +
        scale_color_discrete(name="B & -B") +
        labs(title=c(expression(f(x) == x^2 + Bx + 1 ~~"for different B")))

# varying C
Cs <- Bs

plotMultiCurve(quote(s1^2 + Cs), s1, Cs, extend=function(x) -x) -> p

p + geom_path(aes(color=factor(color))) +
    coord_cartesian(xlim=c(-10, 10)) + 
    ylab(expression(x^2 + C)) + 
    scale_color_discrete(name="C & -C") + 
    labs(title=c(expression(f(x) == x^2 + C ~~"for different C"))) 

## demonstration of how quadratic functions that are multiples of each other have the same roots

f1 <- function(x) -2*x^2 + 3*x + 10
f2 <- function(x) x^2 - (3/2)*x - 5
f3 <- function(x) -6*x^2 + 9*x + 30 
f4 <- function(x) (1/10)*x^2 - (3/20)*x -(1/2)

funcs <- c(f1, f2, f3, f4)

plotMultiFunc(funcs, s1) -> p

p + geom_path(aes(color=factor(func))) + 
        coord_cartesian(ylim=c(-75, 75), xlim=c(-10, 10)) + 
        opts(title="Equivalence of roots of corresponding monic and non-monic quadratic polynomials") + 
        ylab(expression(f(x))) +
        opts(plot.title=theme_text(size=15, vjust=2, hjust=0.2)) +
        opts(plot.margin = grid::unit(c(1,1,1,1), "cm")) +
        scale_color_discrete(name="function", labels=func2expr(funcs))

## a quadratic polynomial A*x^2 + B*x + C is equal to some y at the roots of the corresponding function A*x^2 + B*x + (C-y)

funcs <- c(f1 <- function(x) x^2, f2 <- function(x) x^2 - 3, f3 <- function(x) 0)
funcs <- c(f1 <- function(x) x^2, f2 <- function(x) x^2 - 3)


plotMultiFunc(funcs, s1) -> p

p + geom_path(aes(color=factor(func))) + 
        coord_cartesian(ylim=c(-5, 5), xlim=c(-5, 5)) + 
        labs(title=expression("A quadratic polynomial"~~P(x)~~"equals some"~~y~~"at the roots of"~~P(x-y))) + 
        ylab(expression(f(x))) +
        #labs(plot.title=theme_text(size=15, vjust=2, hjust=0.2)) +
        #opts(plot.margin = grid::unit(c(1,1,1,1), "cm")) +
        scale_color_discrete(name="function", labels=func2expr(funcs)) +
        geom_abline(slope=0, intercept=0, color="grey", alpha=0.3) + 
        geom_segment(x=-sqrt(3), xend=-sqrt(3), y=3, yend=0, linetype=2, color="#00FF0003") +
        geom_segment(x=sqrt(3), xend=sqrt(3), y=3, yend=0, linetype=2, color="#00FF0003")


