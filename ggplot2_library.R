# TODO: Add comment
# 
# Author: xerula
###############################################################################


library(ggplot2)

# themes

# wallcloud
# example: 
theme_wallcloud <- theme_grey()
theme_wallcloud$panel.background <- element_rect(fill = "#222222")
theme_wallcloud$panel.grid.minor <- element_line(colour = "#000000", size=0.15)
theme_wallcloud$panel.grid.major <- element_line(colour="#333333", size=1)
theme_wallcloud$legend.key <- element_rect(fill = "#222222", colour="#FFFFFF")


theme_blizzard <- theme_grey()
theme_blizzard$panel.background = element_blank()
theme_blizzard$panel.grid.minor = element_blank()
theme_blizzard$panel.grid.major = element_blank()
theme_blizzard$legend.key <- element_rect(fill = "#FFFFFF", color="#FFFFFF")
theme_blizzard$legend.position <- "none"

theme_whiteout <- theme_grey()
theme_whiteout$panel.background = element_blank()
theme_whiteout$line <- element_blank()
theme_whiteout$text <- element_blank()
theme_whiteout$title <- element_blank()
theme_whiteout$legend.position <- "none"

theme_Mordor <- theme_grey()
theme_Mordor$panel.background$fill = "#130000"
theme_Mordor$line <- element_blank()
theme_Mordor$text <- element_blank()
theme_Mordor$title <- element_blank()
theme_Mordor$legend.position <- "none"

new_plain_theme <- function(color){
    new_theme <- theme_grey()
    new_theme$panel.background$fill = color
    new_theme$line <- element_blank()
    new_theme$text <- element_blank()
    new_theme$title <- element_blank()
    new_theme$legend.position <- "none"
    theme_set(new_theme)
}

# plot (the previous plot, by default) with panel only, no border
unborderize <- function(p=last_plot()){
    require(grid)
    innards <- ggplot_gtable(ggplot_build(p))
    subplot <- subset(innards$layout, name == "panel")
    with(subplot, grid.draw(innards[t:b, l:r]))
}


# PLOT multiple f(x) curves for different values of some parameter in f(x), e.g. f(x) = A*x^2
# this function takes a function 'expr' in terms of variables 'var' and 's' (sequence), and evaluates and plots the function over domain 's' separately for each value of 'var',
# for example, if we want to see how the plot of the function f(x) = A*x^2 changes for different values of A, we would do plotMultiCurve(quote(A * x^2), x, A), where x is a vector of values over which we evaluate A*x^2 for each of the values in vector A
# the names of the variables in 'expr' must match the names of the corresponding objects supplied as 's' and 'var'
# if 'negs' is true, for each value in A, the function is also evaluated for its additive inverse, and plotted in the same color

plotMultiCurve <- function(expr, s, var, negs=T, title=""){
    domain <- deparse(substitute(s))
    f_var <- deparse(substitute(var))
    expr <- deparse(expr)
    expr <- gsub(f_var, "var", expr, fixed=T)
    expr <- gsub(domain, "rep(s, each=length(var))", expr, fixed=T)
    old_var <- var
    if(negs == T){
        var <- c(var, -var)
        old_var <- rep(old_var, 2)
    }
    res <- eval(parse(text=expr))
    df <- data.frame(x=rep(s, each=length(var)), y=res, color=rep(old_var, length(s)), var=rep(var, length(s)))
    ggplot(data=df, aes(x, y, group=factor(var)))
}

# overplot multiple functions for same domain 'x'
plotMultiFunc <- function(funcs, x){
    temp <- unlist(lapply(funcs, do.call, args=list(x=x)))
    res <- data.frame(x=rep(x, length(funcs)), y=temp, func=rep(1:length(funcs), each=length(x)))
    ggplot(data=res, aes(x, y, group=func))
}

# take list of (one-line) functions and return an expression vector of their bodies for plot labelling
# this may seem nifty but it is often better to create custom formatted labels
func2expr <- function(funcs){
    exprs <- vector(mode="expression", length(funcs))
    for(i in 1:length(exprs)){
        exprs[i] <- as.expression(body(funcs[[i]]))
    }
    exprs
}

# nicer version
func2expr <- function(funcs){
    k <- Vectorize(body)(funcs)
    sapply(k, as.expression)
}

# for when each curve is a separate data frame column
overplot <- function(df){
    curveNames <- colnames(df)
    p <- ggplot(data=df, aes_string(x=curveNames[1]))
    layers <- sapply(curveNames[-1], function(x) geom_line(aes_string(y=x)))
    p + layers
}

