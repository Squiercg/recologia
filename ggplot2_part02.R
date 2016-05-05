install.packages("ggplot2y")

(105*50000000/4469000)/60
# Very basic bar graph
ggplot(data=dat, aes(x=time, y=total_bill)) +
    geom_bar(stat="identity")


# Map the time of day to different fill colors
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(stat="identity")

## This would have the same result as above
# ggplot(data=dat, aes(x=time, y=total_bill)) +
#    geom_bar(aes(fill=time), stat="identity")


# Add a black outline
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", stat="identity")


# No legend, since the information is redundant
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE)

ggplot(data=datn, aes(x=dose, y=length, group=supp, colour=supp)) +
    geom_line() +
    geom_point()
