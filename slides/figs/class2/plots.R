library(ggplot2)
library(MASS)
library(dplyr)
library(cumstats)

# Students are not correlated ------------------------

students <- as.data.frame(mvrnorm(50, c(3,170), matrix(c(1,0,0,1), nrow = 2, ncol = 2)))
colnames(students) <- c("GPA", "Height")

ggplot(students, aes(x=Height, y = GPA)) + geom_point() + theme_light()
ggsave("students_uncorrelated.pdf", width = 15, height = 10, units = "cm")


# Students are correlated to various degree  ----------------

plot_students <- function(name, cor.matrix){
    students <- as.data.frame(mvrnorm(200, c(3,170), cor.matrix))
    colnames(students) <- c("GPA", "Height")
    
    p <- ggplot(students, aes(x=Height, y = GPA)) + geom_point() + theme_light() + 
        expand_limits(x=c(165,174), y=c(0,5))
    ggsave(paste("students_",name,".pdf", sep=""), width = 15, height = 10, units = "cm")
    cat(name, " correlation: ", cor.matrix[2,1]/sqrt(cor.matrix[1,1]*cor.matrix[2,2]))
    return(p)
}

cor.matrix0 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
plot_students("example0", cor.matrix0)

cor.matrix1 <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
plot1 <- plot_students("example1", cor.matrix1)
plot1

cor.matrix2 <- matrix(c(1,-1,-1,1), nrow = 2, ncol = 2)
plot_students("example2", cor.matrix2)

plot1 + expand_limits(y=c(0,30))
ggsave(paste("students_example1_stretched.pdf", sep=""), width = 15, height = 10, units = "cm")

cor.matrix3 <- matrix(c(1,0.5,0.5,1), nrow = 2, ncol = 2)
plot_students("example3", cor.matrix3)

cor.matrix4 <- matrix(c(1,0,0,0.1), nrow = 2, ncol = 2)
plot_students("example4", cor.matrix4)

# Classroom means -------

height.mean <- 170
height.std <- 30
height.var <- height.std^2
class.size <- 5

students <- rnorm(1000, height.mean, height.std)
students.classroom <- matrix(students, ncol = class.size)

class.mean <- apply(students.classroom, 1, mean)
# sd and var are unbiased, need to uncorrect to show the effect
class.sd <- apply(students.classroom, 1, function(x) {sqrt(var(x)*(class.size-1)/class.size)} )
class.var <- apply(students.classroom, 1, function(x) {var(x)*(class.size-1)/class.size} )

# the unbiased versions of the functions
class.sd.unb <- apply(students.classroom, 1, function(x) {sqrt(var(x))})
class.var.unb <- apply(students.classroom, 1, function(x) {var(x)})

classes <- data.frame(class.mean = class.mean, class.sd = class.sd, 
                      class.mean.cummean = cummean(class.mean),
                      class.sd.cummean = cummean(class.sd),
                      class.var.cummean = cummean(class.var),
                      class.sd.unb.cummean = cummean(class.sd.unb),
                      class.var.unb.cummean = cummean(class.var.unb));

classes$class <- seq(nrow(classes))

#mean
ggplot(classes, aes(x=class, y=class.mean)) + 
    geom_point() + xlab("Classroom") + ylab("Class Mean") + theme_light()  +
    geom_hline(aes(yintercept=height.mean), linetype="dashed")
ggsave("heights_mean.pdf", width = 15, height = 10, units = "cm")

ggplot(classes, aes(x=class, y=class.mean.cummean)) + 
    geom_line() + xlab("Classroom") + ylab("Class Mean") + theme_light()  +
    geom_hline(aes(yintercept=height.mean), linetype="dashed")
ggsave("heights_mean_cummean.pdf", width = 15, height = 10, units = "cm")

#sd
ggplot(classes, aes(x=class, y=class.sd)) +
    geom_point() + xlab("Classroom") + ylab("Class Standard Deviation") + theme_light()  +
    geom_hline(aes(yintercept=height.std), linetype="dashed")
ggsave("heights_sd.pdf", width = 15, height = 10, units = "cm")

ggplot(classes, aes(x=class, y=class.sd.cummean)) +
    geom_line() + xlab("Classroom") + ylab("Class Standard Deviation") + theme_light()  +
    geom_hline(aes(yintercept=height.std), linetype="dashed")
ggsave("heights_sd_cummean.pdf", width = 15, height = 10, units = "cm")

#sd-unb
ggplot(classes, aes(x=class, y=class.sd.unb)) + 
    geom_point() + xlab("Classroom") + ylab("Class Standard Deviation") + theme_light()  +
    geom_hline(aes(yintercept=height.std), linetype="dashed")
ggsave("heights_sd_unb.pdf", width = 15, height = 10, units = "cm")

ggplot(classes, aes(x=class, y=class.sd.unb.cummean)) + 
    geom_line() + xlab("Classroom") + ylab("Class Standard Deviation") + theme_light()  +
    geom_hline(aes(yintercept=height.std), linetype="dashed")
ggsave("heights_sd_unb_cummean.pdf", width = 15, height = 10, units = "cm")

#var
ggplot(classes, aes(x=class, y=class.var)) + 
    geom_point() + xlab("Classroom") + ylab("Class Variance") + theme_light()  +
    geom_hline(aes(yintercept=height.var), linetype="dashed")
ggsave("heights_var.pdf", width = 15, height = 10, units = "cm")

ggplot(classes, aes(x=class, y=class.var.cummean)) + 
    geom_line() + xlab("Classroom") + ylab("Class Variance") + theme_light()  +
    geom_hline(aes(yintercept=height.var), linetype="dashed")
ggsave("heights_var_cummean.pdf", width = 15, height = 10, units = "cm")

#var-unb
ggplot(classes, aes(x=class, y=class.var.unb)) + 
    geom_point() + xlab("Classroom") + ylab("Class Variance") + theme_light()  +
    geom_hline(aes(yintercept=height.var), linetype="dashed")
ggsave("heights_var_unb.pdf", width = 15, height = 10, units = "cm")

ggplot(classes, aes(x=class, y=class.var.unb.cummean)) + 
    geom_line() + xlab("Classroom") + ylab("Class Variance") + theme_light()  +
    geom_hline(aes(yintercept=height.var), linetype="dashed")
ggsave("heights_var_unb_cummean.pdf", width = 15, height = 10, units = "cm")


