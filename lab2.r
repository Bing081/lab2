
r = 930526+890801
set.seed(r)
data=shootings_1[sample(4895, 100, replace=TRUE),]

data=droplevels(data)

# --------------------------------- Summary & Visualisation --------------------------------- #
summary(data)

# Check Normality assumption for 'age'
qqnorm(data$age)
qqline(data$age)

# Histogram for age
n = 0:length(data$age)
y = dnorm(n, mean=mean(data$age), sd=sd(data$age))

plot(n, y, type ='h')

# Histogram for 'signs of mental illness'
n = 0:length(data$signs_of_mental_illness)
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
p = x/length(data$signs_of_mental_illness)

y = dbinom(n, size=x, prob = p)
plot(n, y, type = 'h')

# Histogram for 'armed'

# --------------------------------------- Question 1 ---------------------------------------- #

# Since we do not know true variance
# we estimate with sample variance
xbar = mean(data = data$flee)
s = sd(data$age)
n = length(data$age)
alpha=0.05

z = qnorm(alpha, lower.tail = FALSE)
l = xbar-z*s/sqrt(n)

# --------------------------------------- Question 2 ---------------------------------------- #

x = length(data$armed[data$armed=="unarmed"])
phat = x/length(data$armed)
n = length(data$armed)
alpha=0.05
binom.test(x, n, phat, alternative = "two.sided", conf.level = 1-alpha)

# --------------------------------------- Question 3 ---------------------------------------- #


# --------------------------------------- Question 4 ---------------------------------------- #


# --------------------------------------- Question 5 ---------------------------------------- #

