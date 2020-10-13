
r = 930526+890801
set.seed(r)
data=shootings_1[sample(4895, 100, replace=TRUE),]

data=droplevels(data)

# Summary & Visualisation
summary(data)

#Create histograms (?) of age, fleeing, signs of mental illness
hist(data$age, col="skyblue", breaks = sqrt(length(data$age)))

n = 0:length(data$signs_of_mental_illness)
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
p = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE]) /length(data$signs_of_mental_illness)

y = dbinom(n, size=x, prob = p)
plot(n, y, type = 'h')

# Question 1
xbar = mean(data = data$flee)
s = sd(data$age)
n = length(data$age)
alpha=0.05

z = qnorm(alpha, lower.tail = FALSE)
l = xbar-z*s/sqrt(n)


# Question 2
phat = length(data$armed[data$armed=="unarmed"])/length(data$armed)
x = length(data$armed[data$armed=="unarmed"])
n = length(data$armed)
alpha=0.05
binom.test(x, n, phat, alternative = "two.sided", conf.level = 1-alpha)

# Question 3

# Question 4

# Question 5
