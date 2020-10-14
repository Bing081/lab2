
r = 930526+890801
set.seed(r)
data=shootings_1[sample(4895, 100, replace=TRUE),]

data=droplevels(data)

# --------------------------------- Summary & Visualization --------------------------------- #
summary(data)

# Check Normality assumption for 'age'
jpeg(file="Normality_assumption_age.jpeg")
qqnorm(data$age)
qqline(data$age)
dev.off()

# Histogram for 'age'
n = 0:length(data$age)
y = dnorm(n, mean=mean(data$age), sd=sd(data$age))

jpeg(file = "age_distribution.jpeg")
plot(n, y, type ='h')
dev.off()

# Histogram for 'signs of mental illness'
n = 0:length(data$signs_of_mental_illness)
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
p = x/length(data$signs_of_mental_illness)

y = dbinom(n, size=x, prob = p)
jpeg(file = "mental_illness_distribution.jpeg")
plot(n, y, type = 'h')
dev.off()

# Histogram for 'armed'

# Not sure how to do this one. Binomial with unarmed vs armed (gun, knife etc.)?

# --------------------------------------- Question 1 ---------------------------------------- #

# Since we do not know true variance
# we estimate with sample variance
# We have previously shown (line 12-13)
# that the distribution is normal
xbar = mean(data$age)
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

# All steps of hypothesis testing

# Defining variables and stating assumptions
# h0: p = 0.2
p = 0.2
# h1: p > 0.2
# test statistics
  # (X - np0)/sqrt(np0(1-p0))  N(0,1)
# Rejection criteria
  # P-value < alpha (significance level)
  # z0 > za
# Calculation
alpha = 0.05
confidence_level = 1-alpha
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
n = length(data$signs_of_mental_illness)

z0 = (x-n*p)/sqrt(np*(1-p))

# Not sure if this is correct
za = qnorm(alpha, lower.tail=FALSE)

# Conclusions
  # There is enough statistical evidence (p > alpha) 
  # to reject the null-hypothesis (ie p = 0.2)

# --------------------------------------- Question 4 ---------------------------------------- #



