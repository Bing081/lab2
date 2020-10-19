
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
plot(n, y, type ='h', ylab="Propbability")
dev.off()

hist(data$age, xlab = "Age", main = "Histogram of age" )

# Histogram for 'signs of mental illness'
n = 0:length(data$signs_of_mental_illness)
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
p = x/length(data$signs_of_mental_illness)

y = dbinom(n, size=x, prob = p)
jpeg(file = "mental_illness_distribution.jpeg")
plot(n, y, type = 'h')
dev.off()

# Pie-chart for 'signs of mental illness'
true = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
false = length(data$signs_of_mental_illness[data$signs_of_mental_illness==FALSE])
pie(c(true,false), labels = c("true","false"), main = "Signs of mental illness, n = 100") 

# Bar-plot for armed
armed = table(data$armed)
armed_order = armed[order(armed)]
barplot(armed_order, las=2)

# Pie-chart for 'armed'
unarmed = length(data$armed[data$armed=="unarmed"])
armed = length(data$armed[data$armed!="unarmed"])
pie(c(unarmed,armed), labels = c("Unarmed","Armed"), main = "Proportion of unarmed victims, n = 100") 

# --------------------------------------- Question 1 ---------------------------------------- #

# Since we do not know true variance
# we estimate with sample variance
# We have previously shown (line 12-13)
# that the distribution is normal for age, 
# and xbar is also normally distributed
# N(mu, sigma^2/n) and CLT states that if n is large
# we can still use normal distribution
# and substitute sigma with sample sd.
# Otherwise we would have to use T-distr
# ie, a t-quantile.
xbar = mean(data$age)
s = sd(data$age)
n = length(data$age)
alpha=0.05

z = qnorm(alpha, lower.tail = FALSE)
l = xbar-z*s/sqrt(n)

# --------------------------------------- Question 2 ---------------------------------------- #

# Since the population (armed) is binomially
# distributed (either unarmed or not) and
# np(1-p) > 5 we can approximate with
# normal distribution N(np, np(1-p)). Let
# phat denote the estimator X/n and X/n
# is distributed N(p, p(1-p)).Let Z denote
# the standardized variable (phat-p)/sqrt(p(1-p)/n)

x = length(data$armed[data$armed=="unarmed"])
n = length(data$armed)
phat = x/n
alpha=0.05
z = qnorm(alpha/2)

# Since p is unknown we estimate using phat
# Calculate upper and lower CI
CI = phat + c(-1,1)*z*sqrt(phat*(1-phat)/n)

# --------------------------------------- Question 3 ---------------------------------------- #

# All steps of hypothesis testing

# Defining variables and stating assumptions
# h0: p = 0.2
# h1: p > 0.2
# test statistics
  # (x - n*p0)/sqrt(n*p0(1-p0)) N(0,1)
# Rejection criteria
  # z0 > z.alpha or p-value < alpha
# Calculation
p = 0.2
alpha = 0.05
confidence_level = 1-alpha
x = length(data$signs_of_mental_illness[data$signs_of_mental_illness==TRUE])
n = length(data$signs_of_mental_illness)
phat =x/n

z0 = (x-n*p)/sqrt(n*p*(1-p))

# Not sure if this is correct
z.alpha = qnorm(alpha, lower.tail = FALSE)
z0 > z.alpha

# --------------------------------------- Question 4 ---------------------------------------- #



