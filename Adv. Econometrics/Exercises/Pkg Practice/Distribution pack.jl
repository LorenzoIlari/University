using Distributions

## Generating Distributions
N = Normal()
T = TDist(df)
X = Chisq(df)
F = FDist(df1,df2)
B = Binomial(n, p)

## Calculating Probabilities

# for a Normal
d = Normal()        
cdf(d, 1.96)        # P(X ≤ 1.96)
ccdf(d, 1.96)       # P(X > 1.96)
quantile(d, 0.975)  # critic value 97.5%
pdf(d, 0)           # density in 0
p_val = 2 * ccdf(d, abs(x))


