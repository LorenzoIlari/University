using Regress, DataFrames, CSV, LinearAlgebra, ShiftedArrays

# Load the data
sp500 = CSV.read("/Users/lorenzo/Library/Mobile Documents/com~apple~CloudDocs/Università/2. Magistrale/Github/University/Adv. Econometrics/Exercises/Forecasting exercise/Data/sp500_monthly_returns.csv", DataFrame)
returns = sp500[:, 2]

# Manual way
n = length(returns)
y = returns[3:n]
X = hcat(ones(n-2),returns[2:n-1], returns[1:n-2])
beta_manual = (X'X) \ (X'y)
error_manual = y - X * beta_manual
sigma2_manual = sum(error_manual.^2) / (length(y) - size(X, 2))
println("Intercept: ",  round(beta_manual[1], digits=8))
println("Phi1: ",  round(beta_manual[2], digits=8))
println("Phi2: ",  round(beta_manual[3], digits=8))
println("Error Variance: ", round(sigma2_manual, digits=8))  

# Regress way
df = DataFrame(r = returns)
m = Regress.ols(df, @formula(r ~ lags(r, 2)))
error_func = residuals(m)
coeftable(m)

m_hac = m + vcov(Bartlett{NeweyWest}())
coeftable(m_hac)

###Forecasting

##simple ar2 first                                    
P = 60
R = length(returns) - P             # initial sample
r_hat = zeros(P)                    # preallocations
for_err = zeros(P)
hist_mean = zeros(P)
dev_from_mean = zeros(P)

for i = 1:P
    r = df.r[1:R+i]
    x = hcat(ones(length(r)-3), r[2:end-2], r[1:end-3])
    phi_hat = (x'x) \ (x'r[3:end-1])
    r_hat[i] = phi_hat[1] + phi_hat[2]*r[end-1] + phi_hat[3]*r[end-2]
    for_err[i] = r[end] - r_hat[i]
    hist_mean[i] = sum(r)/length(r) 
    dev_from_mean[i] = r_hat[i] - hist_mean[i]
end
msfe_ar2 = sum(for_err.^2)/P
msfe_from_mean = sum(dev_from_mean.^2)/P

## AR(p) where p = 1:12
p = 12 
r_hat_p = zeros(P,p)                    # preallocations
for_err_p = zeros(P,p)
hist_mean_p = zeros(P)
dev_from_mean_p = zeros(P,p)
j = 1:12

# let's try to use Regress function to calculate coefficients
for i = 1:P
    df_p = DataFrame(df[1:R+i,:])
    hist_mean_p[i] = sum(df_p.r)/length(df_p.r)
    for j = 2:12
        m = Regress.ols(df_p, @formula(r ~ lags(r, j)))
        phi_hat_p = coef(m)
        z = vcat(1,df_p[end-j+1:end])
        r_hat_p[i,j] = z' * phi_hat_p
        forc_err_p[i,j] = df_p.r[end] - r_hat_p[i,j]
        dev_from_mean_p[i,j] = r_hat_p[i,j] - hist_mean_p[i]
    end
end

# Manual way: create lagged DataFrame for AR(p)
lagged = DataFrame(r = df.r)
r_hat_m = zeros(P,p)                    # preallocations
for_err_m = zeros(P,p)
hist_mean_m = zeros(P)
dev_from_mean_m = zeros(P,p)

for j = 1:p
    lagged[!, Symbol("lag$j")] = lag(df.r, j)
end
lagged = dropmissing(lagged)

for i = 1:P 
    for j = 1:p
    lagged_p =lagged[1:R+i,:]
    ym = lagged_p.r[1:R+i-1]            # last obs of ym does NOT contain the forecast
    n = length(ym)
    xm = hcat(ones(n-1), Matrix(lagged_p[1:n-1, 2:j+1]))
    phi_manual = (xm'xm) \ (xm'ym[1:end-1])
    ym_hat = vcat(1, ym[end-j+1:end])' * phi_manual

   
    

    end
end




