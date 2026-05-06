using Regress, DataFrames, CSV, LinearAlgebra, Statistics, Distributions

# Load the data
sp500 = CSV.read("/Users/lorenzo/Library/Mobile Documents/com~apple~CloudDocs/Università/2. Magistrale/Github/University/Adv. Econometrics/Exercises/Forecasting exercise/Data/sp500_monthly_returns.csv", DataFrame)
returns = sp500[:, 2]
df = DataFrame(r = returns)  # dataframe is required for @formula in Regress.ols

#%% Estimates
## Manual way
n = length(returns)
y = returns[3:n]
X = hcat(ones(n-2),returns[2:n-1], returns[1:n-2])
beta_manual = X \ y
error_manual = y - X * beta_manual
sigma2_manual = sum(error_manual.^2) / (length(y) - size(X, 2))
println("Intercept: ",  round(beta_manual[1], digits=8))
println("Phi1: ",  round(beta_manual[2], digits=8))
println("Phi2: ",  round(beta_manual[3], digits=8))
println("Error Variance: ", round(sigma2_manual, digits=8))  

## Regress way
m = Regress.ols(df, @formula(r ~ lags(r, 2)))
error_func = residuals(m)
coeftable(m)

m_hac = m + vcov(Bartlett{NeweyWest}())
coeftable(m_hac)

#%% Forecasting 
##simple AR(2) first                                    
P = 60
R = length(returns) - P             # initial sample
r_hat = zeros(P)                    # preallocations
for_err = zeros(P)
hist_mean = zeros(P)
dev_from_mean = zeros(P)

for i = 1:P
    r = df.r[1:R+i]
    x = hcat(ones(length(r)-3), r[2:end-2], r[1:end-3])
    phi_hat = x \ r[3:end-1]
    r_hat[i] = phi_hat[1] + phi_hat[2]*r[end-1] + phi_hat[3]*r[end-2]
    for_err[i] = r[end] - r_hat[i]
    hist_mean[i] = sum(r)/length(r) 
    dev_from_mean[i] = r_hat[i] - hist_mean[i]
end

msfe_ar2 = sum(for_err.^2)/P
msfe_from_mean = sum(dev_from_mean.^2)/P

#%% Expanding Window
## Ex: 1 Find the optimal p that minimize MSFE in AR(p) 

p = 12 
P = 60
R = length(returns) - P 
r_hat_p = zeros(P,p)                    # preallocations
forc_err_p = zeros(P,p)
hist_mean_p = zeros(P)
dev_from_mean_p = zeros(P,p)
msfe_ar2_p = zeros(p)
msfe_from_mean_p = zeros(p)

# let's use Regress function to calculate coefficients
for i = 1:P

    t = R + i - 1                                       # Expanding Window
    df_p = DataFrame(df[1:t, :])
    hist_mean_p[i] = sum(df_p.r)/length(df_p.r)         

    for j = 1:12
        f = term(:r) ~ term(1) + lags(term(:r), j)      # otherwise lags does not use j properly
        m = Regress.ols(df_p, f)
        phi_hat_p = coef(m)

        z = vcat(1.0, df_p.r[end:-1:end-j+1])           # -1 for reverse slicing
        r_hat_p[i,j] = z' * phi_hat_p

        forc_err_p[i,j] = df.r[t+1] - r_hat_p[i,j]
        dev_from_mean_p[i,j] = r_hat_p[i,j] - hist_mean_p[i]

        msfe_ar2_p[j] = sum(forc_err_p[:,j].^2)/P                   # msfe_poos
        msfe_from_mean_p[j] = sum(dev_from_mean_p[:,j].^2)/P

    end
end

p_grid = 1:12

results_msfe = DataFrame(
    p = p_grid,
    msfe_standard = vec(msfe_ar2_p),
    msfe_histmean = vec(msfe_from_mean_p)
)

# the optimal lag order
pstar_standard = results_msfe.p[argmin(results_msfe.msfe_standard)]
pstar_histmean = results_msfe.p[argmin(results_msfe.msfe_histmean)]

println("Optimal p that minimize MSFE (expanding window) is: ", pstar_standard)

fe_p = copy(forc_err_p)
dfm_p = copy(dev_from_mean_p)


## Ex 2: Predict April 2026, CE and omment on the width of the interval 
 # what it says about return predictability?

m_9 = Regress.ols(df, @formula(r ~ lags(r, 9)))
phi_star = coef(m_9)
r_lags = vcat(1, returns[end:-1:end-8])
april_26 =  r_lags'*phi_star

# confidence interval
res = residuals(m_9)
n = length(res)
k = length(phi_star)
se_res = sqrt(sum(res.^2)/(n-k))

ci_lower = april_26 - 1.96 * se_res
ci_upper = april_26 + 1.96 * se_res
(ci_lower, ci_upper)                

level_april26 = exp(april_26+returns[end])



#%% Rolling Window
## Ex 3: same ad above

p = 12 
P = 60
R = length(returns) - P 
W = 240                                 # window size
r_hat_p = zeros(P,p)                    # preallocations
forc_err_p = zeros(P,p)
hist_mean_p = zeros(P)
dev_from_mean_p = zeros(P,p)
msfe_ar2_p = zeros(p)
msfe_from_mean_p = zeros(p)

# let's use Regress function to calculate coefficients
for i = 1:P

    t = R + i - 1                                       # Expanding Window
    df_p = DataFrame(df[1:t, :])                        
    df_w = df_p[end-W:end, :]                           # Rollig Window
    hist_mean_p[i] = sum(df_w.r)/length(df_w.r)         

    for j = 1:12                                    
        f = term(:r) ~ term(1) + lags(term(:r), j)
        m = Regress.ols(df_w, f)
        phi_hat_p = coef(m)

        z = vcat(1.0, df_w.r[end:-1:end-j+1])           # -1 for reverse slicing
        r_hat_p[i,j] = z' * phi_hat_p

        forc_err_p[i,j] = df.r[t+1] - r_hat_p[i,j]
        dev_from_mean_p[i,j] = r_hat_p[i,j] - hist_mean_p[i]

        msfe_ar2_p[j] = sum(forc_err_p[:,j].^2)/P
        msfe_from_mean_p[j] = sum(dev_from_mean_p[:,j].^2)/P

    end
end

p_grid = 1:12

results_msfe_RW = DataFrame(
    p = p_grid,
    msfe_standard_RW = vec(msfe_ar2_p),
    msfe_histmean_RW = vec(msfe_from_mean_p)
)

# the optimal lag order
pstar_standard_RW = results_msfe_RW.p[argmin(results_msfe_RW.msfe_standard_RW)]
pstar_histmean_RW = results_msfe_RW.p[argmin(results_msfe_RW.msfe_histmean_RW)]

println("Optimal p that minimize MSFE (rolling window) is: ", pstar_standard_RW)

# predicting april 2026
m_RW = Regress.ols(df, @formula(r ~ lags(r, 1)))
phi_star = coef(m_RW)
r_lags = vcat(1, returns[end:-1:end-pstar_standard_RW+1])
april_26_RW =  r_lags'*phi_star

# confidence interval
res = residuals(m_RW)
n = length(res)
k = length(phi_star)
se_res = sqrt(sum(res.^2)/(n-k))

ci_lower_RW = april_26_RW - 1.96 * se_res
ci_upper_RW = april_26_RW + 1.96 * se_res
(ci_lower_RW, ci_upper_RW)                # 

level_april26_RW = exp(april_26_RW+returns[end])

## summary
summary_compare = DataFrame(
    method = ["expanding", "rolling"],
    pstar_msfe = [pstar_standard, pstar_standard_RW],
    msfe_at_pstar = [results_msfe.msfe_standard[pstar_standard], results_msfe_RW.msfe_standard_RW[pstar_standard_RW]],
    pstar_msfe_histmean = [pstar_histmean, pstar_histmean_RW],
    msfe_histmean_at_pstar = [results_msfe.msfe_histmean[pstar_histmean], results_msfe_RW.msfe_histmean_RW[pstar_histmean_RW]],
    april26_forecast = [april_26, april_26_RW],
    ci_lower = [ci_lower, ci_lower_RW],
    ci_upper = [ci_upper, ci_upper_RW]
)

println("Confronto finale tra expanding e rolling window:")
println(summary_compare)

#%% Multi-step forecasting
## Ex 4: Produce 3-month-ahead forecasts (h=3) using the iterated forecast

# find the p_star for h=3
h = 3
p = 12
P = 58
n = length(returns)
R = n - P  
r_hat_h3 = zeros(P,p)                    # preallocations
forc_err_h3 = zeros(P,p)
hist_mean_h3 = zeros(P)
dev_from_mean_h3 = zeros(P,p)
msfe_ar2_h3 = zeros(p)
msfe_from_mean_h3 = zeros(p)
fh = zeros(h)

for i = 1:P                          # minus 2 due to 3 period ahead forecast

    t = R + i - 3
    ret = DataFrame(df[1:t, :])
    hist_mean_h3[i] = mean(ret.r)

    for j = 1:p

        f = term(:r) ~ term(1) + lags(term(:r), j)
        m_h = Regress.ols(ret, f)
        phi = coef(m_h)
        r_temp = copy(ret.r)

        for k = 1:h
            z = vcat(1, r_temp[end:-1:end-j+1])
            fh[k] = z'phi
            push!(r_temp, fh[k])                 # to append the forecast in order to iterate the processuntil h=3
        end
        r_hat_h3[i,j] = r_temp[end]
        forc_err_h3[i,j] = r_hat_h3[i,j] - df.r[t+3] 
        
        msfe_ar2_h3[j] = sum(forc_err_h3[:,j].^2)/P
        
    end
end

p_grid = 1:12

results_msfe_h3 = DataFrame(
    p = p_grid,
    msfe_standard_h3 = vec(msfe_ar2_h3),
    msfe_histmean_h3 = vec(msfe_from_mean_h3)
)

# the optimal lag order
pstar_standard_h3 = results_msfe_h3.p[argmin(results_msfe_h3.msfe_standard_h3)]
pstar_histmean_h3 = results_msfe_h3.p[argmin(results_msfe_h3.msfe_histmean_h3)]

println("Optimal p that minimize MSFE (3 periods ahead) is: ", pstar_standard_h3)

# comparing msfe h=1 and h=3 at respective minimum
msfe_h1 = sum(forc_err_p[end-58:end,9].^2)/58
msfe_h3 = sum(forc_err_h3[:,9].^2)/58

# Create a DataFrame summarizing the difference
difference = msfe_h3 - msfe_h1
summary_df = DataFrame(
    Horizon = ["1-step", "3-step"],
    MSFE = [msfe_h1, msfe_h3],
    Difference = [0.0, difference]
)
println("Summary of MSFE differences:")
println(summary_df)

## Ex 5: Diebold–Mariano test
P = 60
j = 9                                  # p star
d = fe_p[:, j].^2 - dfm_p[:, j].^2
d_bar = mean(d)
var_d = var(d)
DM = d_bar / sqrt(var_d / P)
abs_DM = abs(DM)
critical = 1.96
reject_H0 = abs_DM > critical
println("Diebold–Mariano statistic (p=9, h=1): ", DM)
println("|DM| = ", round(abs_DM,digits=3))
println("test at 5%: ", round(abs_DM,digits=3), " > ", critical, " => reject H0? ", reject_H0)

d = Normal()
p_value = 2 * ccdf(d, abs_DM)

println("p-value DM test: ", round(p_value, digits=2))