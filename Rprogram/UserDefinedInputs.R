###################################################
# UserDefinedInputs.R
# User Defined
################################################### 
tradedays = 3045

nstock = 4
nexpir = 3 
expir = c(3/12,6/12,12/12) # month expiration

##### Construct a portfolio

# sl - long stocks
# ss - short stocks
# cl - long calls
# cs - short calls
# pl - long puts
# ps - short puts

init = 10000
liquid = 0.01
sl = c(1,1,1,0)*init*(1-liquid)
ss = c(0,0,0,1)*(init/2)*(1-liquid)
cl = cbind(c(0,0,1),c(0,0,0),c(0,0,0),c(0,0,0))*init*liquid 
cs = cbind(c(1,0,0),c(0,0,0),c(0,0,0),c(0,0,0))*init*liquid
pl = cbind(c(0,0,0),c(0,0,0),c(0,1,0),c(0,0,0))*init*liquid
ps = cbind(c(0,0,0),c(0,0,0),c(0,0,0),c(1,0,0))*init*liquid

# invest $10000 on portfolio 
s0 = 10000

# Parameters for VaR modeling

# Maximum tWo Windows each time
win = 252*c(2,3)
win_yr = c("2yr","3yr")
exp_yr = c("2yr","3yr")
lab =exp(log(0.2)/(win))

horizon    = c(5/252)
hor = c("5d")
VaRp       = c(0.99)
ESp        = c(0.975)

