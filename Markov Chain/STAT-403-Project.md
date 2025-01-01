Project Sim
================
Andrew Liawan
05/04/2024

# Creating a Function That Simulates Discrete Markov Chain of r Component Parameters with m Possible States Observed over t time periods/iterations

``` r
# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters,num.states) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)

  # initialize variable for first state 
  states[1]    <- sample(seq(1,num.states,1),1)

  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1],]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}
```

# Simulation of Component Data

Component Parameters (r) = 1, 2, 3, … , 499, 750

Time periods (t) = 1, 2, 3, … , 99, 100

States (m) = 1, 2, 3, 4

## Stationary Process from beginning to end

``` r
set.seed(403-560)

# P matrix of pij transition probabilities to simulate chains
num.states = 4
P <- t(matrix(c(0.35,0.55,0.06,0.04,
                0.20,0.30,0.40,0.10,
                0.50,0.30,0.20,0.00,
                0.00,0.00,0.70,0.30),nrow=num.states,ncol=num.states))

# Number of Electronic Components (r)
num.comp = 750

# Number of Iterations/Time periods (t)
time = 101 ## t+1 to take account of initial state of component

# Simulating r Markov Processes
chain <- matrix(NA,ncol=num.comp,nrow=time)
for (c in seq_len(num.comp)){
  chain[,c] <- run.mc.sim(P,time,num.states)
}

# Markov Chain of the First Component
chain[,1]
```

    ##   [1] 2 3 1 2 4 4 3 3 2 4 3 2 2 2 3 1 2 3 1 2 4 3 2 3 1 1 1 1 2 4 3 2 1 2 2 2 2
    ##  [38] 2 3 1 1 1 4 4 3 1 2 1 1 1 2 3 1 2 3 2 2 3 1 1 3 1 2 1 3 1 2 4 4 3 2 3 1 1
    ##  [75] 2 3 1 1 2 3 2 2 4 4 3 3 3 1 1 2 4 3 2 3 1 1 1 1 1 1 2

``` r
# Number of Observations of each transition (nij)
N <- list()
for (t in 1:(time-1)){
  N[[t]] <- paste(chain[t,],chain[t+1,],sep="")
}
n <- list("n11"=NULL,"n12"=NULL,"n13"=NULL,"n14"=NULL,
          "n21"=NULL,"n22"=NULL,"n23"=NULL,"n24"=NULL,
          "n31"=NULL,"n32"=NULL,"n33"=NULL,"n34"=NULL,
          "n41"=NULL,"n42"=NULL,"n43"=NULL,"n44"=NULL)
for (i in 1:(time-1)){
  n$n11[i] <- length(N[[i]][N[[i]]=="11"])
  n$n12[i] <- length(N[[i]][N[[i]]=="12"])
  n$n13[i] <- length(N[[i]][N[[i]]=="13"])
  n$n14[i] <- length(N[[i]][N[[i]]=="14"])
  n$n21[i] <- length(N[[i]][N[[i]]=="21"])
  n$n22[i] <- length(N[[i]][N[[i]]=="22"])
  n$n23[i] <- length(N[[i]][N[[i]]=="23"])
  n$n24[i] <- length(N[[i]][N[[i]]=="24"])
  n$n31[i] <- length(N[[i]][N[[i]]=="31"])
  n$n32[i] <- length(N[[i]][N[[i]]=="32"])
  n$n33[i] <- length(N[[i]][N[[i]]=="33"])
  n$n34[i] <- length(N[[i]][N[[i]]=="34"])
  n$n41[i] <- length(N[[i]][N[[i]]=="41"])
  n$n42[i] <- length(N[[i]][N[[i]]=="42"])
  n$n43[i] <- length(N[[i]][N[[i]]=="43"])
  n$n44[i] <- length(N[[i]][N[[i]]=="44"])
}

n$n11
```

    ##   [1] 68 67 83 86 84 82 80 68 59 77 76 93 78 84 84 95 97 74 87 71 53 78 81 83 90
    ##  [26] 97 95 87 83 75 98 85 77 99 70 73 80 65 71 92 87 83 88 97 87 93 91 78 86 86
    ##  [51] 77 86 97 96 87 84 86 63 70 73 82 90 83 86 83 82 97 86 89 76 83 82 70 76 76
    ##  [76] 81 97 87 84 74 72 90 90 73 72 75 71 83 94 98 75 73 89 94 87 92 80 90 74 79

``` r
# Estimates of Transition Probabilities (MLE)

## Under Ho : Stationary
phat_ij <- NULL
for (i in 1:16){
 phat_ij[i] <- sum(n[[i]])/length(chain[1:(time-1),][chain[1:(time-1),]==ceiling(i/4)])
}

round(phat_ij,3)
```

    ##  [1] 0.351 0.544 0.065 0.039 0.202 0.300 0.397 0.101 0.503 0.297 0.200 0.000
    ## [13] 0.000 0.000 0.704 0.296

``` r
## Under Ha : Not Stationary
phat_ijt <- list("phat11"=NULL,"phat12"=NULL,"phat13"=NULL,"phat14"=NULL,
          "phat21"=NULL,"phat22"=NULL,"phat23"=NULL,"phat24"=NULL,
          "phat31"=NULL,"phat32"=NULL,"phat33"=NULL,"phat34"=NULL,
          "phat41"=NULL,"phat42"=NULL,"phat43"=NULL,"phat44"=NULL) 
for (t in 1:(time-1)){
  phat_ijt$phat11[t] <- n[[1]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat12[t] <- n[[2]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat13[t] <- n[[3]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat14[t] <- n[[4]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat21[t] <- n[[5]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat22[t] <- n[[6]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat23[t] <- n[[7]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat24[t] <- n[[8]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat31[t] <- n[[9]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat32[t] <- n[[10]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat33[t] <- n[[11]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat34[t] <- n[[12]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat41[t] <- n[[13]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat42[t] <- n[[14]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat43[t] <- n[[15]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat44[t] <- n[[16]][t]/length(chain[t,][chain[t,]==4])
}

round(phat_ijt$phat11,3)
```

    ##   [1] 0.364 0.333 0.362 0.357 0.362 0.337 0.340 0.292 0.276 0.355 0.390 0.355
    ##  [13] 0.344 0.364 0.332 0.408 0.406 0.319 0.367 0.285 0.275 0.375 0.339 0.353
    ##  [25] 0.357 0.404 0.375 0.366 0.336 0.341 0.407 0.331 0.316 0.409 0.278 0.346
    ##  [37] 0.374 0.273 0.360 0.388 0.348 0.342 0.378 0.390 0.370 0.388 0.357 0.338
    ##  [49] 0.379 0.352 0.318 0.376 0.377 0.375 0.355 0.357 0.343 0.272 0.337 0.315
    ##  [61] 0.374 0.381 0.362 0.374 0.324 0.380 0.377 0.355 0.346 0.323 0.347 0.366
    ##  [73] 0.287 0.333 0.313 0.372 0.418 0.355 0.349 0.336 0.329 0.385 0.363 0.313
    ##  [85] 0.317 0.357 0.316 0.362 0.390 0.397 0.287 0.348 0.389 0.376 0.331 0.398
    ##  [97] 0.329 0.391 0.315 0.332

``` r
# Likelihood Ratio
L <- prod(c((phat_ij[1]/phat_ijt$phat11)^n$n11,
       (phat_ij[2]/phat_ijt$phat12)^n$n12,
       (phat_ij[3]/phat_ijt$phat13)^n$n13,
       (phat_ij[4]/phat_ijt$phat14)^n$n14,
       (phat_ij[5]/phat_ijt$phat21)^n$n21,
       (phat_ij[6]/phat_ijt$phat22)^n$n22,
       (phat_ij[7]/phat_ijt$phat23)^n$n23,
       (phat_ij[8]/phat_ijt$phat24)^n$n24,
       (phat_ij[9]/phat_ijt$phat31)^n$n31,
       (phat_ij[10]/phat_ijt$phat32)^n$n32,
       (phat_ij[11]/phat_ijt$phat33)^n$n33,
       (phat_ij[12]/phat_ijt$phat34)^n$n34,
       (phat_ij[13]/phat_ijt$phat41)^n$n41,
       (phat_ij[14]/phat_ijt$phat42)^n$n42,
       (phat_ij[15]/phat_ijt$phat43)^n$n43,
       (phat_ij[16]/phat_ijt$phat44)^n$n44))

#Chi-Squared Test
chisq <- -2*log(L)
df <- (time-2)*num.states*(num.states-1)
chisq
```

    ## [1] 939.0055

``` r
qchisq(0.05,df,lower.tail=F)
```

    ## [1] 1269.298

## Stationary process from beginning to t = 90, then random samling of states from t = 91 to end

``` r
set.seed(403-560)

# P matrix of pij transition probabilities to simulate chains
num.states = 4
P <- t(matrix(c(0.35,0.55,0.06,0.04,
                0.20,0.30,0.40,0.10,
                0.50,0.30,0.20,0.00,
                0.00,0.00,0.70,0.30),nrow=num.states,ncol=num.states))

# Number of Electronic Components (r)
num.comp = 750

# Number of Iterations/Time periods (t)
time = 101 ## t+1 to take account of initial state of component
time_change = 91

# Simulating r Markov Processes
chain <- matrix(NA,ncol=num.comp,nrow=time)
for (c in seq_len(num.comp)){
  chain[,c] <- c(run.mc.sim(P,time_change,num.states),
                 sample(1:4,time-time_change,replace = T))
}

# Markov Chain of the First Component
chain[,1]
```

    ##   [1] 2 3 1 2 4 4 3 3 2 4 3 2 2 2 3 1 2 3 1 2 4 3 2 3 1 1 1 1 2 4 3 2 1 2 2 2 2
    ##  [38] 2 3 1 1 1 4 4 3 1 2 1 1 1 2 3 1 2 3 2 2 3 1 1 3 1 2 1 3 1 2 4 4 3 2 3 1 1
    ##  [75] 2 3 1 1 2 3 2 2 4 4 3 3 3 1 1 2 4 1 3 2 3 2 2 1 3 4 1

``` r
# Number of Observations of each transition (nij)
N <- list()
for (t in 1:(time-1)){
  N[[t]] <- paste(chain[t,],chain[t+1,],sep="")
}
n <- list("n11"=NULL,"n12"=NULL,"n13"=NULL,"n14"=NULL,
          "n21"=NULL,"n22"=NULL,"n23"=NULL,"n24"=NULL,
          "n31"=NULL,"n32"=NULL,"n33"=NULL,"n34"=NULL,
          "n41"=NULL,"n42"=NULL,"n43"=NULL,"n44"=NULL)
for (i in 1:(time-1)){
  n$n11[i] <- length(N[[i]][N[[i]]=="11"])
  n$n12[i] <- length(N[[i]][N[[i]]=="12"])
  n$n13[i] <- length(N[[i]][N[[i]]=="13"])
  n$n14[i] <- length(N[[i]][N[[i]]=="14"])
  n$n21[i] <- length(N[[i]][N[[i]]=="21"])
  n$n22[i] <- length(N[[i]][N[[i]]=="22"])
  n$n23[i] <- length(N[[i]][N[[i]]=="23"])
  n$n24[i] <- length(N[[i]][N[[i]]=="24"])
  n$n31[i] <- length(N[[i]][N[[i]]=="31"])
  n$n32[i] <- length(N[[i]][N[[i]]=="32"])
  n$n33[i] <- length(N[[i]][N[[i]]=="33"])
  n$n34[i] <- length(N[[i]][N[[i]]=="34"])
  n$n41[i] <- length(N[[i]][N[[i]]=="41"])
  n$n42[i] <- length(N[[i]][N[[i]]=="42"])
  n$n43[i] <- length(N[[i]][N[[i]]=="43"])
  n$n44[i] <- length(N[[i]][N[[i]]=="44"])
}

n$n11
```

    ##   [1]  68  67  81  80  88  92  79  75  64  68  69  79  90  94  91  65  66  76
    ##  [19]  86  72  65  83  69  71  74  91  89  87  78  69  90  87  74  72  76  81
    ##  [37]  84  76  74  71  77  75  83  81 105  97  99  88  89  87  83  82  92  87
    ##  [55] 103  95  88  88  97  84  69  71  83  72  62  93  89  86  91  69  80  90
    ##  [73]  82  91  84  74  74  67  74  80  87  97  97  85  78  81  80  79  81  78
    ##  [91]  55  41  41  49  49  51  57  40  44  47

``` r
# Estimates of Transition Probabilities (MLE)

## Under Ho : Stationary
phat_ij <- NULL
for (i in 1:16){
 phat_ij[i] <- sum(n[[i]])/length(chain[1:(time-1),][chain[1:(time-1),]==ceiling(i/4)])
}

round(phat_ij,3)
```

    ##  [1] 0.339 0.524 0.080 0.057 0.204 0.296 0.388 0.113 0.481 0.290 0.205 0.024
    ## [13] 0.066 0.071 0.578 0.285

``` r
## Under Ha : Not Stationary
phat_ijt <- list("phat11"=NULL,"phat12"=NULL,"phat13"=NULL,"phat14"=NULL,
          "phat21"=NULL,"phat22"=NULL,"phat23"=NULL,"phat24"=NULL,
          "phat31"=NULL,"phat32"=NULL,"phat33"=NULL,"phat34"=NULL,
          "phat41"=NULL,"phat42"=NULL,"phat43"=NULL,"phat44"=NULL) 
for (t in 1:(time-1)){
  phat_ijt$phat11[t] <- n[[1]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat12[t] <- n[[2]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat13[t] <- n[[3]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat14[t] <- n[[4]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat21[t] <- n[[5]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat22[t] <- n[[6]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat23[t] <- n[[7]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat24[t] <- n[[8]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat31[t] <- n[[9]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat32[t] <- n[[10]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat33[t] <- n[[11]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat34[t] <- n[[12]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat41[t] <- n[[13]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat42[t] <- n[[14]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat43[t] <- n[[15]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat44[t] <- n[[16]][t]/length(chain[t,][chain[t,]==4])
}

round(phat_ijt$phat11,3)
```

    ##   [1] 0.343 0.332 0.342 0.339 0.370 0.387 0.326 0.338 0.294 0.322 0.300 0.350
    ##  [13] 0.385 0.388 0.355 0.278 0.351 0.336 0.368 0.283 0.323 0.358 0.309 0.317
    ##  [25] 0.354 0.397 0.365 0.347 0.338 0.325 0.396 0.355 0.302 0.348 0.329 0.354
    ##  [37] 0.354 0.303 0.346 0.332 0.332 0.350 0.353 0.338 0.410 0.385 0.406 0.337
    ##  [49] 0.359 0.355 0.339 0.343 0.385 0.331 0.435 0.352 0.367 0.386 0.375 0.335
    ##  [61] 0.319 0.305 0.342 0.305 0.288 0.408 0.366 0.364 0.367 0.297 0.365 0.370
    ##  [73] 0.324 0.401 0.346 0.315 0.319 0.299 0.368 0.343 0.380 0.404 0.382 0.357
    ##  [85] 0.321 0.342 0.351 0.338 0.333 0.332 0.240 0.230 0.225 0.272 0.255 0.255
    ##  [97] 0.282 0.222 0.246 0.261

``` r
# Likelihood Ratio
L <- prod(c((phat_ij[1]/phat_ijt$phat11)^n$n11,
       (phat_ij[2]/phat_ijt$phat12)^n$n12,
       (phat_ij[3]/phat_ijt$phat13)^n$n13,
       (phat_ij[4]/phat_ijt$phat14)^n$n14,
       (phat_ij[5]/phat_ijt$phat21)^n$n21,
       (phat_ij[6]/phat_ijt$phat22)^n$n22,
       (phat_ij[7]/phat_ijt$phat23)^n$n23,
       (phat_ij[8]/phat_ijt$phat24)^n$n24,
       (phat_ij[9]/phat_ijt$phat31)^n$n31,
       (phat_ij[10]/phat_ijt$phat32)^n$n32,
       (phat_ij[11]/phat_ijt$phat33)^n$n33,
       (phat_ij[12]/phat_ijt$phat34)^n$n34,
       (phat_ij[13]/phat_ijt$phat41)^n$n41,
       (phat_ij[14]/phat_ijt$phat42)^n$n42,
       (phat_ij[15]/phat_ijt$phat43)^n$n43,
       (phat_ij[16]/phat_ijt$phat44)^n$n44))

#Chi-Squared Test
chisq <- -2*log(L)
df <- (time-2)*num.states*(num.states-1)
chisq
```

    ## [1] Inf

``` r
qchisq(0.05,df,lower.tail=F)
```

    ## [1] 1269.298

## Stationary process of P1 Transition Matrix from beginning to t = 90, then stationary process of P2 Transition Matrix from t = 91 to end

``` r
set.seed(403-560)

# P matrix of pij transition probabilities to simulate chains
num.states = 4
P1 <- t(matrix(c(0.35,0.55,0.06,0.04,
                0.20,0.30,0.40,0.10,
                0.50,0.30,0.20,0.00,
                0.00,0.00,0.70,0.30),nrow=num.states,ncol=num.states))
P2 <- t(matrix(c(0.34,0.56,0.05,0.05,
                0.21,0.29,0.39,0.11,
                0.49,0.31,0.19,0.01,
                0.01,0.01,0.69,0.29),nrow=num.states,ncol=num.states))

# Number of Electronic Components (r)
num.comp = 750

# Number of Iterations/Time periods (t)
time = 101 ## t+1 to take account of initial state of component
time_change = 91

# Simulating r Markov Processes
chain <- matrix(NA,ncol=num.comp,nrow=time)
for (c in seq_len(num.comp)){
  chain[,c] <- c(run.mc.sim(P1,time_change,num.states),
                 run.mc.sim(P2,time-time_change,num.states))
}

# Markov Chain of the First Component
chain[,1]
```

    ##   [1] 2 3 1 2 4 4 3 3 2 4 3 2 2 2 3 1 2 3 1 2 4 3 2 3 1 1 1 1 2 4 3 2 1 2 2 2 2
    ##  [38] 2 3 1 1 1 4 4 3 1 2 1 1 1 2 3 1 2 3 2 2 3 1 1 3 1 2 1 3 1 2 4 4 3 2 3 1 1
    ##  [75] 2 3 1 1 2 3 2 2 4 4 3 3 3 1 1 2 4 1 2 3 1 1 1 1 1 1 2

``` r
# Number of Observations of each transition (nij)
N <- list()
for (t in 1:(time-1)){
  N[[t]] <- paste(chain[t,],chain[t+1,],sep="")
}
n <- list("n11"=NULL,"n12"=NULL,"n13"=NULL,"n14"=NULL,
          "n21"=NULL,"n22"=NULL,"n23"=NULL,"n24"=NULL,
          "n31"=NULL,"n32"=NULL,"n33"=NULL,"n34"=NULL,
          "n41"=NULL,"n42"=NULL,"n43"=NULL,"n44"=NULL)
for (i in 1:(time-1)){
  n$n11[i] <- length(N[[i]][N[[i]]=="11"])
  n$n12[i] <- length(N[[i]][N[[i]]=="12"])
  n$n13[i] <- length(N[[i]][N[[i]]=="13"])
  n$n14[i] <- length(N[[i]][N[[i]]=="14"])
  n$n21[i] <- length(N[[i]][N[[i]]=="21"])
  n$n22[i] <- length(N[[i]][N[[i]]=="22"])
  n$n23[i] <- length(N[[i]][N[[i]]=="23"])
  n$n24[i] <- length(N[[i]][N[[i]]=="24"])
  n$n31[i] <- length(N[[i]][N[[i]]=="31"])
  n$n32[i] <- length(N[[i]][N[[i]]=="32"])
  n$n33[i] <- length(N[[i]][N[[i]]=="33"])
  n$n34[i] <- length(N[[i]][N[[i]]=="34"])
  n$n41[i] <- length(N[[i]][N[[i]]=="41"])
  n$n42[i] <- length(N[[i]][N[[i]]=="42"])
  n$n43[i] <- length(N[[i]][N[[i]]=="43"])
  n$n44[i] <- length(N[[i]][N[[i]]=="44"])
}

n$n11
```

    ##   [1]  72  74  91  75  74  88  72  77  71  75  87  87  76  80  87  85  78  85
    ##  [19]  85 102  91  77  92  92  80  76  85  90  83 102  92  99  78  82  87 100
    ##  [37]  95  89  87  86  84 101  79  89  75  92  90  80  86  83  87  79  78  75
    ##  [55]  64  74  74  78  85  76  85 101  85  92  78  72  82  87  88  76  80  85
    ##  [73]  78  81  73  83  72  75  85  96  84  85  80  81  81  79  72  78  79  89
    ##  [91]  72  59  51  68  79  84  86  86  79  77

``` r
# Estimates of Transition Probabilities (MLE)

## Under Ho : Stationary
phat_ij <- NULL
for (i in 1:16){
 phat_ij[i] <- sum(n[[i]])/length(chain[1:(time-1),][chain[1:(time-1),]==ceiling(i/4)])
}

round(phat_ij,3)
```

    ##  [1] 0.351 0.542 0.064 0.042 0.204 0.298 0.396 0.102 0.498 0.298 0.199 0.004
    ## [13] 0.004 0.004 0.692 0.301

``` r
## Under Ha : Not Stationary
phat_ijt <- list("phat11"=NULL,"phat12"=NULL,"phat13"=NULL,"phat14"=NULL,
          "phat21"=NULL,"phat22"=NULL,"phat23"=NULL,"phat24"=NULL,
          "phat31"=NULL,"phat32"=NULL,"phat33"=NULL,"phat34"=NULL,
          "phat41"=NULL,"phat42"=NULL,"phat43"=NULL,"phat44"=NULL) 
for (t in 1:(time-1)){
  phat_ijt$phat11[t] <- n[[1]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat12[t] <- n[[2]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat13[t] <- n[[3]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat14[t] <- n[[4]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat21[t] <- n[[5]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat22[t] <- n[[6]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat23[t] <- n[[7]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat24[t] <- n[[8]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat31[t] <- n[[9]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat32[t] <- n[[10]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat33[t] <- n[[11]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat34[t] <- n[[12]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat41[t] <- n[[13]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat42[t] <- n[[14]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat43[t] <- n[[15]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat44[t] <- n[[16]][t]/length(chain[t,][chain[t,]==4])
}

round(phat_ijt$phat11,3)
```

    ##   [1] 0.371 0.372 0.370 0.304 0.326 0.393 0.301 0.317 0.327 0.315 0.358 0.331
    ##  [13] 0.326 0.359 0.349 0.376 0.348 0.360 0.341 0.392 0.373 0.330 0.400 0.364
    ##  [25] 0.357 0.347 0.363 0.370 0.356 0.410 0.352 0.414 0.307 0.383 0.385 0.402
    ##  [37] 0.377 0.363 0.373 0.360 0.368 0.401 0.310 0.371 0.316 0.391 0.366 0.327
    ##  [49] 0.389 0.343 0.337 0.366 0.325 0.338 0.294 0.333 0.338 0.348 0.354 0.353
    ##  [61] 0.357 0.409 0.335 0.390 0.345 0.319 0.361 0.378 0.355 0.321 0.364 0.340
    ##  [73] 0.344 0.343 0.327 0.356 0.309 0.328 0.365 0.398 0.351 0.348 0.372 0.335
    ##  [85] 0.346 0.341 0.338 0.351 0.339 0.372 0.306 0.295 0.300 0.289 0.359 0.337
    ##  [97] 0.363 0.354 0.348 0.342

``` r
# Likelihood Ratio
L <- prod(c((phat_ij[1]/phat_ijt$phat11)^n$n11,
       (phat_ij[2]/phat_ijt$phat12)^n$n12,
       (phat_ij[3]/phat_ijt$phat13)^n$n13,
       (phat_ij[4]/phat_ijt$phat14)^n$n14,
       (phat_ij[5]/phat_ijt$phat21)^n$n21,
       (phat_ij[6]/phat_ijt$phat22)^n$n22,
       (phat_ij[7]/phat_ijt$phat23)^n$n23,
       (phat_ij[8]/phat_ijt$phat24)^n$n24,
       (phat_ij[9]/phat_ijt$phat31)^n$n31,
       (phat_ij[10]/phat_ijt$phat32)^n$n32,
       (phat_ij[11]/phat_ijt$phat33)^n$n33,
       (phat_ij[12]/phat_ijt$phat34)^n$n34,
       (phat_ij[13]/phat_ijt$phat41)^n$n41,
       (phat_ij[14]/phat_ijt$phat42)^n$n42,
       (phat_ij[15]/phat_ijt$phat43)^n$n43,
       (phat_ij[16]/phat_ijt$phat44)^n$n44))

#Chi-Squared Test
chisq <- -2*log(L)
df <- (time-2)*num.states*(num.states-1)
chisq
```

    ## [1] Inf

``` r
qchisq(0.05,df,lower.tail=F)
```

    ## [1] 1269.298

## Stationary process of P1 Transition Matrix from beginning to t = 90, then stationary process of P2 Transition Matrix from t = 91 to end (r = 150)

``` r
set.seed(403-560)

# P matrix of pij transition probabilities to simulate chains
num.states = 4
P1 <- t(matrix(c(0.35,0.55,0.06,0.04,
                0.20,0.30,0.40,0.10,
                0.50,0.30,0.20,0.00,
                0.00,0.00,0.70,0.30),nrow=num.states,ncol=num.states))
P2 <- t(matrix(c(0.34,0.56,0.05,0.05,
                0.21,0.29,0.39,0.11,
                0.49,0.31,0.19,0.01,
                0.01,0.01,0.69,0.29),nrow=num.states,ncol=num.states))

# Number of Electronic Components (r)
num.comp = 150

# Number of Iterations/Time periods (t)
time = 101 ## t+1 to take account of initial state of component
time_change = 91

# Simulating r Markov Processes
chain <- matrix(NA,ncol=num.comp,nrow=time)
for (c in seq_len(num.comp)){
  chain[,c] <- c(run.mc.sim(P1,time_change,num.states),
                 run.mc.sim(P2,time-time_change,num.states))
}

# Markov Chain of the First Component
chain[,1]
```

    ##   [1] 2 3 1 2 4 4 3 3 2 4 3 2 2 2 3 1 2 3 1 2 4 3 2 3 1 1 1 1 2 4 3 2 1 2 2 2 2
    ##  [38] 2 3 1 1 1 4 4 3 1 2 1 1 1 2 3 1 2 3 2 2 3 1 1 3 1 2 1 3 1 2 4 4 3 2 3 1 1
    ##  [75] 2 3 1 1 2 3 2 2 4 4 3 3 3 1 1 2 4 1 2 3 1 1 1 1 1 1 2

``` r
# Number of Observations of each transition (nij)
N <- list()
for (t in 1:(time-1)){
  N[[t]] <- paste(chain[t,],chain[t+1,],sep="")
}
n <- list("n11"=NULL,"n12"=NULL,"n13"=NULL,"n14"=NULL,
          "n21"=NULL,"n22"=NULL,"n23"=NULL,"n24"=NULL,
          "n31"=NULL,"n32"=NULL,"n33"=NULL,"n34"=NULL,
          "n41"=NULL,"n42"=NULL,"n43"=NULL,"n44"=NULL)
for (i in 1:(time-1)){
  n$n11[i] <- length(N[[i]][N[[i]]=="11"])
  n$n12[i] <- length(N[[i]][N[[i]]=="12"])
  n$n13[i] <- length(N[[i]][N[[i]]=="13"])
  n$n14[i] <- length(N[[i]][N[[i]]=="14"])
  n$n21[i] <- length(N[[i]][N[[i]]=="21"])
  n$n22[i] <- length(N[[i]][N[[i]]=="22"])
  n$n23[i] <- length(N[[i]][N[[i]]=="23"])
  n$n24[i] <- length(N[[i]][N[[i]]=="24"])
  n$n31[i] <- length(N[[i]][N[[i]]=="31"])
  n$n32[i] <- length(N[[i]][N[[i]]=="32"])
  n$n33[i] <- length(N[[i]][N[[i]]=="33"])
  n$n34[i] <- length(N[[i]][N[[i]]=="34"])
  n$n41[i] <- length(N[[i]][N[[i]]=="41"])
  n$n42[i] <- length(N[[i]][N[[i]]=="42"])
  n$n43[i] <- length(N[[i]][N[[i]]=="43"])
  n$n44[i] <- length(N[[i]][N[[i]]=="44"])
}

n$n11
```

    ##   [1] 15 11 11 13 13 15 16 15 16 13 14 18 12 15 20 16 11 14 18 21 17 21 23 16 16
    ##  [26] 16 18 23 17 23 16 21 13 17 19 20 21 16 12 17 20 28 10 14 14 21 17 17 17 15
    ##  [51] 20 21 19 10 11 17 12 13 17 18 16 23 19 15 12 12 15 18 16 13 13 18 17 18 11
    ##  [76] 21 17 21 27 20 16 13 15 19 18 17 12 13 16 28 19 12 11 15 12 18 20 27 24 16

``` r
# Estimates of Transition Probabilities (MLE)

## Under Ho : Stationary
phat_ij <- NULL
for (i in 1:16){
 phat_ij[i] <- sum(n[[i]])/length(chain[1:(time-1),][chain[1:(time-1),]==ceiling(i/4)])
}

round(phat_ij,3)
```

    ##  [1] 0.357 0.538 0.066 0.039 0.196 0.294 0.405 0.105 0.494 0.291 0.210 0.005
    ## [13] 0.005 0.005 0.687 0.304

``` r
## Under Ha : Not Stationary
phat_ijt <- list("phat11"=NULL,"phat12"=NULL,"phat13"=NULL,"phat14"=NULL,
          "phat21"=NULL,"phat22"=NULL,"phat23"=NULL,"phat24"=NULL,
          "phat31"=NULL,"phat32"=NULL,"phat33"=NULL,"phat34"=NULL,
          "phat41"=NULL,"phat42"=NULL,"phat43"=NULL,"phat44"=NULL) 
for (t in 1:(time-1)){
  phat_ijt$phat11[t] <- n[[1]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat12[t] <- n[[2]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat13[t] <- n[[3]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat14[t] <- n[[4]][t]/length(chain[t,][chain[t,]==1])
  phat_ijt$phat21[t] <- n[[5]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat22[t] <- n[[6]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat23[t] <- n[[7]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat24[t] <- n[[8]][t]/length(chain[t,][chain[t,]==2])
  phat_ijt$phat31[t] <- n[[9]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat32[t] <- n[[10]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat33[t] <- n[[11]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat34[t] <- n[[12]][t]/length(chain[t,][chain[t,]==3])
  phat_ijt$phat41[t] <- n[[13]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat42[t] <- n[[14]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat43[t] <- n[[15]][t]/length(chain[t,][chain[t,]==4])
  phat_ijt$phat44[t] <- n[[16]][t]/length(chain[t,][chain[t,]==4])
}

round(phat_ijt$phat11,3)
```

    ##   [1] 0.366 0.262 0.268 0.289 0.302 0.385 0.340 0.366 0.320 0.228 0.298 0.327
    ##  [13] 0.240 0.455 0.400 0.302 0.289 0.341 0.360 0.389 0.370 0.512 0.426 0.296
    ##  [25] 0.372 0.327 0.367 0.469 0.378 0.442 0.327 0.375 0.265 0.362 0.463 0.426
    ##  [37] 0.420 0.327 0.245 0.447 0.385 0.519 0.169 0.378 0.341 0.420 0.405 0.315
    ##  [49] 0.370 0.333 0.370 0.438 0.365 0.256 0.282 0.405 0.261 0.333 0.405 0.391
    ##  [61] 0.327 0.442 0.365 0.300 0.324 0.293 0.312 0.429 0.314 0.325 0.317 0.367
    ##  [73] 0.354 0.375 0.268 0.438 0.327 0.438 0.482 0.370 0.348 0.295 0.326 0.432
    ##  [85] 0.316 0.447 0.267 0.310 0.372 0.500 0.333 0.279 0.314 0.349 0.240 0.474
    ##  [97] 0.392 0.519 0.453 0.314

``` r
# Likelihood Ratio
L <- prod(c((phat_ij[1]/phat_ijt$phat11)^n$n11,
       (phat_ij[2]/phat_ijt$phat12)^n$n12,
       (phat_ij[3]/phat_ijt$phat13)^n$n13,
       (phat_ij[4]/phat_ijt$phat14)^n$n14,
       (phat_ij[5]/phat_ijt$phat21)^n$n21,
       (phat_ij[6]/phat_ijt$phat22)^n$n22,
       (phat_ij[7]/phat_ijt$phat23)^n$n23,
       (phat_ij[8]/phat_ijt$phat24)^n$n24,
       (phat_ij[9]/phat_ijt$phat31)^n$n31,
       (phat_ij[10]/phat_ijt$phat32)^n$n32,
       (phat_ij[11]/phat_ijt$phat33)^n$n33,
       (phat_ij[12]/phat_ijt$phat34)^n$n34,
       (phat_ij[13]/phat_ijt$phat41)^n$n41,
       (phat_ij[14]/phat_ijt$phat42)^n$n42,
       (phat_ij[15]/phat_ijt$phat43)^n$n43,
       (phat_ij[16]/phat_ijt$phat44)^n$n44))

#Chi-Squared Test
chisq <- -2*log(L)
df <- (time-2)*num.states*(num.states-1)
chisq
```

    ## [1] 1187.706

``` r
qchisq(0.05,df,lower.tail=F)
```

    ## [1] 1269.298
