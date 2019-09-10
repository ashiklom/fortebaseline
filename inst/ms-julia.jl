ipft = [2 1 1]
L = [1 1.5 2]

ncoh = length(ipft)

lr = [0.045 0.056 0.056]
lt = [0.025 0.046 0.046]
orient = [0.2 0.1][ipft]

theta = 15
S0_beam = 0.8
S0_diff = 1 - S0_beam
alb_ground = 0.1

nu = lr + lt
omega = nu .+ 0.25 .* (lr .- lt) .* (1 .- orient) .^ 2 ./ (2 .* nu)

phi1 = ...
phi2 = ...

a = [1 - alb_ground ]
