module LogPolar where

logPolarToCartesian :: RealFloat b => (b, b) -> (b, b)
logPolarToCartesian (rho, theta) = let r = exp rho in (r * cos theta, r * sin theta)

cartesianToLogPolar :: RealFloat b => (b, b) -> (b, b)
cartesianToLogPolar (x,y) = (log . sqrt $ (x**2 + y**2), atan2 y x)
