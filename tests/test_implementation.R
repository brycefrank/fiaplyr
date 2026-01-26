source("R/EvalHandler.R")
source("R/PostStratifiedEstimator.R")
source("R/PostStratifiedRatioEstimator.R")
source("R/utils-formula.R")

# Test parse_formula
print("Testing parse_formula...")

f1 <- tree ~ VOLCFGRS
p1 <- parse_formula(f1)
print(paste("f1:", p1$slot, paste(p1$targets, collapse = ", ")))

f2 <- tree ~ VOLCFGRS | VOLCFNET
p2 <- parse_formula(f2)
print(paste("f2:", p2$slot, paste(p2$targets, collapse = ", ")))

f3 <- cond ~ 1
p3 <- parse_formula(f3)
print(paste("f3:", p3$slot, paste(p3$targets, collapse = ", ")))

f4 <- tree ~ a | b | c
p4 <- parse_formula(f4)
print(paste("f4:", p4$slot, paste(p4$targets, collapse = ", ")))

# Test functionality if possible (mocking objects)
# Creating dummy EvalHandler is hard without DB connection.
# But we can check class definition exists.
print("Checking class definition...")
res <- tryCatch(getClass("PostStratifiedRatioEstimator"), error = function(e) e)
print(res)

print("Done.")
