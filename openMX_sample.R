rm(list=ls())

## Path Model

require(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
factorModel <- mxModel("One Factor",
                       type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests,values=0.8),
                       mxPath(from=manifests, arrows=2,values=1),
                       mxPath(from=latents, arrows=2,
                              free=FALSE, values=1.0),
                       mxData(cov(demoOneFactor), type="cov",
                              numObs=500))
summary(factorModelFit <- mxRun(factorModel))

# Matrix Model

require(OpenMx)
data(demoOneFactor)
factorModel <- mxModel("One Factor",
                       mxMatrix("Full", 5, 1, values=0.8,
                                free=TRUE, name="A"),
                       mxMatrix("Symm", 1, 1, values=1,
                                free=FALSE, name="L"),
                       mxMatrix("Diag", 5, 5, values=1,
                                free=TRUE, name="U"),
                       mxAlgebra(A %*% L %*% t(A) + U, name="R"),
                       mxExpectationNormal(covariance = "R",
                                           dimnames = names(demoOneFactor)),
                       mxFitFunctionML(),
                       mxData(cov(demoOneFactor), type="cov", numObs=500))
summary(factorModelFit <- mxRun(factorModel))
