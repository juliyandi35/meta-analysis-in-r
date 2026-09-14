suppressPackageStartupMessages(library(gemtc))
suppressPackageStartupMessages(library(igraph))
data("NetDataGemtc")

network <- mtc.network(data.ab = NetDataGemtc)

plot(network, layout = layout.fruchterman.reingold)

model = mtc.model(network, linearModel = "fixed",
                  n.chain = 4,
                  likelihood = "normal",
                  link = "identity")

mcmc = mtc.run(model, n.adapt = 5000, n.iter = 100000, thin = 10)

rp = rank.probability(mcmc)

sucra = sucra(rp, lower.is.better = TRUE)
sucra
plot(sucra)

# Example 2: construct rank proabability matrix, then use sucra function
rp = rbind(CBT = c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.001500, 0.088025, 0.910475),
           IPT = c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.176975, 0.748300, 0.074725),
           PDT = c(0.000000, 0.000000, 0.000250, 0.021725, 0.976525, 0.001500, 0.000000, 0.000000),
           PLA = c(0.003350, 0.546075, 0.266125, 0.182125, 0.002325, 0.000000, 0.000000, 0.000000),
           PST = c(0.000000, 0.000000, 0.000000, 0.000000, 0.001500, 0.820025, 0.163675, 0.014800),
           SUP = c(0.000000, 0.217450, 0.403950, 0.378000, 0.000600, 0.000000, 0.000000, 0.000000),
           TAU = c(0.000225, 0.232900, 0.329675, 0.418150, 0.019050, 0.000000, 0.000000, 0.000000),
           WLC = c(0.996425, 0.003575, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000))

sucra(rp)
plot(sucra(rp))
