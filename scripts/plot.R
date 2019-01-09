library(igraph)
disline <- function(a, b, Time) {
    abline(v = Time[a], lty = 2, col = "blue")
    abline(v = Time[b], lty = 5, col = "brown")
}

gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

Time <- seq(tmax)
totnum <- rowSums(N)
maxnum <- which.max(totnum)
maxbio <- which.max(totb[, 2])
ylim <- c(1, max(totnum))
fint <- paste("plots/com_plot", fine, ".pdf", sep = "")
pdf(file = fint, width = 12, height = 9)
par(mfrow = c(2, 3), bty = "n", mar = c(3, 0, 0, 0) + 2.6,
    oma = c(0, 3, 0, 0))
xlb <- "Time"
mycol <-  gg_color_hue(length(Species))
mysty <- ifelse(Species %in% producers, 3, 1)
plot(Time, totnum, type = "l", xlim = c(0, tmax), ylim = ylim, ylab = "",
     axes = F, xlab = xlb)
for (i in seq(Species)) {
    lines(Time, N[, i], col = mycol[i], lwd = 1, lty = mysty[i])
}
axis(1, cex.axis = 1.4)
axis(2, las = 2, cex.axis = 1.4)
title("Total number", cex.main = 2)

ylim <- range(totb[, 2])
plot(totb, type = "l", ylim = ylim, ylab = "", axes = F, xlab = xlb)
axis(1, cex.axis = 1.4)
axis(2, las = 2, cex.axis = 1.4)
title("Total biomass", cex.main = 2)


plot(0, 0, type = "l", xlim = c(0, tmax), ylim = range(sp.totb, na.rm = T),
     ylab = "", axes = F, xlab = xlb)
for (i in seq(Species)) {
    lines(Time, sp.totb[, i], col = mycol[i], lwd = 1, lty = mysty[i])
}
axis(1, cex.axis = 1.4)
axis(2, las = 2, cex.axis = 1.4)
title("Tot. biomass per species", cex.main = 2)

plot(0, 0, type = "l", xlim = c(0, tmax), ylim = range(sp.aveb, na.rm = T),
     ylab = "", axes = F, xlab = xlb)
for (i in seq(Species)) {
    lines(Time, sp.aveb[, i], col = mycol[i], lwd = 1, lty = mysty[i])
}
axis(1, cex.axis = 1.4)
axis(2, las = 2, cex.axis = 1.4)
title("avg. biomass per species", cex.main = 2)

g <- graph_from_adjacency_matrix(giug, weighted = NULL, mode = "directed")
lay <- layout_in_circle(g)
V(g)$color <- mycol
plot(g, layout = lay, edge.arrow.size = 0.5)
title("Trophic network", cex.main = 2)
consumers <- Species[!(Species %in% producers)]

#alt barplot(c(length(producers), length(consumers)), horiz = T,
#        xlab = "number of species", cex.axis = 1.4,
#        col = hcl(h = c(100, 30), l = 65, c = 100), border = NA)
#alt text(length(consumers) / 2, 1.85, "consumers", col = "white", cex  = 3)
#alt text(length(producers) / 2, 0.7, "producers",  col = "white", cex  = 3)
# plot nutrient evolution
ylim1 <- range(Nu.dyn[, 1], na.rm = T)
plot(Time, Nu.dyn[, 1], type = "l", ylim = ylim1)
points(Time, Nu.dyn[, 2], col = "red", type = "l")
dev.off()
