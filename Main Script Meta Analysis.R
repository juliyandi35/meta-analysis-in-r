library(netmeta)
library(readxl)
library(openxlsx)
library(tools)

set.seed(123)

# Buat folder output
output_dir <- "output_netmeta_new"
dir.create(output_dir, showWarnings = FALSE)

# Ambil semua nama sheet
sheet_names <- excel_sheets("Pairwise Dataset.xlsx")

sink("output.txt")       # mulai merekam output ke file output.txt
# Loop semua sheet
for (sheet in sheet_names) {
  cat("\n========== Sheet:", sheet, "==========\n")
  
  # Baca data
  data <- read_excel("Pairwise Dataset.xlsx", sheet = sheet)
  # Ekstrak tahun 4 digit terakhir
  data <- data %>%
    mutate(
      year = str_extract(studlab, "\\b(19|20)\\d{2}\\b"),                 # ambil tahun
      author = str_trim(str_remove(studlab, "\\s*\\(?\\b(19|20)\\d{2}\\)?"))  # hapus tahun
    )
  
  # Buat koneksi jaringan
  nc <- netconnection(treat1, treat2, studlab, data = data)
  cat("Jumlah subnet:", nc$n.subnets, "\n")
  
  for (i in seq_len(nc$n.subnets)) {
    # Subset data untuk subnet ke-i
    data_sub <- data[nc$subnet == i, ]
    
    # Validasi: Pastikan setidaknya ada 2 studi dan tidak semua nilai TE atau seTE adalah NA
    if (nrow(data_sub) < 2 || all(is.na(data_sub$TE)) || all(is.na(data_sub$seTE))) {
      cat(paste0("Lewati subnet ", i, ": Tidak cukup studi atau nilai TE/seTE NA.\n"))
      next
    }
    
    # Lanjutkan meta-analisis
    net <- netmeta(TE, seTE, treat1, treat2, studlab,
                   data = data_sub, sm = "OR")
    
    # Lewati jika gagal
    if (is.null(net)) next
    
    cat("\n--- Subnet", i, "---\n")
    print(summary(net))
    
    # Jika subnet punya > 2 treatment, dan "OD" ada dalam jaringan
    if ("OD" %in% net$treat1 || "OD" %in% net$treat2) {
      ran <- rankogram(net, nsim = 100)
      
      prefix <- file.path(output_dir, paste0(gsub("[^a-zA-Z0-9]", "_", sheet), "_subnet", i))
      
      write.xlsx(ran$ranking.matrix.common, paste0(prefix, "_ranking_matrix.xlsx"), rowNames = TRUE)
      write.xlsx(as.data.frame(ran$ranking.common), paste0(prefix, "_ranking_sucra.xlsx"), rowNames = TRUE)
      
      # Netgraph
      png(filename = paste0(prefix, "_netgraph.png"), width = 900, height = 700)
      netgraph(net,
               plastic = FALSE,
               thickness = "number.of.studies",
               points = TRUE,
               number.of.studies = TRUE)
      dev.off()
      
      # Forest plot
      png(filename = paste0(prefix, "_forest.png"), width = 900, height = 700)
      dat <- escalc(measure="OR", ai=event1, n1i=n1, ci=event2, n2i=n2, data=data,
                    slab=paste(author, year), drop00=TRUE)
      
      res <- rma(yi, vi, data=dat, method="DL")
      pred <- predict(res, transf=exp, digits=2)
      pred <- fmtx(c(pred$pred, pred$ci.lb, pred$ci.ub), digits=2)
      k <- nrow(data)
      options(na.action = "na.pass")
      weights <- paste0(fmtx(weights(res), digits=1), "%")
      weights[weights == "NA%"] <- ""
      
      ### adjust the margins
      par(mar=c(10.9,0,1.8,1.3), mgp=c(3,0.2,0), tcl=-0.2)
      
      ### forest plot dengan extra annotations
      sav <- forest(res, atransf=exp, at=log(c(0.01, 0.10, 1, 10, 100)), xlim=c(-30,11),
                    xlab="", efac=c(0,4), lty=c(1,1,0), refline=NA,
                    ilab=cbind(event1, n1, event2, n2, weights),
                    ilab.xpos=c(-20.6,-18.6,-16.1,-14.1,-10.8), ilab.pos=2,
                    cex=0.78, header=c("Study or Subgroup","IV, Random, 95% CI"), mlab="")
      
      segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)
      
      segments(0, -2, 0, k+1, lwd=0.8)
      
      par(xpd=NA) # allow text outside plot area
      
      par(cex=sav$cex, font=2)
      text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
      text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4])), k+3, pos=2, c("Treat 1","Treat 2"))
      text(sav$textpos[2], k+3, "Odds ratio", pos=2)
      text(0, k+3, "Odds ratio")
      text(0, k+2, "IV, Random, 95% CI")
      text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
           c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
      
      ### first hide the non-bold summary estimate text and then add it back in bold font
      rect(sav$textpos[2], -1.5, sav$ilab.xpos[5], -0.5, col="white", border=NA)
      text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)
      
      par(cex=sav$cex, font=1)
      
      ### add 'Favours caffeine'/'Favours decaf' text below the x-axis
      text(log(c(0.01, 100)), -4, c("Treat 1","Treat 2"), pos=c(4,2), offset=-0.5)
      
      ### add 'Not estimable' for the study with missing log odds ratio
      text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)
      
      ### add text for total events
      text(sav$xlim[1], -2, pos=4, "Total events:")
      text(sav$ilab.xpos[c(1,3)], -2, c(sum(dat$ai),sum(dat$ci)), pos=2)
      
      ### add text with heterogeneity statistics
      text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ",
                                                "Tau"^2, " = ", .(fmtx(res$tau2, digits=2)), "; ",
                                                "Chi"^2, " = ", .(fmtx(res$QE, digits=2)),
                                                ", df = ", .(res$k - res$p),
                                                " (", .(fmtp(res$QEp, digits=2, pname="P", add0=TRUE, sep=TRUE, equal=TRUE)), "); ",
                                                I^2, " = ", .(round(res$I2)), "%")))
      
      ### add text for test of overall effect
      text(sav$xlim[1], -4, pos=4, bquote(paste("Test for overall effect: ",
                                                "Z = ", .(fmtx(res$zval, digits=2)),
                                                " (", .(fmtp(res$pval, digits=2, pname="P", add0=TRUE, sep=TRUE, equal=TRUE)), ")")))
      
      ### add text for test of subgroup differences
      text(sav$xlim[1], -5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))
      
      dev.off()
      
      # Funnel plot
      other_treats <- setdiff(unique(c(net$treat1, net$treat2)), c("OD"))
      ord <- c(sort(other_treats), "OD")
      png(filename = paste0(prefix, "_funnel.png"), width = 900, height = 700)
      funnel(net, order = ord)
      dev.off()
    }
  }
}
sink() 