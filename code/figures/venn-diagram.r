rm(list = ls())

library(ggplot2)
library(broom)

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")
load("../empirical-data/simulation-environment.RData")

library(dplyr)
library(VennDiagram)
library(glmnet)
library(ncvreg)
library(scales)
library(xgboost)


for (i in 1:5) {
  lasso <- models[[i]]$lasso
  enet <- models[[i]]$enet
  mcp <- models[[i]]$mcp
  rf <- models[[i]]$rf
  gbm <- models[[i]]$gbm
  
  lasso_coef <- coef(lasso) %>% as.matrix() %>% as.data.frame()
  colnames(lasso_coef) <- "L"
  lasso_coef$predictor <- rownames(lasso_coef)
  
  enet_coef <- coef(enet) %>% as.matrix() %>% as.data.frame()
  colnames(enet_coef) <- "E"
  enet_coef$predictor <- rownames(enet_coef)
  
  mcp_coef <- coef(mcp) %>% as.matrix %>% as.data.frame()
  colnames(mcp_coef) <- "M"
  mcp_coef$predictor <- rownames(mcp_coef)
  
  min_importance <- sort(rf$importance, decreasing = TRUE)[50]
  rf_important <- rf$importance %>% as.data.frame()
  rf_important[rf$importance < min_importance, ] <- 0
  colnames(rf_important) <- "R"
  rf_important$predictor <- rownames(rf_important)
  
  gbm <- models[[1]]$gbm
  
  xgb_importances <- xgb.importance(model = gbm)
  xgb_important <- data.frame(
    X = xgb_importances$Gain[1:50],
    predictor = xgb_importances$Feature[1:50]
  )
  
  gdat <- merge(lasso_coef, enet_coef, by = "predictor") %>%
    merge(mcp_coef, by = "predictor") %>%
    merge(rf_important, by = "predictor") %>%
    merge(xgb_important, by = "predictor")
  
  print(gdat[gdat$L != 0 & gdat$E & gdat$M & gdat$R, ])
  
  # Remove XGBoost for Venn diagrams
  gdat$X <- NULL
  
  gdat[, -1] <- ifelse(gdat[, -1] != 0, 1, 0)
  
  count0 <- gdat %>%
    dplyr::select(c(L,E,M,R))%>%
    mutate (LE = L*E, LM = L*M, LR = L*R, EM =E*M, ER = E*R, MR =M*R, LEM = L*E*M, LER=L*E*R, LMR = L*M*R, EMR = E*M*R, LEMR = L*E*M*R)%>%
    replace(is.na(.), 0) %>%
    summarise_all(sum)
  
  count0
  
  count <- as.numeric(count0)
  head(count)
  # Reference four-set diagram
  library(VennDiagram)
  venn.plot.glm <- draw.quad.venn(
    area1 = count[1],
    area2 = count[2],
    area3 = count[3],
    area4 = count[4],
    n12 = count[5],
    n13 = count[6],
    n14 = count[7],
    n23 = count[8],
    n24 = count[9],
    n34 = count[10],
    n123 = count[11],
    n124 = count[12],
    n134 = count[13],
    n234 = count[14],
    n1234 = count[15],
    category = c("Lasso", "E-net", "MCP", "Random Forest"),
    fill = hue_pal(h = c(120, 240), c = 100)(4),
    #fill = hue_pal()(4),
    lty = "dashed",
    cex = 2,
    cat.cex = 1.5,
    cat.col = c(rep("black",4))
  );
  
  #setEPS()
  #postscript(file = paste("images/venn/venn", i, ".eps", sep = ""), 
  #                        fonts = "serif")
  #grid.draw(venn.plot.glm)
  #dev.off()

  ggsave(
    filename = paste("venn", i, ".eps", sep = ""),
    path = "../../figures/venn",
    plot = venn.plot.glm,
    device = cairo_ps,
    width = 10,
    height = 6,
    unit = "in"
  )

  save_plot(plot = venn.plot.glm,
            filename = paste("venn", i, sep = ""),
            path = "../../figures/venn",
            png_only = TRUE)
}
 