# Functions ---------------------------------------------------------------

coverage <-  
  function(x ){
  lower <- x[1]
  upper <- x[2]

  lower <- round(lower, 3)
  upper <- round(upper, 3)
  ifelse(lower <= 0.74 & 0.74 <= upper, 1, 0)
}

interval_beta <- function(y, 
                          n = 10,
                          alpha = 0.05,
                          a = 1, 
                          b = 1, 
                          hierc = T){
  if(hierc){
    lower <- qbeta(alpha, a + y, b + n - y)
    upper <- qbeta(1 - alpha, a + y, b + n - y)
  }else{
    lower <- qbeta(alpha, 1 + y, 1 + n - y)
    upper <- qbeta(1 - alpha, 1 + y, 1 + n - y)
    
  }
  
  c(lower, upper)
}

get_intervals <-
  function(x = NULL, 
           theta = NULL){
    if(is.null(x)) stop("Please add a proper vector of successes!")
    if(is.null(theta)) stop("Please add a proper vector for theta!")
    x <- matrix(x, ncol = 1)
    
    lower_conf <- apply(x, MARGIN = 1,
                       FUN = function(x){
                         x <- 1  - pbinom(x - 1,
                                     size = 10,
                                     prob = theta)
                         x <- theta[round(x,3) <= 0.05]
                         max(x)
                         })
    
    upper_conf <- apply(x, MARGIN = 1,
                       FUN = function(x){
                        x <-  pbinom(x,
                                size = 10,
                                prob = theta)
                        x <- theta[round(x,3) <= 0.05]
                        
                        if(is.infinite(min(x))){0}else{min(x)}  
                         
                         })
   

    bayes_uniform <- interval_beta(x,
                                   hierc = F)
    bayes_hierarchical <- interval_beta(x,
                                        a = a,
                                        b = b, 
                                        hierc = T)
    
    aux <- c(lower_conf, 
             upper_conf, 
             bayes_uniform,
             bayes_hierarchical)
    
    aux <- ifelse(is.na(aux), 1, aux)
    aux <- matrix(aux, ncol = 6)
    colnames(aux) <- c("CI_L", 
                       "CI_H",
                       "Cred_Unif_L",
                       "Cred_Unif_H",
                       "Cred_Hier_L",
                       "Cred_Hier_H")
    aux
  }


# Simulation --------------------------------------------------------------
iters <- 10000

theta_vec <- seq(from = 0, to = 1, by = 0.0005)
n = 10

a = 0.1
b = 0.1

curve(dbeta(x,a,b))


sims <- 
  apply(matrix(1:iters, ncol = 1), 
        MARGIN = 1,
      FUN = function(x){
        set.seed(x)
        

        x <- matrix(c(x, sum(rbinom(n, 1, prob  = 0.74))), nrow = 2)
        x
      }, 
      simplify = T)|>t()

colnames(sims) <- c("sim", "successes")

intervals <- get_intervals(x = sims[,2], theta = theta_vec)

sims <- cbind(sims, intervals)

coverage_values <-
  apply(sims[,3:8],
        1,
        function(x){
          covg_freq <- coverage(x[1:2])
          covg_unif <- coverage(x[3:4])
          covg_hierc <- coverage(x[5:6])
          c(covg_freq, covg_unif, covg_hierc)
          })|> t()

colnames(coverage_values) <- c("covg_freq", "covg_unif", "covg_hierc")

sims <- cbind(sims, coverage_values)




# Plots -------------------------------------------------------------------
library(ggplot2)
library(patchwork)
n <- 10
x <- 7


posterior_lower <- pbeta(theta_vec, 
                         shape1 = 1 + x, 
                         shape2 = 1 + n - x)


posterior_upper <- 1 - pbeta(theta_vec, 
                             shape1 = 1 + x, 
                             shape2 = 1 + n - x)

a_0 = 0.5
b_0 = 0.5

posterior_lower_hierc <- pbeta(theta_vec, 
                         shape1 = a + x, 
                         shape2 = b + n - x)


posterior_upper_hierc <- 1 - pbeta(theta_vec, 
                             shape1 = a + x, 
                             shape2 = b + n - x)

conf_lower <- 1 - pbinom(x-1, size = n, prob =theta_vec )
conf_upper <- pbinom(x, size = n, prob = theta_vec  )
posterior_interval <- ifelse(posterior_lower >= posterior_upper,
                             posterior_upper, 
                             posterior_lower)

posterior_interval_hierc <- 
  ifelse(posterior_lower_hierc >= posterior_upper_hierc,
         posterior_upper_hierc, 
         posterior_lower_hierc)

conf_interval <- ifelse(conf_lower >= conf_upper,
                        conf_upper, 
                        conf_lower)
text_size <- 9

experiment<- 
  ggplot() + 
  geom_path(aes(x = theta_vec, 
                y = posterior_interval,
                colour = "Posterior Uniform"), linewidth = 1.1,
            alpha = 0.5) + 
  geom_path(aes(x = theta_vec, y = conf_interval,
                colour = "Confidence"), linewidth = 1.1,
            alpha = 0.5) + 
  geom_path(aes(x = theta_vec, 
                y = posterior_interval_hierc,
                colour = "Posterior Hierarchical"), linewidth = 1.1,
            alpha = 0.5) + 
  geom_vline(xintercept = c(0.645,0.742),
             linewidth = 1) + 
  theme_bw() + 
  scale_colour_manual(values = c("Posterior Uniform" = "darkred",
                                 "Posterior Hierarchical" = "pink",
                                 "Confidence" = "darkgreen")) + 
  labs(x = expression(theta), y = "Prob / p.val",
       colour = "Interval type" ) +
  theme(legend.position = "top") +
  ylim(0,1)
  

sim_confidence <- 
  ggplot() + 
  geom_errorbar(aes(x = 1:iters, 
                    ymin =sims[,"CI_L"],
                    ymax =sims[,"CI_H"],
                    colour = factor(sims[,"covg_freq"]))) + 
  geom_hline(yintercept = 0.74, colour = "darkred",
             linewidth = 1.1) +
  ylim(0,1) + 
  labs(x = "Simulation N°",
       y = expression(theta~"interval"),
       colour = "Coverage",
       caption = "Inverse exact binomial test")+ 
  theme_bw() + 
  theme(legend.position = "top",
        text = element_text(size = text_size))

sim_unif_bayes <-
  ggplot() + 
  geom_errorbar(aes(x = 1:iters,
                    ymin =sims[,"Cred_Unif_L"],
                    ymax =sims[,"Cred_Unif_H"],
                    colour = factor(sims[,"covg_unif"]))
                ) + 
  geom_hline(yintercept = 0.74, 
             colour = "darkred", linewidth = 1.1)+
  ylim(0,1) + 
  labs(x = "Simulation N°", y = expression(theta~"interval"),
       colour = "Coverage",
       caption = "Uniform(0,1) prior")+ 
  theme_bw()+ 
  theme(legend.position = "top",
        text = element_text(size = text_size))
  

sim_hierc_bayes <- 
  ggplot() + 
  geom_errorbar(aes(x = 1:iters,
                    ymin =sims[,"Cred_Hier_L"],
                    ymax =sims[,"Cred_Hier_H"],
                    colour = factor(sims[,"covg_hierc"]))
                ) + 
  geom_hline(yintercept = 0.74, 
             colour = "darkred", linewidth = 1.1)+
  ylim(0,1)  + 
  labs(x = "Simulation N°", y = expression(theta~"interval"),
       colour = "Coverage",
       caption = bquote('beta'~(.(a)~","~.(b))~~"prior"))+ 
  theme_bw()+ 
  theme(legend.position = "top",
        text = element_text(size = text_size))
experiment + (sim_confidence / sim_unif_bayes / sim_hierc_bayes)

