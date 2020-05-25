# William Lee
# Code for ML HW #2

bdir = "~/Desktop/machine_learning_code"
setwd(bdir)

class_vect = c("bw_float","bw_non_float","vw_float",
               "containers","tableware","headlamps")

entropyPurity_calc <- function(dataframe) {
  i = 0
  for (element in dataframe[,1]) {
    i = i + 1
    r = dataframe[i,1:6]
    rs = sum(r)
    a = dataframe[i,1]
    b = dataframe[i,2]
    c = dataframe[i,3]
    d = dataframe[i,4]
    e = dataframe[i,5]
    f = dataframe[i,6]
    temp = c(a,b,c,d,e,f)
    entropy = 0
    for (num in temp) {
      if (num != 0){
        sum = -(num/sum(r))*log(num/(sum(r)))
        entropy = entropy + sum
      }
    }
    purity = max(r)/(sum(r))
    dataframe[i,"entropy"] = entropy
    dataframe[i,"purity"] = purity
  }
  tot_sum = 0; j = 0
  for (row in 1:nrow(dataframe)) {j = j + 1; tot_sum = tot_sum + sum(dataframe[j,1:6])}
  ent_w_avg = 0; pur_w_avg = 0; j = 0
  for (row in 1:nrow(dataframe)) {
    j = j + 1
    ent_w_avg = ent_w_avg + (dataframe[j,"entropy"])*(sum(dataframe[j,1:6])/tot_sum)
    pur_w_avg = pur_w_avg + (dataframe[j,"purity"])*(sum(dataframe[j,1:6])/tot_sum)
  }
  print("Weighted Average, Entropy:"); print(ent_w_avg)
  print("Weighted Average, Purity:"); print(pur_w_avg)
  return(dataframe)
}

# 6a
# k = 2
c1 = c(70,65,17,1,5,4)
c2 = c(0,11,0,12,4,25)

k2_sim_df = rbind(c1,c2)
colnames(k2_sim_df) = class_vect

k2_sim_df = as.data.frame(k2_sim_df)

k2_sim_df$entropy = NA
k2_sim_df$purity = NA

k2_sim_df = entropyPurity_calc(k2_sim_df)

# k = 4
c1 = c(15,21,3,1,0,0)
c2 = c(0,11,0,6,4,24)
c3 = c(0,0,0,2,0,0)
c4 = c(55,44,14,4,5,5)

k4_sim_df = rbind(c1,c2,c3,c4)
colnames(k4_sim_df) = class_vect

k4_sim_df = as.data.frame(k4_sim_df)

k4_sim_df$entropy = NA
k4_sim_df$purity = NA

k4_sim_df = entropyPurity_calc(k4_sim_df)

# k = 6
c1 = c(15,21,3,1,0,0)
c2 = c(0,0,0,2,3,23)
c3 = c(0,0,0,2,0,0)
c4 = c(17,2,2,0,0,3)
c5 = c(38,42,12,1,5,2)
c6 = c(0,11,0,7,1,1)

k6_sim_df = rbind(c1,c2,c3,c4,c5,c6)
colnames(k6_sim_df) = class_vect

k6_sim_df = as.data.frame(k6_sim_df)

k6_sim_df$entropy = NA
k6_sim_df$purity = NA

k6_sim_df = entropyPurity_calc(k6_sim_df)

#########################################

# 6b
# k = 2
c1 = c(70,76,17,11,9,29)
c2 = c(0,0,0,2,0,0)

k2_hir_df = rbind(c1,c2)
colnames(k2_hir_df) = class_vect

k2_hir_df = as.data.frame(k2_hir_df)

k2_hir_df$entropy = NA
k2_hir_df$purity = NA

k2_hir_df = entropyPurity_calc(k2_hir_df)

# k = 4
c1 = c(70,75,17,11,8,29)
c2 = c(0,0,0,0,1,0)
c3 = c(0,1,0,0,0,0)
c4 = c(0,0,0,2,0,0)

k4_hir_df = rbind(c1,c2,c3,c4)
colnames(k4_hir_df) = class_vect

k4_hir_df = as.data.frame(k4_hir_df)

k4_hir_df$entropy = NA
k4_hir_df$purity = NA

k4_hir_df = entropyPurity_calc(k4_hir_df)

# k = 6
c1 = c(70,75,17,10,8,28)
c2 = c(0,0,0,1,0,0)
c3 = c(0,0,0,0,1,0)
c4 = c(0,1,0,0,0,0)
c5 = c(0,0,0,0,0,1)
c6 = c(0,0,0,2,0,0)

k6_hir_df = rbind(c1,c2,c3,c4,c5,c6)
colnames(k6_hir_df) = class_vect

k6_hir_df = as.data.frame(k6_hir_df)

k6_hir_df$entropy = NA
k6_hir_df$purity = NA

k6_hir_df = entropyPurity_calc(k6_hir_df)
