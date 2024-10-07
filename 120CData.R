polls_data_2016 <- read.csv(file = "president_general_polls_sorted_end_date_2016.csv")
polls_data_2020 <- read.csv(file = "president_polls_2020.csv")
polls_data_2016_enddate=mdy(polls_data_2016$enddate)
polls_data_2016=polls_data_2016[which(polls_data_2016_enddate>="2016-08-01"),]
index_mich=which(polls_data_2016$state=="Michigan")
index_georgia=which(polls_data_2016$state=="Georgia")
index_nc=which(polls_data_2016$state=="North Carolina")
n1=sum(polls_data_2016$total.clinton[index_mich])
n2=sum(polls_data_2016$total.trump[index_mich])
n1/(n1+n2)-n2/(n1+n2)
n3=sum(polls_data_2016$total.clinton[index_georgia])
n4=sum(polls_data_2016$total.trump[index_georgia])
n3/(n3+n4)-n4/(n3+n4)
n5=sum(polls_data_2016$total.clinton[index_nc])
n6=sum(polls_data_2016$total.trump[index_nc])
n5/(n5+n6)-n6/(n5+n6)
date_mich <- mdy(polls_data_2016$enddate[index_mich])
percentage_diff_mich=(polls_data_2016$total.clinton[index_mich]-polls_data_2016$total.trump[index_mich])/(polls_data_2016$total.clinton[index_mich]+polls_data_2016$total.trump[index_mich])
plot(date_mich,percentage_diff_mich,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Michigan')
abline(a=0,b=0)
date_georgia <- mdy(polls_data_2016$enddate[index_georgia])
percentage_diff_georgia=(polls_data_2016$total.clinton[index_georgia]-polls_data_2016$total.trump[index_georgia])/(polls_data_2016$total.clinton[index_georgia]+polls_data_2016$total.trump[index_georgia])
plot(date_georgia,percentage_diff_georgia,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
abline(a=0,b=0)
date_nc <- mdy(polls_data_2016$enddate[index_nc])
percentage_diff_nc=(polls_data_2016$total.clinton[index_nc]-polls_data_2016$total.trump[index_nc])/(polls_data_2016$total.clinton[index_nc]+polls_data_2016$total.trump[index_nc])
plot(date_nc,percentage_diff_nc,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
abline(a=0,b=0)
t.test(polls_data_2016$total.clinton[index_mich],polls_data_2016$total.trump[index_mich],paired=T,alternative='greater')
t.test(polls_data_2016$total.clinton[index_georgia],polls_data_2016$total.trump[index_georgia],paired=T,alternative='greater')
t.test(polls_data_2016$total.clinton[index_nc],polls_data_2016$total.trump[index_nc],paired=T,alternative='greater')
wilcox.test(polls_data_2016$total.clinton[index_mich], polls_data_2016$total.trump[index_mich], alternative = "greater")
wilcox.test(polls_data_2016$total.clinton[index_georgia], polls_data_2016$total.trump[index_georgia], alternative = "greater")
wilcox.test(polls_data_2016$total.clinton[index_nc], polls_data_2016$total.trump[index_nc], alternative = "greater")
date_mi <- mdy(polls_data_2016$enddate[index_mich])
date_ga <- mdy(polls_data_2016$enddate[index_georgia])
date_nc <- mdy(polls_data_2016$enddate[index_nc])
index_date_mi_aug_nov=which(date_mi >="2016-08-01")
index_date_ga_aug_nov=which(date_ga >="2016-08-01")
index_date_nc_aug_nov=which(date_nc >="2016-08-01")
percentage_diff_mi=(polls_data_2016$total.clinton[index_date_mi_aug_nov]-polls_data_2016$total.trump[index_date_mi_aug_nov])/(polls_data_2016$total.clinton[index_date_mi_aug_nov]+polls_data_2016$total.trump[index_date_mi_aug_nov])
percentage_diff_ga=(polls_data_2016$total.clinton[index_date_ga_aug_nov]-polls_data_2016$total.trump[index_date_ga_aug_nov])/(polls_data_2016$total.clinton[index_date_ga_aug_nov]+polls_data_2016$total.trump[index_date_ga_aug_nov])
percentage_diff_nc=(polls_data_2016$total.clinton[index_date_nc_aug_nov]-polls_data_2016$total.trump[index_date_nc_aug_nov])/(polls_data_2016$total.clinton[index_date_nc_aug_nov]+polls_data_2016$total.trump[index_date_nc_aug_nov])
counts_mi_for_lm <- data.frame(
  data_date = date_mi[index_date_mi_aug_nov],
  percentage_diff = percentage_diff_mi
)
counts_ga_for_lm <- data.frame(
  data_date = date_ga[index_date_ga_aug_nov],
  percentage_diff = percentage_diff_ga
)
counts_nc_for_lm <- data.frame(
  data_date = date_nc[index_date_nc_aug_nov],
  percentage_diff = percentage_diff_nc
)
lm_model_mi=lm(percentage_diff~(data_date),data=counts_mi_for_lm)
lm_model_ga=lm(percentage_diff~(data_date),data=counts_ga_for_lm)
lm_model_nc=lm(percentage_diff~(data_date),data=counts_nc_for_lm)
summary(lm_model_mi)
summary(lm_model_ga)
summary(lm_model_nc)
conf_interval_mi_fitted = predict(lm_model_mi, newdata=counts_mi_for_lm, interval="confidence", level = 0.95)
conf_interval_ga_fitted = predict(lm_model_ga, newdata=counts_ga_for_lm, interval="confidence", level = 0.95)
conf_interval_nc_fitted = predict(lm_model_nc, newdata=counts_nc_for_lm, interval="confidence", level = 0.95)
plot(counts_mi_for_lm$data_date,counts_mi_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Michigan')
polygon(c(rev(counts_mi_for_lm$data_date), counts_mi_for_lm$data_date), 
        c(rev(conf_interval_mi_fitted[,2]), conf_interval_mi_fitted[ ,3]), col = 'grey80', border = NA)
lines(counts_mi_for_lm$data_date,counts_mi_for_lm$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Michigan')
lines(counts_mi_for_lm$data_date,lm_model_mi$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Michigan')
plot(counts_ga_for_lm$data_date,counts_ga_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
polygon(c(rev(counts_ga_for_lm$data_date), counts_ga_for_lm$data_date), 
        c(rev(conf_interval_ga_fitted[,2]), conf_interval_ga_fitted[ ,3]), col = 'grey80', border = NA)
lines(counts_ga_for_lm$data_date,counts_ga_for_lm$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
lines(counts_ga_for_lm$data_date,lm_model_ga$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Georgia')
plot(counts_nc_for_lm$data_date,counts_nc_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_nc_for_lm$data_date), counts_nc_for_lm$data_date), 
        c(rev(conf_interval_nc_fitted[,2]), conf_interval_nc_fitted[ ,3]), col = 'grey80', border = NA)
lines(counts_nc_for_lm$data_date,counts_nc_for_lm$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_nc_for_lm$data_date,lm_model_nc$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')
date_2020= mdy(polls_data_2020$end_date)
date_2020_latest_day=date_2020[1]
index_selected=which(date_2020>='2020-08-01')
polls_data_2020=polls_data_2020[index_selected,]
index_na=which(is.na(polls_data_2020$sample_size)==T)
index_na
polls_data_2020_question_id_num=unique(polls_data_2020$question_id)
for(i in 1:length(unique(polls_data_2020$question_id)) ){
  index_set=which(polls_data_2020$question_id==polls_data_2020_question_id_num[i])
  if(length(index_set)!=2){
    polls_data_2020=polls_data_2020[-index_set,]
  }
}
index_NA=which(is.na(polls_data_2020$sample_size)==T)
index_NA
polls_data_2020=polls_data_2020[-index_NA,]
index_biden_mich_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Michigan")
index_trump_mich_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Michigan")
index_biden_georgia_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Georgia")
index_trump_georgia_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Georgia")
index_biden_nc_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="North Carolina")
index_trump_nc_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="North Carolina")
counts_biden_mich_2020=polls_data_2020$pct[index_biden_mich_2020]*polls_data_2020$sample_size[index_biden_mich_2020]
counts_trump_mich_2020=polls_data_2020$pct[index_trump_mich_2020]*polls_data_2020$sample_size[index_trump_mich_2020]
n1_2020_mich=sum(counts_biden_mich_2020)
n2_2020_mich=sum(counts_trump_mich_2020)
(n1_2020_mich-n2_2020_mich)/(n1_2020_mich+n2_2020_mich)
counts_biden_georgia_2020=polls_data_2020$pct[index_biden_georgia_2020]*polls_data_2020$sample_size[index_biden_georgia_2020]
counts_trump_georgia_2020=polls_data_2020$pct[index_trump_georgia_2020]*polls_data_2020$sample_size[index_trump_georgia_2020]
n1_2020_georgia=sum(counts_biden_georgia_2020)
n2_2020_georgia=sum(counts_trump_georgia_2020)
(n1_2020_georgia-n2_2020_georgia)/(n1_2020_georgia+n2_2020_georgia)
counts_biden_nc_2020=polls_data_2020$pct[index_biden_nc_2020]*polls_data_2020$sample_size[index_biden_nc_2020]
counts_trump_nc_2020=polls_data_2020$pct[index_trump_nc_2020]*polls_data_2020$sample_size[index_trump_nc_2020]
n1_2020_nc=sum(counts_biden_nc_2020)
n2_2020_nc=sum(counts_trump_nc_2020)
(n1_2020_nc-n2_2020_nc)/(n1_2020_nc+n2_2020_nc)
t.test(polls_data_2020$pct[index_biden_mich_2020],polls_data_2020$pct[index_trump_mich_2020],paired=T,alternative='greater')
t.test(polls_data_2020$pct[index_biden_georgia_2020],polls_data_2020$pct[index_trump_georgia_2020],paired=T,alternative='greater')
t.test(polls_data_2020$pct[index_biden_nc_2020],polls_data_2020$pct[index_trump_nc_2020],paired=T,alternative='greater')
wilcox.test(polls_data_2020$pct[index_biden_mich_2020],polls_data_2020$pct[index_trump_mich_2020],alternative='greater')
wilcox.test(polls_data_2020$pct[index_biden_georgia_2020],polls_data_2020$pct[index_trump_georgia_2020],alternative='greater')
wilcox.test(polls_data_2020$pct[index_biden_nc_2020],polls_data_2020$pct[index_trump_nc_2020],alternative='greater')
date_2020= mdy(polls_data_2020$end_date)
counts_mich_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_mich_2020],
  percentage_diff = (counts_biden_mich_2020-counts_trump_mich_2020)/(counts_biden_mich_2020+counts_trump_mich_2020)
)
lm_model_mich_2020=lm(percentage_diff~(data_date),data=counts_mich_for_lm_2020)
summary(lm_model_mich_2020)
conf_interval_mich_fitted_2020= predict(lm_model_mich_2020, newdata=counts_mich_for_lm_2020, interval="confidence",
                                        level = 0.95)
plot(counts_mich_for_lm_2020$data_date,counts_mich_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Michigan')
polygon(c(rev(counts_mich_for_lm_2020$data_date), counts_mich_for_lm_2020$data_date), 
        c(rev(conf_interval_mich_fitted_2020[,2]), conf_interval_mich_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_mich_for_lm_2020$data_date,counts_mich_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Michigan')
counts_georgia_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_georgia_2020],
  percentage_diff = (counts_biden_georgia_2020-counts_trump_georgia_2020)/(counts_biden_georgia_2020+counts_trump_georgia_2020)
)
lm_model_georgia_2020=lm(percentage_diff~(data_date),data=counts_georgia_for_lm_2020)
summary(lm_model_georgia_2020)
conf_interval_georgia_fitted_2020= predict(lm_model_georgia_2020, newdata=counts_georgia_for_lm_2020, interval="confidence",
                                           level = 0.95)
plot(counts_georgia_for_lm_2020$data_date,counts_georgia_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
polygon(c(rev(counts_georgia_for_lm_2020$data_date), counts_georgia_for_lm_2020$data_date), 
        c(rev(conf_interval_georgia_fitted_2020[,2]), conf_interval_georgia_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_georgia_for_lm_2020$data_date,counts_georgia_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
counts_nc_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_nc_2020],
  percentage_diff = (counts_biden_nc_2020-counts_trump_nc_2020)/(counts_biden_nc_2020+counts_trump_nc_2020)
)
lm_model_nc_2020=lm(percentage_diff~(data_date),data=counts_nc_for_lm_2020)
summary(lm_model_nc_2020)
conf_interval_nc_fitted_2020= predict(lm_model_nc_2020, newdata=counts_nc_for_lm_2020, interval="confidence",
                                      level = 0.95)
plot(counts_nc_for_lm_2020$data_date,counts_nc_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_nc_for_lm_2020$data_date), counts_nc_for_lm_2020$data_date), 
        c(rev(conf_interval_nc_fitted_2020[,2]), conf_interval_nc_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_nc_for_lm_2020$data_date,counts_nc_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polls_data_2016_enddate=mdy(polls_data_2016$enddate)
polls_data_2016_after_sep=polls_data_2016[which(polls_data_2016_enddate>=start_date_2016&polls_data_2016_enddate<=end_date_2016),]

poll_state_sum_clinton_2016=aggregate(polls_data_2016_after_sep$total.clinton, by=list(State=polls_data_2016_after_sep$state),FUN=sum)
poll_state_sum_trump_2016=aggregate(polls_data_2016_after_sep$total.trump, by=list(State=polls_data_2016_after_sep$state),FUN=sum)

poll_state_diff_percentage=poll_state_sum_clinton_2016
poll_state_diff_percentage[,2]=(poll_state_sum_clinton_2016[,2]-poll_state_sum_trump_2016[,2])/(poll_state_sum_clinton_2016[,2]+poll_state_sum_trump_2016[,2])
delete_index=which(levels(poll_state_diff_percentage[,1])=='U.S.')
if(length(delete_index)>0){
  poll_state_diff_percentage=poll_state_diff_percentage[-delete_index,]
  poll_state_diff_percentage[,1]
}
library(usmap)
library(ggplot2)
state_poll_2016 <- data.frame(
  state =poll_state_diff_percentage[,1],
  diff_percentage=poll_state_diff_percentage[,2]
)
index_selected=which(date_2020>='2020-09-01') 
polls_data_2020_after_sep=polls_data_2020[index_selected,]
polls_data_2020_after_sep=polls_data_2020_after_sep[which(polls_data_2020$answer=='Biden'|polls_data_2020$answer=='Trump'),]
index_biden_2020=which(polls_data_2020_after_sep$answer=='Biden')
index_trump_2020=which(polls_data_2020_after_sep$answer=='Trump' )
counts_biden_2020=polls_data_2020$pct[index_biden_2020]*polls_data_2020$sample_size[index_biden_2020]
counts_trump_2020=polls_data_2020$pct[index_trump_2020]*polls_data_2020$sample_size[index_trump_2020]
polls_data_2020$total.biden=rep(0,dim(polls_data_2020)[1])
polls_data_2020$total.trump=rep(0,dim(polls_data_2020)[1])
polls_data_2020$total.biden[index_biden_2020]=counts_biden_2020
polls_data_2020$total.trump[index_trump_2020]=counts_trump_2020
poll_state_sum_biden_2020=aggregate(polls_data_2020$total.biden, by=list(State=polls_data_2020$state),FUN=sum)
poll_state_sum_trump_2020=aggregate(polls_data_2020$total.trump, by=list(State=polls_data_2020$state),FUN=sum)
poll_state_sum_biden_2020=poll_state_sum_biden_2020[-1,]
poll_state_sum_trump_2020=poll_state_sum_trump_2020[-1,]
state_poll_2020 <- data.frame(
  state =poll_state_sum_biden_2020[,1],
  diff_percentage=(poll_state_sum_biden_2020[,2]-poll_state_sum_trump_2020[,2])/(poll_state_sum_biden_2020[,2]+poll_state_sum_trump_2020[,2])
)
limit_val=c(min(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage),
            max(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage))
plot_usmap(data = state_poll_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2016") 
plot_usmap(data = state_poll_2020, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2020") 
state_poll_2016$state
state_poll_2020$state
state_poll_2016=state_poll_2016[-c(31,33,50),]
state_poll_2020_2016_diff <- data.frame(
  state =state_poll_2020$state,
  diff=state_poll_2020$diff_percentage-state_poll_2016$diff_percentage
)
plot_usmap(data = state_poll_2020_2016_diff, values = "diff", color = "black") +
  scale_fill_gradient2(name = "difference between 2016 and 2020 (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")+
  ggtitle("difference between 2020 and 2016")
state_poll_2020_2016_diff %>% kbl(digits = 3) %>% kable_styling(bootstrap_options = c("striped", "hover","condensed"))
electoral_polls = data.frame(State = state_poll_2016$state, diff_2016 = state_poll_2016$diff_percentage,diff_2020 = state_poll_2020$diff_percentage, change = state_poll_2020_2016_diff$diff)
electoral_polls
iowa_2016 = polls_data_2016 %>% filter(state == "Iowa")
iowa_2020 = polls_data_2020 %>% filter(state == "Iowa")
florida_2016 = polls_data_2016 %>% filter(state == "Florida") 
florida_2020 = polls_data_2020 %>% filter(state == "Florida")
iowa_acc_2016 = iowa_2016$adjpoll_clinton < iowa_2016$adjpoll_trump
florida_acc_2016 = florida_2016$adjpoll_clinton < florida_2016$adjpoll_trump
sum(iowa_acc_2016)/length(iowa_acc_2016)
sum(florida_acc_2016)/length(florida_acc_2016)