library(shiny)
library(shinydashboard)
library(ggplot2)
#Sys.setlocale("LC_TIME", "English")
library(rdrop2)
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
input_data<- drop_read_csv("final_lc_data_sample.txt", dtoken = token, header = FALSE, sep = "\t",col.names=c("id",
                                                                                       "member_id",
                                                                                       "loan_amnt",
                                                                                       "funded_amnt",
                                                                                       "funded_amnt_inv",
                                                                                       "term",
                                                                                       "int_rate",
                                                                                       "installment",
                                                                                       "grade",
                                                                                       "sub_grade",
                                                                                       "emp_length",
                                                                                       "home_ownership",
                                                                                       "annual_inc",
                                                                                       "verification_status",
                                                                                       "issue_d",
                                                                                       "loan_status",
                                                                                       "pymnt_plan",
                                                                                       "url",
                                                                                       "purpose",
                                                                                       "zip_code",
                                                                                       "addr_state",
                                                                                       "dti",
                                                                                       "delinq_2yrs",
                                                                                       "earliest_cr_line",
                                                                                       "fico_range_low",
                                                                                       "fico_range_high",
                                                                                       "inq_last_6mths",
                                                                                       "mths_since_last_delinq",
                                                                                       "mths_since_last_record",
                                                                                       "open_acc",
                                                                                       "pub_rec",
                                                                                       "revol_bal",
                                                                                       "revol_util",
                                                                                       "total_acc",
                                                                                       "initial_list_status",
                                                                                       "out_prncp",
                                                                                       "out_prncp_inv",
                                                                                       "total_pymnt",
                                                                                       "total_pymnt_inv",
                                                                                       "total_rec_prncp",
                                                                                       "total_rec_int",
                                                                                       "total_rec_late_fee",
                                                                                       "recoveries",
                                                                                       "collection_recovery_fee",
                                                                                       "last_pymnt_d",
                                                                                       "last_pymnt_amnt",
                                                                                       "next_pymnt_d",
                                                                                       "last_credit_pull_d",
                                                                                       "last_fico_range_high",
                                                                                       "last_fico_range_low",
                                                                                       "collections_12_mths_ex_med",
                                                                                       "mths_since_last_major_derog",
                                                                                       "policy_code",
                                                                                       "application_type",
                                                                                       "annual_inc_joint",
                                                                                       "dti_joint",
                                                                                       "verification_status_joint",
                                                                                       "acc_now_delinq",
                                                                                       "tot_coll_amt",
                                                                                       "tot_cur_bal",
                                                                                       "open_acc_6m",
                                                                                       "open_il_6m",
                                                                                       "open_il_12m",
                                                                                       "open_il_24m",
                                                                                       "mths_since_rcnt_il",
                                                                                       "total_bal_il",
                                                                                       "il_util",
                                                                                       "open_rv_12m",
                                                                                       "open_rv_24m",
                                                                                       "max_bal_bc",
                                                                                       "all_util",
                                                                                       "total_rev_hi_lim",
                                                                                       "inq_fi",
                                                                                       "total_cu_tl",
                                                                                       "inq_last_12m",
                                                                                       "acc_open_past_24mths",
                                                                                       "avg_cur_bal",
                                                                                       "bc_open_to_buy",
                                                                                       "bc_util",
                                                                                       "chargeoff_within_12_mths",
                                                                                       "delinq_amnt",
                                                                                       "mo_sin_old_il_acct",
                                                                                       "mo_sin_old_rev_tl_op",
                                                                                       "mo_sin_rcnt_rev_tl_op",
                                                                                       "mo_sin_rcnt_tl",
                                                                                       "mort_acc",
                                                                                       "mths_since_recent_bc",
                                                                                       "mths_since_recent_bc_dlq",
                                                                                       "mths_since_recent_inq",
                                                                                       "mths_since_recent_revol_delinq",
                                                                                       "num_accts_ever_120_pd",
                                                                                       "num_actv_bc_tl",
                                                                                       "num_actv_rev_tl",
                                                                                       "num_bc_sats",
                                                                                       "num_bc_tl",
                                                                                       "num_il_tl",
                                                                                       "num_op_rev_tl",
                                                                                       "num_rev_accts",
                                                                                       "num_rev_tl_bal_gt_0",
                                                                                       "num_sats",
                                                                                       "num_tl_120dpd_2m",
                                                                                       "num_tl_30dpd",
                                                                                       "num_tl_90g_dpd_24m",
                                                                                       "num_tl_op_past_12m",
                                                                                       "pct_tl_nvr_dlq",
                                                                                       "percent_bc_gt_75",
                                                                                       "pub_rec_bankruptcies",
                                                                                       "tax_liens",
                                                                                       "tot_hi_cred_lim",
                                                                                       "total_bal_ex_mort",
                                                                                       "total_bc_limit",
                                                                                       "total_il_high_credit_limit"))


                   
#prepare for numeric variable plot
nums <- sapply(input_data, is.numeric)
numeric_variable_name<-names(input_data[nums])
numeric_variable_name<-numeric_variable_name[!numeric_variable_name %in% c('id','member_id','url','issue_d','zip_code','loan_status')]

#setup grobal variables used cross scripts
global_start<-"2007-01-01"
global_end<-"2016-01-01"
uni_var<-numeric_variable_name[1]
bi_var_1<-numeric_variable_name[1]
bi_var_2<-numeric_variable_name[2]
#input_data_date<-subset(input_data, issue_date>=global_start&&issue_date<=global_end)
input_data_date<-input_data
input_data_date$loan_status_agg<-'NONE'
input_data_date$loan_status_agg<-ifelse(input_data_date$loan_status %in% c('Charged Off','Default','Does not meet the credit policy. Status:Charged Off'),'Default',input_data_date$loan_status_agg)
input_data_date$loan_status_agg<-ifelse(input_data_date$loan_status %in% c('Does not meet the credit policy. Status:Fully Paid','Fully Paid'),'Fully Paid', input_data_date$loan_status_agg)
input_data_date$loan_status_agg<-ifelse(input_data_date$loan_status %in% c('In Grace Period','Late (16-30 days)','Late (31-120 days)'),'Late', input_data_date$loan_status_agg)
input_data_date$loan_status_agg<-ifelse(input_data_date$loan_status=='Current','Current', input_data_date$loan_status_agg)

#covert string date to date #cannot use %b in shinnyapps
input_data_date$issue_year<-substr(input_data_date$issue_d,5,8)
input_data_date$issue_month<-substr(input_data_date$issue_d,1,3)
input_data_date$issue_date <- as.Date(with(input_data_date, paste(issue_year, match((issue_month),c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")), '1', sep="-")))