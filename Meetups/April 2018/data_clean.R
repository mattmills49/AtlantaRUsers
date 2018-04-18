library(dplyr)
library(readr)
library(purrr)
loan_data <- suppressWarnings(read_csv(file = "~/Documents/Data/loan.csv", progress = F,
                                       col_types = cols(annual_inc_joint = "i",
                                                        dti_joint = "i",
                                                        tot_coll_amt = "i",
                                                        tot_cur_bal = "i",
                                                        open_acc_6m = "i", 
                                                        open_il_6m = "i",
                                                        open_il_12m = "i",
                                                        open_il_24m = "i",
                                                        mths_since_rcnt_il = "i",
                                                        total_bal_il = "i",
                                                        il_util = "i",
                                                        open_rv_12m = "i",
                                                        open_rv_24m = "i",
                                                        max_bal_bc = "i",
                                                        all_util = "i",
                                                        total_rev_hi_lim = "i",
                                                        inq_fi = "i",
                                                        total_cu_tl = "i",
                                                        inq_last_12m = "i",
                                                        mths_since_last_major_derog = "i")))

# remove lengthy character values
# char_vars <- vapply(loan_data, class, character(1)) == "character")
# num_levels <- map(loan_data[, char_vars], n_distinct)
loan_data <- select(loan_data, -url, -emp_title, -desc, -title)

# remove variables with only missing values and 1 unique
clean_data <- loan_data %>%
  discard(~ all(is.na(.x))) %>%
  discard(~ n_distinct(.x) == 1) %>%
  discard(~ n_distinct(.x) == 887379)


bad_indicators <- c("Charged Off",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period", 
                    "Default Receiver", 
                    "Late (16-30 days)",
                    "Late (31-120 days)")

clean_data$bad_loan <- factor(1 * (loan_data$loan_status %in% bad_indicators))
# since the bad loan variable was derived from the loan status we need to remove it
clean_data$loan_status <- NULL
clean_loan_data <- map_if(clean_data, ~ class(.x) == "character", as.factor) %>% bind_cols()

saveRDS(clean_loan_data, file = "~/Documents/Presentations/h2o_pres/clean_loan_data.RDS")
