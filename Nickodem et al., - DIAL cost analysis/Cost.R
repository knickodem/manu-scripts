##################################
#                                #
#       DIAL Cost Analysis       #
#          June 2023             #
#                                #
##################################


# --------   Setup   ---------
# packages
library(dplyr)
library(purrr)
library(ggplot2)


#### Function to calculate DIAL cost ####

#' @param t.num number of teachers/DIAL participants
#' @param t.salary annual salary for teachers
#' @param t.hrs teacher hours to complete modules
#' @param c.wage coach hourly wage
#' @param c.hrs average hours of coaching per teacher
#' @param dial per participant cost of DIAL
#' @param s.num number of students per teacher
#' @param c.ratio number of teachers per coach
#' @param c.train average hours of training per coach

calc_cost <- function(t.num, t.salary, t.hrs, c.wage, c.hrs, dial = 40, s.num = 19, c.ratio = 4, c.train = 8){
  t.sph <- t.salary/1440
  # could add logic to prevent c.ratio > t.num
  c.num <- floor(t.num/c.ratio)
  
  c_train <- c.wage*c.train*c.num  
  c_coach <- c.wage*c.hrs*c.num
  t_coach <- t.sph*c.hrs*t.num
  t_modules <- t.sph*t.hrs*t.num
  d_modules <- dial*t.num
  
  ingredient_cost <- data.frame(Category = c("Training", rep("Personnel", 3), "Materials"),
                                Ingredient = c("New coach training (hours)","Coach coaching time (hours)",
                                               "Teacher coaching time (hours)","Teacher module completion (hours)",
                                               "DIAL"),
                                Total = c(c_train, c_coach, t_coach, t_modules, d_modules))
  tot <- sum(ingredient_cost$Total)
  total_cost <- data.frame(Category = c("Total", "Per Teacher", "Per Student"),
                           Total = c(tot, tot/t.num, tot/(t.num*s.num)))
  
  return(list(`Ingredient Cost` = ingredient_cost,
              `Total Cost` = total_cost))
}


# # teacher data - If we need for descriptives
# teachers <- haven::read_sav("00 Data/COMBINED T1_T2/DIAL_TEACHER_DATA_SCORES_3-6-23.sav")
# table(forcats::as_factor(teachers$T_EDUCATION_LEVEL_W1))

###############################################



# -----    Table 1 - Incremental Cost    --------

# Number of hours of coaching - Based on email from Katie on page 12 of Cost Analysis Data Collection Emails.docx
coach.hrs <- c(18, 14, 17.5, 21, 9.5, 23, 10.5, 12.5)
sum(coach.hrs - 8)/33 # each coach had 8 hours of training; 33 total teachers


# Base case with national median salary - https://www.bls.gov/oes/current/oes252021.htm
base <- calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6, c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)

# percent of total by category
cat.pct <- base$`Ingredient Cost` %>%
  group_by(Category) %>%
  summarise(Category_Total = sum(Total)) %>%
  mutate(Category_Pct = Category_Total / base$`Total Cost`$Total[[1]])

sum(base$`Ingredient Cost`$Total[1:2])/ base$`Total Cost`$Total[[1]]  # coaches' time
sum(base$`Ingredient Cost`$Total[3:4])/ base$`Total Cost`$Total[[1]]  # teachers' time

# Knight and Skrtic (2021)
ks.tot <- c(193150, 101917, 114328) # total cost per cycle
ks.cc <- c(189864, 100098, 111348) # coach cost per cycle
ks.tc <- c(3286, 1819, 2979) # teacher cost per cycle
ks.tn <- c(23, 14, 10) # teachers per cycle
ks.tot/ks.tn
ks.cc/ks.tot
ks.tc/ks.tot

# comparison to Knight (2012)
calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
          c.wage = 31.97, c.hrs = 18.84, dial = 40, s.num = 19)


# ----    Cost Effectiveness    ----------
ce <- map_dfr(.x = c(.48, .14, .27),
                  ~tidyr::spread(base$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
                    mutate(ICER = `Per Teacher`/.x)) %>%
  mutate(Effect = c("Instructional Strategies", "Maladaptive Attitudes", "Avoidance Attitudes"))

# ----    Table 2 - Univariate Deterministic Sensitivity Analysis Results    --------

# Teacher salary - 10th, 25th, 75th, and 90th percentile of salaries (see link for base case)
t.salary <- map(.x = c(45470, 49120, 78860, 101310),
                    ~calc_cost(t.num = 33, t.salary = .x, t.hrs = 6,
                               c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
            mutate(ICER = `Per Teacher`/.48)) %>%
  mutate(Scenario = c("$45,470", "$49,120", "$78,860", "$101,310"))


# time on modules - based on America's calculations (DIAL Training Modules Cost Effectiveness Teacher Engagement)
t.modules <- map(.x = c(4.5, 7.25, 12),
                 ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = .x,
                            c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
            mutate(ICER = `Per Teacher`/.48)) %>%
  mutate(Scenario = c("4.5", "7.25", "12"))

# time coaching - based on expectations
coaching <- map(.x = c(5, 10),
                ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                           c.wage = 15, c.hrs = .x, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
            mutate(ICER = `Per Teacher`/.48)) %>%
  mutate(Scenario = c("5", "10"))

# Coach wage - 10th, 50th, and 90th percentile of hourly wages for instructional coaches - https://www.bls.gov/oes/current/oes259031.htm
c.wage <- map(.x = c(20.19, 31.97, 50.58),
              ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                         c.wage = .x, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
            mutate(ICER = `Per Teacher`/.48)) %>%
  mutate(Scenario = c("$20.19", "$31.97", "$50.58"))

# ## number of teachers - negligble impact on per teacher cost
# t.num <- map(.x = c(15, 50, 100),
#              ~calc_cost(t.num = .x, t.salary = 61690, t.hrs = 6.5,
#                         c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
#   map_dfr(.x = .,
#           ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
#             mutate(ICER = `Per Teacher`/.48)) %>%
#   mutate(Scenario = c(15, 50, 100))

## coach to teacher ratio - no rationale for variation
ctratio <- map(.x = c(2, 10),
               ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                          c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19,
                          c.ratio = .x)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
            mutate(ICER = `Per Teacher`/.48)) %>%
  mutate(Scenario = c("1:2", "1:10"))


## effect size

# # calculate 90% confidence interval for teacher outcomes according to Hedges 2007
# load("Output/Analysis_Results.RData")
# t.esCI <- t.es %>%
#   mutate(low_CI = effect.size + qnorm(.05)*effect.size.se,
#          hi_CI = effect.size - qnorm(.05)*effect.size.se)

effect <- map_dfr(.x = c(.06, .89),
                  ~tidyr::spread(base$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
                    mutate(ICER = `Per Teacher`/.x)) %>%
  mutate(Scenario = c("0.06", "0.89"))


## combining tables above
table2 <- tidyr::spread(base$`Total Cost`[1:2,], key = "Category", value = "Total") %>%
  mutate(ICER = `Per Teacher`/.48,
         Scenario = "Base case") %>%
  bind_rows(data.frame(Scenario = "Teacher annual salarya (base case = $61,690)"), t.salary) %>%
  bind_rows(data.frame(Scenario = "Hours to complete modulesb (base case = 6)"), t.modules) %>%
  bind_rows(data.frame(Scenario = "Hours of coaching per teacherc (base case = 1.88)"), coaching) %>%
  bind_rows(data.frame(Scenario = "Coach hourly waged (base case = $15.00)"), c.wage) %>%
  bind_rows(data.frame(Scenario = "Coach-to-teacher ratio (base case = 1:4)"), ctratio) %>%
  bind_rows(data.frame(Scenario = "Effect sizee (base case = 0.48)"), effect) %>% 
  select(Scenario, `Total Cost` = Total, `Cost Per Teacher` = `Per Teacher`, ICER)

# exporting to copy to  final table
# write.csv(table2, file = "table2.csv")

## ----    Identifying linear cost association for each variable    ------
# cost per teacher per unit

## per $5K teacher salary - 0.005472222*5000 = 27.36
salary.plot <- map(.x = seq(40000, 120000, 5000),
                     ~calc_cost(t.num = 33, t.salary = .x, t.hrs = 6,
                                c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Scenario = seq(40000, 120000, 5000)) #%>%
  # ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
  # geom_point() +
  # theme_bw(base_size = 18)

salary.lm <- lm(`Per Teacher` ~ Scenario, data = salary.plot)
salary.out <- parameters::model_parameters(salary.lm)

## per hour on modules - 42.84
t.modules.plot <- map(.x = seq(4, 15, .5),
                      ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = .x,
                                 c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Scenario = seq(4, 15, .5)) #%>%
  # ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
  # geom_point() +
  # theme_bw(base_size = 18)

t.modules.lm <- lm(`Per Teacher` ~ Scenario, data = t.modules.plot)
t.modules.out <- parameters::model_parameters(t.modules.lm)


## per hour of coaching - 46.48
coaching.plot <- map(.x = seq(1, 20, 1),
                   ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                              c.wage = 15, c.hrs = .x, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Scenario = seq(1, 20, 1)) #%>%
  # ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
  # geom_point() +
  # theme_bw(base_size = 18)

coaching.lm <- lm(`Per Teacher` ~ Scenario, data = coaching.plot)
coaching.out <- parameters::model_parameters(coaching.lm)


## per $5 coach wage increase - 2.40*5 = 12
c.wage.plot <- map(.x = seq(15, 50, 5),
                   ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                              c.wage = .x, c.hrs = 1.88, dial = 40, s.num = 19)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Scenario = seq(15, 50, 5))# %>%
# ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
# geom_point() +
# theme_bw(base_size = 18)

c.wage.lm <- lm(`Per Teacher` ~ Scenario, data = c.wage.plot)
c.wage.out <- parameters::model_parameters(c.wage.lm)

## per hour of coach training - 3.64
c.train.plot <- map(.x = seq(0, 8, 1),
                     ~calc_cost(t.num = 33, t.salary = 61690, t.hrs = 6,
                                c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19, c.train = .x)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Scenario = seq(0, 8, 1)) #%>%
# ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
# geom_point() +
# theme_bw(base_size = 18)

c.train.lm <- lm(`Per Teacher` ~ Scenario, data = c.train.plot)
c.train.out <- parameters::model_parameters(c.train.lm)


#### Gathering per unit cost increase estimates ####
perunit <- data.frame(Factor = c("Teacher Salary (per $5000)", "Module Time", "Coaching Time",
                                 "Coach Wage (per $5)", "Coach training"),
                      Cost = c(salary.out$Coefficient[[2]]*5000,
                               t.modules.out$Coefficient[[2]],
                               coaching.out$Coefficient[[2]],
                               c.wage.out$Coefficient[[2]]*5,
                               c.train.out$Coefficient[[2]]))


# ----    Figure 1 - Number of Teachers and Coaches    -------

# ## per teacher - not linear - cost only varies b/c of the 1:4 coach ratio
# t.num.plot <- map(.x = seq(10, 100, 1),
#                      ~calc_cost(t.num = .x, t.salary = 61690, t.hrs = 6.5,
#                                 c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19, c.ratio = 6)) %>%
#   map_dfr(.x = .,
#           ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
#   mutate(Scenario = seq(10, 100, 1)) %>%
#   ggplot(., aes(x = Scenario, y = `Per Teacher`)) +
#   geom_point() +
#   theme_bw(base_size = 18)
# 
# t.num.lm <- lm(`Per Teacher` ~ Scenario, data = t.num.plot)
# parameters::model_parameters(t.num.lm)

## coach to teacher ratio - non-linear
# tried a bunch of transformation to x and y, but nothing seems quite linear (Tukey and Mosteller’s Bulging Rule)
ctratio.data <- map2(.x = rep(seq(1, 20, 1), times = 4),
                     .y = rep(c(15, 33, 50, 200), each = 20),
                     ~calc_cost(t.num = .y, t.salary = 61690, t.hrs = 6,
                                c.wage = 15, c.hrs = 1.88, dial = 40, s.num = 19,
                                c.ratio = .x)) %>%
  map_dfr(.x = .,
          ~tidyr::spread(.x$`Total Cost`, key = "Category", value = "Total")) %>%
  mutate(Ratio = rep(seq(1, 20, 1), times = 4),
         Teachers = rep(c(15, 33, 50, 200), each = 20) %>%
           forcats::as_factor()) %>%
  filter(!(Teachers == "15" & Ratio > 15)) %>%
  group_by(Teachers) %>%
  arrange(Ratio, .by_group = TRUE) %>%
  mutate(diff = `Per Teacher` - lag(`Per Teacher`, default = first(`Per Teacher`)))

ctratio.plot <- ctratio.data %>%
  ggplot(., aes(x = Ratio, y = `Per Teacher`, group = Teachers, linetype = Teachers)) +
  labs(x = "Teachers per coach", y = "Cost per teacher ($)", linetype = "Number of\n Teachers") +
  scale_x_continuous(breaks = seq(1, 20, 2)) +
  scale_y_continuous(breaks = seq(350, 550, 25)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 18)

# ctratio.lm <- lm(log(`Per Teacher`) ~ log(Scenario) , data = ctratio.plot) # + I(Scenario^2)
# parameters::model_parameters(ctratio.lm)
# performance::check_model(ctratio.lm)
# bc <- MASS::boxcox(ctratio.lm)
# bc$x[which.max(bc$y)]


# # ----    Crossing all factors and put it in a regression model?    --------
# # keeping the non-linear coach-to-teacher ratio constant at 1:4
# 
# # variation2
# variation2 <- expand.grid(t.salary = seq(40000, 120000, 5000)/5000,
#                          t.hrs = seq(4, 15, 1),
#                          c.wage = seq(15, 50, 5)/5,
#                          c.hrs = seq(1, 20, 1),
#                          c.train = seq(0, 8, 1)) # 293,760 combinations
# 
# summary(variation2$t.salary)
# 
# y2 <- vector("numeric", nrow(variation2))
# for(i in 1:nrow(variation2)){
#   cost2 <- calc_cost(t.num = 33, t.salary = variation2$t.salary[[i]], t.hrs = variation2$t.hrs[[i]],
#                     c.wage = variation2$c.wage[[i]], c.hrs = variation2$c.hrs[[i]], c.train = variation2$c.train[[i]],
#                     dial = 40, s.num = 19)
#   
#   y2[[i]] <- cost2$`Total Cost`$Total[[2]]
# }
# 
# # t.num, t.salary, t.hrs, c.wage, c.hrs, dial, s.num, c.ratio = 4, c.train = 8
# 
# xy2 <- bind_cols(variation2, data.frame(y = y2))
# 
# reg2 <- lm(y ~ t.salary + t.hrs + c.wage + c.hrs + c.train +
#             t.salary:t.hrs + t.salary:c.wage + t.salary:c.hrs + t.salary:c.train +
#             t.hrs:c.wage + t.hrs:c.hrs + t.hrs:c.train +
#             c.wage:c.hrs + c.wage:c.train +
#             c.hrs:c.train, data = xy2)
# reg.out2 <- parameters::model_parameters(reg2)
# reg.omega2 <- effectsize::omega_squared(reg2, partial = TRUE)


##################################################################

# ----   Saving results    --------
save(base, cat.pct, ce, table2,
     salary.plot, salary.lm, salary.out,
     t.modules.plot, t.modules.lm, t.modules.out,
     coaching.plot, coaching.lm, coaching.out,
     c.wage.plot, c.wage.lm, c.wage.out,
     c.train.plot, c.train.lm, c.train.out,
     perunit,
     ctratio.data, ctratio.plot, #ctratio.lm, ctratio.out,
     # xy2, reg2, reg.out2, reg.omega2,
     file = "Output/Cost_Results.RData")

########################################################


# -----   Exchange rates and Inflation  ------

# Used https://www.exchangerates.org.uk/
# Used BLS CPI Inflation calculator (https://data.bls.gov/cgi-bin/cpicalc.pl) to put exchanged values into dollars comparable to DIAL calculations (May 2022)
# Data for calculation here: https://data.bls.gov/timeseries/CUUR0000SA0; formula is (current CPI - old CPI)/old*100

## Kiva - Huitsing et al., 2020
# using January 1, 2016 euro to dollar exchange b/c article uses 2016-2017 labor agreement for teacher salary, which constitutes the bulk of the cost
e.k <- 1.0878
202.74*e.k # total cost per student over 8 years
35.70*e.k  # one-time cost per student
20.88*e.k  # annual cost per student
818*e.k

## Learning Together - Legood et al (2021)
# Teacher salaries were accessed from Department of Education website on October 17, 2019
p.lt <- 1.2877
# 3 year total combining tables 2 and 3 (i.e., adding trainers and school staff)
(410.07+237.10)*p.lt # intervention per student
(334.58+178.85)*p.lt # control per student
inc.lt <- ((410.07+237.10)-(334.58+178.85)) # calculating incremental cost per student
(inc.lt/3)*p.lt # annual per student

## OBPP - Beckman & Svenson (2015)
# used 2013 prices so using January 1, 2013 exchange
k.ob <- 0.154
4079*k.ob
(4079/3)*k.ob # annual per student
131250*k.ob # ICER
585000*k.ob # willingness to reduce one instance of bullying (Persson & Svensson, 2013)
131250*k.ob

## Second step
# Already in $; WSIPP was in (January) 2018 and Belfield et al (2015) was in (January) 2013 dollars

