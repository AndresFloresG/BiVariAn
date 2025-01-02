# interp_dicot_2k_2sid works

    Code
      interp_dicot_2k_2sid(data = df, var1 = "smoke", var2 = "gender")
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect
      Warning in `chisq.test()`:
      Chi-squared approximation may be incorrect
    Output
      Dado que algunas frecuencias esperadas fueron menores a 5, se realizo el test exacto de Fisher. El analisis no mostro una asociacion estadisticamente significativa entre smoke y gender con un valor p de 1 . Se obtuvo un Odds Ratio de 0.5 con IC95 [ 0.02 , 11.09 ]. 
      
           
            Female Male
        Yes      1    2
        No       2    2
    Condition
      Warning in `chisq.test()`:
      Chi-squared approximation may be incorrect
    Output
      
      	Pearson's Chi-squared test with Yates' continuity correction
      
      data:  tabla
      X-squared = 9.5869e-32, df = 1, p-value = 1
      
      
      	Fisher's Exact Test for Count Data
      
      data:  tabla
      p-value = 1
      alternative hypothesis: true odds ratio is not equal to 1
      95 percent confidence interval:
        0.005686787 21.302339032
      sample estimates:
      odds ratio 
       0.5524655 
      
      [1] "Minimum expected frequencies 1.28571428571429"
      [1] "OR  0.5 IC95 [ 0.0225476971075363 ,  11.0876068100294"

