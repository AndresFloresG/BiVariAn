# interp_continuous_2g works

    Code
      interp_continuous_2g(data = data, "am", "disp")
    Condition
      Warning in `wilcox.test.default()`:
      cannot compute exact p-value with ties
    Output
      
      
       La prueba de T de Student mostro una diferencia estadisticamente significativa entre los grupos 0 y 1 para la variable disp. La diferencia de medias fue de 146.85 con IC95 [72.16, 221.54]. El valor p de la prueba fue 0.000366. 
      
      Resultados de la prueba T:
      
      	Two Sample t-test
      
      data:  cont_data by group_data
      t = 4.0152, df = 30, p-value = 0.0003662
      alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
      95 percent confidence interval:
        72.15611 221.54025
      sample estimates:
      mean in group 0 mean in group 1 
             290.3789        143.5308 
      
      
      Resultados de la prueba U de Mann-Whitney:
      
      	Wilcoxon rank sum test with continuity correction
      
      data:  cont_data by group_data
      W = 214, p-value = 0.0005493
      alternative hypothesis: true location shift is not equal to 0
      

