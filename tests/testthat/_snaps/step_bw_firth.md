# step_bw_firth works

    Code
      stepwise <- step_bw_firth(regression_model, trace = FALSE)
    Output
      logistf::logistf(formula = formula_initial, data = dataprov)
      
      Model fitted by Penalized ML
      Coefficients:
                         coef  se(coef)   lower 0.95  upper 0.95    Chisq         p
      (Intercept) -7.69257184 6.3857111 -26.16291992 4.255866617 1.452349 0.2281514
      mpg          0.25891428 0.1908700  -0.08371816 0.849879312 2.007920 0.1564797
      cyl          0.94423433 0.6938886  -0.42132540 2.681521528 1.766070 0.1838692
      disp        -0.01707044 0.0109232  -0.04456374 0.003339387 2.582845 0.1080273
                  method
      (Intercept)      2
      mpg              2
      cyl              2
      disp             2
      
      Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
      
      Likelihood ratio test=13.81929 on 3 df, p=0.003161735, n=32
      Wald test = 7.617219 on 3 df, p = 0.05462187logistf::logistf(formula = new_formula, data = dataprov)
      
      Model fitted by Penalized ML
      Coefficients:
                          coef   se(coef)   lower 0.95  upper 0.95     Chisq
      (Intercept) -1.855116941 4.08157422 -10.82914191 6.674601421 0.1862043
      mpg          0.139293132 0.14148295  -0.13641823 0.478581207 0.9355306
      disp        -0.006328777 0.00670897  -0.02248537 0.006331968 0.8979015
                          p method
      (Intercept) 0.6660947      2
      mpg         0.3334299      2
      disp        0.3433450      2
      
      Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
      
      Likelihood ratio test=12.56294 on 2 df, p=0.001870652, n=32
      Wald test = 8.655581 on 2 df, p = 0.01319667logistf::logistf(formula = new_formula, data = dataprov)
      
      Model fitted by Penalized ML
      Coefficients:
                        coef  se(coef)   lower 0.95 upper 0.95    Chisq            p
      (Intercept) -5.6925770 2.0046150 -10.81678594  -2.265374 13.04460 0.0003041604
      mpg          0.2622094 0.0971237   0.09779581   0.514326 11.98521 0.0005362443
                  method
      (Intercept)      2
      mpg              2
      
      Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
      
      Likelihood ratio test=11.98521 on 1 df, p=0.0005362443, n=32
      Wald test = 8.193266 on 1 df, p = 0.004204617

