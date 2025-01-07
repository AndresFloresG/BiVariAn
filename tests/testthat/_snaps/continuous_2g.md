# continuous_2g funciona correctamente

    Code
      resultados_insufficient <- continuous_2g(data_insufficient, groupvar = "group",
        flextableformat = FALSE)
    Condition
      Warning in `anova.lm()`:
      ANOVA F-tests on an essentially perfect fit are unreliable
      Warning in `anova.lm()`:
      ANOVA F-tests on an essentially perfect fit are unreliable

# continuous_2g handle errors

    
    Paired is not supported in this function
    Please use continuous_2g_pair(data, groupvar) instead

---

    Invalid alternative. Allowed alternatives are: two.sided, less, greater

