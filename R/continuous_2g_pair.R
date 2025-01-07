#' @importFrom dplyr "%>%"
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr drop_na
#' @importFrom rrtable df2flextable
#' @name continuous_2g_pair
#' @aliases continuous_2g_pair
#' @title Bivariate analysis for 2 groups for paired data
#' @description
#'   Generates a HTML table of bivariate paired analysis for 2 groups.
#' @param data Data frame from which variables will be extracted.
#' @param groupvar Grouping variable. Must have exactly 2 levels.
#' @param ttest_args Arguments to be passed to `t.test()` function.
#' @param wilcox_args Arguments to be passed to `wilcox.test()` function.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#' @returns A dataframe or flextable with containing p values for paired tests along with statistics for normality and homocedasticity.
#'
#'
#' @examples
#' data <- data.frame(group = rep(letters[1:2], 30),
#'                    var1 = rnorm(30, mean = 15, sd = 5),
#'                   var2 = rnorm(30, mean = 20, sd = 2),
#'                   var3 = rnorm(30, mean = 10, sd = 1),
#'                   var4 = rnorm(30, mean = 5, sd =2))
#' data$group<-as.factor(data$group)
#'
#' continuous_2g_pair(data = data, groupvar = "group")
#'
#'
#' @export

continuous_2g_pair <- function(data,
                               groupvar,
                               ttest_args = list(),
                               wilcox_args = list(),
                               flextableformat = TRUE){

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!(groupvar %in% names(data))){
    stop(groupvar, " is not in provided dataframe")
  }

  if(is.character(flextableformat) || !is.logical(flextableformat)){
    stop("flextableformat must be a logical operator")
  }

  if("paired" %in% names(ttest_args) || "paired" %in% names(wilcox_args)){
    warning("\nThe argument 'paired' provided will be ignored")
  }

  valid_alternative <- c("two.sided", "less", "greater")

  if(all("alternative" %in% names(ttest_args))){
    if(!all(ttest_args$alternative %in% valid_alternative)){
      stop("Invalid alternative. Allowed alternatives are: two.sided, less, greater")
    }
  }

  if(all("alternative" %in% names(wilcox_args))){
    if(!all(wilcox_args$alternative %in% valid_alternative)){
      stop("Invalid alternative. Allowed alternatives are: two.sided, less, greater")
    }
  }

  default_pair_args <- list(paired = TRUE, conf.int = TRUE, na.action = na.pass)

  if(length(ttest_args) == 0){
    ttest_args = default_pair_args
  } else {
    ttest_args = modifyList(default_pair_args, ttest_args)
  }

  if(length(wilcox_args) == 0){
    wilcox_args = default_pair_args
  } else {
    wilcox_args = modifyList(default_pair_args, wilcox_args)
  }


    # Convertir la variable de agrupacion en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupacion tiene exactamente dos niveles
  if (length(levels(data[[groupvar]])) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Dividir los datos por grupo
  group_levels <- levels(data[[groupvar]])
  data_group1 <- data[data[[groupvar]] == group_levels[1], ]
  data_group2 <- data[data[[groupvar]] == group_levels[2], ]

  # Verificar si hay discrepancias en las longitudes de los grupos
  has_na_discrepancy <- any(sapply(variables_continuas, function(var) {
    length(na.omit(data_group1[[var]])) != length(na.omit(data_group2[[var]]))
  }))

  if (has_na_discrepancy) {
    warning("The length of one of the groups is mismatched, function will proceed with na removing.", "\nComparison columns has NAs, cannot compute t test nor mean differences\n\n")

  }

  # Crear listas para almacenar resultados
  resultados <- list()

  # Bucle para analizar cada variable continua
  for (var in variables_continuas) {

    group_levels <- levels(data[[groupvar]])
    data_group1 <- data[data[[groupvar]] == group_levels[1], ]
    data_group2 <- data[data[[groupvar]] == group_levels[2], ]

    group1 <- data_group1[[var]]
    group2 <- data_group2[[var]]

    group1 <- na.omit(group1)
    group2 <- na.omit(group2)

    length(group1) = length(group2)


    # Emparejar los grupos
    paired_data <- data.frame(cbind(group1, group2))

    if (nrow(paired_data) < 2) {
      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = NA,
        P_T_Paired = NA,
        P_Wilcoxon = NA,
        Diff_Means = NA,
        CI_Lower = NA,
        CI_Upper = NA
      )
      next
    }

    # Pruebas estadisticas
    tryCatch({
      diff <- paired_data$group1 - paired_data$group2
      shapiro_res <- shapiro.test(diff)$p.value

      bas_form <-list(paired_data$group1, paired_data$group2)
      ttest_args <- modifyList(bas_form, ttest_args)
      wilcox_args <- modifyList(bas_form, wilcox_args)
      t_test <-do.call( t.test, ttest_args)
        t_p <- t_test$p.value
        diff_means <- mean(diff, na.rm = T)
        ci_lower <- t_test$conf.int[1]
        ci_upper <- t_test$conf.int[2]
        wilcox_test <- do.call(wilcox.test, wilcox_args)
        wilcox_p <- wilcox_test$p.value


      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
        P_T_Paired = if (!is.na(t_p)) ifelse(t_p > 0.001, round(t_p, 5), "<0.001*") else NA,
        P_Wilcoxon = if (!is.na(wilcox_p)) ifelse(wilcox_p > 0.001, round(wilcox_p, 5), "<0.001*") else NA,
        Diff_Means = round(diff_means, 5),
        CI_Lower = if (!is.na(ci_lower)){format(round(ci_lower, 5), scientific = FALSE) } else NA,
        CI_Upper = if (!is.na(ci_upper)){format(round(ci_upper, 5), scientific=FALSE) } else NA
      )
    }, error = function(e) {
      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = NA,
        P_T_Paired = NA,
        P_Wilcoxon = NA,
        Diff_Means = NA,
        CI_Lower = NA,
        CI_Upper = NA
      )
    })
  }

  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}


