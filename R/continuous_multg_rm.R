#' @importFrom ez ezANOVA
#' @importFrom nlme lme
#' @name continuous_multg_rm
#' @aliases continuous_multg_rm
#' @title Repeated-measures ANOVA (ezANOVA and/or lme)
#' @description
#' Runs repeated-measures ANOVA per outcome using:
#' (a) \code{ez::ezANOVA} with Mauchly's test and sphericity corrections (GG/HF), and/or
#' (b) a multilevel model via \code{nlme::lme} (random intercept per subject),
#'     assessing fixed effects via nested model comparisons.
#'
#' @param data A \code{data.frame} in long format (one row per measurement per subject).
#' @param idvar Character string: subject identifier variable.
#' @param withinvar Character string: within-subject factor (time/condition).
#' @param dvs Character vector of continuous outcome names. If \code{NULL}, numeric
#'   columns are auto-detected excluding \code{idvar}, \code{withinvar}, and \code{betweenvar}.
#' @param betweenvar (Optional) character string: between-subject factor.
#' @param method One of \code{"both"} (default), \code{"ez"}, or \code{"lme"}.
#' @param correction Sphericity correction for \code{ezANOVA}:
#'   \code{"GG"} (default), \code{"HF"}, or \code{"none"}.
#' @param flextableformat Logical; \code{TRUE} (default) returns a \code{flextable};
#'   \code{FALSE} returns a \code{data.frame}.
#'
#' @return A \code{flextable} or \code{data.frame} with, per outcome (DV):
#'   \code{n_subjects}, \code{within_levels}, (\code{between_levels} if applicable),
#'   \code{p_Mauchly} (if applicable), \code{p_ez} (raw/corrected), and
#'   \code{p_lme} for within/between/interaction.
#'
#' @examples
#' # Hypothetical example (long format):
#' # data_long: columns = id, time, group (optional), y1, y2, ...
#' # continuous_multg_rm(
#' #   data_long,
#' #   idvar = "id",
#' #   withinvar = "time",
#' #   dvs = c("y1","y2"),
#' #   betweenvar = "group",
#' #   method = "both",
#' #   correction = "GG",
#' #   flextableformat = FALSE
#' # )
#'
#' @export
continuous_multg_rm <- function(data,
                                idvar,
                                withinvar,
                                dvs = NULL,
                                betweenvar = NULL,
                                method = c("both","ez","lme"),
                                correction = c("GG","HF","none"),
                                flextableformat = TRUE){

  method <- match.arg(method)
  correction <- match.arg(correction)

  if (!is.data.frame(data)) stop("data must be a data.frame object")
  if (!(idvar %in% names(data))) stop(idvar, " is not present in the data.frame")
  if (!(withinvar %in% names(data))) stop(withinvar, " is not present in the data.frame")
  if (!is.null(betweenvar) && !(betweenvar %in% names(data)))
    stop(betweenvar, " is not present in the data.frame")

  if (!is.logical(flextableformat) || length(flextableformat) != 1L || is.na(flextableformat))
    stop("flextableformat must be a single logical value")

  # Auto-detect outcomes if not supplied
  if (is.null(dvs)) {
    exclude <- c(idvar, withinvar, if (!is.null(betweenvar)) betweenvar)
    dvs <- setdiff(names(data)[vapply(data, is.numeric, logical(1L))], exclude)
  }
  if (length(dvs) == 0L) stop("No numeric outcome variables found in 'dvs'")

  # Coerce factors
  data[[withinvar]] <- as.factor(data[[withinvar]])
  if (!is.null(betweenvar)) data[[betweenvar]] <- as.factor(data[[betweenvar]])

  fmt_p <- function(p){
    if (length(p) == 0L || is.null(p) || is.na(p) || is.nan(p)) return(NA)
    if (p < 0.001) "<0.001*" else round(p, 5)
  }

  . <- DV <- ID <- WITHIN <- BETWEEN <- anova <- NULL
  out_list <- vector("list", length(dvs))
  names(out_list) <- dvs

  for (dv in dvs) {
    if (!(dv %in% names(data))) {
      warning("Variable ", dv, " not found; skipping.")
      next
    }

    cols_keep <- c(idvar, withinvar, if (!is.null(betweenvar)) betweenvar, dv)
    df <- stats::na.omit(data[, cols_keep, drop = FALSE])
    names(df) <- c("ID","WITHIN", if (!is.null(betweenvar)) "BETWEEN", "DV")

    ns <- length(unique(df$ID))
    if (ns < 2L) {
      out_list[[dv]] <- data.frame(
        Variable = if (!is.null(table1::label(data[[dv]]))) table1::label(data[[dv]]) else dv,
        n_subjects = ns,
        within_levels = length(levels(df$WITHIN)),
        between_levels = if (!is.null(betweenvar)) length(levels(df$BETWEEN)) else NA_integer_,
                                                          p_Mauchly = NA, p_ez = NA, p_ez_corr = NA, correction = if (method != "lme") correction else NA,
                                                          p_lme_within = NA,
                                                          p_lme_between = if (!is.null(betweenvar)) NA else NA,
                                                          p_lme_interaction = if (!is.null(betweenvar)) NA else NA,
                                                          stringsAsFactors = FALSE
      )
      next
    }

    p_mauchly <- NA_real_
    p_ez_raw  <- NA_real_
    p_ez_corr <- NA_real_
    p_lme_w   <- NA_real_
    p_lme_b   <- if (!is.null(betweenvar)) NA_real_ else NA
    p_lme_int <- if (!is.null(betweenvar)) NA_real_ else NA

    # ---- ezANOVA ----
    if (method %in% c("both","ez")) {
      df$WITHIN <- as.factor(df$WITHIN)
      if (!is.null(betweenvar)) df$BETWEEN <- as.factor(df$BETWEEN)

      ez_res <- try(
        if (is.null(betweenvar)) {
          ez::ezANOVA(
            data = df, dv = .(DV), wid = .(ID), within = .(WITHIN),
            detailed = TRUE, type = 3, return_aov = FALSE
          )
        } else {
          ez::ezANOVA(
            data = df, dv = .(DV), wid = .(ID), within = .(WITHIN), between = .(BETWEEN),
            detailed = TRUE, type = 3, return_aov = FALSE
          )
        },
        silent = TRUE
      )

      if (!inherits(ez_res, "try-error")) {
        # Mauchly (if k > 2)
        if (length(levels(df$WITHIN)) > 2L) {
          mau <- ez_res[["Mauchly's Test for Sphericity"]]
          if (is.null(mau)) mau <- ez_res[["Mauchly"]]
          if (!is.null(mau) && "p" %in% names(mau)) {
            p_mauchly <- suppressWarnings(as.numeric(mau$p[1]))
          }
        }
        # Raw p for WITHIN
        a <- ez_res$ANOVA
        if (!is.null(a) && "Effect" %in% names(a)) {
          prow <- a[a$Effect == "WITHIN", , drop = FALSE]
          if (nrow(prow) == 1L && "p" %in% names(prow)) {
            p_ez_raw <- suppressWarnings(as.numeric(prow$p))
          }
        }
        # Sphericity corrections
        if (length(levels(df$WITHIN)) > 2L) {
          sc <- ez_res[["Sphericity Corrections"]]
          if (!is.null(sc) && "Effect" %in% names(sc)) {
            sc_row <- sc[sc$Effect == "WITHIN", , drop = FALSE]
            if (nrow(sc_row) == 1L) {
              p_ez_corr <- switch(
                correction,
                GG   = suppressWarnings(as.numeric(sc_row[["p[GG]"]])),
                HF   = suppressWarnings(as.numeric(sc_row[["p[HF]"]])),
                none = p_ez_raw
              )
            } else {
              p_ez_corr <- p_ez_raw
            }
          } else {
            p_ez_corr <- p_ez_raw
          }
        } else {
          p_ez_corr <- p_ez_raw
        }
      } else {
        warning("ez::ezANOVA failed for ", dv, ": ", as.character(ez_res))
      }
    }

    # ---- lme (multilevel) ----
    if (method %in% c("both","lme")) {
      df$WITHIN <- as.factor(df$WITHIN)
      if (!is.null(betweenvar)) df$BETWEEN <- as.factor(df$BETWEEN)

      f_full <- if (is.null(betweenvar)) {
        stats::as.formula("DV ~ WITHIN")
      } else {
        stats::as.formula("DV ~ WITHIN * BETWEEN")
      }

      full <- try(nlme::lme(
        fixed = f_full,
        random = ~ 1 | ID,
        data = df,
        method = "ML",
        na.action = stats::na.omit
      ), silent = TRUE)

      if (!inherits(full, "try-error")) {
        # WITHIN
        f_nowithin <- if (is.null(betweenvar)) stats::as.formula("DV ~ 1") else stats::as.formula("DV ~ BETWEEN")
        m_nowithin <- try(nlme::lme(
          fixed = f_nowithin, random = ~ 1 | ID, data = df, method = "ML", na.action = stats::na.omit
        ), silent = TRUE)
        if (!inherits(m_nowithin, "try-error")) {
          comp_w <- try(anova(full, m_nowithin), silent = TRUE)
          if (!inherits(comp_w, "try-error") && "p-value" %in% names(comp_w)) {
            p_lme_w <- suppressWarnings(as.numeric(utils::tail(comp_w$`p-value`, 1)))
          }
        }

        if (!is.null(betweenvar)) {
          # BETWEEN
          f_nobet <- stats::as.formula("DV ~ WITHIN")
          m_nobet <- try(nlme::lme(
            fixed = f_nobet, random = ~ 1 | ID, data = df, method = "ML", na.action = stats::na.omit
          ), silent = TRUE)
          if (!inherits(m_nobet, "try-error")) {
            comp_b <- try(anova(full, m_nobet), silent = TRUE)
            if (!inherits(comp_b, "try-error") && "p-value" %in% names(comp_b)) {
              p_lme_b <- suppressWarnings(as.numeric(utils::tail(comp_b$`p-value`, 1)))
            }
          }

          # Interaction
          f_noint <- stats::as.formula("DV ~ WITHIN + BETWEEN")
          m_noint <- try(nlme::lme(
            fixed = f_noint, random = ~ 1 | ID, data = df, method = "ML", na.action = stats::na.omit
          ), silent = TRUE)
          if (!inherits(m_noint, "try-error")) {
            comp_i <- try(anova(full, m_noint), silent = TRUE)
            if (!inherits(comp_i, "try-error") && "p-value" %in% names(comp_i)) {
              p_lme_int <- suppressWarnings(as.numeric(utils::tail(comp_i$`p-value`, 1)))
            }
          }
        }
      } else {
        warning("nlme::lme failed for ", dv, ": ", as.character(full))
      }
    }

    variable_lab <- if (!is.null(table1::label(data[[dv]]))) table1::label(data[[dv]]) else dv

    out_list[[dv]] <- data.frame(
      Variable = variable_lab,
      n_subjects = ns,
      within_levels = length(levels(df$WITHIN)),
      between_levels = if (!is.null(betweenvar)) length(levels(df$BETWEEN)) else NA_integer_,
      p_Mauchly = if (method %in% c("both","ez")) fmt_p(p_mauchly) else NA,
      p_ez = if (method %in% c("both","ez")) fmt_p(p_ez_raw) else NA,
      p_ez_corr = if (method %in% c("both","ez")) fmt_p(p_ez_corr) else NA,
      correction = if (method %in% c("both","ez")) correction else NA,
      p_lme_within = if (method %in% c("both","lme")) fmt_p(p_lme_w) else NA,
      p_lme_between = if (!is.null(betweenvar) && method %in% c("both","lme")) fmt_p(p_lme_b) else NA,
      p_lme_interaction = if (!is.null(betweenvar) && method %in% c("both","lme")) fmt_p(p_lme_int) else NA,
      stringsAsFactors = FALSE
    )
  }

  res_df <- do.call(rbind, out_list)
  rownames(res_df) <- NULL

  if (isTRUE(flextableformat)) {
    rrtable::df2flextable(res_df, vanilla = TRUE)
  } else {
    res_df
  }
}
