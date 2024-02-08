mean.delta.analysis <- function(daily.summary, dosage, results.path) {

    diff_list <- lapply(split(daily.summary, daily.summary$patient), function(x) calculate.daily.summary.mean.delta(x, plot_vars))

    diff_df <- plyr::rbind.fill(diff_list)
    diff_df <- cbind(patient = patients, diff_df)
    print(summary(diff_df))

    # merge with dose
    diff_df <- merge(
        x = diff_df,
        y = dosage[, c('patient', 'dose', 'relative.dose.intensity')],
        by = 'patient',
        all.x = TRUE
        )

    # calculate delta per variable
    doses <- sort(unique(dosage$dose), na.last = NA)
    diff_df$dose.fct <- factor(diff_df$dose, levels = doses)

    # output in table
    diff_output <- lapply(plot_vars, function(x) {

        # table for each dose level
        dose.diff.model <- lm(diff_df[, x] ~ 0 + diff_df$dose.fct)
        dose.diff.table <- cbind.data.frame(dose = doses, summary(dose.diff.model)$coefficient)

        # table for all patients
        dose.diff.model.all <- lm(diff_df[, x] ~ 1)
        dose.diff.table.all <- cbind.data.frame(dose = 0, summary(dose.diff.model.all)$coefficient)
        dose.diff.table <- rbind(dose.diff.table.all, dose.diff.table)

        dose.diff.string <- paste0(round(dose.diff.table$Estimate, 1),
                                    ' (', round(dose.diff.table$Estimate - dose.diff.table[, 'Std. Error']*1.96, 1),
                                    ' - ', round(dose.diff.table$Estimate + dose.diff.table[, 'Std. Error']*1.96, 1),
                                    ')')
        dose.diff.df <- as.data.frame(matrix(dose.diff.string, ncol = length(dose.diff.string)))
        colnames(dose.diff.df) <- dose.diff.table$dose

        dose.diff.df
    })

    dose_diff_df <- plyr::rbind.fill(diff_output)
    colnames(dose_diff_df) <- paste0('Dose: ', colnames(dose_diff_df), ' (minutes)')
    dose_diff_df <- cbind(Variable = vars_nice_names, dose_diff_df)

    filename = print(file.path(
        results.path,
        generate.filename(
            'phase1_0b-prostate', 'daily_summary_before7_after7_diff_mean_ci', 'tsv'
            )
        ))
    write.table(dose_diff_df, filename, quote = F, sep = '\t', row.names = F, col.names = T)

}


calculate.daily.summary.mean.delta <- function(daily.summary, vars,
                                        days = 7) {

    print(vars[!vars %in% colnames(daily.summary)])
    vars <- vars[vars %in% colnames(daily.summary)]

    res <- sapply(vars, function(v) {

        x <- daily.summary$nday
        y <- daily.summary[, v]

        # TODO: adhoc dealing with duplicated x for now
        if (any(duplicated(x))) {
            which.dup <- which(duplicated(x) | duplicated(x, fromLast = T))
            if (v %in% c('sleep_length')) {
                z <- sum(y[which.dup])
                y <- c(z, y[-which.dup])
            } else {
                # includes start.diff.midnight, take first value
                y <- y[-which.dup[length(which.dup)]]
            }

            x <- x[!duplicated(x)]
            stopifnot(length(x) == length(y))
        }

        before <- y[1:days]
        before.mean <- mean(before, na.rm = T)

        # actually takes the last days + 1
        after <- y[(length(y)-days+1):length(y)]
        after.mean <- mean(after, na.rm = T)

        # print(y)
        # print(c(before.mean, after.mean))

        res <- after.mean - before.mean

    })

    res <- as.data.frame(matrix(res, nrow = 1, byrow = T))
    colnames(res) <- vars

    return(res)

}
