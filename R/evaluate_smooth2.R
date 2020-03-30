`evaluate_smooth2` <- function(object, ...) {
    UseMethod("evaluate_smooth2")
}

##' @export
##' @rdname evaluate_smooth2
`evaluate_smooth2.gam` <- function(object, smooth = NULL, n = 100, newdata = NULL,
                                   unconditional = FALSE, overall_uncertainty = TRUE,
                                   dist = 0.1, ...) {
    ## if particular smooths selected
    smooth_ids <- if (!is.null(smooth)) {
         which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }

    smooths <- get_smooths_by_id(object, smooth_ids) # extract the mgcv.smooth objects

    ## loop over the smooths and evaluate them
    sm_list <- vector(mode = "list", length = length(smooths))

    for (i in seq_along(sm_list)) {
        sm_type <- smooth_type(smooths[[i]])
        sm_list[[i]] <-
            switch(sm_type,
                   `1d_smooth`  = eval_1d_smooth(smooths[[i]],
                                               n = n,
                                               newdata = newdata,
                                               unconditional = unconditional,
                                               overall_uncertainty = overall_uncertainty),
                   `2d_smooth`  = eval_2d_smooth(smooths[[i]],
                                               n = n,
                                               newdata = newdata,
                                               unconditional = unconditional,
                                               overall_uncertainty = overall_uncertainty),
                   re_smooth  = eval_re_smooth(smooths[[i]],
                                               newdata = newdata,
                                               unconditional = unconditional),
                   fs_smooth  = eval_fs_smooth(smooths[[i]],
                                               n = n,
                                               newdata = newdata,
                                               unconditional = unconditional,
                                               overall_uncertainty = overall_uncertainty),
                   mrf_smooth = eval_mrf_smooth(smooths[[i]],
                                                newdata = newdata,
                                                unconditional = unconditional,
                                                overall_uncertainty = overall_uncertainty),
                   tensor_smooth = eval_tensor_smooth(smooths[[i]],
                                                      n = n,
                                                      newdata = newdata,
                                                      unconditional = unconditional,
                                                      overall_uncertainty = overall_uncertainty)
                   )
    }
}

`smooth_type` <- function(smooth) {
    sm_type <- if (is_1d_smooth(smooth)) {
        "1d_smooth"
    } else if (is_2d_smooth(smooth)) {
        "2d smooth"
    } else if (is_re_smooth(smooth)) {
        "re_smooth"
    } else if (is_fs_smooth(smooth)) {
        "fs_smooth"
    } else if (is_mrf_smooth(smooth)) {
        "mrf_smooth"
    } else if (is_general_tensor_smooth(smooth)) {
        "tensor_smooth"
    } else {
        stop("Uknown type of smooth")
    }

    sm_type
}
