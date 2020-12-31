# Utility Functions for the pricesensitivitymeter package that are *not* exported



# Newton Miller Smith Extension: Interpolating the matrix with purchase probabilities

interpolate_nms_matrix <- function(nms_matrix) {

for (i in seq_len(nrow(nms_matrix))) {
  interpolate_prob <- NA

  # try linear interpolation between three pairs of values
  interpolate_prob <- try(c(
    # linear interpolation between first pair of values (usually: "too cheap" and "cheap")
    seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[1]],
            to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
            length.out = which(!is.na(nms_matrix[i, ]))[2] - which(!is.na(nms_matrix[i, ]))[1] + 1),
    # linear interpolation between second pair of values (usually: "cheap" to "expensive")
    seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
            to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
            length.out = which(!is.na(nms_matrix[i, ]))[3] - which(!is.na(nms_matrix[i, ]))[2] + 1)[-1],
    # linear interpolation between third pair of values (usually: "expensive" to "too expensive")
    seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
            to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[4]],
            length.out = which(!is.na(nms_matrix[i, ]))[4] - which(!is.na(nms_matrix[i, ]))[3] + 1)[-1]),
    silent = TRUE)

  # if try() function throws a silent error, perform interpolation between two pairs of values instead
  if(inherits(interpolate_prob, "try-error")) {
    # linear interpolation between first pair of values (usually: "too cheap"/"cheap" OR "cheap"/"expensive")
    interpolate_prob <- c(
      seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[1]],
              to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
              length.out = which(!is.na(nms_matrix[i, ]))[2] - which(!is.na(nms_matrix[i, ]))[1] + 1),
      # linear interpolation between second pair of values (usually: "cheap"/"expensive" OR "expensive"/"too expensive")
      seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
              to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
              length.out = which(!is.na(nms_matrix[i, ]))[3] - which(!is.na(nms_matrix[i, ]))[2] + 1)[-1])
  }

  # write vector with interpolated values to matrix
  nms_matrix[i, min(which(!is.na(nms_matrix[i, ]))):max(which(!is.na(nms_matrix[i, ])))] <- interpolate_prob
}


  # purchase probabilities outside of the individual's personal price range must be set to zero
  nms_matrix[is.na(nms_matrix)] <- 0

  return(nms_matrix)
}
