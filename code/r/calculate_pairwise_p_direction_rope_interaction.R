# calculate_pairwise_p_direction_rope_interaction()
# input: samples (stacked_marginals), kind = c(p_direction, rope)
# output: table of kind
# only for physiographic region

calculate_pairwise_p_direction_rope_interaction <- function(samples, kind) {
  imap_dfr(samples, ~ calculate_p_direction_rope_interaction(.x, kind = kind) %>%
             mutate(Condition = .y),
           .id = NULL) |>
    relocate(Condition)
}
