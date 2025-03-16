# Soft Non-numeric Occurrence Estimation plot = SNOE plot

# snoe_plot <- function(x) {
#
#
#   df |>
#     ggplot2::ggplot(ggplot2::aes(x = group, y = value, fill = subgroup)) +
#     ggdist::stat_ccdfinterval(ggplot2::aes(slab_alpha = ggplot2::after_stat(f)),
#                               thickness = 1, position = "dodge", fill_type = "segments", alpha=0
#     )
#
#   ggplot2::ggplot(df_na, ggplot2::aes(x = value, y)) +
#     ggplot2::geom_bar(ggplot2::aes(fill = y), stat = "identity") +
#     ggplot2::scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
#
#
#
# ggplot2::ggplot(df_na, ggplot2::aes(xmax = value, y = y)) +
#     ggplot2::geom_ribbon(ggplot2::aes(fill = y, xmin = 0), stat = "identity") +
#     ggplot2::scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
#
# ### https://stackoverflow.com/questions/53397131/gradient-fill-in-ggplot2
#
# n <- 1169
# df22 <- data.frame(x = 1:n, val = seq(0, 0.5, length.out = n), type = 1)
#
#
# grad_ungroup <- grid::linearGradient(
#   c("blue", "red"),
#   x1 = grid::unit(0, "npc"), y1 = grid::unit(0, "npc"),
#   x2 = grid::unit(0, "npc"), y2 = grid::unit(1, "npc")
# )
#
# ggplot2::ggplot(df22, ggplot2::aes(x = x)) +
#   ggplot2::geom_ribbon(ggplot2::aes(ymax = val, ymin = 0),
#     fill = grad_ungroup
#   )
#
#
# }
