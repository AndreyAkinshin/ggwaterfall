#' @importFrom stats quantile IQR
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x >= (qnt[1] - H) & x <= (qnt[2] + H)]
}

#' @importFrom ggplot2 theme element_blank
theme_blank <- function() theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_blank()
)

#' A simple density plot for the waterfall function
#'
#' @param x The data to be displayed.
#' @param x.lims The range of values which should be displayed.
#' @param fill The color which will be used for the inside plot area.
#'   This value is passed to \code{\link[ggplot2]{geom_density}}.
#' @param col The color which will be used for the plot border.
#'   This value is passed to \code{\link[ggplot2]{geom_density}}.
#' @param bw The smoothing bandwidth to be used.
#'   This value is passed to \code{\link[ggplot2]{geom_density}}.
#'   The default value is \code{\link[stats]{bw.SJ}}.
#' @param show.underline When \code{TRUE}, a horiznotal line will be drawn under
#'   the plot.
#'   The default value is \code{TRUE}.
#' @param alpha The alpha level for the inside area of the plot.
#'   The default value is \code{0.8}.
#' @param label.expand The relative expand value for the right side.
#'   The default value is \code{0.0}.
#'
#' @return ggplot-based plot which can be passed to the
#'   \code{\link{waterfall}} function.
#'
#' @importFrom ggplot2 ggplot aes geom_density xlim stat_density geom_line
#' @importFrom ggplot2 expand_scale
#'
#' @export
build_density <- function(x, x.lims, fill, col = "black", bw = "SJ",
                          show.underline = T, alpha = 0.8, label.expand = 0.0) {
  y <- NULL # See https://stackoverflow.com/a/8096882/

  p <- ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(alpha = alpha,
                 fill = fill,
                 col = "transparent",
                 bw = bw) +
    stat_density(col = col,
                 bw = bw,
                 geom = "line") +
    scale_x_continuous(
      limits = c(x.lims[1], x.lims[2]),
      expand = c(0, 0, label.expand, 0)) +
    theme_blank()
  if (show.underline)
    p <- p + geom_line(data = data.frame(x = x.lims, y = c(0, 0)),
                       aes(x = x, y = y),
                       col = col)
  p
}

#' A simple frequency trail plot for the waterfall function
#'
#' You can find the definition of frequency trails in
#' \href{http://www.brendangregg.com/FrequencyTrails/intro.html}{Brendan Gregg's Blog}
#'
#' @param x The data to be displayed.
#' @param x.lims The range of values which should be displayed.
#' @param fill The color which will be used for the inside plot area.
#' @param col The color which will be used for the plot border.
#' @param bw The smoothing bandwidth to be used.
#'   The value is passed to \code{\link[stats]{density}}.
#'   The default value is \code{\link[stats]{bw.SJ}}.
#' @param show.underline When \code{TRUE}, a horiznotal line will be drawn under
#'   the plot.
#'   The default value is \code{TRUE}.
#' @param alpha The alpha level of individual plots.
#'   The default value is \code{0.8}.
#' @param label.expand The relative expand value for the right side.
#'   The default value is \code{0.0}.
#'
#' @importFrom ggplot2 ggplot geom_polygon aes geom_line geom_path geom_point
#' @importFrom ggplot2 xlim geom_hline expand_scale
#' @importFrom stats quantile density
#' @importFrom utils head tail
#'
#' @export
build_ft <- function(x, x.lims, fill, col = "black", bw = "SJ",
                     show.underline = T, alpha = 0.8, label.expand = 0.0) {
  y <- NULL # See https://stackoverflow.com/a/8096882/

  q1 <- quantile(x)[2]
  q3 <- quantile(x)[4]
  iqr <- q3 - q1
  lf <- q1 - 1.5 * iqr
  uf <- q3 + 1.5 * iqr
  x.main <- x[x > lf & x < uf]
  x.out <- x[x <= lf | x >= uf]

  den <- density(x.main,
                 from = max(x.lims[1], lf),
                 to = min(x.lims[2], uf),
                 n = 1024,
                 bw = bw)
  df.path <- data.frame(x = den$x, y = den$y)
  df.poly <- data.frame(
    x = c(head(den$x, 1), den$x, tail(den$x, 1)),
    y = c(0, den$y, 0)
  )

  aesxy <- aes(x = x, y = y)
  p <- ggplot() +
    geom_polygon(data = df.poly, mapping = aesxy, fill = fill, alpha = alpha) +
    geom_path(data = df.path, mapping = aesxy, col = col) +
    scale_x_continuous(
      limits = c(x.lims[1], x.lims[2]),
      expand = c(0, 0, label.expand, 0)) +
    theme_blank()
  if (show.underline)
    p <- p + geom_line(data = data.frame(x = x.lims, y = c(0, 0)),
                       mapping = aesxy,
                       col = col)
  if (length(x.out) > 0)
    p <- p + geom_point(data = data.frame(x = x.out, y = 0),
                        mapping = aesxy,
                        col = col)

  p
}

#' Generic waterfall plot
#'
#' Draw a composition of overlapped plots.
#'
#' Inspired by the series of
#' \href{http://www.brendangregg.com/frequencytrails.html}{blog posts}
#' by Brendan Gregg.
#'
#' @param x List of vectors which will be displayed.
#'   Each list element will be transformed to an individual plot.
#' @param build_plot A function which draws a plot for each vector.
#'   This function should have the following signature:
#'   `function(x, x.lims, fill, col, bw, show.underline, alpha)`.
#'   There are two predefined functions which can be used:
#'   \code{\link{build_density}} and \code{\link{build_ft}}.
#' @param palette Palette which will be used for plots.
#'   Some palette examples: \itemize{
#'     \item \code{colorRampPalette(c("red", "orange"))(8)},
#'     \item \code{colorRampPalette(c("yellow", "yellow4"))(9)},
#'     \item \code{colorRampPalette(c("royalblue", "royalblue4"))(10)},
#'     \item \code{colorRampPalette(c("purple", "purple4"))(11)},
#'     \item \code{colorRampPalette(c("green2", "green4"))(12)}
#'     \item \code{rainbow(7)}
#'   }
#'   The default value is
#'   \code{colorRampPalette(c("red", "orange"))(min(10, length(x)))}.
#' @param bg The background color.
#'   The default value is \code{"white"}.
#' @param col The line color which will be used for axis lines, text labels,
#'   and plot contours.
#'   The default value is \code{"black"}.
#' @param metric An optional function which returns a single numeric value for
#'   a numeric vector. Can be used in labels (when \code{show.labels} is
#'   \code{TRUE}, the metric value van be used in \code{calc.label}) and
#'   for sorting order.
#' @param calc.label A function which calculates labels for each plot; should be
#'   used with \code{show.labels = TRUE}. The function should have the following
#'   signature: \code{function(index, name, metric.value)}.
#'   If \code{calc.label} is \code{NULL}, the following default algorithm will
#'   be applied: if an \code{x} element has a name label, this name will be
#'   used; otherwise if \code{metric} is defined, the metric value will be used;
#'   otherwise the list index will be used.
#'   The default value is \code{NULL}.
#' @param sort.value Defines how the individual plots will be sorted.
#'   If \code{sort.value} is \code{NULL}, the plots will not be sorted.
#'   If \code{sort.value} is \code{"metric"} and \code{metric} is defined,
#'   the plots will be sorted by the given metric function.
#'   Otherwise, \code{sort.value} should be a function which takes a numeric
#'   vector and returns a single numeric value which will be used for sorting.
#'   The default value is \code{NULL}.
#' @param show.labels When \code{TRUE}, labels for each individual plot will be
#'   drawn on the right side. The text of the labels is contrlled by
#'   the \code{calc.labels} function.
#'   The default value is \code{FALSE}.
#' @param show.axis When \code{TRUE}, a horizontal axis with ticks will be drawn
#'   at the bottom of the whole waterfall plot.
#'   The default value is \code{TRUE}.
#' @param show.underline When \code{TRUE}, a horiznotal line will be drawn under
#'   each individual plot.
#'   The default value is \code{TRUE}.
#' @param hide.outliers When \code{TRUE}, the outlier values will be
#'   automatically removed from each \code{x} element based on the
#'   \href{https://en.wikipedia.org/wiki/Outlier#Tukey's_fences}{Tukey's fences}.
#'   The default value is \code{FALSE}.
#' @param title The text for the title.
#' @param bw The smoothing bandwidth to be used. Will be passed to
#'   \code{build_plot}.
#'   The default value is \code{\link[stats]{bw.SJ}}.
#' @param overlap The overlap factor of the individual plots.
#'   If \code{overlap} is \code{0}, the plots will not be overlapped.
#'   If \code{overlap} is \code{1}, all the plots will be drawn on the same place.
#'   The default value is \code{0.4}.
#' @param alpha The alpha level for the inside area of the each individual plot.
#'   The default value is \code{0.8}.
#' @param label.expand The relative expand value for the right side.
#'   Will be used only if \code{show.labels} is \code{TRUE}.
#'   The default value is \code{0.15}.
#' @param style.pulsar Applies the plot style which is similar to
#'   the iconic plot of a pulsar and the
#'   \href{https://en.wikipedia.org/wiki/Unknown_Pleasures}{"Unknown Pleasures"}
#'   Joy Division album cover. If it's true, the values of \code{palette},
#'   \code{bg}, \code{col}, \code{alpha}, \code{show.axis},
#'   \code{show.underline} will be ignored.
#'   The default value is \code{FALSE}.
#'
#' @examples
#' waterfall(rdistrs(5), build_plot = build_density)
#' waterfall(rdistrs(5), build_plot = build_ft)
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom ggplot2 ggsave ggplot theme element_rect ggtitle annotate
#' @importFrom ggplot2 geom_point unit scale_x_continuous ylim element_line
#' @importFrom ggplot2 element_blank element_text
#'
#' @export
waterfall <- function(
  x,
  build_plot,
  palette = NULL,
  bg = "white",
  col = "black",
  metric = NULL,
  calc.label = NULL,
  sort.value = NULL,
  show.labels = F,
  show.axis = T,
  show.underline = T,
  hide.outliers = F,
  title = NULL,
  bw = "SJ",
  overlap = 0.4,
  alpha = 0.8,
  label.expand = 0.15,
  style.pulsar = F
  ) {
  if (hide.outliers)
    x <- lapply(x, remove_outliers)
  if (is.null(palette))
    palette <- colorRampPalette(c("red", "orange"))(min(10, length(x)))
  if (show.labels && is.null(calc.label)) {
    calc.label <- function(index, name, metric.value) {
      if (!is.null(name) && name != "")
        name
      else if (!is.na(metric.value))
        format(metric.value, digits = 2, nsmall = 2)
      else
        index
    }
  }
  if (!show.labels)
    label.expand <- 0
  if (style.pulsar) {
    palette <- "black"
    bg <- "black"
    col <- "white"
    show.axis <- F
    show.underline <- F
    alpha <- 1
  }
  show.title = !is.null(title) && title != ""

  N <- length(x)
  if (is.null(metric)) {
    metrics <- rep(NA, N)
  } else {
    metrics <- sapply(x, metric)
  }
  if (is.null(sort.value)) {
    order <- 1:N
  } else if (is.character(sort.value) && sort.value == "metric" && !is.null(metric)) {
    order <- sort(metrics, index.return = T)$ix
  } else {
    sort.metric <- sapply(x, sort.value)
    order <- sort(sort.metric, index.return = T)$ix
  }
  x.lims <- range(unlist(x))

  wf.padding.top <- ifelse(show.title, 0.05, 0) # Space for title
  wf.padding.bottom <- ifelse(show.axis, 0.06, 0) # space for axis
  wf.axis.height <- ifelse(show.axis, 0.03, 0)
  wf.height <- 1 / N ^ (1 - overlap)
  wf.y <- function(i) 1 - wf.padding.top - wf.height - (i - 1) *
    (1 - wf.padding.bottom - wf.axis.height- wf.padding.top - wf.height)/(N - 1)
  wf.fill <- function(i) palette[((i - 1) %% length(palette)) + 1]

  p.base <- ggplot() + theme(plot.background = element_rect(fill = bg),
                             panel.background = element_rect(fill = bg))
  if (show.title)
    p.base <- p.base +
    ggtitle(title) +
    theme(plot.title = element_text(color = col, hjust = 0.5))
  p <- ggdraw() + draw_plot(p.base)
  for (i in 1:N) {
      xi <- x[[order[i]]]
      mi <- metrics[order[i]]
      pi <- build_plot(x = xi,
                       x.lims = x.lims,
                       fill = wf.fill(i),
                       col = col,
                       bw = bw,
                       show.underline = show.underline,
                       alpha = alpha,
                       label.expand = label.expand)
      if (show.labels)
          pi <- pi +
            annotate("text",
                     label = paste0(" ", calc.label(i, names(x)[order[i]], mi)),
                     x = x.lims[2],
                     y = 0,
                     hjust = 0,
                     vjust = 0,
                     colour = col,
                     family = "mono")
      p <- p + draw_plot(pi, 0, wf.y(i), 1, wf.height)
  }

  if (show.axis) {
    axis.range <- x.lims[2] - x.lims[1]
    axis.breaks <- pretty(
      c(x.lims[1] + axis.range * 0.1, x.lims[2] - axis.range * 0.1),
      n = 5)
    axis.plot <- ggplot(data.frame()) +
      geom_point() +
      scale_x_continuous(
        limits = c(x.lims[1], x.lims[2]),
        expand = c(0, 0, label.expand, 0),
        breaks = axis.breaks) +
      ylim(0, 1) +
      theme(
        axis.line = element_line(colour = col),
        axis.ticks = element_line(colour = col),
        axis.ticks.x = element_line(colour = col),
        axis.text.x = element_text(colour = col),
        panel.grid = element_line(colour = col),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_rect(fill = bg, colour = bg),
        plot.background = element_rect(fill = bg, colour = bg),
        panel.background = element_rect(fill = bg, colour = bg)
      )
    if (show.labels)
     axis.plot <- axis.plot
    p <- p + draw_plot(axis.plot, 0, wf.axis.height, 1, wf.padding.bottom)
  }

  p
}

#' Density waterfall plot
#'
#' Draw a composition of overlapped density plots.
#'
#' Inspired by the series of
#' \href{http://www.brendangregg.com/frequencytrails.html}{blog posts}
#' by Brendan Gregg.
#'
#' @inheritParams waterfall
#'
#' @examples
#' waterfall_density(rdistrs(5))
#' waterfall_density(rdistrs(7), style.pulsar = TRUE)
#'
#' @export
waterfall_density  <- function(
  x,
  palette = NULL,
  bg = "white",
  col = "black",
  metric = NULL,
  calc.label = NULL,
  sort.value = NULL,
  show.labels = F,
  show.axis = T,
  show.underline = T,
  hide.outliers = F,
  title = NULL,
  bw = "SJ",
  overlap = 0.4,
  alpha = 0.8,
  label.expand = 0.15,
  style.pulsar = F
) waterfall(
  x = x,
  build_plot = build_density,
  palette = palette,
  bg = bg,
  col = col,
  metric = metric,
  calc.label = calc.label,
  sort.value = sort.value,
  show.labels = show.labels,
  show.axis = show.axis,
  show.underline = show.underline,
  hide.outliers = hide.outliers,
  title = title,
  bw = bw,
  overlap = overlap,
  alpha = alpha,
  label.expand = label.expand,
  style.pulsar = style.pulsar
)

#' Frequency trail waterfall plot
#'
#' Draw a composition of overlapped frequency trail plots.
#' You can find the definition of frequency trails in
#' \href{http://www.brendangregg.com/FrequencyTrails/intro.html}{Brendan Gregg's Blog}.
#'
#' Inspired by the series of
#' \href{http://www.brendangregg.com/frequencytrails.html}{blog posts}
#' by Brendan Gregg.
#'
#' @inheritParams waterfall
#'
#' @examples
#' waterfall_ft(rdistrs(5))
#' waterfall_ft(rdistrs(7), style.pulsar = TRUE)
#'
#' @export
waterfall_ft <- function(
  x,
  palette = NULL,
  bg = "white",
  col = "black",
  metric = NULL,
  calc.label = NULL,
  sort.value = NULL,
  show.labels = F,
  show.axis = T,
  show.underline = T,
  hide.outliers = F,
  title = NULL,
  bw = "SJ",
  overlap = 0.4,
  alpha = 0.8,
  label.expand = 0.15,
  style.pulsar = F
) waterfall(
  x = x,
  build_plot = build_ft,
  palette = palette,
  bg = bg,
  col = col,
  metric = metric,
  calc.label = calc.label,
  sort.value = sort.value,
  show.labels = show.labels,
  show.axis = show.axis,
  show.underline = show.underline,
  hide.outliers = hide.outliers,
  title = title,
  bw = bw,
  overlap = overlap,
  alpha = alpha,
  label.expand = label.expand,
  style.pulsar = style.pulsar
)