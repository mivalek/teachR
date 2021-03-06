#' An R Markdown output format for remark.js slides
#' 
#' This is an edited version of \code{xaringan::moon_reader()} that works with
#' blogdown. Below is the original documentation because I'm lazy.
#' 
#' This output format produces an HTML file that contains the Markdown source
#' (knitted from R Markdown) and JavaScript code to render slides.
#' \code{tsukuyomi()} is an alias of \code{moon_reader()}.
#'
#' Tsukuyomi is a genjutsu to trap the target in an illusion on eye contact.
#'
#' If you are unfamiliar with CSS, please see the
#' \href{https://github.com/yihui/xaringan/wiki}{xaringan wiki on Github}
#' providing CSS slide modification examples.
#' @param css A vector of CSS file paths. Two default CSS files
#'   (\file{default.css} and \file{default-fonts.css}) are provided in this
#'   package, which was borrowed from \url{https://remarkjs.com}. If the
#'   character vector \code{css} contains a value that does not end with
#'   \code{.css}, it is supposed to be a built-in CSS file in this package,
#'   e.g., for \code{css = c('default', 'extra.css')}), it means
#'   \code{default.css} in this package and a user-provided \code{extra.css}. To
#'   find out all built-in CSS files, use \code{xaringan:::list_css()}. With
#'   \pkg{rmarkdown} >= 2.8, Sass files (filenames ending with \file{.scss} or
#'   \file{.sass}) can also be used, and they will be processed by the
#'   \pkg{sass} package, which needs to be installed.
#' @param self_contained Whether to produce a self-contained HTML file by
#'   embedding all external resources into the HTML file. See the \sQuote{Note}
#'   section below.
#' @param seal Whether to generate a title slide automatically using the YAML
#'   metadata of the R Markdown document (if \code{FALSE}, you should write the
#'   title slide by yourself).
#' @param yolo Whether to insert the
#'   \href{https://kbroman.wordpress.com/2014/08/28/the-mustache-photo/}{Mustache
#'    Karl (TM)} randomly in the slides. \code{TRUE} means insert his picture on
#'   one slide, and if you want him to be on multiple slides, set \code{yolo} to
#'   a positive integer or a percentage (e.g. 0.3 means 30\% of your slides will
#'   be the Mustache Karl). Alternatively, \code{yolo} can also be a list of the
#'   form \code{list(times = n, img = path)}: \code{n} is the number of times to
#'   show an image, and \code{path} is the path to an image (by default, it is
#'   Karl).
#' @param chakra A path to the remark.js library (can be either local or
#'   remote). Please note that if you use the default remote latest version of
#'   remark.js, your slides will not work when you do not have Internet access.
#'   They might also be broken after a newer version of remark.js is released.
#'   If these issues concern you, you should download remark.js locally (e.g.,
#'   via \code{\link{summon_remark}()}), and use the local version instead.
#' @param nature (Nature transformation) A list of configurations to be passed
#'   to \code{remark.create()}, e.g. \code{list(ratio = '16:9', navigation =
#'   list(click = TRUE))}; see
#'   \url{https://github.com/gnab/remark/wiki/Configuration}. Besides the
#'   options provided by remark.js, you can also set \code{autoplay} to a number
#'   (the number of milliseconds) so the slides will be played every
#'   \code{autoplay} milliseconds; alternatively, \code{autoplay} can be a list
#'   of the form \code{list(interval = N, loop = TRUE)}, so the slides will go
#'   to the next page every \code{N} milliseconds, and optionally go back to the
#'   first page to restart the play when \code{loop = TRUE}. You can also set
#'   \code{countdown} to a number (the number of milliseconds) to include a
#'   countdown timer on each slide. If using \code{autoplay}, you can optionally
#'   set \code{countdown} to \code{TRUE} to include a countdown equal to
#'   \code{autoplay}. To alter the set of classes applied to the title slide,
#'   you can optionally set \code{titleSlideClass} to a vector of classes; the
#'   default is \code{c("center", "middle", "inverse")}.
#' @param anchor_sections,... For \code{tsukuyomi()}, arguments passed to
#'   \code{moon_reader()}; for \code{moon_reader()}, arguments passed to
#'   \code{rmarkdown::\link{html_document}()}.
#' @note Do not stare at Karl's picture for too long after you turn on the
#'   \code{yolo} mode. I believe he has Sharingan.
#'
#'   For the option \code{self_contained = TRUE}, it encodes images as base64
#'   data in the HTML output file. The image path should not contain the string
#'   \code{")"} when the image is written with the syntax \verb{![](PATH)} or
#'   \verb{background-image: url(PATH)}, and should not contain the string
#'   \code{"/>"} when it is written with the syntax \verb{<img src="PATH" />}.
#'   Rendering slides in the self-contained mode can be time-consuming when you
#'   have remote resources (such as images or JS libraries) in your slides
#'   because these resources need to be downloaded first. We strongly recommend
#'   that you download remark.js (via \code{\link{summon_remark}()}) and use a
#'   local copy instead of the default \code{chakra} argument when
#'   \code{self_contained = TRUE}, so remark.js does not need to be downloaded
#'   each time you compile your slides.
#'
#'   When the slides are previewed via \code{xaringan::\link{inf_mr}()},
#'   \code{self_contained} will be temporarily changed to \code{FALSE} even if
#'   the author of the slides set it to \code{TRUE}. This will make it faster to
#'   preview slides locally (by avoiding downloading remote resources explicitly
#'   and base64 encoding them). You can always click the Knit button in RStudio
#'   or call \code{rmarkdown::render()} to render the slides in the
#'   self-contained mode (these approaches will respect the
#'   \code{self_contained} setting).
#'
#'   Each page has its own countdown timer (when the option \code{countdown} is
#'   set in \code{nature}), and the timer is (re)initialized whenever you
#'   navigate to a new page. If you need a global timer, you can use the
#'   presenter's mode (press \kbd{P}).
#' @references \url{https://naruto.fandom.com/wiki/Tsukuyomi}
#' @importFrom htmltools tagList tags htmlEscape HTML
#' @export
#' @examples
#' # rmarkdown::render('foo.Rmd', 'teachR::xaringan_slides')
xaringan_slides = function(
  self_contained = TRUE, seal = TRUE, yolo = FALSE,
  chakra = 'https://remarkjs.com/downloads/remark-latest.min.js', nature = list(highlightStyle = "github", highlightLines = TRUE, countIncrementalSlides = FALSE),
  anchor_sections = FALSE, ...
) {
  
  file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
    paste(tools::file_path_sans_ext(file),
          ".", meta_ext, ".", ext,
          sep = ""
    )
  }
  
  get_parent_env_with <- function(var_names) {
    for (frame in rev(sys.frames())[-1]) {
      present <- all(vapply(
        var_names, exists, logical(1),
        envir = frame, inherits = FALSE
      ))
      if (present) return(frame)
    }
    stop(
      "No parent environment found with ",
      paste(var_names, collapse = ", ")
    )
  }
  
  tmp_js = tempfile('xaringan', fileext = '.js')  # write JS config to this file
  tmp_md = tempfile('xaringan', fileext = '.md')  # store md content here (bypass Pandoc)
  options(xaringan.page_number.offset = if (seal) 0L else -1L)
  
  if (is.numeric(autoplay <- nature[['autoplay']])) {
    autoplay = list(interval = autoplay, loop = FALSE)
  }
  play_js = if (is.numeric(intv <- autoplay$interval) && intv > 0) sprintf(
    'setInterval(function() {slideshow.gotoNextSlide();%s}, %d);',
    if (!isTRUE(autoplay$loop)) '' else
      ' if (slideshow.getCurrentSlideIndex() == slideshow.getSlideCount() - 1) slideshow.gotoFirstSlide();',
    intv
  )
  
  if (isTRUE(countdown <- nature[['countdown']])) countdown = autoplay
  countdown_js = if (is.numeric(countdown) && countdown > 0) sprintf(
    '(%s)(%d);', xaringan:::pkg_file('js/countdown.js'), countdown
  )
  
  if (is.null(title_cls <- nature[['titleSlideClass']]))
    title_cls = c('middle', 'inverse')
  title_cls = paste(c(title_cls, 'title-slide'), collapse = ', ')
  
  before = nature[['beforeInit']]
  for (i in c('countdown', 'autoplay', 'beforeInit', 'titleSlideClass')) nature[[i]] = NULL
  
  
  html_document2 = function(
    ..., includes = list(), mathjax = 'default', pandoc_args = NULL
  ) {
    if (length(includes) == 0) includes = list()
    includes$before_body = c(includes$before_body, tmp_md)
    includes$after_body = c(tmp_js, includes$after_body)
    if (identical(mathjax, 'local'))
      stop("mathjax = 'local' does not work for moon_reader()")
    if (!is.null(mathjax)) {
      if (identical(mathjax, 'default')) {
        mathjax = 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-MML-AM_CHTML'
      }
      pandoc_args = c(pandoc_args, '-V', paste0('mathjax-url=', mathjax))
      mathjax = NULL
    }
    pandoc_args = c(pandoc_args, '-V', paste0('title-slide-class=', title_cls))
    rmarkdown::html_document(
      ..., includes = includes, mathjax = mathjax, pandoc_args = pandoc_args
    )
  }
  
  highlight_hooks = NULL
  if (isTRUE(nature$highlightLines)) {
    hooks = knitr::hooks_markdown()[c('source', 'output')]
    highlight_hooks = list(
      source = function(x, options) {
        hook = hooks[['source']]
        res = hook(x, options)
        xaringan:::highlight_code(res)
      },
      output = function(x, options) {
        hook = hooks[['output']]
        res = xaringan:::highlight_output(x, options)
        hook(res, options)
      }
    )
  }
  
  # don't use Pandoc raw blocks ```{=} (#293)
  opts = options(htmltools.preserve.raw = FALSE)
  
  pre_knit <- function(input, ...) {
    render_env <- get_parent_env_with("knit_input")
    pre_knit_input <- get("knit_input", envir = render_env)
    intermediates_loc <- get("intermediates_loc", envir = render_env)
    
    setup_file = here::here('themes/teachR/static/R/slides_setup.R')
    if (file.exists(setup_file)) {
      setup_file = paste0("\nsource('", setup_file, "')")
    } else { setup_file = "" }
    add_to_setup = paste0("\\1", setup_file, "\nxaringanExtra:::register_panelset_knitr_hooks()")
    
    rmd_text <- readChar(input, file.info(input)$size)
    rmd_text <- gsub("\r\n", "\n", rmd_text)
    rmd_text <- sub("(```\\s*\\{\\s*r.*?setup.*?\\})", add_to_setup, rmd_text)
    preprocessed_rmd_file <- intermediates_loc(
      file_with_meta_ext(pre_knit_input, "preproc")
    )
    cat(rmd_text, file = preprocessed_rmd_file)
    assign("knit_input", preprocessed_rmd_file, envir = render_env)
  }
  
  rmarkdown::output_format(
    rmarkdown::knitr_options(knit_hooks = highlight_hooks),
    NULL, clean_supporting = self_contained,
    pre_processor = function(
      metadata, input_file, runtime, knit_meta, files_dir, output_dir
    ) {
      res = xaringan:::split_yaml_body(input_file)
      xfun::write_utf8(res$yaml, input_file)
      res$body = xfun::protect_math(res$body)
      if (self_contained) {
        xaringan:::clean_env_images()
        res$body = xaringan:::encode_images(res$body)
        cat(sprintf(
          "<script id='self-contained'>const selfContained = %s;</script>",
          xfun::tojson(as.list(xaringan:::env_images, all.names = TRUE))
        ), file = tmp_js, append = TRUE)
      }
      content = htmltools:::htmlEscape(xaringan:::yolofy(res$body, yolo))
      Encoding(content) = 'UTF-8'
      xfun::write_utf8(content, tmp_md)
      c(
        if (seal) c('--variable', 'title-slide=true'),
        if (!identical(body, res$body)) c('--variable', 'math=true')
      )
    },
    pre_knit = pre_knit,
    on_exit = function() {
      file.remove(list.files(pattern = "preproc\\.[Rr]md"))
      unlink(c(tmp_md, tmp_js))
      options(opts)
    },
    base_format = html_document2(
      self_contained = self_contained, theme = NULL, highlight = NULL,
      template = system.file('resources', 'template-slides.html', package = 'teachR', mustWork = TRUE),
      anchor_sections = anchor_sections, ...
    )
  )
}
