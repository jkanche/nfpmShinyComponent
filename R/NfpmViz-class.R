setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))

#' Data container for an nfpm Track.
#'
#' @import htmltools
#' @importFrom methods new
#' @export
NfpmViz <- setRefClass("NfpmViz",
  fields=list(
    data="ListOrNULL",
    colors="CharacterOrNULL",
    settings="ListOrNULL",
    id="character"
  ),
  methods=list(
    initialize=function(data=NULL, colors=NULL,
      settings=NULL, id=rand_id(.self$get_component_type())) {
      .self$data <- data

      .self$colors <- .self$get_default_colors()
      .self$set_colors(colors)

      .self$settings <- .self$get_default_settings()
      .self$set_settings(settings)

      .self$id <- id
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "nfpm-viz"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("networkViz")
    },
        get_data=function() {
      "Get chart data"
      .self$data
    },
    get_colors=function() {
      "Get chart colors"
      .self$colors
    },
    get_settings=function() {
      "Get chart settings"
      .self$settings
    },
    set_data=function(data) {
      "Set chart data"
      .self$data <- data
      invisible()
    },
    set_colors=function(colors) {
      "Set chart colors"
      .self$colors[seq_len(length(colors))] <- colors
      invisible()
    },
    set_settings=function(settings) {
      "Modify current settings
      \\describe{
        \\item{settings}{List of new settings.
          Call get_available_settings for settings available to modify.
        }
      }"
      chart_settings <- .self$settings
      for (setting in names(settings)) {
        if (setting %in% names(chart_settings))
          chart_settings[[setting]] <- settings[[setting]]
      }
      .self$settings <- chart_settings

      invisible()
    },
    get_attributes=function() {
      "Get attributes for rendering component"
      
      json_data <- json_writer(.self$data)

      c(list(
        "data"=json_data,
        colors=json_writer(.self$colors),
        settings=json_writer(.self$settings))
      )
    },
    render_component=function(shiny=FALSE) {
      "Render to html"
      chart <- tag(.self$get_name(), .self$get_attributes())
      
      htmltools::attachDependencies(chart,
                      .self$get_dependencies(shiny))
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    show=function() {
      if (isTRUE(getOption('knitr.in.progress'))) {
        knitr::knit_print(.self$render_component()) 
      } else {
        # temporary directory for output
        tmp_dir <- tempfile(pattern=rand_id("networkViz"))
        dir.create(tmp_dir)

        # output file
        index_html <- file.path(tmp_dir, "index.html")

        # save file
        save_html(.self$render_component(), file=index_html) 

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    },
    get_dependencies=function(shiny=FALSE) {
      if (isTRUE(getOption('knitr.in.progress'))) { 
        polymer_lib <- system.file(package="networkViz", 
                                   "nfpm-viz", "build", "nfpm.html")
      } else if (shiny) { 
        shiny::addResourcePath('networkViz', 
                               system.file(package="networkViz", "nfpm-viz/build/")) 
        polymer_lib <- "networkViz/nfpm.html" 
      } else { 
        polymer_lib <- "lib/nfpm-viz-1/nfpm.html" 
      } 

      list(
        webcomponents=htmlDependency(
          name="webcomponents",
          version="0.7.24",
          src=system.file(package="networkViz", "nfpm-viz", "build"),
          script="webcomponents-lite.js"),
        polymer=htmlDependency(
          name="nfpm-viz",
          version="1",
          head=paste0("<link rel='import' href='",  polymer_lib, "'>"),
          src=system.file(package="networkViz", "nfpm-viz", "build"),
          all_files=TRUE)
      )
    }
  )
)