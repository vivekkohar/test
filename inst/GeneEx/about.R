library(markdown)

about <-
  tabPanel("About",


           fluidPage(
          #    img(src='JAX.gif', align = "right"),

              fluidRow(
                withMathJax(),
               #     includeMarkdown("about.md"),
               # includeHTML(rmarkdown::render("AboutRMD.Rmd")),
                includeHTML("About.html"),
                hr()

             ),

             hr()
           )

  )
