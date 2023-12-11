make <- function() {
  job::job(
    title = "Render",
    {{ quarto::quarto_render() }}
  )
}
