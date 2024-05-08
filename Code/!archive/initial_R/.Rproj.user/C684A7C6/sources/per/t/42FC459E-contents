

make_texreg <- function(name, model, custom.model.names = NULL, custom.coef.map = NULL, digits = 3,
                        custom.gof.rows = NULL, custom.note = NULL, scalebox= 1, caption = NULL, refresh = FALSE, 
                        custom.header = NULL){
  
  addspacing <- function(table){
    x <- paste0("\\renewcommand{\\baselinestretch}{1.25}%\n", table)
    y <- paste0(x, "\n\\renewcommand{\\baselinestretch}{1.67}%")
    return(y)
  }
  
  table <- texreg(l = model,
                  custom.header = custom.header,
                  custom.model.names = custom.model.names,
                  custom.coef.map = custom.coef.map,
                  custom.note = custom.note,
                  custom.gof.rows = custom.gof.rows,
                  caption = caption,
                  digits = digits,
                  include.ci = F,
                  caption.above = T,
                  include.adjrs = F,
                  include.rmse = F,
                  threeparttable = T,
                  custom.gof.names = c(NA, "N"),
                  label = paste0("tab:", name),
                  scalebox = scalebox,
                  use.packages = F,
                  float.pos = "H",
                  booktabs = T)
  print(table) # maybe change this to cat table at the bottom
  table <- addspacing(table)
  if(refresh == T){
    cat(table, sep = "\n", file = paste0(save_path, "tables/", name, ".tex"))
  }
}

make_kable <- function(name, model, caption, add_header = NULL,
                       scalebox = 1,
                       footnote = NULL, refresh = FALSE){
  
  addspacing <- function(table){
    x <- paste0("\\renewcommand{\\baselinestretch}{1.25}%\n", table)
    y <- paste0(x, "\n\\renewcommand{\\baselinestretch}{1.67}%")
    return(y)
  }
  
  footnote = paste0(footnote, "}")
  
  table <- kable(model, "latex",
                 digits = 2,
                 caption = caption,
                 booktabs = T,
                 escape = F,
                 linesep = "",
                 align = "c",
                 label = name) %>% 
    kable_styling(latex_options = c("hold_position", "scale_down")) %>% 
    add_header_above(add_header) %>% 
    add_footnote(footnote, escape = F, threeparttable = T,
                 notation ="none") %>% 
    paste0()
  
  # these three lines move the "centering" and "resizebox" to be above "begin{threeparttable}", which is an artifact of using
  # the add_footnote command instead of the footnote command. If we don't do these three lines, then 
  # the "resizebox" interferes with the threeparttable in a way that does not produce a workable latex document 
  cut = unlist(strsplit(table, "\\n"))[!unlist(lapply(strsplit(table, "\\n"), str_detect, "centering|resizebox"))]
  copy = unlist(strsplit(table, "\\n"))[unlist(lapply(strsplit(table, "\\n"), str_detect, "centering|resizebox"))]
  table = paste0(c(cut[1], copy, cut[2:length(cut)]), collapse = "\n")
  
  # changes footnote size to "scriptsize" to standardize with texreg
  table <- str_replace(table, "\\\\small", "\\\\scriptsize\\{")
  # changes "resizebox" to "scalebox", with adjustable scalebox parameter
  table <- str_replace(table, "\\\\resizebox\\{\\\\linewidth\\}\\{!\\}", paste0("\\\\scalebox{", scalebox, "}"))
  # edits tablenotes environment to "flushleft
  table <- str_replace(table, "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")
  # edits the "item" in the footnote environment to move it back so that it is flush with the table
  table <- str_replace(table, "\\\\item \\[\\]", "\\\\item[\\\\hspace{-5mm}]")
  # left justifies the first column 
  table <- str_replace(table, "\\\\begin\\{tabular\\}\\[t\\]\\{c", "\\\\begin{tabular}[t]{l")
  
  table <- addspacing(table)
  if(refresh == T){
    cat(table, sep = "\n", file = paste0(save_path, "tables/", name, ".tex"))
  }
  cat(table, sep = "\n")
}
