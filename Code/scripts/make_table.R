make_kable <- function(model, col.names, caption, refresh = TRUE, scalebox = 1,
                       footnote = "", add_header = F, column_spec = F, resize = FALSE,
                       lr = F){
  
  addspacing <- function(table){
    x <- paste0("\\renewcommand{\\baselinestretch}{1.25}%\n", table)
    y <- paste0(x, "\n\\renewcommand{\\baselinestretch}{1.67}%")
    z <- str_replace(y, "\\n\\\\usepackage\\{threeparttable\\}\\n", "")
    return(z)
  }
  
  
  table <- kable(model,
                 digits = 3,
                 col.names = col.names,
                 caption = caption,
                 booktabs = T,
                 escape = F,
                 linesep = "",
                 align = c("l", rep("c", length(col.names)-1))) %>% 
    kable_styling(latex_options = c("HOLD_position"), position = "center")
  
  if(add_header == T){
    table <- table %>% 
      add_header_above(c("", "Initial Losing Coalition (4 -- 6)" =  1, 
                         "New Winning Coalitions (6 -- 4 and 10 -- 0)" = 2), escape = F)
  }
  
  if(column_spec == T){
    table <- table %>% 
      column_spec(6, bold = T) 
  }
  table <- table %>% 
    add_footnote(str_wrap(footnote, width = 1000), threeparttable = T, escape = F,
                 notation = "none")
  
  
  table <- paste0(table)
  table <- addspacing(table)
  
  # cut = unlist(strsplit(table, "\\n"))[!unlist(lapply(strsplit(table, "\\n"), str_detect, "resizebox"))]
  # copy = unlist(strsplit(table, "\\n"))[unlist(lapply(strsplit(table, "\\n"), str_detect, "resizebox"))]
  # table = paste0(c(cut[1], copy, cut[2:length(cut)]), collapse = "\\n")
  # if(lr == T){
  #  table <- str_replace(table, "\\\\resizebox\\{\\\\linewidth\\}\\{\\!\\}\\{", 
  #                        "\\\\makebox\\{\\\\linewidth\\}\\{")
  #   table <- str_replace(table, "\\\\small", "\\\\linespread\\{1\\}\\\\footnotesize")
  # }
  
  if(resize == TRUE){
    table <- str_replace(table, "\\\\begin\\{three", paste0("\\\\resizebox\\{\\\\linewidth\\}\\{\\!\\}\\{\n\\\\begin\\{three"))
    table <- str_replace(table, "end\\{threeparttable\\}", "end\\{threeparttable\\}\\}")
  }
  #table <- str_replace(table, "\\\\begin\\{three", paste0("\\\\scalebox\\{", scalebox, "\\}\\{\n\\\\begin\\{three"))
  table <- str_replace(table, "\\\\small", "\\\\scriptsize")
  table <- str_replace(table, "\\\\item \\[\\]", "\\\\item\\[\\\\hspace\\{-5mm\\}\\]")
  table <- str_replace(table, "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")
  ## Move up \centering
  table <- str_replace(table, "\\\\centering", "")
  table <- str_replace(table, "\\[H\\]", "\\[H\\]\n\\\\centering")
  table <- str_replace(table, "\\\\end\\{tabular\\}", paste0("\\\\end\\{tabular\\}\\\\n\\\\label\\{tab:", deparse(substitute(model)), "\\}"))
  
  table <- unlist(str_split(table, "\\\\n"))
  
  
  
  
  
  cat(table, sep = "\n")
  
  if(refresh == TRUE){
    cat(table, sep = "\n", file = paste0(save_path, "tables/",
                                         deparse(substitute(model)), ".tex"))
  }
}
