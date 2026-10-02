# ── Function Call Graph ────────────────────────────────────────────────────────
# Maps function definitions, call relationships, and data-flow objects across
# R scripts. Uses R's built-in AST parser (getParseData) so comments, strings,
# and inactive code are never misread as calls.
#
# Node types:
#   circles  — package functions (coloured by source file)
#   squares  — data objects that flow between two package functions
#              (i.e.  obj <- pkg_func(...)  and  other_pkg_func(obj, ...))
#   black ring — exported function (listed in NAMESPACE): a user entry point
#
# Edge types:
#   solid grey    — direct call:        f() calls g(...)
#   dashed blue   — indirect reference: f passes g by name without calling it,
#                   e.g. do.call(g, ...), lapply(x, g), labels = g, or f
#                   defines g inside itself (nested helper)
#   dotted orange — S3 dispatch:        f returns an object of class "cls" and
#                   R calls print.cls()/summary.cls() etc. on it automatically
#
# Run from the package root:
#   source("inst/function_call_graph.R")
# ──────────────────────────────────────────────────────────────────────────────

library(igraph)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)

# ── Config ─────────────────────────────────────────────────────────────────────
folder_path     <- "R"           # relative to working directory
exclude_pattern <- "Name clash"  # skip backup/conflict files


# ── 1. Collect R files ─────────────────────────────────────────────────────────
r_files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
r_files <- r_files[!grepl(exclude_pattern, r_files)]
message("Parsing ", length(r_files), " files: ",
        paste(basename(r_files), collapse = ", "))


# ── 2. Parse each file ─────────────────────────────────────────────────────────
parse_file <- function(filepath) {
  filename <- basename(filepath)

  parsed <- tryCatch(
    parse(filepath, keep.source = TRUE),
    error = function(e) { message("  Parse error in ", filename, ": ", e$message); NULL }
  )
  if (is.null(parsed)) return(NULL)

  pd <- getParseData(parsed, includeText = TRUE)
  if (is.null(pd) || nrow(pd) == 0) return(NULL)

  # ── Function definitions ──────────────────────────────────────────────────
  # Parse tree for  name <- function(args) { body }:
  #   assign_expr  [id=A]
  #     lhs_expr   [id=B]  ← contains SYMBOL "name"
  #     LEFT_ASSIGN "<-"
  #     func_expr  [id=C]  ← parent of FUNCTION token
  #       FUNCTION "function"
  func_rows <- pd[pd$token == "FUNCTION", ]
  defs <- do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(func_rows)), function(i) {
    func_expr_id   <- func_rows$parent[i]
    assign_expr_id <- pd[pd$id == func_expr_id, "parent"]
    if (length(assign_expr_id) == 0 || is.na(assign_expr_id) || assign_expr_id == 0)
      return(NULL)
    assign_children <- pd[pd$parent == assign_expr_id, ]
    if (!any(assign_children$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")))
      return(NULL)
    lhs_expr_ids <- assign_children$id[
      assign_children$token == "expr" & assign_children$id != func_expr_id]
    lhs_symbols  <- pd[pd$parent %in% lhs_expr_ids & pd$token == "SYMBOL", ]
    if (nrow(lhs_symbols) == 0) return(NULL)
    func_row <- pd[pd$id == func_expr_id, ]
    data.frame(func_name  = lhs_symbols$text[1], file = filename,
               line_start = func_row$line1, line_end = func_row$line2,
               expr_id    = func_expr_id, stringsAsFactors = FALSE)
  })))
  if (is.null(defs)) defs <- data.frame(
    func_name = character(), file = character(), line_start = integer(),
    line_end  = integer(),   expr_id = integer(), stringsAsFactors = FALSE)

  # ── Object productions: var <- func(...) ─────────────────────────────────
  la_rows   <- pd[pd$token == "LEFT_ASSIGN", ]
  obj_prods <- do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(la_rows)), function(i) {
    la       <- la_rows[i, ]
    children <- pd[pd$parent == la$parent, ]
    lhs_e    <- children[children$token == "expr" & children$col2 < la$col1, ]
    if (nrow(lhs_e) == 0) return(NULL)
    lhs_sym  <- pd[pd$parent == lhs_e$id[1] & pd$token == "SYMBOL", ]
    if (nrow(lhs_sym) == 0) return(NULL)
    rhs_e    <- children[children$token == "expr" & children$col1 > la$col1, ]
    if (nrow(rhs_e) == 0) return(NULL)
    rhs_func <- pd[pd$parent == rhs_e$id[1] & pd$token == "SYMBOL_FUNCTION_CALL", ]
    if (nrow(rhs_func) == 0) return(NULL)
    data.frame(obj = lhs_sym$text[1], func = rhs_func$text[1],
               file = filename, line = la$line1, stringsAsFactors = FALSE)
  })))
  if (is.null(obj_prods)) obj_prods <- data.frame(
    obj = character(), func = character(), file = character(),
    line = integer(), stringsAsFactors = FALSE)

  # ── Function calls ────────────────────────────────────────────────────────
  call_rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL", ]
  if (nrow(call_rows) == 0)
    return(list(file = filename, defs = defs, calls = data.frame(),
                obj_prods = obj_prods, obj_uses = data.frame(),
                refs = data.frame(caller = character(), ref = character(),
                                  file = character(), line = integer(),
                                  stringsAsFactors = FALSE)))

  def_lookup <- if (nrow(defs) > 0)
    setNames(defs$func_name, as.character(defs$expr_id))
  else character(0)

  parent_map <- setNames(pd$parent, as.character(pd$id))

  find_enclosing <- function(node_id) {
    cur  <- parent_map[as.character(node_id)]
    seen <- character(0)
    while (!is.na(cur) && cur != 0) {
      key <- as.character(cur)
      if (key %in% seen) break
      seen <- c(seen, key)
      if (key %in% names(def_lookup)) return(def_lookup[[key]])
      cur <- parent_map[key]
      if (is.na(cur)) break
    }
    "__toplevel__"
  }

  callers <- vapply(call_rows$id, find_enclosing, character(1))
  calls   <- data.frame(caller = callers, callee = call_rows$text,
                        file = filename, line = call_rows$line1,
                        stringsAsFactors = FALSE)

  # ── Object consumption: SYMBOL args passed to function calls ─────────────
  # Named args (func(x = df)) have structure: arg_expr → SYMBOL "x", EQ_SUB, expr → SYMBOL "df"
  # So we look one level deeper too, and exclude symbols that are named-arg names (followed by EQ_SUB).
  eq_sub_parents <- unique(pd$parent[pd$token == "EQ_SUB"])
  obj_uses <- do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(call_rows)), function(i) {
    call_id    <- call_rows$parent[i]
    func_nm    <- call_rows$text[i]
    arg_exprs  <- pd[pd$parent == call_id & pd$token == "expr", ]
    if (nrow(arg_exprs) == 0) return(NULL)
    inner_expr <- pd[pd$parent %in% arg_exprs$id & pd$token == "expr", ]
    all_parents <- c(arg_exprs$id, inner_expr$id)
    syms <- pd[pd$parent %in% all_parents & pd$token == "SYMBOL", ]
    syms <- syms[!syms$parent %in% eq_sub_parents, ]  # drop named-arg names
    if (nrow(syms) == 0) return(NULL)
    data.frame(obj = syms$text, func = func_nm,
               file = filename, line = syms$line1, stringsAsFactors = FALSE)
  })))
  if (is.null(obj_uses)) obj_uses <- data.frame(
    obj = character(), func = character(), file = character(),
    line = integer(), stringsAsFactors = FALSE)

  # ── Indirect references: functions used by name, not called ─────────────
  # A function can be used without the  name(...)  call syntax, which the
  # call detection above cannot see:
  #   do.call(impute_phases, args)     lapply(x, parse_prefix)
  #   scale_x_continuous(labels = date_fmt)   do.call("fname", args)
  # These show up as plain SYMBOL tokens (or strings in do.call/match.fun/get).
  # Symbols that are local variables or arguments of the enclosing function
  # (e.g. a variable that happens to be called `ord`) are ignored, so a
  # variable is not mistaken for a function.
  local_vars <- function(fn) {                 # variables assigned in function fn
    if (is.na(fn) || fn == "__toplevel__") return(character(0))
    ids <- defs$expr_id[defs$func_name == fn]
    out <- character(0)
    for (id in ids) {
      # all descendants of the function expression
      desc <- id; frontier <- id
      while (length(frontier)) {
        frontier <- pd$id[pd$parent %in% frontier]; desc <- c(desc, frontier)
      }
      sub <- pd[pd$id %in% desc, ]
      out <- c(out, sub$text[sub$token == "SYMBOL_FORMALS"])
      for (a in sub$id[sub$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")]) {
        par_id <- pd$parent[pd$id == a]
        kids   <- pd[pd$parent == par_id & pd$token == "expr", ]
        if (nrow(kids) < 2) next
        rhs_is_fun <- any(pd$token[pd$parent == kids$id[2]] == "FUNCTION")
        if (rhs_is_fun) next                    # a nested function, not a variable
        lhs <- pd$text[pd$parent == kids$id[1] & pd$token == "SYMBOL"]
        out <- c(out, lhs)
      }
    }
    unique(out)
  }
  sym_rows <- pd[pd$token == "SYMBOL", ]
  str_rows <- pd[pd$token == "STR_CONST", ]
  if (nrow(str_rows)) {
    # only strings given to do.call / match.fun / get name a function
    in_fun_arg <- vapply(str_rows$parent, function(p) {
      call_id <- pd$parent[pd$id == p]
      fn <- pd$text[pd$parent %in% pd$id[pd$parent == call_id] &
                    pd$token == "SYMBOL_FUNCTION_CALL"]
      length(fn) > 0 && fn[1] %in% c("do.call", "match.fun", "get")
    }, logical(1))
    str_rows <- str_rows[in_fun_arg, ]
    str_rows$text <- gsub("^[\"']|[\"']$", "", str_rows$text)
  }
  ref_rows <- rbind(sym_rows, str_rows)
  refs <- if (nrow(ref_rows)) {
    enc <- vapply(ref_rows$id, find_enclosing, character(1))
    r   <- data.frame(caller = enc, ref = ref_rows$text, file = filename,
                      line = ref_rows$line1, stringsAsFactors = FALSE)
    r   <- r[r$caller != "__toplevel__" & r$caller != r$ref, ]
    lv  <- lapply(setNames(unique(r$caller), unique(r$caller)), local_vars)
    keep <- mapply(function(cl, rf) !rf %in% lv[[cl]], r$caller, r$ref)
    r[keep, ]
  } else data.frame(caller = character(), ref = character(), file = character(),
                    line = integer(), stringsAsFactors = FALSE)

  # S3 classes created in each function. Only strings that SET a class count:
  #   class(x) <- c("cls", "list")      structure(x, class = c("cls", "list"))
  # (not inherits(x, "cls"), which only tests for it)
  descendants <- function(id) {
    out <- id; frontier <- id
    while (length(frontier)) { frontier <- pd$id[pd$parent %in% frontier]; out <- c(out, frontier) }
    out
  }
  value_exprs <- integer(0)
  for (a in pd$id[pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")]) {       # class(x) <- ...
    kids <- pd[pd$parent == pd$parent[pd$id == a] & pd$token == "expr", ]
    if (nrow(kids) < 2) next
    lhs_calls <- pd$text[pd$id %in% descendants(kids$id[1]) & pd$token == "SYMBOL_FUNCTION_CALL"]
    if ("class" %in% lhs_calls) value_exprs <- c(value_exprs, kids$id[2])
  }
  for (ss in pd$id[pd$token == "SYMBOL_SUB" & pd$text == "class"]) {   # class = ...
    sib <- pd[pd$parent == pd$parent[pd$id == ss], ]
    sib <- sib[order(sib$line1, sib$col1), ]
    pos <- which(sib$id == ss)
    nxt <- sib[seq_len(nrow(sib)) > pos & sib$token == "expr", ]
    if (nrow(nxt)) value_exprs <- c(value_exprs, nxt$id[1])
  }
  cls_ids  <- unlist(lapply(value_exprs, descendants))
  cls_rows <- pd[pd$id %in% cls_ids & pd$token == "STR_CONST", ]
  classes  <- if (nrow(cls_rows)) data.frame(
    func = vapply(cls_rows$id, find_enclosing, character(1)),
    cls  = gsub("^[\"']|[\"']$", "", cls_rows$text), stringsAsFactors = FALSE)
  else data.frame(func = character(), cls = character())

  list(file = filename, defs = defs, calls = calls,
       obj_prods = obj_prods, obj_uses = obj_uses, refs = refs,
       classes = classes)
}

results <- Filter(Negate(is.null), lapply(r_files, parse_file))


# ── 3. Combine across files ─────────────────────────────────────────────────────
defs_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (nrow(x$defs) > 0) x$defs[, c("func_name", "file", "line_start")] else NULL))
all_defs <- if (length(defs_list) > 0) do.call(rbind, defs_list) else
  data.frame(func_name = character(), file = character(), line_start = integer(),
             stringsAsFactors = FALSE)
# Drop infix operators (%||%, %in%, etc.) — not meaningful as graph nodes
all_defs <- all_defs[!grepl("^%.*%$", all_defs$func_name), ]

calls_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (nrow(x$calls) > 0) x$calls else NULL))
all_calls <- if (length(calls_list) > 0) do.call(rbind, calls_list) else
  data.frame(caller = character(), callee = character(), file = character(),
             line = integer(), stringsAsFactors = FALSE)

prods_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (nrow(x$obj_prods) > 0) x$obj_prods else NULL))
all_obj_prods <- if (length(prods_list) > 0) do.call(rbind, prods_list) else
  data.frame(obj = character(), func = character(), file = character(),
             line = integer(), stringsAsFactors = FALSE)

uses_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (nrow(x$obj_uses) > 0) x$obj_uses else NULL))
all_obj_uses <- if (length(uses_list) > 0) do.call(rbind, uses_list) else
  data.frame(obj = character(), func = character(), file = character(),
             line = integer(), stringsAsFactors = FALSE)

refs_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (!is.null(x$refs) && nrow(x$refs) > 0) x$refs else NULL))
all_refs <- if (length(refs_list) > 0) do.call(rbind, refs_list) else
  data.frame(caller = character(), ref = character(), file = character(),
             line = integer(), stringsAsFactors = FALSE)

cls_list <- Filter(Negate(is.null), lapply(results, function(x)
  if (!is.null(x$classes) && nrow(x$classes) > 0) x$classes else NULL))
all_classes <- if (length(cls_list) > 0) do.call(rbind, cls_list) else
  data.frame(func = character(), cls = character(), stringsAsFactors = FALSE)

# Exported functions (user entry points), read from NAMESPACE if present
exported <- if (file.exists("NAMESPACE")) {
  ns <- readLines("NAMESPACE")
  c(sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)),
    sub("^S3method\\(([^,]+),\\s*([^)]+)\\)$", "\\1.\\2",
        grep("^S3method\\(", ns, value = TRUE)))
} else character(0)
exported <- gsub("[\"`]", "", exported)

message(nrow(all_defs),  " function definitions found")
message(nrow(all_calls), " call sites found (total, including base R)")


# ── 4. Build edge tables ────────────────────────────────────────────────────────
known_funcs <- unique(all_defs$func_name)

# Function → function (direct internal calls)
internal_edges <- all_calls %>%
  filter(callee %in% known_funcs, caller %in% known_funcs, caller != callee) %>%
  select(from = caller, to = callee, call_file = file) %>%
  distinct()

# pkg_func → object  (function produces a named result)
obj_prod_edges <- all_obj_prods %>%
  filter(func %in% known_funcs) %>%
  transmute(from = func, to = obj, call_file = file) %>%
  distinct()

# object → pkg_func  (object passed as argument to a function)
# Only include objects that were also produced by a known function (keeps it clean)
produced_objs <- unique(obj_prod_edges$to)
obj_use_edges <- all_obj_uses %>%
  filter(func %in% known_funcs, obj %in% produced_objs) %>%
  filter(!obj %in% c("df", "data", "x")) %>% 
  transmute(from = obj, to = func, call_file = file) %>%
  distinct()

# Function -> function, indirect (used by name, see parse_file)
ref_edges <- all_refs %>%
  filter(ref %in% known_funcs, caller %in% known_funcs) %>%
  transmute(from = caller, to = ref, call_file = file) %>%
  distinct()

# Function -> S3 method: f creates an object of class "cls", R dispatches
# print.cls() / summary.cls() / ... on it
s3_generics <- c("print", "summary", "format", "plot", "autoplot", "as.data.frame",
                 "toString", "str", "head")
s3_pat      <- paste0("^(", gsub(".", "\\.", paste(s3_generics, collapse = "|"),
                                 fixed = TRUE), ")\\.(.+)$")
s3_methods  <- known_funcs[grepl(s3_pat, known_funcs)]
dispatch_edges <- if (length(s3_methods)) {
  do.call(rbind, lapply(s3_methods, function(m) {
    cls  <- sub(s3_pat, "\\2", m)
    from <- unique(all_classes$func[all_classes$cls == cls & all_classes$func %in% known_funcs])
    from <- setdiff(from, m)
    if (!length(from)) return(NULL)
    data.frame(from = from, to = m, call_file = NA_character_, stringsAsFactors = FALSE)
  }))
} else NULL

all_edges <- bind_rows(
    mutate(internal_edges, edge_type = "direct"),
    mutate(obj_prod_edges, edge_type = "direct"),
    mutate(obj_use_edges,  edge_type = "direct"),
    mutate(ref_edges,      edge_type = "reference"),
    if (!is.null(dispatch_edges)) mutate(dispatch_edges, edge_type = "dispatch")) %>%
  distinct(from, to, .keep_all = TRUE)       # a direct call wins over a reference

message(sum(all_edges$edge_type == "reference"), " indirect references, ",
        sum(all_edges$edge_type == "dispatch"), " S3 dispatch edges added")

# Node tables
func_nodes <- all_defs %>%
  transmute(name = func_name, file, node_type = "function") %>% distinct()

# Object nodes: only those that both receive AND pass data (bridges between funcs)
bridging_objs <- intersect(unique(obj_prod_edges$to), unique(obj_use_edges$from))
obj_nodes <- if (length(bridging_objs) > 0) {
  data.frame(name = bridging_objs, file = "object", node_type = "object",
             stringsAsFactors = FALSE)
} else {
  data.frame(name = character(), file = character(), node_type = character(),
             stringsAsFactors = FALSE)
}

all_nodes <- bind_rows(func_nodes, obj_nodes) %>% distinct(name, .keep_all = TRUE)

message(length(bridging_objs), " bridging objects: ",
        if (length(bridging_objs)) paste(bridging_objs, collapse = ", ") else "(none)")
message(nrow(all_nodes), " nodes, ", nrow(all_edges), " edges in graph")


# ── 5. Build igraph object ──────────────────────────────────────────────────────
# Hard-remove any infix operators (%||% etc.) that survived earlier filters
all_nodes <- all_nodes[!grepl("%", all_nodes$name, fixed = TRUE), ]
all_edges <- all_edges[!grepl("%", all_edges$from, fixed = TRUE) &
                       !grepl("%", all_edges$to,   fixed = TRUE), ]

g <- graph_from_data_frame(all_edges[, c("from", "to", "edge_type")],
                           directed = TRUE, vertices = all_nodes)

files_sorted <- sort(unique(func_nodes$file))
pal          <- hue_pal()(length(files_sorted))
file_colours <- c(setNames(pal, files_sorted), object = "#cccccc")

V(g)$color  <- file_colours[V(g)$file]
V(g)$deg_in <- degree(g, mode = "in")
V(g)$size   <- rescale(V(g)$deg_in + 1, to = c(6, 24))
# Opacity falls with node size: small nodes solid (alpha 1), the largest hubs
# see-through (alpha 0.35), so nodes and labels underneath stay visible.
V(g)$alpha  <- rescale(V(g)$size, from = c(6, 24), to = c(1, 0.35))
V(g)$fill   <- mapply(adjustcolor, V(g)$color, alpha.f = V(g)$alpha)
V(g)$shape  <- ifelse(V(g)$node_type == "object", "square", "circle")
V(g)$exported <- V(g)$name %in% exported
V(g)$frame  <- ifelse(V(g)$exported, "black", "white")

edge_styles <- data.frame(
  type  = c("direct", "reference", "dispatch"),
  label = c("direct call", "used by name (indirect)", "S3 dispatch (print etc.)"),
  col   = c("grey65", "#3b75c4", "#e08a00"),
  lty   = c(1, 2, 3), dash = c("solid", "dash", "dot"), stringsAsFactors = FALSE)
es <- edge_styles[match(E(g)$edge_type, edge_styles$type), ]
E(g)$color <- es$col
E(g)$lty   <- es$lty


# ── 6. Static igraph plot ───────────────────────────────────────────────────────
# Fruchterman-Reingold for the connected functions. Unconnected nodes
# (exported entry points with no internal links) are stacked in a column on
# the left instead of being scattered, which would squeeze the main graph.
set.seed(42)
lay <- layout_with_fr(g, niter = 3000)
iso <- which(degree(g) == 0)
if (length(iso) && length(iso) < vcount(g)) {
  main <- setdiff(seq_len(vcount(g)), iso)
  rx <- range(lay[main, 1]); ry <- range(lay[main, 2])
  lay[iso, 1] <- rx[1] - 0.12 * diff(rx)
  lay[iso, 2] <- seq(ry[2], ry[1], length.out = length(iso) + 2)[-c(1, length(iso) + 2)]
}

plot_call_graph <- function() {
  old_par <- par(mar = c(0, 0, 1, 0))
  on.exit(par(old_par))
  plot(
    g,
    layout             = lay,
    vertex.size        = V(g)$size,
    vertex.color       = V(g)$fill,
    vertex.shape       = V(g)$shape,
    vertex.label       = V(g)$name,
    vertex.label.cex   = 0.55,
    vertex.label.dist  = 0.9,
    vertex.frame.color = V(g)$frame,
    edge.arrow.size    = 0.22,
    edge.color         = E(g)$color,
    edge.lty           = E(g)$lty,
    main               = "Function Call Graph",
    xlim               = c(-1, 1.55)     # leaves room on the right for the legends
  )
  legend("topright", inset = 0.01,
         legend = c(names(file_colours)[names(file_colours) != "object"], "data object"),
         fill   = c(file_colours[names(file_colours) != "object"], "#cccccc"),
         cex = 0.62, title = "Source file", bty = "n", xpd = TRUE)
  legend("bottomright", inset = 0.01,
         legend = c(edge_styles$label, "exported (user-facing)"),
         col    = c(edge_styles$col, "black"),
         lty    = c(edge_styles$lty, NA), lwd = c(1.5, 1.5, 1.5, NA),
         pch    = c(NA, NA, NA, 1), pt.cex = 1.2,
         cex = 0.62, title = "Links", bty = "n", xpd = TRUE)
}

plot_call_graph()


# ── 7. Interactive plotly plot ──────────────────────────────────────────────────
node_df <- data.frame(
  name      = V(g)$name,
  x         = lay[, 1],
  y         = lay[, 2],
  file      = V(g)$file,
  color     = V(g)$color,
  size      = V(g)$size,
  deg_in    = V(g)$deg_in,
  deg_out   = degree(g, mode = "out"),
  node_type = V(g)$node_type,
  exported  = V(g)$exported,
  alpha     = V(g)$alpha,
  stringsAsFactors = FALSE
)

el     <- as_edgelist(g)
idx    <- match(el[, 1], node_df$name)
jdx    <- match(el[, 2], node_df$name)
ok     <- !is.na(idx) & !is.na(jdx)

p_interactive <- plot_ly()
for (k in seq_len(nrow(edge_styles))) {
  sel <- ok & E(g)$edge_type == edge_styles$type[k]
  if (!any(sel)) next
  p_interactive <- p_interactive %>% add_trace(
    type = "scatter", mode = "lines",
    x = as.vector(rbind(node_df$x[idx[sel]], node_df$x[jdx[sel]], NA)),
    y = as.vector(rbind(node_df$y[idx[sel]], node_df$y[jdx[sel]], NA)),
    line = list(color = edge_styles$col[k], width = 1, dash = edge_styles$dash[k]),
    name = edge_styles$label[k], hoverinfo = "none", showlegend = TRUE)
}

p_interactive <- p_interactive %>%
  add_trace(
    type         = "scatter",
    mode         = "markers+text",
    x            = node_df$x,
    y            = node_df$y,
    text         = node_df$name,
    textposition = "top center",
    textfont     = list(size = 9, color = "black"),
    marker       = list(
      size   = node_df$size * 1.6,
      color  = node_df$color,
      opacity = node_df$alpha,
      symbol = ifelse(node_df$node_type == "object", "square", "circle"),
      line   = list(color = ifelse(node_df$exported, "black", "white"), width = 1)
    ),
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      "Type: ", node_df$node_type, "<br>",
      "File: ", node_df$file, "<br>",
      ifelse(node_df$exported, "Exported (user-facing)<br>", ""),
      "In-degree: ",  node_df$deg_in,  "<br>",
      "Out-degree: ", node_df$deg_out,
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) %>%
  layout(
    title       = list(text = "QuantWealthR — Function Call Graph",
                       font = list(size = 15)),
    xaxis       = list(showgrid = FALSE, zeroline = FALSE,
                       showticklabels = FALSE, title = ""),
    yaxis       = list(showgrid = FALSE, zeroline = FALSE,
                       showticklabels = FALSE, title = ""),
    hovermode   = "closest",
    legend      = list(title = list(text = "<b>Source file</b>"),
                       itemsizing = "constant"),
    paper_bgcolor = "#f8f8f8",
    plot_bgcolor  = "#f8f8f8"
  )

for (f in files_sorted) {
  p_interactive <- p_interactive %>%
    add_trace(type = "scatter", mode = "markers",
              x = list(NA_real_), y = list(NA_real_),
              name = f,
              marker = list(color = file_colours[[f]], size = 9),
              hoverinfo = "none", showlegend = TRUE)
}

print(p_interactive)


# ── 8. Summary ──────────────────────────────────────────────────────────────────
cat("\n── Definitions per file ──────────────────────────────────────\n")
print(sort(table(all_defs$file), decreasing = TRUE))

cat("\n── Most-called package functions ─────────────────────────────\n")
called_counts <- sort(table(all_calls$callee[all_calls$callee %in% known_funcs]),
                      decreasing = TRUE)
print(head(called_counts, 20))

no_caller <- known_funcs[!known_funcs %in% all_edges$to]
cat("\n── Entry points: exported, not used internally (expected) ────\n")
cat(paste(sort(intersect(no_caller, exported)), collapse = "\n"), "\n")
cat("\n── Not exported and never used: possibly dead code ───────────\n")
dead <- setdiff(no_caller[!grepl("%", no_caller, fixed = TRUE)], exported)   # operators like %||% are used as x %||% y
cat(if (length(dead)) paste(sort(dead), collapse = "\n") else "(none)", "\n")

cat("\n── Unexported helpers (leading dot) ──────────────────────────\n")
helpers <- known_funcs[startsWith(known_funcs, ".")]
cat(paste(sort(helpers), collapse = "\n"), "\n")
