#!/usr/bin/env Rscript
# 问卷数据质控扫描（步骤 ①②③④⑤⑥）—— 只读，不修改任何数据文件
# 用法: Rscript qc_scan.R <数据文件> [-r 规则.yaml] -o <输出目录>

suppressPackageStartupMessages({
  library(haven); library(dplyr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------- 参数 ----------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("用法: qc_scan.R <数据文件> [-r 规则.yaml] -o <输出目录>")
data_path <- args[1]
rules_path <- NULL; out_dir <- "qc_scan_out"
i <- 2
while (i <= length(args)) {
  if (args[i] == "-r") { rules_path <- args[i+1]; i <- i + 2 }
  else if (args[i] == "-o") { out_dir <- args[i+1]; i <- i + 2 }
  else stop("未知参数: ", args[i])
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

LOG <- file.path(out_dir, "扫描日志.txt")
logcon <- file(LOG, "w", encoding = "UTF-8")
msg <- function(...) { s <- paste0(...); cat(s, "\n"); writeLines(s, logcon) }

num <- function(x) suppressWarnings(as.numeric(x))
is_labelled <- function(x) inherits(x, "haven_labelled") || !is.null(attr(x, "labels"))
is_datecol  <- function(x) inherits(x, "Date") || inherits(x, "POSIXct")

# ---------------- 读数据 ----------------
msg("=== 问卷数据质控扫描 ===")
msg("数据文件: ", data_path)
if (file.exists(data_path)) msg("md5: ", tools::md5sum(data_path))
ext <- tolower(tools::file_ext(data_path))
d <- switch(ext,
  sav  = haven::read_sav(data_path),
  dta  = haven::read_dta(data_path),
  xlsx = , xls = as.data.frame(readxl::read_excel(data_path), check.names = FALSE),
  csv  = utils::read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE,
                         fileEncoding = "UTF-8"),
  stop("不支持的格式: .", ext)
)
d <- as.data.frame(d, check.names = FALSE)
n <- nrow(d); p <- ncol(d)
msg("规模: ", n, " 行 × ", p, " 列")
msg("")

nm <- names(d)
## 逐列类型归类
type <- sapply(d, function(x) {
  if (is_datecol(x)) "date"
  else if (is.character(x) || is.factor(x)) "text"
  else if (is_labelled(x)) "categorical"
  else "numeric"
})

# 变量元数据
meta <- do.call(rbind, lapply(nm, function(v) {
  x <- d[[v]]; vl <- attr(x, "labels"); lb <- attr(x, "label")
  data.frame(变量 = v,
             类型 = type[[v]],
             标签 = if (length(lb)) paste(as.character(lb), collapse = " | ") else "",
             值标签数 = if (is.null(vl)) 0L else length(vl),
             非缺失 = sum(!is.na(x)),
             缺失 = sum(is.na(x)),
             缺失率 = sprintf("%.1f%%", 100*sum(is.na(x))/n),
             唯一值数 = length(unique(na.omit(x))),
             stringsAsFactors = FALSE)
}))
utils::write.csv(meta, file.path(out_dir, "05_变量元数据.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# ---------------- 规则 ----------------
if (!is.null(rules_path)) {
  suppressPackageStartupMessages(library(yaml))
  rules <- yaml::yaml.load_file(rules_path)
  msg("加载规则表: ", rules_path)
} else {
  rules <- list()
}
dup_key <- rules$dupes$key
miss_key <- rules$missing$key
out_cfg <- rules$outliers
if (is.null(out_cfg)) out_cfg <- list(method = "iqr", k = 1.5, columns = "auto")

# ================= ① 重复个案 =================
msg("① 查找重复个案 ...")
key <- if (!is.null(dup_key) && length(dup_key)) dup_key else nm
key <- intersect(key, nm)
sub <- d[, key, drop = FALSE]
dupmask <- duplicated(sub) | duplicated(sub, fromLast = TRUE)
dup_rows <- which(dupmask)
dup_out <- if (length(dup_rows)) {
  cbind(行号 = dup_rows, as.data.frame(sub[dup_rows, , drop = FALSE]))
} else data.frame(行号 = integer(0))
utils::write.csv(dup_out, file.path(out_dir, "01_重复个案.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
msg("   判定键: ", if (!is.null(dup_key) && length(dup_key)) paste(key, collapse=",") else "全部列")
msg("   重复行数: ", length(dup_rows))

# ================= ② 非法值 =================
msg("② 查找非法值 ...")
illegal <- list()
# 2a. 分类变量：取值超出值标签定义
for (v in nm[type == "categorical"]) {
  x <- d[[v]]; vl <- attr(x, "labels")
  legal <- unique(num(vl)); obs <- unique(num(x)); obs <- obs[!is.na(obs)]
  ex <- setdiff(obs, legal)
  if (length(ex)) {
    rows <- which(num(x) %in% ex)
    illegal[[length(illegal)+1]] <- data.frame(
      规则 = paste0(v, "_label_range"), 类型 = "取值超出值标签",
      变量 = v, 行号 = rows, 值 = num(x)[rows], stringsAsFactors = FALSE)
  }
}
# 2b. 用户自定义规则
for (r in rules$rules) {
  if (is.null(r$check)) next
  v <- r$column
  if (r$check == "in_set" && !is.null(v) && v %in% nm) {
    bad <- which(!is.na(num(d[[v]])) & !(num(d[[v]]) %in% unlist(r$values)))
    if (length(bad)) illegal[[length(illegal)+1]] <- data.frame(
      规则 = r$id %||% paste0(v,"_in_set"), 类型 = "不在合法集合内",
      变量 = v, 行号 = bad, 值 = num(d[[v]])[bad], stringsAsFactors = FALSE)
  }
  if (r$check == "between" && !is.null(v) && v %in% nm) {
    x <- num(d[[v]])
    # 注意：这里必须用向量化的 & 而不是 && —— x 是整列，
    #   `!is.null(r$min) && x < r$min` 会因 && 只接受标量而报错
    lo_bad <- if (!is.null(r$min)) !is.na(x) & x < r$min else rep(FALSE, length(x))
    hi_bad <- if (!is.null(r$max)) !is.na(x) & x > r$max else rep(FALSE, length(x))
    bad <- unname(which(lo_bad | hi_bad))
    if (length(bad)) illegal[[length(illegal)+1]] <- data.frame(
      规则 = r$id %||% paste0(v,"_between"), 类型 = "超出范围",
      变量 = v, 行号 = bad, 值 = unname(x[bad]), stringsAsFactors = FALSE)
  }
  if (r$check == "not_null" && !is.null(v) && v %in% nm) {
    bad <- which(is.na(d[[v]]))
    if (length(bad)) illegal[[length(illegal)+1]] <- data.frame(
      规则 = r$id %||% paste0(v,"_not_null"), 类型 = "不允许缺失",
      变量 = v, 行号 = bad, 值 = NA, stringsAsFactors = FALSE)
  }
  if (r$check == "regex" && !is.null(v) && v %in% nm) {
    x <- as.character(d[[v]])
    bad <- which(!is.na(x) & x != "" & !grepl(unlist(r$values)[1], x))
    if (length(bad)) illegal[[length(illegal)+1]] <- data.frame(
      规则 = r$id %||% paste0(v,"_regex"), 类型 = "不符合正则",
      变量 = v, 行号 = bad, 值 = x[bad], stringsAsFactors = FALSE)
  }
}
illegal_df <- if (length(illegal)) do.call(rbind, illegal) else
  data.frame(规则=character(0),类型=character(0),变量=character(0),行号=integer(0),值=numeric(0))
msg("   非法值条数: ", nrow(illegal_df), "（已剔除缺失；缺失另见 ④）")

# ================= ③ 逻辑关系 =================
msg("③ 核查逻辑关系 ...")
logic <- list()
push <- function(id, desc, rows, v1 = NA, v2 = NA) {
  if (!length(rows)) return(invisible())
  logic[[length(logic)+1]] <<- data.frame(
    检查项 = id, 说明 = desc, 行号 = rows,
    值A = v1[rows], 值B = v2[rows], stringsAsFactors = FALSE)
}
# 3a. 用户自定义 expr
for (r in rules$rules) {
  if (identical(r$check, "expr") && !is.null(r$expr)) {
    ok <- tryCatch(eval(parse(text = r$expr), envir = d), error = function(e) NULL)
    if (is.null(ok)) { msg("   规则 ", r$id, " 求值失败，跳过"); next }
    push(r$id %||% "expr", r$desc %||% r$expr, which(!is.na(ok) & !ok))
  }
}
# 3b. 日期列两两先后（同名前缀 = 开始/结束 命名的才查）
dcols <- nm[type == "date"]
if (length(dcols) >= 2) {
  for (i in seq_len(length(dcols)-1)) for (j in (i+1):length(dcols)) {
    a <- dcols[i]; b <- dcols[j]
    bad <- which(!is.na(d[[a]]) & !is.na(d[[b]]) & d[[a]] > d[[b]])
    push(paste0(a,"_gt_",b), paste0(a, " 晚于 ", b, "（日期先后存疑）"), bad,
         as.character(d[[a]]), as.character(d[[b]]))
  }
}
# 3c. 身高/体重/BMI 三者自洽（自动识别，含单位探测）
hcol <- nm[grepl("HEIGHT|身高", nm, ignore.case = TRUE)][1]
wcol <- nm[grepl("WEIGHT|体重", nm, ignore.case = TRUE)][1]
bcol <- nm[grepl("^BMI|BMI$", nm, ignore.case = TRUE)][1]
if (!any(is.na(c(hcol, wcol, bcol)))) {
  H <- num(d[[hcol]]); W <- num(d[[wcol]]); B <- num(d[[bcol]])
  ok <- !is.na(H) & !is.na(W) & !is.na(B) & H > 0
  # 探测单位：比较 (W/2)/H^2 与 W/H^2 哪个更契合记录的 BMI
  fit_jin <- mean(abs((W/2)/(H/100)^2 - B) <= 0.5, na.rm = TRUE)
  fit_kg  <- mean(abs(  W  /(H/100)^2 - B) <= 0.5, na.rm = TRUE)
  msg(sprintf("   [单位探测] 体重按【斤】契合 %.1f%%，按【公斤】契合 %.1f%%",
              100*fit_jin, 100*fit_kg))
  m_jin <- ok & abs((W/2)/(H/100)^2 - B) <= 0.5
  m_kg  <- ok & abs(  W  /(H/100)^2 - B) <= 0.5
  if (fit_jin > 0.9 && fit_kg > 0.005) {
    msg("   ⚠ 检测到体重单位可能混用！按斤 ", sum(m_jin), " 行；按公斤 ", sum(m_kg), " 行")
    push("weight_unit_mixed",
         sprintf("体重单位混用：%d 行是斤、%d 行是公斤", sum(m_jin), sum(m_kg)),
         which(m_kg), W, W)
  } else {
    good <- if (fit_jin >= fit_kg) m_jin else m_kg
    push("bmi_inconsistent",
         sprintf("BMI 与身高体重不自洽（按%s）", if (fit_jin >= fit_kg) "斤" else "公斤"),
         which(ok & !good), B, round(if (fit_jin >= fit_kg) (W/2)/(H/100)^2 else W/(H/100)^2, 2))
  }
}
reason <- function(x) is.null(attr(x, "labels"))
logic_df <- if (length(logic)) do.call(rbind, logic) else
  data.frame(检查项=character(0),说明=character(0),行号=integer(0),值A=character(0),值B=character(0))
msg("   逻辑问题条数: ", nrow(logic_df))

# 合并 ②③ 输出
allchk <- rbind(
  if (nrow(illegal_df)) data.frame(类别="非法值", 规则=illegal_df$规则, 说明=illegal_df$类型,
                                   变量=illegal_df$变量, 行号=illegal_df$行号,
                                   值A=as.character(illegal_df$值), 值B="", stringsAsFactors=FALSE),
  if (nrow(logic_df)) data.frame(类别="逻辑关系", 规则=logic_df$检查项, 说明=logic_df$说明,
                                 变量="", 行号=logic_df$行号,
                                 值A=as.character(logic_df$值A), 值B=as.character(logic_df$值B),
                                 stringsAsFactors=FALSE)
)
if (is.null(allchk)) allchk <- data.frame(类别=character(0),规则=character(0),说明=character(0),
                                          变量=character(0),行号=integer(0),值A=character(0),值B=character(0))
utils::write.csv(allchk, file.path(out_dir, "02_非法值与逻辑异常.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# ================= ④ 缺失值 =================
msg("④ 缺失值处理 ...")
miss_sum <- meta[, c("变量","类型","非缺失","缺失","缺失率","标签")]
miss_sum <- miss_sum[order(-miss_sum$缺失), ]
utils::write.csv(miss_sum, file.path(out_dir, "03_缺失值_列汇总.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
key_miss_rows <- integer(0)
if (!is.null(miss_key) && length(miss_key)) {
  kk <- intersect(miss_key, nm)
  if (length(kk)) {
    key_miss_rows <- which(apply(d[, kk, drop = FALSE], 1, function(r) any(is.na(r))))
    if (length(key_miss_rows))
      utils::write.csv(cbind(行号 = key_miss_rows, d[key_miss_rows, , drop = FALSE]),
                       file.path(out_dir, "03_关键列缺失行.csv"),
                       row.names = FALSE, fileEncoding = "UTF-8")
  }
}
msg("   有缺失的列: ", sum(meta$缺失 > 0), " / ", p,
    "；关键列(", paste(miss_key %||% "-", collapse=","), ")缺失行: ", length(key_miss_rows))

# ================= ⑤ 异常连续变量 =================
msg("⑤ 查找异常连续变量 ...")
oc <- out_cfg
method <- oc$method %||% "iqr"; k <- oc$k %||% if (method == "iqr") 1.5 else 3
if (!is.null(oc$columns) && !identical(oc$columns, "auto") && length(oc$columns)) {
  cont_cols <- intersect(oc$columns, nm)
  msg("   使用规则表指定的列: ", length(cont_cols), " 个")
} else {
  # 自动识别真连续变量：数值型 且 唯一值 >= 10 且 非缺失足够多
  #   非缺失阈值取 min(100, n*0.3)，小样本（如演示数据 60 行）也能识别出连续变量
  min_n <- min(100L, max(10L, floor(n * 0.3)))
  cont_cols <- nm[sapply(nm, function(v) {
    if (type[[v]] != "numeric") return(FALSE)
    x <- na.omit(num(d[[v]]))
    length(x) >= min_n && length(unique(x)) >= 10
  })]
  msg("   自动识别真连续变量: ", length(cont_cols), " 个",
      "（判据：唯一值≥10 且非缺失≥", min_n, "；已排除二值/低基数变量）")
}
outl <- list()
for (v in cont_cols) {
  x <- num(d[[v]]); xv <- x[!is.na(x)]
  if (length(xv) < 10) next
  if (method == "iqr") {
    q1 <- quantile(xv, .25); q3 <- quantile(xv, .75); iqr <- q3 - q1
    lo <- q1 - k*iqr; hi <- q3 + k*iqr
  } else {
    m <- mean(xv); s <- sd(xv); lo <- m - k*s; hi <- m + k*s
  }
  bad <- unname(which(!is.na(x) & (x < lo | x > hi)))
  if (length(bad)) outl[[length(outl)+1]] <- data.frame(
    变量 = v, 行号 = bad, 值 = unname(x[bad]), 下界 = round(lo,2), 上界 = round(hi,2),
    离群占比 = sprintf("%.1f%%", 100*length(bad)/length(xv)), stringsAsFactors = FALSE)
}
outl_df <- if (length(outl)) do.call(rbind, outl) else
  data.frame(变量=character(0),行号=integer(0),值=numeric(0),下界=numeric(0),上界=numeric(0),离群占比=character(0))
utils::write.csv(outl_df, file.path(out_dir, "04_异常连续变量.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
msg("   离群条数: ", nrow(outl_df))
# 假阳性预警
if (nrow(outl_df)) {
  hi_prop <- outl_df[as.numeric(sub("%","",outl_df$离群占比)) > 15, ]
  if (nrow(hi_prop)) {
    v2 <- unique(hi_prop$变量)
    msg("   ⚠ ", length(v2), " 个变量的离群占比 >15%，疑似零膨胀/低基数变量导致的假阳性：")
    msg("     ", paste(head(v2, 10), collapse=", "))
  }
}

# ================= 自动生成规则表 =================
auto_rules <- list(dataset = tools::file_path_sans_ext(basename(data_path)),
                   dupes = list(key = nm[1]),
                   missing = list(key = nm[1]),
                   outliers = list(method = method, k = k, columns = "auto"),
                   rules = list())
for (v in nm[type == "categorical"]) {
  x <- d[[v]]; vl <- attr(x, "labels")
  legal <- sort(unique(num(vl))); obs <- sort(unique(num(x))); obs <- obs[!is.na(obs)]
  if (length(legal) && !length(setdiff(obs, legal)))
    auto_rules$rules[[length(auto_rules$rules)+1]] <-
      list(id = paste0(gsub("[^A-Za-z0-9_]","_",v), "_legal"),
           check = "in_set", column = v, values = as.list(legal), severity = "error")
}
suppressPackageStartupMessages(library(yaml))
writeLines(yaml::as.yaml(auto_rules, unicode = TRUE),
           file.path(out_dir, "规则_自动生成.yaml"), useBytes = TRUE)

# ================= ⑥ 问题报告 =================
msg("⑥ 生成问题报告 ...")
rp <- file.path(out_dir, "问题报告.md")
o <- file(rp, "w", encoding = "UTF-8")
W <- function(...) writeLines(paste0(...), o)
W("# 数据质控问题报告\n")
W("> **只读扫描，未修改任何数据。**\n")
W("- 数据文件：`", basename(data_path), "`")
if (file.exists(data_path)) W("- md5：`", tools::md5sum(data_path), "`")
W("- 规模：", n, " 行 × ", p, " 列")
W("- 生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
W("- 扫描脚本：`questionnaire-cleaning/scripts/qc_scan.R`")
W("")
W("---\n")
W("## 概览\n")
W("| 步骤 | 结果 |")
W("|---|---|")
W("| ① 重复个案 | ", length(dup_rows), " 行 |")
W("| ② 非法值 | ", nrow(illegal_df), " 条 |")
W("| ③ 逻辑关系 | ", nrow(logic_df), " 条 |")
W("| ④ 缺失（有缺失的列） | ", sum(meta$缺失 > 0), " / ", p, " 列 |")
W("| ⑤ 异常连续变量 | ", nrow(outl_df), " 条（", length(cont_cols), " 个变量参与检查） |")
W("")
W("## ① 重复个案\n")
key_desc <- if (!is.null(dup_key) && length(dup_key))
  paste0("`", paste(intersect(dup_key, nm), collapse="`, `"), "`") else "全部列（未指定关键列）"
if (!length(dup_rows)) {
  W("按 ", key_desc, " 判定，**没有重复**。\n")
} else {
  W("判定键：", key_desc, "　重复行：**", length(dup_rows), "** 行\n")
  W("明细见 `01_重复个案.csv`。\n")
}
W("## ② 非法值\n")
if (!nrow(illegal_df)) {
  W("**没有发现非法值。**\n")
} else {
  tb <- sort(table(illegal_df$规则), decreasing = TRUE)
  W("| 规则 | 条数 |"); W("|---|---|")
  for (i in seq_along(tb)) W("| ", names(tb)[i], " | ", tb[[i]], " |")
  W("\n> 注：缺失值**不计入**非法值（已剔除），缺失情况见 ④。\n")
  W("明细见 `02_非法值与逻辑异常.csv`。\n")
}
W("## ③ 逻辑关系\n")
if (!nrow(logic_df)) {
  W("**没有发现逻辑矛盾。**\n")
} else {
  tb <- sort(table(logic_df$检查项), decreasing = TRUE)
  W("| 检查项 | 条数 | 说明 |"); W("|---|---|---|")
  for (i in seq_along(tb)) {
    d0 <- logic_df$说明[logic_df$检查项 == names(tb)[i]][1]
    W("| ", names(tb)[i], " | ", tb[[i]], " | ", d0, " |")
  }
  W("")
  for (chk in names(tb)) {
    sub <- logic_df[logic_df$检查项 == chk, ]
    W("### ", chk, "（", nrow(sub), " 行）\n")
    W(sub$说明[1], "\n")
    rows <- sub$行号
    W("行号：`", paste(head(rows, 60), collapse=", "),
      if (length(rows) > 60) paste0(" ... 共 ", length(rows), " 行") else "", "`\n")
  }
}
W("## ④ 缺失值\n")
top <- head(miss_sum[miss_sum$缺失 > 0, ], 15)
if (!nrow(top)) {
  W("**没有缺失值。**\n")
} else {
  W("缺失最多的 15 列：\n")
  W("| 变量 | 缺失 | 缺失率 | 标签 |"); W("|---|---|---|---|")
  for (i in seq_len(nrow(top)))
    W("| ", top$变量[i], " | ", top$缺失[i], " | ", top$缺失率[i], " | ",
      substr(top$标签[i], 1, 40), " |")
  W("\n> **提醒**：问卷缺失常来自跳转逻辑（如仅女生作答的题目），属结构性缺失，")
  W("> 不应按随机缺失填补。请结合问卷设计判断。\n")
}
W("## ⑤ 异常连续变量\n")
if (!nrow(outl_df)) {
  W("**没有发现离群值。**\n")
} else {
  tb <- sort(table(outl_df$变量), decreasing = TRUE)
  W("检查方法：", method, "（k = ", k, "），参与检查的真连续变量 ", length(cont_cols), " 个。\n")
  W("| 变量 | 离群条数 | 离群占比 |"); W("|---|---|---|")
  for (i in seq_along(head(tb, 20))) {
    v <- names(tb)[i]
    W("| ", v, " | ", tb[[i]], " | ", outl_df$离群占比[outl_df$变量==v][1], " |")
  }
  W("\n> ⚠ **离群 ≠ 错误**。IQR 法对零膨胀变量（多数取 0 的计分题）会大量误判；")
  W("> 占比超过 15% 的变量应先怀疑方法适用性，而不是直接当异常处理。\n")
  W("明细见 `04_异常连续变量.csv`。\n")
}
W("## 变量结构\n")
W("| 类型 | 变量数 |"); W("|---|---|")
for (t in names(table(type))) W("| ", t, " | ", table(type)[[t]], " |")
badname <- nm[!grepl("^[A-Za-z@#$][A-Za-z0-9_@#$]{0,63}$", nm)]
if (length(badname)) {
  W("\n⚠ **", length(badname), " 个变量名不符合 SPSS 经典命名规范**（导出 .sav 会报错）：")
  W("`", paste(head(badname, 15), collapse="`, `"),
    if (length(badname) > 15) paste0("` … 共 ", length(badname), " 个") else "`", "\n")
}
multilab <- nm[sapply(d, function(x) length(attr(x, "label")) > 1)]
if (length(multilab)) W("\n⚠ **", length(multilab), " 个变量的标签属性含多个值**：`",
                       paste(multilab, collapse="`, `"), "`\n")
W("")
W("---\n")
W("*本报告由 qc_scan.R 自动生成；全程只读，未修改任何数据文件。*")
close(o)

# ================= 收尾 =================
msg("")
msg("扫描完成。产出目录: ", normalizePath(out_dir))
msg("  - 问题报告.md")
msg("  - 01_重复个案.csv / 02_非法值与逻辑异常.csv")
msg("  - 03_缺失值_列汇总.csv / 04_异常连续变量.csv / 05_变量元数据.csv")
msg("  - 规则_自动生成.yaml")
msg("")
msg("下一步：核对问题报告，把【确认要改】的项写进 修正.yaml，再运行 qc_apply.R")
close(logcon)
