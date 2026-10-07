#!/usr/bin/env Rscript
# 应用修正 + 记录日志 + 导出 clean 数据（步骤 ⑦⑧）
# 用法: Rscript qc_apply.R <数据文件> -f <修正.yaml> [-o 输出目录] [--scan <扫描目录>]

suppressPackageStartupMessages({ library(haven); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("用法: qc_apply.R <数据文件> -f 修正.yaml [-o 输出目录] [--scan 扫描目录]")
data_path <- args[1]; fixes_path <- NULL; out_dir <- "qc_clean"; scan_dir <- NULL
i <- 2
while (i <= length(args)) {
  if (args[i] == "-f") { fixes_path <- args[i+1]; i <- i+2 }
  else if (args[i] == "-o") { out_dir <- args[i+1]; i <- i+2 }
  else if (args[i] == "--scan") { scan_dir <- args[i+1]; i <- i+2 }
  else stop("未知参数: ", args[i])
}
if (is.null(fixes_path)) stop("必须用 -f 指定修正指令文件（修正.yaml）")
dir.create(file.path(out_dir, "clean"), showWarnings = FALSE, recursive = TRUE)

LOGCSV <- file.path(out_dir, "修改日志.csv")
LOGTXT <- file.path(out_dir, "修改日志.txt")
changelog <- list()
addlog <- function(step, rule_id, action, row, column, old, new, note) {
  changelog[[length(changelog) + 1]] <<- data.frame(
    序号 = length(changelog) + 1, 步骤 = step, 规则 = rule_id, 动作 = action,
    行号 = if (length(row)) paste(row, collapse = ",") else "",
    变量 = column %||% "",
    原值 = if (length(old)) paste(as.character(old), collapse = ",") else "",
    新值 = if (length(new)) paste(as.character(new), collapse = ",") else "",
    说明 = note %||% "", stringsAsFactors = FALSE)
}
msgcon <- file(LOGTXT, "w", encoding = "UTF-8")
msg <- function(...) { s <- paste0(...); cat(s, "\n"); writeLines(s, msgcon) }
num <- function(x) suppressWarnings(as.numeric(x))

# ---------------- 读入 ----------------
msg("=== 应用修正并导出 clean 数据 ===")
msg("数据文件: ", data_path)
if (file.exists(data_path)) msg("原文件 md5: ", tools::md5sum(data_path))
ext <- tolower(tools::file_ext(data_path))
d0 <- switch(ext,
  sav  = haven::read_sav(data_path),
  dta  = haven::read_dta(data_path),
  xlsx = , xls = as.data.frame(readxl::read_excel(data_path), check.names = FALSE),
  csv  = utils::read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE,
                         fileEncoding = "UTF-8"),
  stop("不支持的格式"))
d0 <- as.data.frame(d0, check.names = FALSE)
d <- d0
msg("读入: ", nrow(d), " 行 × ", ncol(d), " 列")
addlog("读入", "", "读取", integer(0), "", NA, NA,
       paste0(basename(data_path), " (", nrow(d), "×", ncol(d), ")"))

suppressPackageStartupMessages(library(yaml))
fx <- yaml::yaml.load_file(fixes_path)
msg("加载修正指令: ", fixes_path, "（", length(fx$fixes), " 条）")
msg("")

# ---------------- ⑦ 应用修正 ----------------
msg("⑦ 应用修正 ...")
resolve_rows <- function(f) {
  if (!is.null(f$rows)) return(as.integer(f$rows))
  if (!is.null(f$where)) {
    ok <- tryCatch(eval(parse(text = f$where), envir = d), error = function(e) NULL)
    if (is.null(ok)) { msg("   !! where 求值失败，跳过: ", f$where); return(integer(0)) }
    return(which(!is.na(ok) & ok))
  }
  integer(0)
}
applied <- 0
for (f in fx$fixes) {
  rid <- f$id %||% paste0("fix", applied + 1)
  act <- f$type %||% "set_value"
  if (act == "rename_columns") {
    mp <- f$map
    for (old in names(mp)) {
      if (old %in% names(d)) {
        names(d)[names(d) == old] <- mp[[old]]
        addlog("⑦", rid, "改名", integer(0), old, old, mp[[old]], f$desc %||% "")
        applied <- applied + 1
      }
    }
    next
  }
  col <- f$column
  if (is.null(col)) { msg("   !! 未指定 column，跳过: ", rid); next }
  # add_column 的目标列本来就不存在，先处理
  if (act == "add_column") {
    rows <- resolve_rows(f)
    nv <- if (!is.null(f$default)) rep(f$default, nrow(d)) else rep(NA, nrow(d))
    if (length(rows)) nv[rows] <- f$value
    d[[col]] <- nv
    addlog("⑦", rid, "新增列", rows, col, NA, f$value,
           paste0(f$desc %||% "", "（其余 ", nrow(d)-length(rows), " 行 = ", f$default %||% "NA", "）"))
    msg("   ", rid, ": 新增列 ", col, "（", length(rows), " 行标记为 ", f$value, "）")
    applied <- applied + 1
    next
  }
  # compute：按表达式生成/覆盖一列（用于单位换算这类派生列）
  # ⚠ 只做确定性换算，不许用来"猜"缺失值
  if (act == "compute") {
    if (is.null(f$expr)) { msg("   !! compute 缺少 expr，跳过: ", rid); next }
    old <- if (col %in% names(d)) d[[col]] else NULL
    nv <- tryCatch(eval(parse(text = f$expr), envir = d), error = function(e) NULL)
    if (is.null(nv)) { msg("   !! compute 表达式求值失败，跳过: ", rid); next }
    d[[col]] <- nv
    addlog("⑦", rid, if (is.null(old)) "新增派生列" else "覆盖派生列",
           integer(0), col,
           if (is.null(old)) NA else paste0("(原列 ", length(old), " 值)"),
           paste0("按表达式生成：", f$expr), f$desc %||% "")
    msg("   ", rid, ": ", if (is.null(old)) "新增" else "覆盖", "派生列 ", col, "（", f$expr, "）")
    applied <- applied + 1
    next
  }
  if (!col %in% names(d)) { msg("   !! 变量不存在，跳过: ", col); next }
  rows <- resolve_rows(f)
  if (!length(rows)) { msg("   !! 未匹配到行，跳过: ", rid); next }
  if (act == "set_value") {
    old <- d[[col]][rows]
    d[[col]][rows] <- f$value
    addlog("⑦", rid, "改值", rows, col, old, f$value, f$desc %||% "")
    msg("   ", rid, ": ", length(rows), " 行  ", col, " → ", f$value)
    applied <- applied + 1
  } else if (act == "set_na") {
    old <- d[[col]][rows]
    d[[col]][rows] <- NA
    addlog("⑦", rid, "置缺失", rows, col, old, NA, f$desc %||% "")
    msg("   ", rid, ": ", length(rows), " 行  ", col, " → NA")
    applied <- applied + 1
  } else {
    msg("   !! 未知修正类型: ", act)
  }
}
msg("   共应用 ", applied, " 条修正")
msg("")

# ---------------- ⑧ 导出 ----------------
msg("⑧ 导出 clean 数据 ...")
# 8a. CSV（逐列按类型处理，字符列保持字符）
conv <- function(x) {
  if (is.character(x) || is.factor(x)) return(as.character(x))
  if (inherits(x, "Date") || inherits(x, "POSIXct")) return(as.character(x))
  if (inherits(x, "haven_labelled")) return(num(x))
  if (is.numeric(x)) return(x)
  as.character(x)
}
d_csv <- as.data.frame(lapply(d, conv), stringsAsFactors = FALSE, check.names = FALSE)
p_csv <- file.path(out_dir, "clean", paste0(tools::file_path_sans_ext(basename(data_path)), "_clean.csv"))
utils::write.csv(d_csv, p_csv, row.names = FALSE, fileEncoding = "UTF-8")
msg("   ", basename(p_csv), " (", file.size(p_csv), " 字节)")
addlog("⑧", "", "导出", integer(0), "", NA, NA, basename(p_csv))

# 8b. SAV（值标签保留；变量名需符合 SPSS 规范）
sanitize <- function(x) {
  y <- gsub("[^A-Za-z0-9_@#$]", "_", x)   # 非合法字符 -> 下划线
  y <- gsub("_+", "_", y)                  # 合并连续下划线
  y <- gsub("^_+|_+$", "", y)              # 去首尾下划线
  # SPSS 变量名必须以字母/@/#/$ 开头且非空；纯中文名替换后会变空串
  bad <- !nzchar(y) | !grepl("^[A-Za-z@#$]", y)
  if (any(bad)) y[bad] <- paste0("V", which(bad))
  y <- substr(y, 1, 64)
  make.unique(y, sep = "_")
}
nm <- names(d)
bad <- !grepl("^[A-Za-z@#$][A-Za-z0-9_@#$]{0,63}$", nm)
name_map <- data.frame()
d_sav <- d
if (any(bad)) {
  newnm <- sanitize(nm)
  # ⚠ 必须记录【所有】发生变化的列名 —— 不只是非法的那些
  #   因为 sanitize 里的 make.unique 会把重名的合法列也改名
  #   （如「PQSI总分」被清洗成 PQSI 后，原「PQSI」列会被挤成 PQSI_1）
  changed <- which(nm != newnm)
  name_map <- data.frame(原变量名 = nm[changed], 新变量名 = newnm[changed],
                         stringsAsFactors = FALSE)
  names(d_sav) <- newnm
  utils::write.csv(name_map, file.path(out_dir, "clean", "变量名改动.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  msg("   ⚠ 变量名不合法 ", sum(bad), " 个；为避免重名共改动 ",
      nrow(name_map), " 个（完整映射见 变量名改动.csv）")
  addlog("⑧", "", "改名(SPSS规范)", integer(0),
         paste(name_map$原变量名, collapse = ","), NA, NA,
         paste0("共 ", nrow(name_map), " 个变量名改写（含因重名被动改名的列），映射见 clean/变量名改动.csv"))
}
p_sav <- file.path(out_dir, "clean", paste0(tools::file_path_sans_ext(basename(data_path)), "_clean.sav"))
sav_ok <- tryCatch({ haven::write_sav(d_sav, p_sav); TRUE }, error = function(e) {
  msg("   !! .sav 导出失败: ", conditionMessage(e)); FALSE })
if (sav_ok) { msg("   ", basename(p_sav), " (", file.size(p_sav), " 字节)")
  addlog("⑧", "", "导出", integer(0), "", NA, NA, basename(p_sav)) }
msg("")

# 8c. 日志落盘
cl <- if (length(changelog)) do.call(rbind, changelog) else
  data.frame(序号=integer(0),步骤=character(0),规则=character(0),动作=character(0),
             行号=character(0),变量=character(0),原值=character(0),新值=character(0),说明=character(0))
utils::write.csv(cl, LOGCSV, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(utils::capture.output(print(cl, row.names = FALSE)), msgcon)

# ---------------- README.md ----------------
scan_info <- ""
if (!is.null(scan_dir) && file.exists(file.path(scan_dir, "问题报告.md"))) {
  scan_info <- file.path(scan_dir, "问题报告.md")
}
o <- file(file.path(out_dir, "README.md"), "w", encoding = "UTF-8")
W <- function(...) writeLines(paste0(...), o)
W("# Clean 数据集说明\n")
W("> 由 `questionnaire-cleaning` 技能生成。原文件**未被修改**。\n")
W("## 数据来源\n")
W("- 原始文件：`", basename(data_path), "`")
if (file.exists(data_path)) W("- 原始文件 md5：`", tools::md5sum(data_path), "`")
W("- 规模：", nrow(d), " 行 × ", ncol(d), " 列")
W("- 清洗时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
W("- 修正指令：`", basename(fixes_path), "`（", length(fx$fixes), " 条）")
if (nzchar(scan_info)) W("- 扫描报告：`", scan_info, "`")
W("")
W("## 做了什么（共 ", applied, " 条改动）\n")
if (nrow(cl) && sum(cl$步骤 == "⑦") > 0) {
  sub <- cl[cl$步骤 == "⑦", ]
  W("| 规则 | 动作 | 变量 | 影响行数 | 说明 |")
  W("|---|---|---|---|---|")
  for (i in seq_len(nrow(sub))) {
    nr <- if (nzchar(sub$行号[i])) length(strsplit(sub$行号[i], ",")[[1]]) else 0
    W("| ", sub$规则[i], " | ", sub$动作[i], " | ", sub$变量[i], " | ", nr, " | ", sub$说明[i], " |")
  }
} else W("**没有对数据做任何修改**（修正指令为空或不含改动项）。")
W("")
if (nrow(name_map)) {
  W("### 变量名改写（仅为满足 SPSS 命名规范，内容未变）\n")
  W("| 原变量名 | 新变量名 |"); W("|---|---|")
  for (i in seq_len(nrow(name_map))) W("| ", name_map$原变量名[i], " | ", name_map$新变量名[i], " |")
  W("\n完整映射见 `变量名改动.csv`。\n")
}
W("## 没做什么（重要）\n")
W("本次修正指令**只覆盖了上表列出的项**。以下几类问题扫描已发现但**未在本文件中处理**")
W("（若某类已出现在上表，说明本次已处理，以表格为准）：\n")
W("- **需要回查原始问卷才能确定的问题** —— 本 skill 不会猜测正确值")
W("- **缺失值填补** —— 问卷缺失多为跳转逻辑导致的**结构性缺失**，")
W("  按随机缺失填补会引入虚假数据，一律不做")
W("- **离群值** —— 离群不等于错误；IQR 法对零膨胀计分题会大量误判")
W("- **无法判定哪个字段错**的逻辑矛盾（如年龄与年级不符，可能是真实的入学晚）")
W("")
W("> 处理这些需要：先回查原始问卷确认正确值或确认应置缺失，再写入新的修正指令重新运行。")
W("> 扫描报告（`问题报告.md`）里列了全部异常及行号，可作为回查清单。")
W("")
W("## 产出文件\n")
W("| 文件 | 说明 |"); W("|---|---|")
W("| `clean/", basename(p_csv), "` | 清洗后数据（UTF-8 CSV） |")
if (sav_ok) W("| `clean/", basename(p_sav), "` | 清洗后数据（SPSS .sav，**值标签已保留**） |")
W("| `修改日志.csv` / `修改日志.txt` | ⑦ 每一步改动的前后值逐条留痕 |")
if (nrow(name_map)) W("| `clean/变量名改动.csv` | 变量名映射表 |")
W("| `README.md` | 本文件 |")
W("")
W("## 变量结构\n")
tp <- sapply(d, function(x) {
  if (inherits(x, "Date")) "日期"
  else if (is.character(x) || is.factor(x)) "文本"
  else if (inherits(x, "haven_labelled")) "分类(带值标签)"
  else "数值"
})
W("| 类型 | 变量数 |"); W("|---|---|")
for (t in names(table(tp))) W("| ", t, " | ", table(tp)[[t]], " |")
W("")
W("## 复现方式\n")
W("```bash")
W("# 1. 扫描（只读）")
W('Rscript <skill>/scripts/qc_scan.R "', basename(data_path), '" -o 扫描目录')
W("# 2. 按扫描结果编写 修正.yaml")
W("# 3. 应用修正并导出")
W('Rscript <skill>/scripts/qc_apply.R "', basename(data_path), '" -f 修正.yaml -o 输出目录')
W("```")
W("")
W("---")
W("*本文件由 qc_apply.R 自动生成。*")
close(o)
msg("   README.md")
msg("")
msg("完成。产出目录: ", normalizePath(out_dir))
close(msgcon)
