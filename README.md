# questionnaire-cleaning

> 问卷 / 流行病学调查数据的**质控与清洗**工具（R 实现），按标准 8 步流程执行，
> 产出 clean 数据集（CSV + SAV）、**逐条修改日志**和数据集说明 README。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](./LICENSE)

---

## 它解决什么问题

问卷数据清洗最怕两件事：**一是脚本悄悄改数据**，二是**工具报出一堆假阳性把所有问题淹没**。

本工具的设计围绕这两点：

- **扫描与改动彻底分离** —— ①②③④⑤⑥ 步只读扫描，⑦⑧ 步才动数据，
  且只应用**人工在 `修正.yaml` 里显式确认过**的修正。绝不自动填补缺失值或"修正"矛盾值。
- **默认参数避开已知假阳性** —— 见下方[七个陷阱](#七个陷阱实测踩过)。

## 8 步流程

| # | 步骤 | 脚本 | 写数据 |
|---|---|---|---|
| ① | 查找重复个案 | `qc_scan.R` | 否 |
| ② | 查找非法值 | `qc_scan.R` | 否 |
| ③ | 核查逻辑关系 | `qc_scan.R` | 否 |
| ④ | 缺失值处理 | `qc_scan.R` | 否 |
| ⑤ | 查找异常连续变量 | `qc_scan.R` | 否 |
| ⑥ | 输出所有异常 | `qc_scan.R` | 否 |
| ⑦ | 改动并记录日志 | `qc_apply.R` | **是** |
| ⑧ | 得到 clean 数据 | `qc_apply.R` | **是** |

## 安装

### 作为 Claude Code 技能

```bash
git clone https://github.com/<你的用户名>/questionnaire-cleaning.git \
  ~/.claude/skills/questionnaire-cleaning
```

> Windows 用户把 `~/.claude/skills/` 换成 `C:\Users\<你>\.claude\skills\`。

### 仅用脚本（不需要 Claude Code）

直接调用 `scripts/` 下的 R 脚本即可，见下方用法。

### 依赖

```r
install.packages(c("haven", "dplyr", "yaml", "readxl", "openxlsx2"))
```

`haven` 用于读写 `.sav`；只处理 CSV 时可不装。

## 用法

```bash
# ①-⑥ 扫描（只读，不动数据）
Rscript scripts/qc_scan.R "数据.sav" -o "扫描结果"
#    有自定义规则时：
Rscript scripts/qc_scan.R "数据.sav" -r "规则.yaml" -o "扫描结果"

# ⑦-⑧ 应用修正并导出（先按扫描结果编写 修正.yaml）
Rscript scripts/qc_apply.R "数据.sav" -f "修正.yaml" -o "清洗结果" --scan "扫描结果"
```

### 扫描产出

```
问题报告.md              ← 先看这个：四类异常计数 + 明细 + 行号
01_重复个案.csv
02_非法值与逻辑异常.csv
03_缺失值_列汇总.csv
04_异常连续变量.csv
05_变量元数据.csv
规则_自动生成.yaml       ← 从值标签自动推导的 in_set 规则，可另存后扩充
扫描日志.txt
```

### 清洗产出

```
README.md                ← 数据集说明（做了什么 / 没做什么 / 变量结构 / 复现方式）
修改日志.csv / .txt      ← 每条改动的前后值逐条留痕
clean/*_clean.csv        ← UTF-8
clean/*_clean.sav        ← 值标签保留；不合 SPSS 规范的变量名自动改写
clean/变量名改动.csv     ← 完整改名映射（含因重名被动改名的列）
```

## 修正指令（`修正.yaml`）

支持五种类型，模板见 [`templates/fixes_template.yaml`](templates/fixes_template.yaml)：

```yaml
fixes:
  # 改值（有唯一正确答案的笔误）
  - id: recordda_year
    type: set_value
    column: RECORDDA
    desc: "录入日期年份误写为 2004，应为 2024"
    rows: [222, 223, 224]            # 也可用 where: "表达式"
    value: "2024-06-03"

  # 置缺失（值确定是错的但不知正确值 —— 置缺失比留错值安全）
  - id: bad_height
    type: set_na
    column: HEIGHT
    rows: [3582]
    desc: "身高 100cm 明显录入错误，无原始记录，置缺失"

  # 新增标记列（不改原值）
  - id: weight_unit_flag
    type: add_column
    column: WEIGHT_unit
    default: "斤"
    rows: [64, 127]
    value: "kg"
    desc: "标记体重单位"

  # 派生列（确定性换算，如单位统一）
  - id: weight_kg
    type: compute
    column: WEIGHT_kg
    expr: "ifelse(is.na(WEIGHT), NA, ifelse(WEIGHT_unit == 'kg', WEIGHT, WEIGHT/2))"
    desc: "按单位统一换算为公斤"

  # 批量改名
  - id: rename_map
    type: rename_columns
    map:
      量表评分（总计）: 量表评分_总计
```

## 扫描规则（`规则.yaml`）

模板见 [`templates/rules_template.yaml`](templates/rules_template.yaml)。支持 `in_set` / `between` /
`not_null` / `regex` / `expr`（跨变量逻辑表达式，如 `"is.na(ONSETDT) | ONSETDT >= BIRTHDT"`）。

**不提供规则文件也能跑** —— 会自动用全部列判重复、从值标签推导 `in_set` 规则、
对自动识别的真连续变量做离群检查。

## 七个陷阱（实测踩过）

这些不是理论，是在真实数据（6960×240 的中学生问卷）上一条条踩出来的：

| # | 陷阱 | 本工具的处理 |
|---|---|---|
| 1 | **IQR 不能套分类变量**：二值变量 Q1=Q3 → IQR=0 → 取值为 1 的行全被判离群。实测 94036 条"离群"里 **63.9%** 是这么来的 | `outliers.columns` 默认 `auto`，只扫真连续变量（唯一值≥10 且非缺失≥100） |
| 2 | **零膨胀变量也会被 IQR 误判**：中位=0、Q1=Q3=0 的计分题（七到九成取 0） | 离群占比 >15% 时主动警告"疑似假阳性" |
| 3 | **`in_set` 类检查会把缺失算成违规**：实测 35618 条"违规"里 **35616 条**是缺失 | 缺失与非法值**分开统计**，非法值一律剔除缺失后再计 |
| 4 | **单位混用比单位错误更危险**：不能只看全列中位数判断单位 | **内置逐行交叉验证**（用数据自带的 BMI 反推体重单位，并报告两套单位各多少行） |
| 5 | **`.sav` 变量名可能不合 SPSS 规范**（中文名、全角括号），`haven::write_sav()` 直接报错 | 自动改写并**生成完整映射表** |
| 6 | **字符列别用 `as.numeric()`**：会把文本静默转成 NA（实测毁掉一个 5998 行文本的列） | 逐列按类型处理 |
| 7 | **改名会"连带改名"**：中文名清洗后可能与已有列重名，`make.unique` 把原本合法的列也改了（实测「PQSI总分」→`PQSI` 后挤走了原 `PQSI`） | 映射表记录**所有变化**的列，不只是名字不合法的 |

## 设计原则

**不制造假数据。** 扫描发现的异常分两类：

- **可确定修正**（有唯一正确答案的笔误）→ 需人工确认后写入 `修正.yaml`
- **必须回查原始问卷**（异常值、矛盾答案、漏填）→ **只记录，绝不猜、绝不填**

自动填补缺失值或"修正"矛盾值等于制造假数据。遇到无法判定的矛盾（如年龄与年级不符，
可能只是真实的入学晚），本工具**不会擅自改动任何一个字段**。

## 目录结构

```
questionnaire-cleaning/
├── SKILL.md                      # Claude Code 技能定义
├── README.md                     # 本文件
├── LICENSE
├── CHANGELOG.md
├── scripts/
│   ├── qc_scan.R                 # 步骤 ①-⑥（只读）
│   └── qc_apply.R                # 步骤 ⑦-⑧（写数据）
├── templates/
│   ├── rules_template.yaml
│   └── fixes_template.yaml
└── examples/                     # 可直接跑的演示（含故意植入的错误）
    ├── demo_data.csv
    ├── demo_rules.yaml
    └── demo_fixes.yaml
```

## 快速试跑

```bash
Rscript scripts/qc_scan.R  examples/demo_data.csv -r examples/demo_rules.yaml -o /tmp/demo_scan
Rscript scripts/qc_apply.R examples/demo_data.csv -f examples/demo_fixes.yaml -o /tmp/demo_clean
```

## 相关工具

- [EpiData](https://www.epidata.dk/) —— 问卷录入，产出 `.qes` / `.rec` / `.chk`
- [Rclean](https://github.com/Preventive-Medicine-renzhi-stu/R-clean) —— 同类 R 包，
  采用更自动化的路线（本工具的[七个陷阱](#七个陷阱实测踩过)有部分正来自对比它的输出）

## 许可

[MIT](./LICENSE)
