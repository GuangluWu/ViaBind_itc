# ITC/CSC 数据提取脚本（R 版）

当前目录保留三个主脚本：

- `xml_to_xlsx.R`：从 ITC XML 提取并输出单个 `.xlsx`
- `csc_to_xlsx.R`：从 `.csc` 直接输出单个 `.xlsx`（3 个核心 sheet）
- `nitc_to_xlsx.R`：从 `.nitc` 直接输出单个 `.xlsx`（3 个核心 sheet）

## 1. 环境要求

- R 4.1+
- 安装依赖：

```r
install.packages(c("xml2", "writexl"))
```

## 2. XML 一体化导出：`xml_to_xlsx.R`

基础命令：

```bash
Rscript xml_to_xlsx.R 2-1bd.xml
```

指定输出文件：

```bash
Rscript xml_to_xlsx.R 2-1bd.xml -o 2-1bd_extracted_r/xml_extract.xlsx
```

输出文件：

- `*.xlsx`（单文件，包含 3 个 sheet）
- `experiment_parameters`
- `titration_points`
- `heatflow`

## 3. CSC 一体化导出：`csc_to_xlsx.R`

基础命令：

```bash
Rscript csc_to_xlsx.R aatoCB8-g1.csc
```

指定输出文件：

```bash
Rscript csc_to_xlsx.R aatoCB8-g1.csc -o aatoCB8-g1_parsed/csc_extract.xlsx
```

输出文件：

- `*.xlsx`（单文件，包含 3 个 sheet）
- `experiment_parameters`
- `titration_points`
- `heatflow`

## 4. NITC 一体化导出：`nitc_to_xlsx.R`

`.nitc` 文件是 gzip 封装的 ITC 数据，脚本会自动解压并提取。

基础命令：

```bash
Rscript nitc_to_xlsx.R 'os-cb8-e7-2 2024-11-11_22.41 E7.nitc'
```

指定输出文件：

```bash
Rscript nitc_to_xlsx.R 'os-cb8-e7-2 2024-11-11_22.41 E7.nitc' -o os-cb8-e7-2_parsed/nitc_extract.xlsx
```

输出文件：

- `*.xlsx`（单文件，包含 3 个 sheet）
- `experiment_parameters`
- `titration_points`
- `heatflow`
