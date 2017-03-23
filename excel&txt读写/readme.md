# 使用说明

## dependency:

+ **readxl**
+ **openxlsx**

## 使用方法

``sopen_txt``和``sopen_excel``均用来批量读取一个目录下对应类型全部文件。

```r
file_handler <- sopen_txt(dirpath)
file_handler <- sopen_excel(dirpath)
```

***

``ssave_txt``用来快速保存**data.frame**为**txt**文件,tab分割，不显示行标，字符编码UTF-8

```r
ssave_txt(data_frame, file_path.txt)
```

***

``ssave_excel``用来把一个**data.frame**保存为**excel**文件,此处会依赖系统的压缩工具，windows下请安装[Rtools](https://cran.r-project.org/bin/windows/Rtools/Rtools34.exe)

```r
ssave_excel(data_frame, file_path.xlsx)
```

***

``ssave_multi_excel``把一个由多个**data.frame**构成的列表保存为一个多个**sheet**组成的**excel**文件

```r
ssave_multi_excel(data_frame_list, file_path.xlsx)
```
