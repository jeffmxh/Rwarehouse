## 使用说明：

### dependency: 
+ dply
+ stringr

### 格式

```r
find_dict_word(data = , colname= , dict = , dupli_bool = TRUE/FALSE, filter_bool = TRUE/FALSE)
```
### 参数说明
+ data:输入的数据框
+ colname:需要进行统计的列
+ dict:需要统计的词典
+ dupli_bool:当指定为**TRUE**时会统计记录条数，**FALSE**会直接统计词语出现频次
+ filter_bool:控制结果中频次为0的项是否输出

### 先指定统计的字典

```r
dict_vec = c("元旦","小寒","大寒","腊八节","小年","立春","除夕","春节","情人节","雨水","元宵节","上元节","惊蛰","二月二","龙抬头","春分","寒食节","清明节","谷雨","立夏","佛诞","母亲节","小满","芒种","端午节","夏至","小暑","大暑","立秋","七夕","中元节","处暑","白露","中秋节","秋分","国庆节","寒露","重阳节","霜降","立冬","小雪","大雪","冬至")
```

### 直接统计词频

```r
find_dict_word(data1, "content", dict_vec, dupli_bool = FALSE, filter_bool = FALSE)
```

### 统计记录数量

```r
find_dict_word(data1, "content", dict_vec, dupli_bool = TRUE, filter_bool = FALSE)
```
