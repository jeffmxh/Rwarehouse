# 使用说明：

## dependency: 
**dplyr**,**stringr**

## 格式

```r
find_dict_word(data = , colname= , dict = , dupli_bool = TRUE/FALSE)
```

## 先指定统计的字典

```r
dict_vec = c("元旦","小寒","大寒","腊八节","小年","立春","除夕","春节","情人节","雨水","元宵节","上元节","惊蛰","二月二","龙抬头","春分","寒食节","清明节","谷雨","立夏","佛诞","母亲节","小满","芒种","端午节","夏至","小暑","大暑","立秋","七夕","中元节","处暑","白露","中秋节","秋分","国庆节","寒露","重阳节","霜降","立冬","小雪","大雪","冬至")
```

## 直接统计词频

```r
find_dict_word(data1, "content", dict_vec, FALSE)
```

## 统计记录数量

```r
find_dict_word(data1, "content", dict_vec, TRUE)
```
