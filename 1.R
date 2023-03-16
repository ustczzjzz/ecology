#安装"ade4"程序包#
install.packages("ade4") 

#载入‘ade4’程序包#
library(ade4)

#读取"ade4"程序包数"doubs"数据集#
dobus<- data("doubs",package = "ade4")

#查看数据对象"doubs"的类型#
class(doubs)

#查看数据对象"doubs"中变量以及变量的类型#
str(doubs)





