
## 練習12：壓歲錢例子中，一讀到missing value就跳出，且告訴我是第幾筆missing (用while來作do…until…迴圈)

one <- FALSE
count <- 1
x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000) 
while(!done){
  if (is.na(x[count])==FALSE){
    count = count + 1
  } else {
    done <- TRUE
    print(count)
  }
}

## 練習13：請求出2~100所有的質數 (用while來作do…until…迴圈)

pnum = 2
sp = 1
pnum.list <- c()
while(pnum < 100){
  num = 2
    while(num <= pnum){
       if(num != pnum){
          if(pnum%%num == 0){
            break
          } else {
            num = num + 1
          }
       } else if(num == pnum){
         pnum.list[sp] = pnum
         sp = sp + 1
         break
      }  
    }
    pnum = pnum + 1
}
pnum.list



