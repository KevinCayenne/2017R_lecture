
## �m��12�G�������Ҥl���A�@Ū��missing value�N���X�A�B�i�D�ڬO�ĴX��missing (��while�ӧ@do�Kuntil�K�j��)

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

## �m��13�G�ШD�X2~100�Ҧ������ (��while�ӧ@do�Kuntil�K�j��)

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


