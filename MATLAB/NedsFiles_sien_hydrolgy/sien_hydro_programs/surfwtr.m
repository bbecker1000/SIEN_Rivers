% surfwtr
%Calculates the mean annual flow, time to center of mass, and
%days to cumulative Q equal to 0.98 of annual runoff
load('MRHq.txt')
dmq=MRHq(:,4);
mth=MRHq(:,1);
day=MRHq(:,2);
yr=MRHq(:,3);
%recdyr = input('Years of Record');
recdyr = 94;
for n = 1:recdyr
    sumq(n) = 0.0;
    smq(n) = 0.0;
end
mindys = 1;
maxdys = 365;
for n = 1:recdyr;
    qt(n)=0.0;
    for m = mindys:maxdys;
        sumq(n) = sumq(n) + dmq(m);
        if (mth(m)== 2) && (day(m) == 29);
            maxdys = maxdys + 1;
        end
    end
    wyr(n) = yr(maxdys);
    dur(n) = maxdys-mindys + 1;
    mdq(n) = sumq(n)/dur(n);
    for m = mindys:maxdys;
        nct = (m - mindys) + 1;
        qt(n) = qt(n) + (nct*dmq(m));
        if (mth(m) > 3) && (mth(m) < 8);
            smq(n) = smq(n) + dmq(m);
        end
    end
    cmt(n) = qt(n)/sumq(n);
    frsmq(n) = smq(n)/sumq(n);
    mindys = maxdys + 1;
    maxdys = maxdys + 365;
end
    n = 1;
    fid1 = fopen('MRHq_snmlt','w');
    while n <= recdyr
    fprintf(fid1,'   %5.0f    %6.2f    %6.2f       %6.2f       %6.5f\n'...
        ,wyr(n),dur(n),mdq(n),cmt(n),frsmq(n));
    n = n + 1;
end 
fclose(fid1)
   
    