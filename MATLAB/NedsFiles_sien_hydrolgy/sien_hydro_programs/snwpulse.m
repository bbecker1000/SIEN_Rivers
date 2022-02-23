% snmonset
%Calculates the onset of snowmelt in calendar days from Jan. 1 as the cumulative minimum departure from
%the mean flow over the period calendar days 9 to 248.
load('MRHq.txt')
dmq=MRHq(:,4);
mth=MRHq(:,1);
day=MRHq(:,2);
yr=MRHq(:,3);
%recdyr = input('Years of Record');
recdyr = 94;
for n = 1:recdyr;
    sumq(n) = 0.0;
    smq(n) = 0.0;
    swpulse(n) = 0.0;
    cmd(n) = 0.0;
    dypulse(n) = 0.0;
end
mindys = 1;
maxdys = 365;
tma = mindys + 100;
tmb = mindys + 339;
%Calculate discharge for period from day 100 to day 339 in water year
for n = 1:recdyr;
    for m = tma:tmb;
        sumq(n) = sumq(n) + dmq(m);
        if (mth(m)== 2) && (day(m) == 29);
            maxdys = maxdys + 1;
        end
    end
    wyr(n) = yr(maxdys);
    mdq(n) = sumq(n)/240;
    for m = tma:tmb;
        cmd(n) = cmd(n) + (dmq(m) - mdq(n));
        if cmd(n) < swpulse(n)
            swpulse(n) = cmd(n);
            dypulse(n) =(m - tma) + 8;
        end
    end
    mindys = maxdys + 1;
    maxdys = maxdys + 365;
    tma = mindys + 100;
    tmb = mindys + 339;
end
    n = 1;
    fid1 = fopen('MRHqsnoset','w');
    while n <= recdyr
    fprintf(fid1,'   %5.0f    %6.2f    %6.2f       %5.0f\n'...
        ,wyr(n),mdq(n),swpulse(n),dypulse(n));
    n = n + 1;
end 
fclose(fid1)
   
    