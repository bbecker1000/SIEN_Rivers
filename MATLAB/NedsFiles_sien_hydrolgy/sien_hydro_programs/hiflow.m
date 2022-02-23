% hiflow
%Calculates the low flow statistics from the time series of daily values,
load('MRHq.txt')
dmq=MRHq(:,4);
mth=MRHq(:,1);
day=MRHq(:,2);
yr=MRHq(:,3);
%recdyr = input('Years of Record');
recdyr = 94;
    hi3df = [];
    hi7df = [];
    hi10df = [];
    hi14df = [];
mindys = 1;
maxdys = 365;
tma = mindys + 30;
tmb = maxdys;
for n = 1:recdyr;
        for m = tma:tmb;
            if(mth(m)== 2)&& (day(m)== 29);
                maxdys = maxdys + 1;
            end
            tmb = maxdys;
        end
        wyr(n) = yr(maxdys);
        hi3df(n) = 10;
        hi7df(n) = 10;
        hi10df(n) = 10;
        hi14df(n) = 10;
        for m = tma:tmb;
            hq3d = (dmq(m)+dmq(m-1)+dmq(m-2))/3;
            hq7d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6))/7;
            hq10d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6)+dmq(m-7)+dmq(m-8)+dmq(m-9))/10;
            hq14d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6)+dmq(m-7)+dmq(m-8)+dmq(m-9)+dmq(m-10)+dmq(m-11)...
                +dmq(m-12)+dmq(m-13))/14;
            if hq3d > hi3df(n);
                hi3df(n) = hq3d;
                mhi3df(n) = m - (mindys - 1);
            end
            if hq7d > hi7df(n);
                hi7df(n) = hq7d;
                mhi7df(n) = m - (mindys - 1);
            end
            if hq10d > hi10df(n);
                hi10df(n) = hq10d;
                mhi10df(n) = m - (mindys - 1);
            end
            if hq14d > hi14df(n);
                hi14df(n) = hq14d;
                mhi14df(n) = m - (mindys - 1);
            end
        end
        mindys = maxdys + 1;
        maxdys = maxdys + 365;
        tma = mindys;
        tmb = maxdys;
end
        hi3dx = sort(hi3df);
        hi7dx = sort(hi7df);
        hi10dx = sort(hi10df);
        hi14dx = sort(hi14df);
        for n = 1:recdyr;
            exdP(n) = (recdyr - (n-1))/(recdyr + 1);
        end
        xxp = [0.98, 0.95, 0.90, 0.80, 0.70, 0.6, 0.50, 0.3333, 0.20,...
            0.1429, 0.10, 0.0667, 0.05, 0.040, 0.0333, 0.02857, 0.025,...
            0.02222, 0.020];
        for p = 2:19;
            ri(p) = 1/xxp(p);
            for n = 2:recdyr;
                if exdP(n-1) > xxp(p) && exdP(n) <= xxp(p);
                    Px = ((xxp(p)-exdP(n))/(exdP(n-1)-exdP(n)));
                    hi7D(p) = hi7dx(n) + Px*(hi7dx(n-1)-hi7dx(n));
                    hi3D(p) = hi3dx(n) + Px*(hi3dx(n-1)-hi3dx(n));
                    hi14D(p) = hi14dx(n) + Px*(hi14dx(n-1)-hi14dx(n));
                    hi10D(p) = hi10dx(n) + Px*(hi10dx(n-1)-hi10dx(n));
                end
            end
        end
fid1 = fopen('mrhhif1','w');
fid2 = fopen('mrhhif2','w');
fid3 = fopen('mrhhif3X','w');
for n = 1:recdyr;
    fprintf(fid1,'%7.f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n'...
       ,wyr(n),hi3df(n),hi7df(n),hi10df(n),hi14df(n),mhi3df(n),mhi14df(n));
end
for n = 1:recdyr;
    fprintf(fid2,' %8.4f  %8.2f  %8.2f  %8.2f  %8.2f\n'...
        ,exdP(n),hi3dx(n),hi7dx(n),hi10dx(n),hi14dx(n));
end
for p = 2:19;
    fprintf(fid3,' %7.3f  %8.2f   %8.2f  %8.2f  %8.2f\n'...
        ,xxp(p),hi3D(p),hi7D(p),hi10D(p),hi14D(p));
    p = p + 1;
end
fclose(fid1);
fclose(fid2);
fclose(fid3);
    
   
    