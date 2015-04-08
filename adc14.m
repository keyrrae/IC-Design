%%General N bit SAR ADC with M bit main-dac
clear all;
N=14; %N-bit ADC
M=6; %M-bit main-dac
Vref=1; %Full scale
sigmacap=0.001/sqrt(2); %percentage mismatch of unit cap=0.1%
numexp=1; %Number of experiments
%vinscale=(32^2)/(40+31*32);
vinscale=1;
figure;
reset(RandStream.getDefaultStream);
inlmax=0;
for num=1:numexp
%    [dacout1,dacout2,dacout3,dacout4]=fdac14(N,M,sigmacap);
    [dacout10,Cmaindac_bin,Csubdac_bin]=fbindac14(N,M,sigmacap);

    histo=zeros(1,2^N);
    for i=1:2^(N+3)
        vin=(i-1)*vinscale/2^(N+3);
        %vin=vinscale*64/4096;
%         j=mod(i,4)+1;
%         switch j
%             case 1
%               dacout10=dacout;
%             case 2
%               dacout10=dacout;
%             case 3
%               dacout10=dacout;
%             case 4
%               dacout10=dacout;
%         end
        dacout00=dacout10-2^(N-7-1);
        dacout11=dacout10+2^(N-7-1);

        %%% SAR CYCLES PRIOR TO ERROR CORRECTION
        for cycle=1:7
            if cycle==1
                dac_code(cycle)=2^(N-cycle);
            else
                dac_code(cycle)=dac_code(cycle-1)+compout(cycle-1)*2^(N-cycle);
            end
            ref(cycle)=(dacout10(dac_code(cycle)+1))/2^N;
            if(vin>=ref(cycle))
                compout(cycle)=1;
                adcout(cycle)=1;
            else
                compout(cycle)=-1;
                adcout(cycle)=0;
            end
        end
        
        %%% START OF ERROR CORRECTION CYCLE
        dac_code(8)=dac_code(7)+0.5*(compout(7)-1)*2^(N-7);
        ref=dacout10(dac_code(8)+1+2^(N-7-1))/2^N;
        if vin>ref
           ebit=1;
           dacout=dacout11;
        else
           ebit=0;
           dacout=dacout00;
        end

        for cycle=9:N+1
           if cycle==9
               dac_code(cycle)=dac_code(cycle-1)+2^(N-cycle+1);
           else
               dac_code(cycle)=dac_code(cycle-1)+compout(cycle-1)*2^(N-cycle+1);
           end
           
           if dac_code(cycle)> 2^N-1
               dac_code(cycle)=2^N-1;
           elseif dac_code(cycle)<0
               dac_code(cycle+6)=0;
           end
           
           ref(cycle)=dacout(dac_code(cycle)+1)/2^N;

           if vin>=ref(cycle)
               compout(cycle)=1;
               adcout(cycle)=1;
           else
               compout(cycle)=-1;
               adcout(cycle)=0;
           end
        end
            
%        val1=bin2dec(num2str(dac_code(1:7)))*32;
%        val2=bin2dec(num2str(dac_code(9:13)));
        val=dac_code(N+1)+0.5*(compout(N+1)-1)+2*(ebit-0.5)*2^(N-7-1);
        if val > 2^N-1
            val=2^N-1;
        elseif val < 0
            val=0;
        end        
        histo(val+1)=histo(val+1)+1;
    end
    
    for i=1:2^N
        dnl(i)=.125*(histo(i)-8);
        inl(i)=sum(dnl(1:i));
    end
    %figure;
    inlmax1=max(abs(inl));
    if inlmax1>inlmax
        inlmax=inlmax1;
        inl_worst=inl;
    end
    dmin(num)=min(dnl);
    dmax(num)=max(dnl);
    if abs(dmin(num))>abs(dmax(num))
        dnum(num)=dmin(num);
    else
        dnum(num)=dmax(num);
    end
    imin(num)=min(inl);
    imax(num)=max(inl);
    if abs(imin(num))>abs(imax(num))
        inum(num)=imin(num);
    else
        inum(num)=imax(num);
    end
    
    dmid(num)=dnl(2^(N-M));
    dmid1(num)=dnl(2^(N-M)+1);
    imid(num)=inl(2^(N-1));
    imid1(num)=inl(2^(N-2));
    subplot(3,1,1);
    plot(histo);
    title('HISTOGRAM of ADC output for a ramp (step=LSB/8)');
    hold on;
    subplot(3,1,2);
    plot(dnl);
    title('DNL (unit is LSB)');
    hold on;
    subplot(3,1,3);
    plot(inl);
    title('INL (unit is LSB)');
    hold on;
end
figure;
plot(inl_worst);
