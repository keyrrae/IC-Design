%%=========================================================================
% enter user defined parameters here:
N=18;                % resolution of ADC in bits
% dataformat = 0;      % straight binary
dataformat = 1;      % two's complement
% dataformat = 2;      % sign and magnitude
Maj = 9;             % the number of bits in MSB segment
Mid = 5;             % the number of bits in middle segment
LSB = 4;             % the number of LSB bits to be tested by histogram
MinimumBinWidth=0.1; % minimum non-missing bin width in units of LSB
initial_sample_throw = 0;  % number of intitial samples to be thrown away
last_sample_throw = 0;     % number of last few samples to be thrown away
first_code = 1;             % first code to be tested
last_code = 2^N-2;          % last code to be tested
flag = 1;                   % LS best fit line
% flag = 0;                   % end point fit line
%=========================================================================

% % enter reference and input information for gain error and offset computation
% Vrefp=4.096;
% Vrefn=0;
% Vinmax=Vrefp+0.020;
% Vinmin=Vrefn-0.020;

%% load file containing ADC output codes
[datafile, pathname] = uigetfile('*.*', 'Pick a raw data file');  %ask user to pick data file
if isequal(datafile,0) || isequal(pathname,0)
    disp('User pressed cancel, or invalid file')
    return;
end
y = load(fullfile(pathname, datafile));
if dataformat == 1
    y(y>=2^(N-1)) = y(y>=2^(N-1)) - 2^N;
elseif dataformat == 2
    y(y>=2^(N-1)) = 2^(N-1) - y(y>=2^(N-1));
else
    %y=y;
end

%figure; plot(y);
M = length(y);
nn = (0:M-1)';  % sai=mple index

%% If the first few and last few codes are corrupted, throw them away.
k1=1;
y1=y(1);
% y1 = first_code;
while y(k1)<=y1
    k1=k1+1;
end
k2=M;
y2=y(end);
% y2 = last_code;
while y(k2)>=y2
    k2=k2-1;
end
y=y(k1+initial_sample_throw:k2-last_sample_throw);
nn=nn(k1+initial_sample_throw:k2-last_sample_throw);

%% code for new method with ramp input
% % If the ADC's last few LSB have systematic errors,
% % compute the LSB code bin widths by LSB histogram
% h=hist(mod(y,2^LSB),(0:2^LSB-1));
% dnlLSB=h/mean(h)-1; inlLSB=cumsum(dnlLSB);
% % inlLSB=[0 inlLSB(1:end-1)];
%
% % % move from non-ideal LSB code bin centers to ideal bin centers
% yc=y;
% for k=1:2^LSB
%     y(mod(y,2^LSB)==k-1) = y(mod(y,2^LSB)==k-1) + inlLSB(k)+dnlLSB(k)/2;
% end

%% first identify ramp and compute ADC errors
yc = y;
one=ones(size(y));
n = nn;
% n=cumsum(one)-1;
theta = [n one]\y;     %solve for LS fit line
ye = [n one]*theta - y; %deviation from LS fit line

%% get the code index for the three segments
Tail = N - Maj -Mid;                   %the number of bits in tail segment
yTail = mod(yc, 2^Tail);               %output value contributed by Tail bits
yMid = mod(yc, 2^(Mid+Tail)) - yTail;  %that contributed by Mid bits
yMaj = yc - yMid - yTail;              %that contributed by Maj bits

INL=zeros(2^N,1);
yref=(0:2^N-1);
yTail_ref=mod(yref,2^Tail);
yMid_ref=mod(yref,2^(Mid+Tail)) - yTail_ref;
yMaj_ref=yref-yMid_ref-yTail_ref;

%% compute center errors for each MSB segment
% ymin=min(yc); ymax=max(yc);
% kmin=floor(ymin/2^(N-Maj));
% kmax=floor(ymax/2^(N-Maj));

eMaj=zeros(2^Maj,1); % error in each major interval's mid point
for k=1:2^Maj
    ind=(yMaj==(k-1)*2^(Mid+Tail)+yMaj(1));
    if sum(ind) > 0
        eMaj(k) = mean(ye(ind));
        ye(ind) = ye(ind)-eMaj(k);
    end
    INL(yMaj_ref==(k-1)*2^(Mid+Tail))=eMaj(k);
end

%% compute center errors for each Mid segment
eMid=zeros(2^Mid,1);
for k=1:2^Mid
    ind=(yMid==(k-1)*2^Tail);
    eMid(k) = mean(ye(ind));
    ye(ind) = ye(ind)-eMid(k);
    INL(yMid_ref==(k-1)*2^Tail) = INL(yMid_ref==(k-1)*2^Tail) + eMid(k);
end

%% compute center errors for each Mid segment
eTail=zeros(2^Tail,1);
for k=1:2^Tail
    ind=(yTail==(k-1));
    eTail(k) = mean(ye(ind));
    ye(ind) = ye(ind)-eTail(k);
    INL(yTail_ref==(k-1)) = INL(yTail_ref==(k-1)) + eTail(k);
end


%% Compute end-point or LS best fit line INL DNL
C2=0:2^N-1;
DNL=diff(INL); DNL = DNL - mean(DNL);
INL=[0; cumsum(DNL)];    DNL=[DNL;0];
if flag == 1
    A=ones(size(INL));     A=[A cumsum(A)];
    X=A\INL;    INL=INL - A*X;
    DNL = [diff(INL);0];
else                  %end point linearity test
    DNL=diff(INL); DNL = DNL - mffan(DNL);
    INL=[0; cumsum(DNL)];    DNL=[DNL;0];
end

%% account for missing codes
ind=DNL<-1;  %account for missing codes
disp('The number of missing code(s) in true DNL is:');
disp(-sum(ceil(DNL(ind))));

while sum(ind)>0
    DNL([ind(2:end);ind(1)])=DNL([ind(2:end);ind(1)])+DNL(ind)+1;
    DNL(ind)=-1;    ind=DNL<-1;
end

%% load hist DNL, computes INL, do end point fit line or LS best fit line
[datafile1, pathname] = uigetfile('*.*', 'PickRHT DNL file');  %ask user to pick data file
if isequal(datafile1,0) || isequal(pathname,0)
    disp('User pressed cancel, or invalid file')
    return;
end
DNLi = load(fullfile(pathname, datafile1));  % load raw data into variable s
ind=DNLi<-1+MinimumBinWidth;  %count # missing codes
disp('The number of missing code(s) in true DNLi is:');
disp(-sum(ceil(DNLi(ind))));

if flag == 1  % LS best fit line
    INLi = [0; cumsum(DNLi)];
    A=ones(size(INLi));    A=[A cumsum(A)];
    X=A\INLi;    INLi=INLi-A*X;
    DNLi=diff(INLi);
else         % end point fit line
    DNLi = DNLi - mean(DNLi);
    INLi = [0; cumsum(DNLi)];
end

%% plotting INL DNL plots
figure(1);
subplot(211); plot(INLi); hold; plot(INL,'r');hold off; grid;
title(datafile); ylabel('INL'); xlim([0 2^N]);
xlabel(sprintf('Segmentation: %d, %d, %d',Maj, Mid, Tail));
subplot(212); plot(DNLi); hold; plot(DNL,'r');hold off; grid;
title('Blue: from TI; Red: tested'); xlim([0 2^N]);
ylabel('DNL'); xlabel('ADC output code');
