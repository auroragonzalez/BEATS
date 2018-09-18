function[ck,ch,cs,temperature,a_temperature,b_temperature]= ChCsforEstimation(Ck,Ch,Cs,tempratureHard,LowLimtemperature,UpLimtemperature)
%% Test whether estimation data is in Hard Data
Ck_in_Ch=ismember(Ch,Ck,'rows');
Ch(Ck_in_Ch,:)=[];
ch=Ch;
temperature=tempratureHard';
temperature(Ck_in_Ch)=[];
%% Test whether estimation data is in Soft Data
if(isempty(Cs)==1)
    cs=[];
    a_temperature=[];
    b_temperature=[];
else
Ck_in_Cs=ismember(Cs,Ck,'rows');
Cs(Ck_in_Cs,:)=[];
cs=Cs;
%LowLimtemperature= tempratureSoft-(LowUpLimit./10);
%UpLimtemperature=  tempratureSoft+(LowUpLimit./10);
a_temperature=LowLimtemperature';
a_temperature(Ck_in_Cs)=[];
b_temperature=UpLimtemperature';
b_temperature(Ck_in_Cs)=[];
end
ck=Ck;
end
