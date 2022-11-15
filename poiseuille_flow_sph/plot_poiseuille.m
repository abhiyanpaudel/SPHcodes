clc
clear all
close all

for i = 0:10
    data = load(sprintf('%d.dat',i));
    plot(data(:,1),data(:,2),'o','LineWidth',2)
    hold on
end
