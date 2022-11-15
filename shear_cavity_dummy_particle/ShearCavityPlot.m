clc
clear all
close all

data1 = load('f_xv.dat');

x1=data1(:,1);
y1=data1(:,2);
u1=data1(:,3);
v1=data1(:,4);
 data2 = load('boundary_par.dat');
 x2=data2(:,1);
 y2=data2(:,2);
%   data3 = load('virtual_par.dat');
%   x3=data3(:,1);
% y3=data3(:,2);
% data4 = load('Dummy.dat');
% x4=data4(:,1);
% y4=data4(:,2);

figure(1)
plot(x1,y1,'ro','MarkerFaceColor','g','MarkerEdgeColor','k',...
    'Markersize',3)
 hold on
 plot(x2,y2,'ro','MarkerFaceColor','r','MarkerEdgeColor','k',...
   'Markersize',4)
hold on 
quiver(x1,y1,u1,v1,'LineWidth',2)
%   hold on
%    plot(x3,y3,'ro','MarkerFaceColor','b','MarkerEdgeColor','k',...
%     'Markersize',6)
% hold on
% plot(x4,y4,'*','MarkerFaceColor','r','MarkerEdgeColor','r',...
%     'Markersize',4)
xlabel('x ')
ylabel('y')
title('Particle Distribution')
 axis equal

 % figure(2)
% plot(x2,y2,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% xlabel('x ')
% ylabel('y')
% title('Multi Element Flap(FlaP1)')
% %axis([0 0.9, -0.3 0.3])
% 
% figure(3)
% plot(x3,y3,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% xlabel('x ')
% ylabel('y')
% title('Multi Element Flap(Slat)')
% axis([0 0.9, -0.3 0.3])
% 
% figure(4)
% plot(x4,y4,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% xlabel('x ')
% ylabel('y')
% title('Multi Element Flap(Slat)')
% axis([0 0.9, -0.3 0.3])

% Assembly of multi element flap

% plot(x1,y1,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% hold on
% plot(x2,y2,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% hold on
% plot(x3,y3,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% %hold on 
% %plot(x4,y4,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
% %   'Markersize',4)
% xlabel('x ')
% ylabel('y')
% title('Multi Element Flap')
% axis([-0.2 1.3, -0.5 0.5])

%Assembly of multi element flap with no marker

% plot(x1,y1,'-','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% patch(x1,y1,'b')
% hold on
% plot(x2,y2,'-','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% patch(x2,y2,'r')
% hold on
% plot(x3,y3,'-','MarkerFaceColor','g','MarkerEdgeColor','k',...
%     'Markersize',4)
% patch(x3,y3,'k')
% %hold on 
% %plot(x4,y4,'-rs','MarkerFaceColor','g','MarkerEdgeColor','k',...
% %   'Markersize',4)
% xlabel('x ')
% ylabel('y')
% title('Multi Element Flap')
% axis([-0.2 1.3, -0.5 0.5])
