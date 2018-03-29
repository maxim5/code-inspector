data = dlmread('ISS_to_DARM.txt');

f = data(:,1);
data = data(:,2:end);

n_sets = size(data, 2)/2;

combiner = spalloc(n_sets*2, n_sets, n_sets*2);
for ii=1:n_sets,
    combiner(2*(ii-1)+(1:2), ii) = [1; i];
end

data = data * combiner;


offsets = [15 25 7.5 7.5 5 10 15];
offsets = log(offsets)/log(15);

loglog(f, abs(data), 'o-')

cm = flag();
colormap(cm);
set(gca, 'clim', [min(offsets)*0.9 max(offsets)*1.2])
clim = get(gca, 'clim');

colors = interp1(linspace(clim(1), clim(2), size(cm, 1)), cm, offsets);

set(gca, 'ColorOrder', colors);
set(gca, 'NextPlot', 'replacechildren');
loglog(f, abs(data), 'o-')


legend('1','2','3','4','5','6','7')

%%
subplot(111);
hold off;
for ii=0:2:12,
data = dlmread(sprintf('ISS_to_DARM_ref%d.txt', ii));


f = data(:,1);
X = data(:,2) + i*data(:,3);

cm = jet();
colors = interp1(linspace(log10(clim(1)), log10(clim(2)), size(cm, 1)), cm, log10(offsets(ii/2 + 1)));
colormap(cm);
set(gca, 'clim', [min(offsets)*0.9 max(offsets)*1.2])
clim = get(gca, 'clim');

subplot(2,1,1);
loglog(f, abs(X), '-o', 'color', colors)
set(gca, 'xscale', 'log', 'yscale', 'log');
axis tight
grid on
ylabel('magnitude');
title('L1:LSC-DARM_ERR / L1:PSL-ISS_ILMONPD_W versus DARM offset', 'interpreter', 'none', 'FontName', 'Courier');
hold all
legend('15 pm','25 pm', '7.5 pm', '7.5 pm',  '5 pm',  '10 pm', '15 pm');

subplot(2,1,2);
hold all;
p = unwrap(angle(X))*180/pi;
if (mean(p) < -180)
    p = p + 360;
end
semilogx(f, p , '-o', 'color', colors);
axis tight
ylim([-200 200]);
set(gca, 'YTick', 45*(-4:4), 'Xscale', 'log');
grid on
ylabel('phase [degrees]');
xlabel('frequency [Hz]');
end

