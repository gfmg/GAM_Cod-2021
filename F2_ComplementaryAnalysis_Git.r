library(hypervolume)
library(lattice)
library(gridExtra)

load("data.pregam.RData")

# Center and scale day of the year
data.pregam$J.day.sc<- (data.pregam$J.day - 
							mean(data.pregam$J.day, na.rm= T)) / 
						sd(data.pregam$J.day.sc, na.rm= T)

# Center Lat and Lon
data.pregam$ShootLat.sc<- (data.pregam$ShootLat - mean(data.pregam$ShootLat, na.rm= T))
data.pregam$ShootLon.sc<- data.pregam$ShootLon - mean(data.pregam$ShootLon, na.rm= T)
# Scale Lat and Lon by same amount:
LatLon.sd<- sd(c(data.pregam$ShootLon.sc, data.pregam$ShootLat.sc), na.rm= T)
data.pregam$ShootLat.sc<- data.pregam$ShootLat.sc / LatLon.sd
data.pregam$ShootLon.sc<- data.pregam$ShootLon.sc / LatLon.sd

# variables of interest for hypervolumes
VOI<- c("J.day.sc", "ShootLat.sc", "ShootLon.sc")

# list of gear types
GEAR<- unique(data.pregam$gear)

# Compute hypervolumes for each gear
HypV<- list()

for(i in GEAR){
	print(i)
	HypV[[i]]<- hypervolume(data.pregam[data.pregam$gear == i, VOI], method = "gaussian")
}

save.image("HypV.RData")
# load("HypV.RData")


# create pairs of hypervolumes to compute ovelap measures
HypV.sets<- list()
for(i in GEAR){
	for(j in GEAR){
		HypV.sets[[i]][[j]]<- hypervolume_set(HypV[[i]], HypV[[j]], num.points.max= 50000, check.memory= FALSE)
	}
}

save(HypV.sets, file= "HypVsets.RData")
# load("HypVsets.RData")

# compute proportion of overlap between gear spatio-temporal volumes
Overlap.Mat.asym<- matrix(1, nrow= length(GEAR), ncol= length(GEAR), dimnames= list(GEAR, GEAR))

for(i in GEAR){
	for(j in GEAR){
		Overlap.Mat.asym[GEAR == j, GEAR==i]<- HypV.sets[[i]][[j]]@HVList$Intersection@Volume / HypV.sets[[i]][[j]]@HVList$HV1@Volume
	}
}

# plot cumulative overlap with all other gears
barplot(sort(apply(Overlap.Mat.asym, 1, sum) - 1))
# highest is GOV, followed very closely by MTH, MTR and MTN

# order by decreasing cumulative overlap with all other gears
Overlap.Mat.Reorder<- order(apply(Overlap.Mat.asym, 1, sum))

# plot proportion of overlap between gear spatio-temporal volumes
cols <- colorRampPalette(c("white","red"))(256)

lpa<- levelplot(Overlap.Mat.asym[Overlap.Mat.Reorder, Overlap.Mat.Reorder],
			at= seq(0, 1, .01), col.regions=cols,
			ylab= "Proportion of ...", xlab= "included in ...",
			panel = function(...) {
				arg <- list(...)
                panel.levelplot(...)
                panel.text(arg$x, arg$y, round(arg$z,1))
			})

lpa


# compute maximum proportion of overlap between gear spatio-temporal volumes,
# using smallest gear volume as denominator (symmetrical)
Overlap.Mat.max<- matrix(1, nrow= length(GEAR), ncol= length(GEAR), dimnames= list(GEAR, GEAR))

for(i in GEAR){
	for(j in GEAR){
		min.vol<- min(HypV.sets[[i]][[j]]@HVList$HV1@Volume, HypV.sets[[i]][[j]]@HVList$HV2@Volume)
		Overlap.Mat.max[GEAR == j, GEAR==i]<- HypV.sets[[i]][[j]]@HVList$Intersection@Volume / min.vol
	}
}

levelplot(Overlap.Mat.max,
			at= seq(0, 1, .01), col.regions=cols,
			ylab= "", xlab= "")

# Format as HypervolumeList
hv_gear_list <- new("HypervolumeList")

hv_gear_list@HVList<- vector(mode= "list", length= length(GEAR))

for (i in 1:length(GEAR)){
    hv_gear_list@HVList[[GEAR[i]]] <- HypV[[i]]
	hv_gear_list@HVList[[GEAR[i]]]@Name<- as.character(GEAR[i])
}


# plot 3D representation of spatio-temporal volumes for pairs of gears
# exclude raw data
# Beware, long run!
for(i in 2:length(GEAR)){
	for(j in 1:(i-1)){
		plot(hv_gear_list[[GEAR[c(i, j)]]], show.3d= T,
			show.contour= F, point.alpha.min= 0.9, 
			point.dark.factor= 0, cex.data= 0.1,
			cex.random= 0.9, num.points.max.data= 1,#100000, 
			num.points.max.random= 5000, 
			plot.3d.axes.id= c(1, 3, 2))
		hypervolume_save_animated_gif(image.size = 500, duration= 10, rpm= 6,
			file.name= paste("GraphsWithoutObservations/GearVolumes_", GEAR[i], "_vs_", GEAR[j], sep= ""))
	}
}


# Combine all these animated plots (static)
library(magick)
library(grid)
library(ggplot2)

n.gear<- length(GEAR)
layout.mat<- matrix(1:(n.gear^2), n.gear, n.gear)
levelplot(layout.mat)

grobs.list<- list()
counter<- 0

# lower triangle: extract 57th frame from each animation
for(i in 2:length(GEAR)){
	for(j in 1:(i-1)){
		counter<- counter + 1
		print(counter)
		print(paste("GearVolumes_", GEAR[i], "_vs_", GEAR[j], sep= ""))
		tmp<- magick::image_crop(
			magick::image_read(
				paste("GraphsWithoutObservations/GearVolumes_", GEAR[i],
					"_vs_", GEAR[j], ".gif", sep= "")
				)[57], 
			"370x390+50+50")
		ggtmp<- image_ggplot(tmp, interpolate= T) + theme(plot.margin = margin(0, 0, 0, 0))
		grobs.list[[layout.mat[j, i]]]<- ggtmp
		gc()
		gc()
	}
}
save(grobs.list, file= "GraphsWithoutObservations/GrobsListUpper57.RData")
# load("GraphsWithoutObservations/GrobsListUpper.RData")

# diagonal: Gear name
for(i in 1:length(GEAR)){
	grobs.list[[layout.mat[i, i]]]<- textGrob(GEAR[i])
}
# upper triangle: colour scale for proportion of volume overlap
for(j in 2:length(GEAR)){
	for(i in 1:(j-1)){
		NcolsMinusOne<- length(cols) - 1
		grobs.list[[layout.mat[j, i]]]<- rectGrob(gp= 
			gpar(fill= cols[findInterval(
				x= Overlap.Mat.max[j, i],
				vec= (0:NcolsMinusOne)/NcolsMinusOne,
				all.inside= T)
				]))
	}
}

pdf("GraphsWithoutObservations/GearOverlaps.pdf", width= 30, height= 30)
do.call(grid.arrange, c(grobs.list, ncol= n.gear))
dev.off()

# repeat plot for a subset of exemplary gears
GEAR.sub<- c(1, 2, 6, 10)
gearsub<- do.call(arrangeGrob, c(grobs.list[layout.mat[GEAR.sub, GEAR.sub]], ncol= length(GEAR.sub)))
ggsave("GraphsWithoutObservations/GearOverlaps_subset.pdf", gearsub, width= 14, height= 14)
ggsave("GraphsWithoutObservations/GearOverlaps_subset.png", gearsub, width= 14, height= 14, dpi= 600)

p1<- grobs.list[[layout.mat[1, 6]]]
p2<- grobs.list[[layout.mat[1, 2]]]
p3<- grobs.list[[layout.mat[2, 6]]]
p4<- grobs.list[[layout.mat[6, 10]]]
lpa.ext<- grid.arrange(lpa,p1,p2,p3,p4, layout_matrix = cbind(c(1,1), c(1,1), c(2,3), c(4,5)))
ggsave("GraphsWithoutObservations/Figure2extended.png", lpa.ext, width= 14, height= 7, dpi= 600)

lpa.basic<- grid.arrange(lpa)
ggsave("GraphsWithoutObservations/Figure2.png", lpa.basic, width= 7, height= 7, dpi= 600)


