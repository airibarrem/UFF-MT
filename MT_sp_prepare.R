#rm(list=ls())
# UFF MATO GROSSO SPATIAL ANALYSES WRAPPER

rm(list=ls())

#master.dir = '/media/alvaro/Windows/Users/Alvaro/Documents/IIS_PROJECTS/SpAM/'
master.dir = 'C:/IIS/SpAM/'

require('gdxrrw')
require('maptools')
source(paste0(master.dir,'LU_gen.R'))

# Reading shapefiles produced using GDXplot.R from GAMS output model.gdx
sp = list(BAU=0,FTY=0,SEM=0)
sp$BAU = shapefile('./in/gdx/MT_MUNI_BAU_2030_LULC.shp')
sp$FTY = shapefile('./in/gdx/MT_MUNI_FTY_2030_LULC.shp')
sp$SEM = shapefile('./in/gdx/MT_MUNI_SEM_2030_LULC.shp')

# Setting land-use names from the set in the GAMS model
lu.names = c('SOYA', 'SOMAI', 'SOCOT', 'CATTL', 'GPCTL', 'SINGL', 'SELOG', 'NOSYS')
lu.fullnames = c('Single soy plantations', 'Soy-maize double-cropping',
                 'Soy-cotton ouble cropping', 'Traditional cattle-ranching',
                 'Improved cattle-ranching', 'Other crop', 'Logging', 'Forest')

# Getting land-use information for each scenario
lu = lapply(names(sp),function(x){sp[names(sp) %in% x][[1]]@data[,names(sp[names(sp) %in% x][[1]]@data) %in% lu.names]})
names(lu) = names(sp)
lu = lapply(lu, function(x){row.names(x)=sp$BAU@data$codigo_ibg; return(x)})

# Checking consistency of GAMS input
if (round(Reduce('*',rowSums(lu$BAU)),digits=6) != 1){warning('Inconsistent land-use input')}

# Reading raster files currently inside ./in/iis/
sp.files = c(grep('tif$',dir('./in/sp/'),value=T), grep('shp$',dir('./in/sp/'),value=T))

# Background (base) raster
if (!('bgd.tif' %in% sp.files)){
  bgd = raster('./in/iis/municipios_mt_wgs.tif')
  bgd = bgd/bgd
  bgd[bgd == 1] = 0
  #writeRaster(bgd,filename='./in/sp/bgd.tif')  
} #else {bgd = raster('./in/sp/bgd.tif')}

# Reclassifying land-use key to the following classification:
# 1: Single Soy, 2: Forest, 3: Pasture, 4: Single (Cotton + Irrigated), 5: Soy-Cotton, 6: Soy-Corn, 7: Unclassified
if (!('LU2006.tif' %in% sp.files)){
  LU.2006 = rcompose('./in/iis/GY2006.tif',bgd)
  LU.2006[LU.2006 == -2] = 2 # Forest
  LU.2006[LU.2006 == -1] = 3 # Pasture
  LU.2006[LU.2006 == 11] = 4 # Irrigated
  LU.2006[LU.2006 == 99] = 7 # Unclassified
  writeRaster(LU.2006, filename='./in/sp/LU2006.tif') 
}

# Auxiliary files
if (!('roads-MT.tif' %in% sp.files)){
  roads = rcompose('./in/iis/roads_mt_wgs.tif',bgd)
  roads[roads == 6] = 4
  writeRaster(roads, filename='./in/sp/roads-MT.tif')  
}

if (!('slope-MT.tif' %in% sp.files)){
  slope = rcompose('./in/iis/Declividade_br_1km.tif',bgd)
  writeRaster(slope, filename='./in/sp/slope-MT.tif')  
}

if (!('silos-MT.shp' %in% sp.files)){
  silos = raster('./in/iis/distance_silos_soja.tif')
  silos = (silos==0)
  silos = SpatialPoints(coordinates(silos)[which(values(silos)),])
  shapefile(silos, filename='./in/sp/silos-MT.shp')  
}

if (!('sh-MT.shp' %in% sp.files)){
  sh = raster('./in/iis/distance_slaughterhouses.tif')
  sh = (sh==0)
  sh = SpatialPoints(coordinates(sh)[which(values(sh)),])
  shapefile(sh, filename='./in/sp/sh-MT.shp')  
}

if (!('markets-MT.shp' %in% sp.files)){
  markets = readShapePoints('./in/iis/Cidades_MT.shp')
  crs(markets) = crs(bgd)
  shapefile(markets, filename='./in/sp/markets-MT.shp')
}
  

if (!('UC-MT.tif' %in% sp.files)){
  UC = rcompose('./in/iis/UCs_mt_wgs.tif',bgd)
  UC = (UC>0)
  writeRaster(UC, filename='./in/sp/UC-MT.tif')
}


# Setting up the config file for whole-state harmonize data run
cfg.vec = list(lu.names=lu.names, lu.yields=paste(rep(1,length(lu.names))),
               lu.prices = paste(rep(1,length(lu.names))),
               lu.costs = paste(rep(1,length(lu.names))),
               lu.prods = paste(rep(1,length(lu.names))),
               lu.incrs = paste(rep(1,length(lu.names))),
               lu.file = 'LU2006.tif',
               lu.classes = c('SOYA', 'NOSYS', 'CATTL', 'SINGL', 'SOCOT', 'SOMAI', 'UNCL'),
               lu.masks = c('UNCL'),
               lc.classes = c('crop', 'for_close', 'grass', 'crop', 'crop', 'crop', 'other'),
               lc.slope = 'slope-MT.tif',
               tc.roads = 'roads-MT.tif',
               road.cls = c('road_main', 'road_trail', 'road_trail', 'road_acc'),
               pu.names = c('Silos', 'Slaughterhouses', 'Markets'),
               pu.files = c('silos-MT.shp', 'sh-MT.shp', 'markets-MT.shp') )

# Printing the config file formatted using the cfg.vec above
print.cfg(cfg.vec, out.dir='./in/')


# Setting up weights files for whole state, per supply chain
wgt.vec = vector('list',length(lu.names)-1)
names(wgt.vec) = lu.names[-length(lu.names)]

wgt.vec$SOYA = list(sc.name='SOYA',
                    lu.names = c('SOYA'), lu.weights = c(28.7),
                    pu.names = c('Silos'), pu.weights = c(-28.6),
                    sp.files = c('UC-MT.tif'),
                    sp.weights = c(-1000))

wgt.vec$SOMAI = list(sc.name='SOMAI',
                    lu.names = c('SOMAI'), lu.weights = c(28.7),
                    pu.names = c('Silos'), pu.weights = c(-28.6),
                    sp.files = c('UC-MT.tif'),
                    sp.weights = c(-1000))

wgt.vec$CATTL = list(sc.name='CATTL',
                    lu.names = c('CATTL'), lu.weights = c(28.7),
                    pu.names = c('Slaughterhouses'), pu.weights = c(-28.6),
                    sp.files = c('UC-MT.tif'),
                    sp.weights = c(-1000))

wgt.vec$GPCTL = list(sc.name='GPCTL',
                     lu.names = c('CATTL'), lu.weights = c(28.7),
                     pu.names = c('Slaughterhouses'), pu.weights = c(-28.6),
                     sp.files = c('UC-MT.tif'),
                     sp.weights = c(-1000))

wgt.vec$SINGL = list(sc.name='SINGL',
                     lu.names = c('SINGL'), lu.weights = c(28.7),
                     pu.names = c('Markets'), pu.weights = c(-28.6),
                     sp.files = c('UC-MT.tif'),
                     sp.weights = c(-1000))

wgt.vec$SELOG = list(sc.name='SELOG',
                     lu.names = c('NOSYS'), lu.weights = c(28.7),
                     pu.names = c('Markets'), pu.weights = c(-28.6),
                     sp.files = c('UC-MT.tif'),
                     sp.weights = c(-1000))

lapply(wgt.vec,print.wgt,out.dir='./in/BAU/')

# Runing harmonize data script for the whole state
MT.data = harmonize.data(cfg.name='cfg.txt', cfg.dir='./in/', in.df=NULL,
                         sf.on=T, cores=7, quiet=F)


# Running main script and spliting the result
BAU.res = LU.gen(cfg.dir='./in/BAU/', init.value=0.95, quiet=F)
BAU.data = BAU.res[[1]]
BAU.proj = BAU.res[[2]]

BAU.proj$Mosaic[BAU.proj$Mosaic >= (which(names(BAU.proj) == 'Coffee_r'))] = BAU.proj$Mosaic[BAU.proj$Mosaic >= (which(names(BAU.proj) == 'Coffee_r'))] - 1
BAU.proj$Coffee = BAU.proj$Coffee + BAU.proj$Coffee_r
BAU.proj = BAU.proj[-which(names(BAU.proj) == 'Coffee_r')]

write.csv(BAU.data$Config_opt, file='./out/BAU/BAU_config.csv')


SEM.res = LU.gen(cfg.dir='./in/SEM/', init.value=0.95, quiet=F)
SEM.data = SEM.res[[1]]
SEM.proj = SEM.res[[2]]

SEM.proj$Mosaic[SEM.proj$Mosaic >= (which(names(SEM.proj) == 'Cocoa'))] = SEM.proj$Mosaic[SEM.proj$Mosaic >= (which(names(SEM.proj) == 'Cocoa'))] - 1
SEM.proj$Mosaic[SEM.proj$Mosaic >= (which(names(SEM.proj) == 'Coffee_r'))] = SEM.proj$Mosaic[SEM.proj$Mosaic >= (which(names(SEM.proj) == 'Coffee_r'))] - 1
SEM.proj$Coffee = SEM.proj$Coffee + SEM.proj$Coffee_r
SEM.proj = SEM.proj[-which(names(SEM.proj) == 'Coffee_r')]
SEM.proj$Cocoa = SEM.proj$Cocoa + SEM.proj$Cocoa_s
SEM.proj = SEM.proj[-which(names(SEM.proj) == 'Cocoa_s')]

write.csv(SEM.data$Config_opt, file='./out/SEM/SEM_config.csv')


bgd = BAU.data$Sp_data$Background_mask

# Computes production based on LU.COMB areas and UFF yields (ton/ha)
#sm.area=function(x){cond.area(BAU.data$LU_now[[x]])}
#production = mapply('*', BAU.data$Config_opt$yield[-3],lapply(match(BAU.data$Config_opt$s_chains,names(BAU.data$LU_now))[-3],sm.area))
#names(production) = BAU.data$Config_opt$s_chains[-3]


LU_now_mos = BAU.data$LU_now
LU_now_mos = BAU.data$LU_now[match(names(BAU.proj),names(LU_now_mos))]
LU_now_mos = LU_now_mos[!unlist(lapply(LU_now_mos,is.null))]
LU_now_mos = Reduce('+',mapply('*',1:length(LU_now_mos),LU_now_mos))

# Output mosaics
writeRaster(LU_now_mos+bgd, './out/SM-2012.tif',overwrite=T)
writeRaster(BAU.proj$Mosaic, './out/BAU/SM-BAU.tif',overwrite=T)
writeRaster(SEM.proj$Mosaic, './out/SEM/SM-SEM.tif',overwrite=T)

# Creating layer for deforestation for LU.proj
BAU.proj$Deforestation = BAU.data$LU_now$Forest - BAU.proj$Forest
SEM.proj$Deforestation = SEM.data$LU_now$Forest - SEM.proj$Forest

# Avoided deforestation map
defor.avoided = BAU.proj$Deforestation - SEM.proj$Deforestation

# Conversion rates
BAU.conv = lapply(BAU.proj, function(x){cond.area(x*BAU.proj$Deforestation)})
SEM.conv = lapply(SEM.proj, function(x){cond.area(x*SEM.proj$Deforestation)})

conv.data = data.frame('BAU'=as.numeric(BAU.conv),'SEM'=as.numeric(SEM.conv))[-which(names(BAU.proj) %in% c('Forest','Water', 'Other', 'Mosaic')),]
row.names(conv.data) = names(BAU.proj[-which(names(BAU.proj) %in% c('Forest','Water', 'Other', 'Mosaic'))])

write.csv(conv.data,file='./out/conv_data.csv')


# Carbon stock map
c.stk = BAU.data$Sp_data$"SM-CSTK.tif"

# Estimate of total carbon emission
BAU.closs = cond.area(c.stk*BAU.proj$Deforestation)/100
SEM.closs = cond.area(c.stk*SEM.proj$Deforestation)/100

# SEM carbon additionality: avoided emissions + increased stocks due to
# agroforestry Coffee: 8059 ha recovered from average ton/ha lost
SEM.cadd = abs(SEM.closs - BAU.closs) + (2435 * (SEM.closs / cond.area(SEM.proj$Deforestation)))

# Output images

jpeg("./out/avoided_defor.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(defor.avoided,
       col.regions=colorRampPalette(colors=c('lightsteelblue','darkblue'),interpolate='spline'))
dev.off()

writeRaster(defor.avoided,'./out/SEM/avoided_defor.tif',overwrite=T)

jpeg("./out/cocoa_indepth.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj$Cocoa,-1*SEM.proj$Cocoa,BAU.proj$Cocoa - SEM.proj$Cocoa,
             (BAU.proj$Cocoa * BAU.proj$Deforestation) - (SEM.proj$Cocoa * SEM.proj$Deforestation)),
       names.attr=c('BAU', 'SEM', 'BAU - SEM', 'BAU defor. - SEM defor.'),
       col.regions=colorRampPalette(colors=c('red', 'lightsteelblue','darkblue')))
dev.off()

jpeg("./out/Coffee_indepth.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj$Coffee,-1*SEM.proj$Coffee,BAU.proj$Coffee - SEM.proj$Coffee,
             (BAU.proj$Coffee * BAU.proj$Deforestation) - (SEM.proj$Coffee * SEM.proj$Deforestation)),
       names.attr=c('BAU', 'SEM', 'BAU - SEM', 'BAU defor. - SEM defor.'),
       col.regions=colorRampPalette(colors=c('red', 'lightsteelblue','darkblue')))
dev.off()

jpeg("./out/Maize_indepth.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj$Maize,-1*SEM.proj$Maize,BAU.proj$Maize - SEM.proj$Maize,
             (BAU.proj$Maize * BAU.proj$Deforestation) - (SEM.proj$Maize * SEM.proj$Deforestation)),
       names.attr=c('BAU', 'SEM', 'BAU - SEM', 'BAU defor. - SEM defor.'),
       col.regions=colorRampPalette(colors=c('red', 'lightsteelblue','darkblue')))
dev.off()

jpeg("./out/Beef_indepth.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj$Beef,-1*SEM.proj$Beef,BAU.proj$Beef - SEM.proj$Beef,
             (BAU.proj$Beef * BAU.proj$Deforestation) - (SEM.proj$Beef * SEM.proj$Deforestation)),
       names.attr=c('BAU', 'SEM', 'BAU - SEM', 'BAU defor. - SEM defor.'),
       col.regions=colorRampPalette(colors=c('red', 'lightsteelblue','darkblue')))
dev.off()

jpeg("./out/Diverse_indepth.jpg", res=600, width=10, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj$Diverse,-1*SEM.proj$Diverse,BAU.proj$Diverse - SEM.proj$Diverse,
             (BAU.proj$Diverse * BAU.proj$Deforestation) - (SEM.proj$Diverse * SEM.proj$Deforestation)),
       names.attr=c('BAU', 'SEM', 'BAU - SEM', 'BAU defor. - SEM defor.'),
       col.regions=colorRampPalette(colors=c('red', 'lightsteelblue','darkblue')))
dev.off()


jpeg("./out/SM-LU-2012.jpg", res=600, width=20, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.data$LU_now[-c(9,10)])+bgd,names.attr=names(BAU.data$LU_now[-c(9,10)]),
       col.regions=colorRampPalette(colors=c('lightsteelblue','darkblue'),interpolate='spline'))
dev.off()

jpeg("./out/SM-CAP-2012.jpg", res=600, width=20, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.data$Sp_data$"SM-CAP-TEMP.tif",BAU.data$Sp_data$"SM-CAP-PERM.tif"),names.attr=c('Temporary crop', 'Permanent crop'),
       col.regions=colorRampPalette(colors=c('lightsteelblue','darkblue'),interpolate='spline'))
dev.off()

jpeg("./out/SM-TC-2012.jpg", res=600, width=20, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.data$TC_maps),
       col.regions=colorRampPalette(colors=c('lightsteelblue','dodgerblue4','darkblue'),interpolate='spline'))
dev.off()

jpeg("./out/SM-PRIORS.jpg", res=600, width=15, height=9, unit='cm', pointsize=6)
spplot(stack(SEM.data$SC_priors),
       col.regions=colorRampPalette(colors=c('lightsteelblue','dodgerblue4','darkblue'),interpolate='spline'))
dev.off()

jpeg("./out/BAU/SM-BAU-2024.jpg", res=600, width=20, height=9, unit='cm', pointsize=6)
spplot(stack(BAU.proj[-c(9,11)]),
       col.regions=colorRampPalette(colors=c('lightsteelblue','dodgerblue4','darkblue'),interpolate='spline'))
dev.off()

jpeg("./out/SEM/SM-SEM-2024.jpg", res=600, width=20, height=9, unit='cm', pointsize=6)
spplot(stack(SEM.proj[-c(9,11)]),
       col.regions=colorRampPalette(colors=c('lightsteelblue','dodgerblue4','darkblue'),interpolate='spline'))
dev.off()
