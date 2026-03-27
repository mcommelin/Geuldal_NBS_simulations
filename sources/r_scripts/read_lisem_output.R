require(lubridate)
require(tidyverse)


args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  basepath <- 'Z:/PR/5611_10/_CAL_RUN_20230622/LISEM_runs/hpc_runs'
  src_path <- 'Z:/PR/5611_10/Geuldal_NBS_simulations/sources'
  refdate <-'2023-06-22'
  scenario <- 'res'
  resolution <- 10
  dhydro_path <- 'Z:/PR/5611_10/_CAL_RUN_20230622/DHYDRO'
}else{
  basepath <- args[1]
  src_path <- args[2]
  refdate <- args[3]
  scenario <- args[4]
  resolution <- args[5]
  dhydro_path <- args[6]
}

subdirs <- list.files(basepath)
subdirs <- subdirs[(subdirs != 'rain')&(subdirs != 'swatre')]

bc_output <- file.path(dhydro_path, 'boundaryconditions.bc')
ext_output <- file.path(dhydro_path, 'bnd.ext')

point_descriptions <- read_csv(file.path(src_path,'setup/outpoints_description.csv'))
lateral_names <- read_csv(file.path(src_path,'setup/hpc/subcatch_id_link.csv'))
lateral_names$latknoop_ID  <- make.unique(lateral_names$latknoop_ID , sep = "_")

template_file <- file.path(src_path, 'boundaryconditions.bc')

bc <- readLines(template_file)
bc2 <- bc[1:6]
ext2 <- '# written by HYDROLIB-core 0.6.0'
ext2 <- append(ext2, '[General]')
ext2 <- append(ext2, 'fileVersion = 2.01')
ext2 <- append(ext2, 'fileType    = extForce')
ext2 <- append(ext2, '')
numlats <- 0
for(i in seq(7,length(bc))){
  if (bc[i] == '[Forcing]'){
    if (grepl('constant',bc[i+2])){
      for(j in seq(0,8)){
        bc2 <- append(bc2, bc[i+j])
      }
      ext2 <- append(ext2, '[Boundary]')
      ext2 <- append(ext2, 'quantity = waterlevelbnd')
      ext2 <- append(ext2, gsub('name     = ', 'nodeId    = ', bc[i+1]))
      ext2 <- append(ext2, paste('forcingFile   =', last(strsplit(bc_output,'/')[[1]])))
      ext2 <- append(ext2, '')
    }else if (grepl('timeseries',bc[i+2])){
      # let op: alleen constante randvoorwaarden vooralsnog ondersteund (geen tijdreeksen in het model)
      if ((grepl('waterlevelbnd',bc[i+8]))|(grepl('waterlevelbnd',bc[i+8]))){
         for (j in seq(i+1,length(bc))){
           if(grepl('Forcing', bc[j])){
             break
           }
         }
         for(k in seq(i+1,j-2)){
           bc2 <- append(bc2, bc[i+k])
         }
      }else{
        # lateral discharge
        numlats <- numlats + 1
        print(paste0('Lateral node: ',bc[i+1],' (num ',numlats,')'))
      }
    }
  }
}  
 
output_list <- vector("list", length = length(subdirs))
i <- 1
for(folder in subdirs){
  if (substr(folder,nchar(folder)-3,nchar(folder)) != paste0('_',resolution,'m')){
    next 
  }else{
    subcatchment <- substr(folder, 1, nchar(folder)-4)
    
    #point_row <- point_descriptions[(point_descriptions["name"] == subcatchment)&(point_descriptions["cell_size"] == resolution),]
    #catchment_point <- point_row['point']
    
    catchment_point <- subcatchment
    lateral_name <- lateral_names[which(lateral_names['LISEM_ID'] == catchment_point),'latknoop_ID']
    
    hydromap <- file.path(basepath, folder, scenario, 'res')
    hydrofile <- list.files(hydromap, full.names=T, pattern='hydrographs-_1')
    if (!any(file.exists(hydrofile))){
      print(paste('For subcatchment',subcatchment,'no output file exists yet.'))
      next
    }
    print(paste0('File found for point ',catchment_point,': catchment ',subcatchment,', with lateral node ',lateral_name,'.'))
    hy_names <- readLines(hydrofile)[2] %>%
      str_split(",", simplify = TRUE) %>%
      str_remove_all(" |#")
    output  <- read_csv(hydrofile, skip = 2) %>%
      rename_with(~hy_names) %>%
      mutate(doy = floor(Time),
           mod = round((Time %% 1) * 24 * 60, digits = 5),
           hours = str_pad(floor(as.numeric(mod)/60), width = 2, side = "left", pad = "0"),
           mins = str_pad(floor(as.numeric(mod) %% 60), width = 2, side = "left", pad = "0"),
           date = as.Date(as.numeric(doy), origin = paste0(year(refdate), "-01-01")),
           datestring = paste0(date, " ", hours, ":", mins),
           timestamp = ymd_hm(datestring)) %>%
      distinct() %>%
      select(timestamp, all_of(hy_names))#
    
      png(file.path(dhydro_path,paste0(subcatchment, '.png')), width=1200,height=500)
      plot(output$timestamp, output$Qall, col='blue', type="l", main=subcatchment, ylab="Discharge [l/s]", xlab="")
      abline(h=seq(0,3,0.25), col='lightgrey', lty=3, lwd=0.5)
      lines(output$timestamp, output$Qbound, col='red', type="l")
      lines(output$timestamp, output$Qchan1, col='green', type="l", lty=2)
      legend("topright", c('Qall', 'Qbound', 'Qchan1'), col=c('blue','red','green'), pch=c("l","l","l"), lty=c(1,1,2))
      dev.off()

      output <- output %>% 
        select(timestamp, Qall) %>% 
        group_by(timestamp)%>% 
        summarise(across(everything(),mean),.groups = "drop")
      
      output <- output %>% mutate(Qall = Qall / 1000) %>% rename(!!as.character(lateral_name['latknoop_ID']) := Qall) 
      if(i==1){
        result <- output
      }else{
        result <- cbind(result, output)
      }
      
      # append to bcfile
      bc2 <- append(bc2, '[Forcing]')
      bc2 <- append(bc2, paste0('name              = ',lateral_name))
      bc2 <- append(bc2, 'function          = timeseries')
      bc2 <- append(bc2, 'timeInterpolation = linear')
      bc2 <- append(bc2, 'offset            = 0.0')
      bc2 <- append(bc2, 'factor            = 1.0')
      bc2 <- append(bc2, 'quantity          = time')
      bc2 <- append(bc2, paste0('unit              = minutes since ',strftime(output$timestamp[1],'%Y-%m-%d %H:%M:%S')))
      bc2 <- append(bc2, 'quantity          = lateral_discharge')
      bc2 <- append(bc2, 'unit              = m3/s')
      for (t in seq(nrow(output))){
        bc2 <- append(bc2, paste0(as.numeric(output$timestamp[t]-output$timestamp[1], units='mins'),'    ',sprintf('%13.9f',output[t,2]/1000)))
      }
      
      ext2 <- append(ext2, '[Lateral]')
      ext2 <- append(ext2, paste('id = ',lateral_name))
      ext2 <- append(ext2, 'locationType = 1d')
      # Voeg branchId/chainage toe op basis van de x,y coordinaten. Deze moeten nog gesnapped in de DHYDRO-modelbouw en toegevoegd aan de CSV.
      ext2 <- append(ext2, paste('branchId = ',""))
      ext2 <- append(ext2, paste('chainage = ',""))
      ext2 <- append(ext2, paste('discharge = ', last(strsplit(bc_output,'/')[[1]])))                     
      ext2 <- append(ext2, "")
      
      i <- i + 1
  }
}

# write all laterals as a table
write_csv(result,file.path(dhydro_path, 'all_laterals.csv'))

# write all laterals to DHYDRO forcing files
writeLines(bc2, con=bc_output)
writeLines(ext2, con=ext_output)

