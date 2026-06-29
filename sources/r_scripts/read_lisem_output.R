require(lubridate)
require(tidyverse)


args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  run_path <- 'Z:/PR/5611_10/LISEM/125/LISEM_runs/hpc_runs'
  src_path <- 'Z:/PR/5611_10/LISEM/aanvulling/LISEM_sources'
  refdate <-'2023-06-22'
  scenario <- 'res_20230622'
  resolution <- 10
  dhydro_start <- '2023-06-20 00:00:00'
  dhydro_end <- '2023-07-01 00:00:00'
  dhydro_path <- 'Z:/PR/5611_10/LISEM/125/LISEM_runs/DHYDRO'
}else{
  run_path <- args[1]
  src_path <- args[2]
  refdate <- args[3]
  scenario <- args[4]
  resolution <- args[5]
  dhydro_start <- args[6]
  dhydro_end <- args[7]
  dhydro_path <- args[8]
}

subdirs <- list.files(run_path)
subdirs <- subdirs[(subdirs != 'rain')&(subdirs != 'swatre')]

bc_template <- file.path(src_path, 'boundaryconditions_template.bc')
bc_output <- file.path(dhydro_path, 'boundaryconditions.bc')

baseflow <- read.csv(file.path(src_path, 'baseflow_geplitst.csv'))

dhydro_start = as.POSIXct(dhydro_start, tz="UTC", '%Y-%m-%d %H:%M:%S')
dhydro_end = as.POSIXct(dhydro_end, tz="UTC", '%Y-%m-%d %H:%M:%S')

point_descriptions <- read_csv(file.path(src_path,'setup/outpoints_description.csv'))
lateral_names <- read_csv(file.path(src_path,'setup/hpc/subcatch_id_link.csv'))
lateral_names$latknoop_ID  <- make.unique(lateral_names$latknoop_ID , sep = "_")

bc <- readLines(bc_template)
bc2 <- bc[seq_len(grep('lateral_discharge',bc)[1]-6)]

output_list <- vector("list", length = length(subdirs))
i <- 1

# subdirs <- subdirs[c(1,2,3)]

for(folder in subdirs){
  if (substr(folder,nchar(folder)-3,nchar(folder)) != paste0('_',resolution,'m')){
    next 
  }else{
    subcatchment <- substr(folder, 1, nchar(folder)-4)
    
    #point_row <- point_descriptions[(point_descriptions["name"] == subcatchment)&(point_descriptions["cell_size"] == resolution),]
    #catchment_point <- point_row['point']
    
    catchment_point <- subcatchment
    lateral_name <- lateral_names[which(lateral_names['LISEM_ID'] == catchment_point),'latknoop_ID']
    
    bf <- baseflow[which(baseflow$lateral_id == as.character(lateral_name)), ]$HBV_baseflow
    
    hydromap <- file.path(run_path, folder, scenario, 'res')
    hydrofile <- list.files(hydromap, full.names=T, pattern='hydrographs-_')
    if (length(hydrofile)>1){
      print('Multiple Hydrographs files found. Taking the first one.')
      hydrofile <- hydrofile[1]
    }
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
           date = as.Date(as.numeric(doy-1), origin = paste0(year(refdate), "-01-01")),
           datestring = paste0(date, " ", hours, ":", mins),
           timestamp = ymd_hm(datestring)) %>%
      distinct() %>%
      select(timestamp, all_of(hy_names))#

      png(file.path(dhydro_path,paste0(subcatchment, '.png')), width=1200,height=500)
      plot(output$timestamp, output$Qall, col='blue', type="l", main=subcatchment, ylab="Discharge [l/s]", xlab="", ylim=c(0,2000))
            abline(h=seq(0,3,0.25), col='lightgrey', lty=3, lwd=0.5)
      lines(output$timestamp, output$Qbound, col='red', type="l")
      lines(output$timestamp, output$Qchan1, col='green', type="l", lty=2)
      abline(h=bf*1e3, col='black',lty=2,lwd=1.5)
      lines(output$timestamp, output$Qall + bf*1e3, col='magenta', type="l", lty=2)
      legend("topright", c('Qall', 'Qbound', 'Qchan1','baseflow', 'total'), col=c('blue','red','green', 'black','magenta'), pch=c("l","l","l","l","l"), lty=c(1,1,2,2,1))
      dev.off()

      output <- output %>% 
        select(timestamp, Qall, Qbound) %>% 
        group_by(timestamp)%>% 
        summarise(across(everything(),mean),.groups = "drop")
      
      # divide by 1000 and add baseflow
      print(paste('baseflow', lateral_name, bf))
      output <- output %>% mutate(Qall = Qall / 1000 + Qbound / 1000 +  bf) %>% rename(!!as.character(lateral_name['latknoop_ID']) := Qall) %>% select(-Qbound)
      
      if(i==1){
        result <- output
      }else{
        result <- cbind(result, output[,2])
      }
      
      offset_before = as.numeric(first(output)$timestamp - dhydro_start, units="mins")
      
      # # append to bcfile
      bc2 <- append(bc2, '[Forcing]')
      bc2 <- append(bc2, paste0('name              = ',lateral_name))
      bc2 <- append(bc2, 'function          = timeseries')
      bc2 <- append(bc2, 'timeInterpolation = linear')
      bc2 <- append(bc2, 'offset            = 0.0')
      bc2 <- append(bc2, 'factor            = 1.0')
      bc2 <- append(bc2, 'quantity          = time')
      #bc2 <- append(bc2, paste0('unit              = minutes since ',strftime(output$timestamp[1],'%Y-%m-%d %H:%M:%S', tz='UTC')))
      bc2 <- append(bc2, paste0('unit              = minutes since ',strftime(dhydro_start, '%Y-%m-%d %H:%M:%S', tz='UTC')))
      bc2 <- append(bc2, 'quantity          = lateral_discharge')
      bc2 <- append(bc2, 'unit              = m3/s')
      for (t in seq(nrow(output))){
        bc2 <- append(bc2, paste0(as.numeric(output$timestamp[t]-output$timestamp[1], units='mins')+offset_before,'    ',sprintf('%13.9f',output[t,2])))
      }
      bc2 <- append(bc2, '')
      
      i <- i + 1
  }
}

# write all laterals as a table


write_csv(result,file.path(dhydro_path, 'all_laterals.csv'))
block1 <- list()
for(i in seq(length(output$timestamp)+offset_before,(as.numeric(dhydro_end - last(output)$timestamp, units="mins")-1))){
  block1 <- append(block1, paste(i,'    0.000000000'))
}

block2 <- list()
for(i in seq(0,as.numeric(first(output)$timestamp - dhydro_start, units="mins")-1)){
  block2 <- append(block2, paste(i,'    0.000000000'))
}

positions1 <- which(map_lgl(bc2, \(line) any(grepl(paste(offset_before + length(output$timestamp)-1, ' '), line))))
for (pos in sort(positions1, decreasing = TRUE)) {
  bf <- as.numeric(baseflow[which(baseflow$lateral_id == as.character(gsub(" ","",strsplit(unlist(bc2[pos-1808]),'=')[[1]][2]))),]$HBV_baseflow)
  print(paste(as.character(gsub(" ","",strsplit(unlist(bc2[pos-1808]),'=')[[1]][2])), bf))
  block <- lapply(block1, gsub, pattern = "    0.000000000", replacement = paste0("   ",bf))
  bc2 <- append(bc2, block, after = pos)
  
}

positions2 <- which(map_lgl(bc2, \(line) any(grepl('unit              = m3/s', line))))
for (pos in sort(positions2, decreasing = TRUE)) {
  bf <- as.numeric(baseflow[which(baseflow$lateral_id == as.character(gsub(" ","",strsplit(unlist(bc2[pos-8]),'=')[[1]][2]))),]$HBV_baseflow)
  print(paste(as.character(gsub(" ","",strsplit(unlist(bc2[pos-8]),'=')[[1]][2])), bf))
  block <- lapply(block2, gsub, pattern = "    0.000000000", replacement = paste0("   ",bf))
  bc2 <- append(bc2, block, after = pos)
}

# write all laterals to DHYDRO forcing files
writeLines(unlist(bc2), con=bc_output)
