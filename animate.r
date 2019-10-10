# 3D animated version of Ed Hawkins' warming stripes using rayshader.
#
# Plot annual temperature anomalies for two series and include a line plot with
# CO2 concentration below
#
# Create GIF from sepearte frames in postprocessing using e.g. ImageMagick,
# or create a movie from the frames using FFMPEG or proper video editing software.
#
# Philip Kraaijenbrink
# 20191010

library(tidyverse)
library(rgl)
library(rayshader)     # devtools::install_github('tylermorganwall/rayshader')
library(pkrf)          # devtools::install_github('kraaijenbrink/pkrf')

rm(list=ls)


# Settings ------------------------------------------------------------------------------------

exproot <- 'f:/ani-frames'                            # root output dir for animation frames

# animation settings
fpy     <- 8                                          # frames per year
riseper <- 5                                          # width of the frontal wave (years)
years   <- 1975:2018                                  # years of data
maxw    <- 5                                          # width of plot in inches
asprat  <- 9/16                                       # aspect ratio of plot
anitype <- '3d'                                       # export '2d' or '3d' animation?
winsize <- c(1920,1080)

# derivatives
frames  <- 1:((length(years)+riseper*2)*fpy)
n       <- length(frames)

# raytracing settings
rays    <- T                                          # use raytracing?
sangles <- seq(-30,40,length.out=length(frames))      # vector with azimuths
eangles <- spline(x=c(1,n*0.5,n),                     # vector with elevations angles
                  y=c(10,45,15),
                  xout=frames)$y

# camera view angles
thetas  <- spline(x=c(1,n*0.25,n*0.5,n*0.75,n),       # vector with thetas
                  y=c(360,345,335,328,325),
                  xout=frames)$y
phis    <- spline(x=c(1,n*0.25,n*0.5,n*0.75,n),       # vector with phis
                  y=c(80,45,32,27,25),
                  xout=frames)$y

# plot defined angle vectors for reference
tibble(Frame=frames,Theta=thetas,Phi=phis,Azimuth=sangles,Elevation=eangles) %>%
  gather('Type','Angle',2:5) %>%
  ggplot() +
  geom_path(aes(Frame,Angle,color=Type),size=1) +
  facet_wrap(~Type, scales='free') +
  guides(color='none') + 
  theme_pk()

# debug export tweaks
# frames <- frames[frames %% 30 == 1]                  # run only 1 in 30 frames
# frames <- c(1,as.integer(n*0.5),n)                   # run only three frames in total



# Prep data -----------------------------------------------------------------------------------

refyrs <- 1:5    # calculate anomalies only with respect to first 5 years
tmon <- read_csv('./data/temp-and-co2.csv', skip=11) %>%
  filter(Year >= 1975, Year <= 2018) %>%
  
  # fill first four missing years of KTM with bias corrected global record
  mutate(T_ktm = ifelse(is.na(T_ktm), T_glob + (mean(T_ktm[1:10], na.rm=T) - mean(T_glob[1:10], na.rm=T)), T_ktm)) %>%
  
  # get temperature anomalies
  mutate(T_ktm_rel  = T_ktm-mean(T_ktm[refyrs], na.rm=T),
         T_rdam_rel = T_rdam-mean(T_rdam[refyrs], na.rm=T),
         T_glob_rel = T_glob-mean(T_glob[refyrs], na.rm=T)) %>%
  
  # add alpha modulation column
  mutate(a=1) %>%
  
  # force names lowercase
  setNames(tolower(names(.)))

# build table with interpolated co2 values for every animation frame
co2frame <- tibble(year=seq(years[1],tail(years,1),length.out=length(years)*fpy),
                   co2=approx(tmon$year, tmon$co2_glob, year)$y)
filler   <- co2frame[1:(fpy*riseper),]*NA
co2frame <- bind_rows(co2frame[rep(1,fpy*riseper),],
                      co2frame,
                      co2frame[rep(nrow(co2frame),fpy*riseper),])



# Prep plot -----------------------------------------------------------------------------------

ypos   <- c(1.5,2.5)    # y position for the two warming stripes strips
crange <- c(-0.9,0.9)   # rel range for CO2 line

p <- ggplot(tmon) +
  geom_vline(xintercept=seq(1975,2020,5), colour='white') +
  geom_tile(aes(year, ypos[2] , fill=t_ktm_rel, alpha=a)) +
  geom_tile(aes(year, ypos[1] , fill=t_glob_rel, alpha=a)) +
  scale_alpha_continuous(range=c(0,1),limits=c(0,1)) +
  scale_fill_gradientn(colours=pkRamp('rwb',12,rev=T)[c(3,6:11)], limits=c(-0.5,2.0), oob=scales::squish, name='°C') +
  guides(fill  = guide_colorbar(label.position='left',ticks.colour='black'),
         alpha = 'none') +
  labs(x        = NULL,
       y        = NULL,
       title    = expression(bold('Temperature and global CO'[2]~'concentration')),
       subtitle = expression(italic('T')~'relative to 1975–1980 mean'),
       caption  = 'Data source: ECMWF, KNMI, NASA & NOAA                                                               @philipkraai') +
  scale_y_continuous(breaks=c(0.9,1.5,2.5), labels=c('409 ppm','Global','Kathmandu')) +
  scale_x_continuous(breaks=seq(1975,2020,5)) +
  coord_cartesian(expand=F, xlim=c(1974.5,2018.5), ylim=c(-1,3)) +
  theme_mh_minimal(base_size=10, grid=T) +
  theme(legend.position      = 'right',
        legend.justification = c(0,0.15),
        legend.key.height    = unit(0.46,'cm'),
        legend.title         = element_text(margin=margin(b=6)),
        legend.margin        = margin(r=4,l=12),
        plot.margin          = margin(6,20,12,12),
        plot.subtitle        = element_text(margin=margin(0,6,30,0),size=rel(0.8)),
        plot.title           = element_text(size=rel(0.8)),
        plot.caption         = element_text(margin=margin(9,0,0,0), hjust=0, size=rel(0.6), lineheight=1),
        axis.text.y          = element_text(size=rel(1.0), hjust=1, vjust=0.5, face='bold'),
        axis.text.x          = element_text(size=rel(1.0)),
        axis.ticks           = element_line(colour=pkPal()[1], lineend='butt'),
        panel.background     = element_rect(fill='gray94',colour=NA),
        panel.grid.minor     = element_blank(),
        panel.grid.major     = element_blank()
        )




# Animate -------------------------------------------------------------------------------------

if (anitype=='3d'){rgl.open()}p
# for (f in frames){
for (f in 1:40){

  # get filename of export frame
  fn       <- file.path(exproot,sprintf('%04d.png',f))

  # get tibble with modulation
  i        <- (f-(riseper+1)*fpy)/fpy
  modframe <- tibble(year=years-min(years),
                     zmod=cos((year-i)*(2*pi/(2*riseper)))*0.5+0.5) %>%
    mutate(zmod=ifelse(year>(i+riseper),0,ifelse(year<i,1,zmod)))

  curco2 <- co2frame[f,]
  curco2_scl <- scales::rescale(curco2$co2, crange, from=range(tmon$co2_glob))

  
  # apply modulation to data 
  plotdat  <- tmon %>% mutate(t_ktm_rel=t_ktm_rel*modframe$zmod,
                              t_rdam_rel=t_rdam_rel*modframe$zmod,
                              t_glob_rel=t_glob_rel*modframe$zmod,
                              a=modframe$zmod)
  
  # update plot and add CO2 line and point for the current frame
  curplot  <- p %+% plotdat + 
    scale_y_continuous(breaks=c(curco2_scl,1.5,2.5),
                       labels=c(sprintf('%.0f ppm',curco2$co2), 'Global', 'Kathmandu')) +
    geom_line(aes(year, scales::rescale(co2, crange, from=range(tmon$co2_glob)), colour=co2),
              size=0.7, lineend='round', data=co2frame[1:f,]) + 
    geom_point(aes(year, scales::rescale(co2, crange, from=range(tmon$co2_glob)), colour=co2),
              size=2.0, data=co2frame[f,]) +
    scale_colour_gradientn(colours=pkRamp('inferno'), guide='none', limits=range(tmon$co2_glob))

  # plot/render current frame
  if (anitype=='2d'){
    ggsave(fn,curplot, width=maxw, height=maxw*asprat)
  }
  if (anitype=='3d'){
      plot_gg(curplot, width=maxw, height=maxw*asprat, height_aes='fill', multicore=T, scale=150,
              raytrace=rays, sunangle=sangles[f], anglebreaks=seq(eangles[f]-2,eangles[f]+2,1/3),
              lineantialias=T, windowsize=winsize, theta=thetas[f], phi=phis[f], zoom=0.65, fov=42)
    render_snapshot(fn, clear=T)
  }
}
if (anitype=='3d'){rgl.close()}
