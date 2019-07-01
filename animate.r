# 3D animated version of Ed Hawkins' warming stripes using rayshader
# Philip Kraaijenbrink
# 20190701

library(tidyverse)
library(rgl)
library(rayshader)     # devtools::install_github('tylermorganwall/rayshader')
library(pkrf)          # devtools::install_github('kraaijenbrink/pkrf')


# Settings ------------------------------------------------------------------------------------

exproot <- './exports/stripe_frames'                  # root output dir for animation frames

# animation settings
fpy     <- 5                                          # frames per year
riseper <- 12                                         # width of the frontal wave (years)
years   <- 1901:2018                                  # years of data
maxw    <- 4.5                                        # width of plot in inches
asprat  <- 9/16                                       # aspect ratio of plot
anitype <- '3d'                                       # export '2d' or '3d' animation?

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

# plot defined angle vectors
ggplot(tibble(Frame=frames,Theta=thetas,Phi=phis,Azimuth=sangles,Elevation=eangles) %>% gather('Type','Angle',2:5)) +
  geom_path(aes(Frame,Angle,color=Type),size=1) + facet_wrap(~Type, scales='free') + guides(color='none')

# debug export tweaks
# frames <- frames[frames %% 30 == 1]                  # run only 1 in 30 frames
# frames <- c(1,as.integer(n*0.5),n)                   # run only three frames in total


# Prep data -----------------------------------------------------------------------------------

daturl <- 'https://projects.knmi.nl/klimatologie/onderzoeksgegevens/homogeen_260/tg_hom_mnd260.txt'
tmon    <- read_table(daturl, skip=28, col_names=F) %>%
  rename(station=1,date=2,tas=3) %>%
  mutate(year=as.numeric(str_sub(date,0,4))) %>%
  filter(year<=2018) %>%
  group_by(year) %>%
  summarise(tas=mean(tas)) %>% ungroup() %>%
  mutate(reltas=tas-mean(tas)) %>%
  mutate(a=1)


# Prep plot -----------------------------------------------------------------------------------

p <- ggplot(tmon) +
  geom_tile(aes(year,1, fill=reltas, alpha=a)) +
  scale_alpha_continuous(range=c(0,1),limits=c(0,1)) +
  scale_fill_gradient2(low=scales::muted('darkblue'),high=scales::muted('tomato4'),
                       limits=c(-2,2), oob=scales::squish, name='°C') +
  guides(fill  = guide_colorbar(label.position='left',ticks.colour='black'),
         alpha = 'none') +
  labs(x        = NULL,
       y        = NULL,
       title    = 'Annual temperature anomalies',
       subtitle = 'De Bilt, The Netherlands',
       caption  = 'Data source: KNMI                                                                                             @philipkraai') +
  scale_y_continuous(breaks=c(-100,100)) +
  scale_x_continuous(breaks=seq(1900,2020,20)) +
  coord_cartesian(expand=F, xlim=c(1900.5,2018.5)) +
  theme_mh_minimal(base_size=10, grid=T) +
  theme(legend.position      = 'left',
        legend.justification = c(0,0.5),
        legend.key.height    = unit(0.47,'cm'),
        legend.title         = element_text(margin=margin(b=6)),
        legend.margin        = margin(r=18),
        plot.margin          = margin(6,20,12,12),
        plot.subtitle        = element_text(margin=margin(0,6,30,0),size=rel(0.8)),
        plot.title           = element_text(size=rel(0.8)),
        plot.caption         = element_text(margin=margin(9,0,0,0), hjust=0, size=rel(0.6), lineheight=1),
        axis.text.y          = element_blank(),
        axis.text.x          = element_text(size=rel(1.0)),
        axis.ticks           = element_line(colour=pkPal()[1], lineend='square'),
        panel.background     = element_rect(fill='gray94',colour=NA),
        panel.grid.minor     = element_blank(),
        panel.grid.major     = element_line(colour='white', lineend='butt'))


# Animate -------------------------------------------------------------------------------------

if (anitype=='3d'){rgl.open()}
for (f in frames){

  # get filename of export frame
  fn       <- file.path(exproot,sprintf('%04d.png',f))

  # get tibble with modulation
  i        <- (f-(riseper+1)*fpy)/fpy
  modframe <- tibble(year=years-min(years),
                     zmod=cos((year-i)*(2*pi/(2*riseper)))*0.5+0.5) %>%
    mutate(zmod=ifelse(year>(i+riseper),0,ifelse(year<i,1,zmod)))

  # apply modulation to data and update plot
  plotdat  <- tmon %>% mutate(reltas=reltas*modframe$zmod, a=modframe$zmod)
  curplot  <- p %+% plotdat

  # plot/render current frame
  if (anitype=='2d'){
    ggsave(fn,curplot, width=maxw, height=maxw*asprat)
  }
  if (anitype=='3d'){
    plot_gg(curplot, width=maxw, height=maxw*asprat, height_aes='fill', multicore=T, scale=150,
            raytrace=rays, sunangle=sangles[f], anglebreaks=seq(eangles[f]-2,eangles[f]+2,0.25),
            lineantialias=T, windowsize=c(2560,1440),
            theta=thetas[f], phi=phis[f], zoom=0.65, fov=42)
    render_snapshot(fn, clear=T)
  }
}
if (anitype=='3d'){rgl.close()}
