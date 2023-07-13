# script to render gganimate file as multiple images that will be rendered as a .gif or .mp4 at a later step

library(tidyverse)
library(ggplot2)
library(gganimate)

dir_animation <- "./animation_test/"
if (!dir.exists(dir_animation)) dir.create(dir_animation)


animated_plot <- 
  ggplot(mtcars, aes(x = wt, y = hp, colour = as.factor(cyl))) +
  geom_point() +
  transition_states(cyl, transition_length = 3, state_length = 1) +
  enter_fade() +
  exit_fade() +
  labs(title = "Cyl: {closest_state}")

## save directly as gif
anim_save(
  filename = file.path(dir_animation, "anim_test_1stage.gif"), 
  animation = animate(animated_plot)
  )

## save directly as mp4
anim_save(
  filename = file.path(dir_animation, "anim_test_1stage.mp4"), 
  animation = animate(animated_plot, 
                      renderer = av_renderer())
)

## save frames to animate in later step
## by default, FPS and nframes are set to 10 and 100, respectively, resulting in a 10sec video


## set FPS to be equivalent between the two:
fps = 30
duration = 10
nframes = fps * duration

animate(
  animated_plot,
  fps = fps,
  nframes = nframes,
  renderer = file_renderer(
    dir = file.path(dir_animation, "anim_test_2stage_frames"),
    prefix = "anim_test_2stage_frames",
    overwrite = TRUE
  )
)

## save frames as mp4
library(av)
av_encode_video(
  input = list.files(file.path(dir_animation, "anim_test_2stage_frames"), full.names = TRUE),
  framerate = 30,
  output = file.path(dir_animation, "anim_test_2stage.mp4")
)

## save frames as gif
library(gifski)
gifski(
  png_files = list.files(file.path(dir_animation, "anim_test_2stage_frames"), full.names = TRUE),
  gif_file = file.path(dir_animation, "anim_test_2stage.gif"),
  delay = duration / nframes
)
