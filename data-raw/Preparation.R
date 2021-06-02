# Tutorial preparation: adjusted data sets, visualizations, and analyses

# Load all required packages.
library(tidyverse) #for general data wrangling and visualization
library(knitr) #for tables
library(igraph) #for network definition
library(ggraph) #for network visualization
library(RColorBrewer) #for colour palettes
library(lme4) #for (multilevel) general models
library(goldfish) #for statistical analysis of dynamic networks

# Load the data sets as provided to the user.
load("data-raw/basicdata.RData")

# Standard colors to use
brewercolors <- brewer.pal( 5, name =  "Spectral")
brewercolors[3] <- "#ffff00"
names(brewercolors) <- c("Red", "Orange", "Yellow", "Green", "Blue")
#usage: colour = brewercolors["Blue"]

# Tutorial 1 ####

# Animated visualization of Break 1 ####

# - for every start of a playmate tie or utterance (with/out target), there is a
#   snapshot of the network (sequence entry)
# - the network shows the pupils (names, sex as shape, colour for ethnicity)
#   with their friendship ties (not varying), playmate ties, utterance loudness
#   (vertex size or size of circle behind vertex), and utterances towards peers
#   (positive versus negative)
# - network layout is fixed or interpolated between layouts calculated over all
#   ties and interactions within a selected time span, e.g., with an
#   interpolation for each event start within the selected time span:
#   - starting with layout based only on friendships
#   - (if possible) with different weights for different types of ties and
#   interactions (less weight for friendships?)
#   : layout_with_kk() gives larger distances for larger weights, the opposite of layout_with_fr()
#   - (if possible) with shorter distances for positive utterances and longer
#   distances for negative utterances (if not possible, ignore utterances in
#   layout?) ; weights between 0 and 1 work better than negative weights in layout_with_fr()
# - strategy: for a time point (in the sequence) select the (constant and)
#   dynamic attributes and ties  as well as the layout coordinates that happen/are
#   happening at this time point

# determine the sequence of event start times in Break 1 (531 time points)
timepoints <- pupils_dyn %>%
  #retain onset time
  select(onset) %>%
  #add to (onset in) pairs_dyn
  bind_rows(pairs_dyn) %>%
  #select Break 1
  filter(breakID == 1) %>%
  #keep one row for each time point
  group_by(onset) %>%
  summarise(.groups = "drop") %>%
  #add zero as start moment
  bind_rows(tibble(onset = 0)) %>%
  #rename onset
  rename(timepoint = onset) %>%
  #arrange (ensure that 0 becomes first entry)
  arrange(timepoint)

# determine starting positions of pupils based on friendship (not necessary: at time = 0 there are only friendships?)
# create data frame of edges
edges <- pairs_const %>%
  #keep only friendships and one friendship case per pair
  filter(from < to & friend == 1) %>%
  #select friendships
  select(from, to)
# be sure that the nodes are sorted by ID
pupils_const <- pupils_const %>%
  arrange(ID)
#create network (graph)
friends_net <- graph_from_data_frame(edges, directed = FALSE, vertices = pupils_const)
#calculate layout (positions of nodes)
set.seed(2584)
start_layout <- create_layout(friends_net, layout = 'kk') %>%
  #only keep coordinates
  select(x, y) %>%
  #save as matrix
  as.matrix()
# cleanup
rm(friends_net)

# loop for each time point
for (t in 1:nrow(timepoints)) {

  # create the network for time point t
  # create nodes
  nodes <- pupils_dyn %>%
    #select dynamic attributes (if any) for the current time point
    filter(
      breakID == 1 & #we only visualize network evolution in the first break
      onset <= timepoints$timepoint[t] & #dynamic attribute must start at or before the current timepoint...
      terminus >= timepoints$timepoint[t] #...and end at or after the current timepoint
    ) %>%
    #and add constant attributes (ensuring that all pupils are present)
    full_join(pupils_const, by = "ID") %>%
    #keep relevant variables
    select(ID, loudness, gameapp, label, sex:adhd) %>%
    # set missing values on loudness and gameapp to 0 (no sound, not gaming)
    mutate(
      loudness = ifelse(is.na(loudness), 0, loudness),
      gameapp = ifelse(is.na(gameapp), 0, gameapp)
    ) %>%
    #sort by vertex ID
    arrange(ID) %>%
    # ensure that there is only one case per pupil
    group_by(ID) %>%
    slice_head(n = 1) %>% #select the first row for a pupil (in case there are duplicates)
    ungroup()
  # create edges: one row for undirected edge
  edges <- pairs_dyn %>%
    #select dynamic relations (if any) for the current time point
    filter(
      breakID == 1 & #we only visualize network evolution in the first break
        onset <= timepoints$timepoint[t] & #dynamic attribute must start at or before the current timepoint...
        terminus >= timepoints$timepoint[t] #...and end at or after the current timepoint
    ) %>%
    #add constant ties (friendships) as additional rows
    bind_rows(pairs_const) %>%
    #keep relevant rows
    filter(
      (is.na(friend) | friend == 1) & #remove rows originally from pairs_const that are not friends (friend == 0)
      (((is.na(dyntie) | dyntie == "Playmate") & from < to ) | #for friends and playmates, only retain one case
      dyntie == "Utterance" ) #but keep all (directed) utterances
    ) %>%
    #set variable dyntie for friendships and change negative into character variable
    mutate(
      dyntie = ifelse(is.na(dyntie), "Friendship", dyntie),
      Valence = ifelse(negative == 1, "neg", "pos") #for plotting within Shiny, use colornames such as red and darkgreen
      ) %>%
    #select relevant variables
    select(from, to, dyntie, Valence)
  #create network (graph)
  temp_net <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
  #calculate layout (positions of nodes)
  temp_layout <- create_layout(temp_net, layout = "igraph", algorithm = 'kk', coords = start_layout, dim = 2) # start_layout) #
  #store as starting positions for next plot
  start_layout <- temp_layout %>%
    #only keep coordinates
    select(x, y) %>%
    #save as matrix
    as.matrix()

  # create plot
  ggraph(temp_layout) +
    geom_edge_link(
      aes(filter = (dyntie == "Friendship")),
      colour = "grey",
      alpha = 0.7,
      edge_width = 2.3
      ) +
    geom_edge_link(
      aes(filter = (dyntie == "Playmate")),
      colour = "black",
      edge_width = 0.7
      ) +
    geom_edge_fan(
      aes(
        filter = (dyntie == "Utterance"),
        #alpha = stat(index),
        colour = Valence
        ),
      show.legend = FALSE,
      arrow = arrow(angle = 30, length = unit(3, "mm"), type = "closed"),
      end_cap = circle(4, 'mm'),
      edge_width = 1.5
    ) +
    geom_node_point(
      aes(fill = ethnicity, shape = ifelse(sex == 1, "girl", "boy")),
      size = 6
    ) +
    geom_node_point(
      aes(
        filter = (loudness > 0),
        alpha = loudness
      ),
      show.legend = FALSE,
      shape = 1,
      colour = brewercolors["Blue"],
      stroke = 2,
      size = 14
    ) +
    geom_node_text(aes(label = label), nudge_y = 0.1, vjust = 0, size = 3) +
    geom_text(
      label = paste0("Time ",
                     format(trunc(timepoints$timepoint[t]), width = 2), ":",
                     formatC(round(60 * (timepoints$timepoint[t] %% 1)),
                            flag = "0", width=2)),
      x = max(temp_layout$x), y = max(temp_layout$y),
      hjust = 1, #aligned left
      vjust = 0, #aligned bottom
      size = 4
      ) +
    scale_edge_alpha("Edge direction", guide = "none") +
    scale_shape_manual(name = "Sex", values = c(22, 21)) +
    scale_fill_brewer(name = "Ethnicity", type = "div", palette = 1) +
    guides(fill = guide_legend(override.aes=list(shape=21))) + #bug fix
    scale_alpha(range = c(0.4, 0.8)) + #relative loudness, not absolute due to automatic rescaling
    scale_edge_color_manual(values = c(neg = brewercolors[["Red"]], pos = brewercolors[["Green"]])) +
    theme_void()

  #save plot to png file for quick rendering
  # note: Shiny needs seconds to render the ggraph/ggplot code for one time point.
  ggsave(paste0("inst/tutorials/Session1/images/breakdyn", t, ".png"),
         device = "png", width = 18, height = 12, units = "cm", dpi = 200)

}

