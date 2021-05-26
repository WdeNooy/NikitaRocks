# Generating the data.

library(tidyverse)
library(randomNames)
library(igraph)
library(knitr)
library(ndtv)
library(goldfish)

# Constants ####
n_pupils = 26
n_breaks <- 10 #number of breaks the pupils were observed

# set random seed
set.seed(68946)
# Generate pupils of one class with time-constant properties. ####
pupils <- data_frame(
  ID = 1:n_pupils, #integer ID code
  present = rep(TRUE, n_pupils),
  onset = rep(0, n_pupils),
  terminus = rep(n_pupils * 30, n_pupils),
  sex = round(runif(n_pupils, min = 0, max = 1.2)),
  ethnicity = round(runif(n_pupils, min = 1.6, max = 6.2)),
  adhd = round(rlnorm(n_pupils), digits = 1)
) %>%
  # add appropriate names
  mutate(label = randomNames(gender = sex, ethnicity = ethnicity, which.names = "first")) %>%
  #reorder variables
  select(ID, label, present, onset, terminus, sex, ethnicity, adhd) %>%
  #change ethnicity into a character variable
  mutate(
    ethnicity = case_when(
      ethnicity == 2 ~ "A",
      ethnicity == 3 ~ "B",
      ethnicity == 4 ~ "C",
      ethnicity == 5 ~ "D",
      ethnicity == 6 ~ "E"
    )
  )

# Generate time-constant friendship relations among the pupils. ####
# generate a dyad for every pair of pupils
set.seed(68946)
friends <- pupils %>%
  # Carthesian or cross-join, generating all combinations of x and y
  full_join(pupils, by = character()) %>%
  # remove loops
  filter(label.x != label.y) %>%
  # calculate 'probability' of friendship choice
  mutate(
    score = 3 * (sex.x == sex.y) + # strong sex homophily
             1 * (ethnicity.x == ethnicity.y) + #weak ethnicity homophily
             sqrt(1 / (0.05 + abs(adhd.x - adhd.y))) + #adhd similarity
             rnorm(n = n_pupils * (n_pupils - 1), mean = 0, sd = 3),
    friend = ifelse( score > quantile(score, probs = c(0.65)), 1, 0)
  ) %>%
  #only keep friendship choices (arcs)
  filter(friend == 1)
#only keep reciprocated friendship choices
friends <- friends %>%
  semi_join(friends, by = c("label.x" = "label.y", "label.y" = "label.x")) %>%
  #drop loops
  filter(label.x != label.y) %>%
  #add constant weight
  mutate(friend = 1) %>%
  #adjust sender and recipient variable names
  select(from = ID.x, to = ID.y, friend) %>%
  #add friendship between isolates Carla and Zameel
  bind_rows(
    data_frame(
      from = c(9, 11),
      to = c(11, 9),
      friend = 1
      )
  )

# check friendship network with package igraph
# create graph
net_friends <- graph_from_data_frame(d = friends[friends$from < friends$to,], vertices = pupils[,1:6], directed = FALSE)
# plot graph with vertex color indicating pupil sex
colors <- c("tomato", "gold") # select two colors (numbered 1 and 2)
V(net_friends)$color <- colors[V(net_friends)$sex + 1] #assign colors to sex scores
l <- layout_with_fr(net_friends) #use Fruchterman-Ringold layout
plot(net_friends,
     edge.arrow.size=0.4,
     vertex.label.family="Helvetica",
     vertex.label.cex=0.6, #relative vertex size
     vertex.label.color="black",
     layout = l)
# table of vertex degree (number of network neighbors) in proportions: nice variation
kable( data_frame(
  degree = 0:(length(degree.distribution(net_friends)) - 1), #degree, starting with 0
  degree.distribution(net_friends)) #proportion of pupils with this degree
  )

# Create pairs_const ####
# add friendships and homophily/similarity variables to list of constant
# features of (directed or undirected) pairs
pairs_const <- pupils %>%
  # Carthesian or cross-join, generating all combinations of x and y
  full_join(pupils, by = character()) %>%
  # remove loops
  filter(ID.x != ID.y) %>%
  #add friendship variable
  left_join(friends, by = c("ID.x" = "from", "ID.y" = "to")) %>%
  #set missing friend score to 0 (no friendship) and add homophily indicators (0/1)
  mutate(
    simil_sex = ifelse(sex.x == sex.y, 1, 0),
    simil_ethn = ifelse(ethnicity.x == ethnicity.y, 1, 0),
    simil_adhd = sqrt(0.5 / (0.05 + abs(adhd.x - adhd.y))),
    friend = ifelse(is.na(friend) & is.na(friend), 0, 1)
    ) %>%
  #keep relevant variables
  select(from = ID.x, to = ID.y, friend:simil_adhd)
# cleanup
rm(friends, l)

# # Generate breaks ####
# # 5 days (numbered) with two breaks each (morning and afternoon).
# breaks <- data_frame(
#   breakID = 1:10,
#   day = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
#   period = rep(c(0, 1), 5) # 0 = morning break, 1 = afternoon break
# )

# Generate 'empty' dynamic dyadic data : ####
#   number of days since last played together at start of
#   observation period e.g., according to self-reports by pupils (asymmetric)
# generate (directed) list of pupil pairs
# set random seed
set.seed(68946)
pairs_dyn <- pupils %>%
  # Carthesian or cross-join, generating all combinations of x and y
  full_join(pupils, by = character()) %>%
  # keep and rename relevant variables
  select(from = ID.x, to = ID.y) %>%
  # remove loops
  filter(from != to) %>%
  # add situation just before start of 1st break
  mutate(
    breakID = 1, #start with first break
    onset = 0, #event start time (in minutes per break)
    terminus = 0, #event end time (in minutes per break)
    onset.censored = FALSE, #`ndtv` event start unobserved
    terminus.censored = FALSE, #`ndtv` event end unobserved (externally forced)
    playmate = NA_integer_, #pair are not playmates (0 = no, 1 = yes)
    loudness = NA_integer_, #loudness of utterance: NA = no utterance
    negative = NA_integer_, #utterance valence towards recipient: 1 = negative, 0 = neutral/positive, NA = no utterance
    smaht = NA_integer_ #utterance includes 'wicked smaht': (0 = no, 1 = yes, NA = no utterance); to = NA if utterance without recipient
    ) %>%
  # add friendships
  left_join(pairs_const, by = c("from", "to")) %>%
  #generate number of days since last played together
  mutate(
    score = -3 * friend + # strong effect of friendship
      -1 * simil_sex + # weak (additional) sex homophily
      -2 * simil_ethn + #moderate ethnicity homophily
      -0.2 * simil_adhd + #adhd similarity
      rnorm(n = nrow(pairs_const), mean = 0, sd = 2),
    lastplayed = round(1 - min(score) + score)
    ) %>%
  #remove helper variable score and fixed pair characteristics
  select(from:smaht, lastplayed)

# Generate pupil-specific aptitudes ####
# activity [0, 1], sociability [0, 1] and friendship dependence [0, 1] scores
# create temp data frame with pupil label and degree
deg <- data_frame(
  ID = as.numeric(names(igraph::degree(net_friends))),
  degree = unname(igraph::degree(net_friends))
  )
set.seed(68946)
vertices_fixed_simulation <- pupils %>%
  #add pupil's degree in friendship network as popularity indicator
  left_join(deg, by="ID") %>%
  #calculate pupil-specific scores
  mutate(
    #activity as probability of changing playmates per minute (1 to 6 times per 30 min. break)
    activity_play = runif(n_pupils, 1/30, 6/30),
    #sociability weakly related to friendship popularity
    sociability = degree +
      rnorm(n_pupils, mean = 0, sd = 1.5 * sd(degree) ),
    #rescale to [0.1, 0.9] (always some, never perfect sociability)
    sociability = 0.1 + (sociability - min(sociability)) / (1.25*(max(sociability) - min(sociability))),
    # friendship dependence, totally random
    frienddep = runif(n_pupils, 0, 1),
    #loudness level (weakly correlated with adhd level and sex)
    loudnesslevel = -1.0 * sex + 0.3 * adhd + rnorm(n_pupils, mean = 0, sd = 0.5),
    #rescale to [0.1, 0.9] (always some, never perfect sociability)
    loudnesslevel = 0.1 + (loudnesslevel - min(loudnesslevel)) / (1.25*(max(loudnesslevel) - min(loudnesslevel))),
    #negativity level (weakly neg correlated with number of friends)
    negativity = 4.5 * sqrt(degree) +
      rnorm(n_pupils, mean = 0, sd = 1.75 * sd(degree) ),
    #rescale to [0.1, 0.9] (always some, never perfect negativity)
    negativity = 0.1 + (negativity - min(negativity)) / (1.25*(max(negativity) - min(negativity))),
    #game adoption level (weakly correlated with sex)
    adoptionlevel = -2.0 * sex + rnorm(n_pupils, mean = 0, sd = 0.5),
    #rescale to [0.1, 0.9] (always some, never perfectly open for adoption)
    adoptionlevel = 0.1 + (adoptionlevel - min(adoptionlevel)) / (1.25*(max(adoptionlevel) - min(adoptionlevel)))
  ) %>%
  #keep relevant variables
  select(ID, label, degree:adoptionlevel)
#cleanup
rm(deg)

# Generate time-stamped playmate ties during each break (which lasts for 30 minutes). ####
# For estimation with `goldfish::`, playmate ties are assumed to be dyadic,
#  with a strong tendency for triadic closure among playmates (forming cliques).
# For each time-point, select a random pupil and decide:
#  a. whether the pupil changes its relations or not:
#     - depends on activity_play score
#  b. if relations are changed, whether a current playmate is dumped (only if
#     there is a playmate) or a new playmate is selected:
#     - depends on sociability score: more sociable, higher probability of adding a new playmate
#  c1. if a playmate is dumped, which one:
#     the current playmate who scores lowest on:
#     - pupil's friendship dependence score (interaction with friendship tie)
#     - friendship tie with pupil (positive)
#     - ethnicity homophily
#     - popularity (degree) of pupil (positive)
#     - pupil's sociability score (interaction with pupil's popularity)
#     - (time) number of breaks since last time playing together (negative - positive - negative)
#     - number of shared playmates (closure)
#  c2a. if a playmate is added, which one:
#     the eligible playmate who scores highest on the indices under (c1)
#     note: can be playmate dropped at previous time point
#  c2b. does the selected candidate accept the pupil as playmate:
#     probability of candidate acceptance by intended playmate depends on (on average high prob):
#     - number of playmates (curvilinear: pos - neg - pos)
current_break = 1 #TBD: change into loop over breaks
set.seed(4322)
for (i in 1:(30*n_pupils)) {
  #for each time point (minute * number of pupils, so each pupil may try to act once each minute) do:

  #select one random pupil
  pupil_act <- round(runif(1, 0.51, 0.49 + n_pupils))
  #determine the current playmates of this pupil (unordered relation, so it suffices
  # to use only target pupils)
  pupil_playmates <- pairs_dyn %>%
    #select cases in current time with this pupil as playmate
    filter(
      from == pupil_act & #this pupils is involved in the event
      playmate == 1 & #event is playing together
      onset < i/n_pupils & #event started before current time point
      is.na(terminus) #event has not ended yet
    ) %>%
    #keep necessary variables
    select(from, to)
  #determine if this pupil is going to act
  # if a random proportion is smaller than its activity probability or, if not
  # playing yet, its activity probability plus a time bonus (higher activity at
  # break start)
  if (runif(1, 0, 1) < vertices_fixed_simulation[[pupil_act, "activity_play"]] |
      (nrow(pupil_playmates) == 0 & runif(1, 0, 1) < (vertices_fixed_simulation[[pupil_act, "activity_play"]] +
       round(dlnorm(c((i - 1) * 0.05), meanlog = 0), digits = 3))) |
       nrow(pupil_playmates) > 4 #necessary?
      ) {
    # create data frame of all alters (potential new playmates and playmates to be dropped)
    alters <- data_frame(ID = 1:n_pupils) %>%
      # eliminate this pupil from the array
      filter(ID != pupil_act) # %>%
    # assign score to all alters (either for selecting new playmate or dropping current paymate):
    # - pupil's sociability score (interaction with pupil's popularity): from vertices_...
    # - pupil's friendship dependence score (interaction with friendship tie): from vertices_...
    # - friendship tie with pupil (positive): from pairs_const
    # - ethnicity homophily: from pairs_const
    # - adhd similarity: from pairs_const
    # - (time) number of days since last time played together (negative - positive - negative): from pairs_dyn
    # - number of shared playmates (closure): from pairs_dyn
    # - popularity (number of current playmates) of candidate pupil (positive): from pairs_dyn
    # helper data frame: playmate ties among all pupils
    pupils_playmates <- pairs_dyn %>%
      #select cases of playmates in current time
      filter(
        playmate == 1 & #event is playing together
          onset < i/n_pupils & #event started before current time point
          is.na(terminus) #event has not ended yet
      ) %>%
      #keep unique alters for each pupil; rename alter to tertius
      group_by(from, tertius = to) %>%
      summarise(.groups="drop")
    # add other tie characteristics and calculate attractiveness score of all alters
    alters <- pupils_playmates %>%
      # count number of shared playmates (closure) and add to alters
      # join head of first to tail of second to get playmates at two steps
      full_join(pupils_playmates, by = c("tertius" = "from")) %>%
      # select if head equals the selected pupil
      filter(from == pupil_act) %>%
      # count number of times Step-2 playmate (tertius.y) appears = number of shared playmates (tail)
      count(tertius.y) %>%
      # add to eligible playmates
      right_join(alters, by = c("tertius.y" = "ID")) %>%
      # if no shared playmates, n == NA, so set new variable to 0
      mutate(n_shared_playmates = ifelse(is.na(n), 0, n)) %>%
      # remove original count variable
      select(ID = tertius.y, n_shared_playmates) %>%
      #add this pupil to create (ordered) pairs
      mutate(
        from = pupil_act,
        to = ID #rename potential playmate variable
      ) %>%
      #add candidate's current number of playmates
      left_join(pupils_playmates, by = c("to" = "from")) %>%
      #count candidate's number of current playmates (tertius)
      group_by(from, to, n_shared_playmates) %>%
      summarise(popularity = sum(!is.na(tertius)), .groups="keep") %>%
      #add number of days since last played together from pairs_dyn (use only necessary variables)
      left_join(pairs_dyn[,c("from", "to", "onset", "lastplayed")], by = c("from", "to")) %>%
      #retain last (is current) value of lastplayed
      filter(!is.na(lastplayed)) %>%
      arrange(from, to, desc(onset)) %>%
      group_by(from, to, n_shared_playmates, popularity) %>%
      summarise(lastplayed = first(lastplayed), .groups="drop") %>%
      #add this pupil's friendship dependency and sociability (constants!)
      left_join(vertices_fixed_simulation, by = c("from" = "ID")) %>%
      #add friendship tie variable (and other fixed pair variables: similarities)
      left_join(pairs_const, by = c("from", "to")) %>%
      #keep relevant variables
      select(-label, -degree) %>%
      #calculate attractiveness score of all eligible playmates
      #play around with the weights
      mutate( attractiveness =
          # - pupil's sociability score (interaction with pupil's popularity)
          # - popularity (number of current playmates) of candidate pupil (positive)
          scale(sociability, center = TRUE, scale = FALSE) * popularity +
          # - pupil's friendship dependence score (interaction with friendship tie)
          # - friendship tie with pupil (positive)
          5 * friend * frienddep +
          # - ethnicity homophily
          1.5 * simil_ethn +
          # - adhd similarity
          2 * simil_adhd +
          # - (time) number of days since last time played together (negative - positive - negative)
          -5 * I(lastplayed == 0) + -0.2 * lastplayed +
          # - number of shared playmates (closure)
          5 * n_shared_playmates
          #no random error! highest/lowest value is selected
      )
    # use sociability to determine if new playmate selected or playmate dumped
    if ( nrow(pupil_playmates) < 4 & #don't create new tie if ego already has 4+ playmates
         (
      nrow(pupil_playmates) == 0 | #no playmates or ...
      (max(alters$n_shared_playmates) > 0 & max(alters$n_shared_playmates) < 2) | #closure in small group
      runif(1, 0, 1) < vertices_fixed_simulation[[pupil_act, "sociability"]] #...sufficiently sociable
      )
      ){
      # select new playmate: alter with highest attractiveness, who is not already a playmate
      candidate_playmate <- alters %>%
        #remove current playmates
        anti_join(pupil_playmates, by=c("to" = "to")) %>%
        #select case with highest attractiveness
        slice_max(attractiveness, n = 1) %>%
        #add variables that appear in pairs_dyn (to which this case may be added)
        mutate(
          breakID = current_break,
          onset = i/n_pupils, #time as minute with decimal places
          terminus = NA, #event has just been started, not finished
          onset.censored = F, #event start observed
          terminus.censored = F, #event ending not (yet) forced
          playmate = 1, #start of (event =) playing together
          loudness = NA, #not an utterance
          negative = NA, #not an utterance
          smaht = NA, #not an utterance
          lastplayed = 0 #last played together: now (0 days before)
        )
      # add arc in opposite direction (playmate event is symmetrical)
      candidate_playmate <- candidate_playmate %>%
        #exchange head and tail
        rename(from = to, to = from) %>%
        #add to original row
        bind_rows(candidate_playmate)

      # get accepted as playmate
      # probability of candidate acceptance by intended playmate depends on (on average high prob):
      # - number of shared playmates: full acceptance for small group
      # - number of candidate's playmates (popularity curvilinear: high - low - high)
      if ( candidate_playmate$n_shared_playmates[1] < 4 & #do not add new tie in not so small play group
        (candidate_playmate$n_shared_playmates[1] > 0  | #always close small play group
        ((candidate_playmate$popularity[1] < 2) & (runif(1, 0, 1) < 0.8 )) | #high acceptance if no or 1 playmate
        ((candidate_playmate$popularity[1] >= 2) & (candidate_playmate$popularity[1] < 5) & (runif(1, 0, 1) < 0.2 )) #low acceptance
        )
      ) {
        # candidate accepts playing together
        pairs_dyn <- candidate_playmate %>%
          # remove variables that do not belong in pairs_dyn
          select(from, to, breakID:smaht, lastplayed) %>%
          #add rows
          bind_rows(pairs_dyn)
      }
    } else {
      # dump one playmate (there is at least one)
        # select playmate with lowest attractiveness
        dump_playmate <- alters %>%
          #keep current playmates (if any, result can be empty)
          semi_join(pupil_playmates, by=c("to" = "to")) %>%
          #select case with lowest attractiveness
          slice_min(attractiveness, n = 1)
        # dump this playmate: set terminus to current time
        pairs_dyn <- pairs_dyn %>%
          #set terminus time (terminus.censored is FALSE by default)
          mutate( terminus = ifelse(
            (from == pupil_act | to == pupil_act) & #this pupil is involved in the event as initial sender or target
              (to == dump_playmate$to[1] | from == dump_playmate$to[1]) & #
              playmate == 1 & #event is playing together
              onset < i/n_pupils & #event started before current time point (superfluous?)
              is.na(terminus), #event has not ended yet
            i/n_pupils, #tie to be closed (time = break minute with decimals)
            terminus #other ties, not to be changed
            )
          )
    } #end of add versus dump choice
  } #end of if loop: act?
} #end of loop: for each time point

#terminate all 'open' playmate events with terminus.censored set to TRUE (play
# ended by school bell / break end)
pairs_dyn <- pairs_dyn %>%
  mutate(
    terminus.censored = ifelse(is.na(terminus), TRUE, FALSE),
    terminus = ifelse(is.na(terminus), i/n_pupils, terminus)
    )

#go to next break, adjust current_break and add 2 days (weekend) to lastplayed for all pupils?

#cleanup
rm(alters, candidate_playmate, dump_playmate, pupil_playmates, pupils_playmates, i, pupil_act)

#visualize the network
#remove cases for onset == 0 (initial situation) from pairs_dyn
#library(ndtv)
edges <- pairs_dyn %>%
  filter(onset > 0 & playmate == 1 & from < to) %>%
  #make vertex IDs integers
  mutate(head = as.integer(from), tail = as.integer(to)) %>%
#and reorder for display with network & ndtv
  select(head, tail, breakID:lastplayed)

#visually inspect playmates network (in one break?)
#create base network (undirected)
break1 <- network(edges, vertex.attr = pupils, matrix.type = "edgelist",
                  directed = F, loops = F, multiple = F,
                  ignore.eval = F #otherwise, edge attributes are not accessible
                  )
break1 %v% "sexcolor" <- colors[break1 %v% "sex" + 1]
# plot(break1, vertex.col="sexcolor") #check
# NOTE: networkDynamic() cannot handle tibbles.
break1_dyn <- networkDynamic(
  base.net = break1,
  vertex.spells = as.data.frame(pupils[,c(4,5,1)]),
  edge.spells = as.data.frame(edges[,c(4,5,1,2)])
)
#static plot
plot(break1_dyn, vertex.col = break1 %v% "sexcolor", displaylabels = T)
# film strip
# note: enlarge Plots panel to avoid error 'margins too large'
# filmstrip(break1_dyn, displaylabels=F, mfrow=c(1, 5),
#           slice.par=list(start=1, end=30, interval=6,
#                          aggregate.dur=6, rule='any'))
# movie
# to speed up, calculate animated layout first
# note: with end=29, last frame shows change from 29 to 30
compute.animation(break1_dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=29, interval=1,
                                 aggregate.dur=1, rule='any'))

render.d3movie(break1_dyn, usearrows = F,
               displaylabels = F,
               vertex.col = break1 %v% "sexcolor",
               launchBrowser=T, filename="Break1-Network-Dynamic.html",
               render.par=list(tween.frames = 30, show.time = T),
               plot.par=list(mar=c(0,0,0,0)) )

# Analyze playmates ####

# Use goldfish, treating playmates as undirected tie.
# library(goldfish)

# Step 1. Define the nodeset (defineNodes)
# Data frame with fields label (chr) and present (logical) and optionally
# starting values for node characteristics (character, boolean, or numeric?).
# Select relevant variables from pupils.
playmates <- pupils %>%
  #sort by label to ensure the same order as in the matrix of friendships
  arrange(label) %>%
  #select relevant variables.
  select(label, present, sex, ethnicity, adhd)
playmates <- defineNodes(nodes = playmates)

# Step 1A. (optional). Define changes to node attributes (linkEvents).
# no time-varying node attributes used to simulate this data set

# Step 2. Define the network (defineNetwork)
# Creates empty matrix of actors.
# All outcome/predictor networks must be defined on this node set (matrix.
playNetwork <- defineNetwork(nodes = playmates, directed = FALSE)
# Create additional networks used as (dyadic) predictors
# For a static network, create a 0/1 matrix with rows and columns in the same order as in the list of nodes.
#  Note: The matrix columns must have the node labels as dimnames,
#  but the rows do not need to have dimnames.
friends_matrix <- pairs_const %>%
  #keep only the sender and receiver
  select(from, to, friend) %>%
  #add labels to IDs (so goldfish can match the matrix to the nodes)
  left_join(pupils[,c("ID", "label")], by = c("from" = "ID")) %>%
  left_join(pupils[,c("ID", "label")], by = c("to" = "ID")) %>%
  #label.x is sender, label.y is receiver: sort to match the nodes
  arrange(label.x, label.y) %>%
  #put 'to' values in columns
  pivot_wider(
    id_cols = label.x,
    names_from = label.y, #receiver provides the new columns (variables)
    values_from = friend, #variable friend indicates 0/1 friendship
    values_fill = 0, #replace missing values (e.g., on the diagonal) by 0
    names_sort = TRUE #ensure that the columns are in the correct order
    ) %>%
  #drop the from variable (row 1 has ID 1, etc.)
  select(-label.x) %>%
  #turn into a matrix
  as.matrix()
# create the network of friendships
friendsNetwork <- defineNetwork(matrix = friends_matrix, nodes = playmates, directed = FALSE)
# create (empty) network of time-varying lastplayed scores
lastplayedNetwork <- defineNetwork(nodes = playmates, directed = FALSE)

# Step 3. Define the eventlist of the network (linkEvents)
# The data frame changeEvents must contain (only?) variables: time, sender,
# receiver, and increment. Character fields sender and receiver must contain the
# labels that occur in data frame actors.
# Time can be POSIXct, integer, and numeric(with decimals?)
# Increment: create (1) or dissolve (-1) action (numeric).
# Instead of increment: replace, which updates the value of an attribute or tie (pair)?
# select and adjust the playmate events
# How do we specify right-censoring? Just don't add the terminating event?
# Note: onset and terminus must become different rows.
playing <- pairs_dyn %>%
  #select playmate cases and only one row for each pair (undirected data)
  filter(playmate == 1 & from < to) %>%
  #add labels to IDs (so goldfish can match the matrix to the nodes)
  left_join(pupils[,c("ID", "label")], by = c("from" = "ID")) %>%
  left_join(pupils[,c("ID", "label")], by = c("to" = "ID")) %>%
  #rename onset and terminus
  rename(
    onset.time = onset,
    terminus.time = terminus
  ) %>%
  #stack onset and terminus
  pivot_longer(
    cols = onset.time:terminus.censored,
    names_to = c("type", ".value"),
    names_sep = "\\.",
    values_drop_na = TRUE
  ) %>%
  #filter out censored times
  filter(!censored) %>%
  #recode increment into 1 for onset and -1 for terminus
  mutate(increment = ifelse(
    type == "onset", 1, -1
  )) %>%
  #select and rename relevant variables
  select(time, sender = label.x, receiver = label.y, increment) %>%
  #order by time
  arrange(time) %>%
  #a tibble throws a warning: Unknown or uninitialised column: `replace`.
  as.data.frame()
# create the network
playNetwork <- linkEvents(x = playNetwork, changeEvents = playing, nodes = playmates)
# create list of events for lastplayedNetwork
lastplaying <- pairs_dyn %>%
  #select only one row for each pair (undirected data)
  filter(from < to) %>%
  #add labels to IDs (so goldfish can match the matrix to the nodes)
  left_join(pupils[,c("ID", "label")], by = c("from" = "ID")) %>%
  left_join(pupils[,c("ID", "label")], by = c("to" = "ID")) %>%
  #label.x is sender, label.y is receiver
  #keep the sender and receiver, lastplayed, and onset start time of new lastplayed value
  #select and rename relevant variables
  select(time = onset, sender = label.x, receiver = label.y, replace = lastplayed) %>%
  #order by time
  arrange(time) %>%
  #a tibble throws a warning: Unknown or uninitialised column: `replace`.
  as.data.frame()
# create the network
lastplayedNetwork <- linkEvents(x = lastplayedNetwork, changeEvents = lastplaying, nodes = playmates)


# Step 4. Define the dependent events
playDependent <- defineDependentEvents(
  events = playing, #data frame containing the event list that should be considered as a dependent variable in models
  nodes = playmates, #data frame or a nodes.goldfish object containing the nodes used in the event list
  defaultNetwork = playNetwork #name of a goldfish network object
)

# Step 5. Estimate the model.
# A multinomial receiver choice model
model_play <- estimate(playDependent ~
                    tie(lastplayedNetwork, weighted = TRUE) + #days since last played
                    tie(friendsNetwork) + #friendship as dyadic predictor
                    indeg(playNetwork) + #alter's current popularity as a playmate
                    trans(playNetwork) + #closure of current playmate clusters
                    same(playmates$sex) + #sex homophily
                    same(playmates$ethnicity) + #ethnicity homophily
                    sim(playmates$adhd), #adhd similarity
                  model = "DyNAM", subModel = "choice_coordination")
summary(model_play)

# Milestone pairs_dyn
# copy pairs_dyn for backup
pairs_dyn_playmates <- pairs_dyn

#reset pairs_dyn to mileston
pairs_dyn <- pairs_dyn_playmates

# Generate utterances ####

# for relational events model(s) in continuous time
# processes:
# a. who is going to act when?
#    IVs:
#    - pupil-specific speech activity level = adhd level
#    - playmates group(?) activity: saying something because someone in your group says something
#    - reciprocity: higher probability of answering if you are addressed
# b. the action is directed towards whom? ; including to no one (general utterance)
#    IVs:
#    - higher no addressee probability if pupil has higher speech activity level (adhd level)
#    - higher no addressee probability for higher overall loudness level
#    - higher probability for current playmate
#    - higher probability for member of current playgroup (but not current playmate)
#    - reciprocity: sender of incoming utterance: high selection probability
#    - reciprocity by utterance negativity interaction: even higher probability
#    - higher probability for adhd similarity
#    - transitivity/balance
# c. what is the nature of the action? loudness, negativity (negative vs. neu/pos), using 'smaht' expression for predicting event (tie) value
#    IVs:
#    loudness:
#    - pupil-specific loudness level (weakly correlated with adhd level and sex)
#    - louder for higher overall loudness level
#    - loudness reciprocity (direct addressee)
#    - loudness reciprocity by sex interaction (girls adjust more to alter's loudness)
#    negativity (negative vs. neu/pos):
#    - pupil-specific negativity level (weakly neg correlated with number of friends; activity sender)
#    - (recipient popularity) more negativity for addressee with higher adhd score (?)
#    - friends: less negativity
#    - in/outgroup (playmate groups): more negativity between groups
#    - reciprocity for negativity
#    - transitivity/balance for negativity
#    using 'smaht' expression (subset of non-negative utterances): {NO PRIORITY}
#    - no addressee: higher probability of 'smaht'
#    -
#    -
set.seed(34349)
current_break = 1 #TBD: change into loop over breaks
for (i in 1:(30*n_pupils)) {
  #for each time point (minute * number of pupils, so each pupil may try to act once each minute) do:

  #select one random pupil
  pupil_act <- round(runif(1, 0.51, 0.49 + n_pupils))

  #determine the current playmates groups of all pupils
  pupils_playmates <- pairs_dyn %>%
    #select all playmate ties at current time
    filter(
        playmate == 1 & #event is playing together
        onset < i/n_pupils & #event started before current time point
        terminus >= i/n_pupils #event has not ended yet
    ) %>%
    #keep necessary variables
    select(from, to, playmate)
  # add loop for last vertex, so all vertices are always present in the network
  edges_play <- pupils_playmates %>%
    bind_rows(data_frame(from = n_pupils, to = n_pupils))
  # create network
  net_play <- network(edges_play, vertex.attr = pupils, matrix.type = "edgelist",
                        directed = T, loops = F, multiple = F, ignore.eval = T)
  # extract vector of component membership for all pupils
  # note: each isolate has its own component
  play_component <- component.dist(net_play)$membership
  #cleanup
  rm(edges_play, net_play)

  #determine if there was an utterance within the playgroup in the preceding n_pupils time points (2 minutes)
  play_group_utterances <- pairs_dyn  %>%
    #filter relevant utterances
    filter(
      from != pupil_act & #disregard selected pupil's own utterances
      !is.na(loudness) & #event must be utterance
      i < (onset + 2)*n_pupils & #utterance must have started less than a minute before the current time
      play_component[from] == play_component[pupil_act] #utterance must be by playgroup mate of selected pupil
    )

  #collect utterances addressing the selected pupil in the preceding n_pupils
  # time points (2 minutes = too short?) for reciprocity effect
  # only keep last utterance received per alter
  received_utterances <- pairs_dyn %>%
    #filter relevant utterances
    filter(
      to == pupil_act & #selected pupil was addressee
      !is.na(loudness) & loudness > 0 & #event must be utterance
      i < (onset + 3)*n_pupils, #utterance must have started less than X minutes before the current time
      breakID == current_break #utterance in this break
    ) %>%
    # only keep last per alter (if any) and select relevant variables
    arrange(onset) %>%
    group_by(from, to) %>%
    summarise(
      from = last(from), #switch sender and receiver for incoming utterances
      to = last(to), #switch sender and receiver for incoming utterances
      loudness = last(loudness),
      negative = last(negative),
      smaht = last(smaht),
      .groups = "drop"
    )

  # calculate proportion of balanced triples created by a negative utterance
  # = number of negative semipaths of length 2 between pupil_act and pupil_target
  #   over all semipaths of length 2 (if none: set to pupil-specific negativity level)
  #   in preceding 5 minutes (or shorter)
  # first create table of relevant utterances
  help_utterances  <- pairs_dyn %>%
    filter(
      onset < i/n_pupils & #event started before current time point
      onset > (i/n_pupils) - 5 & #but not longer ago than 5 minutes
      breakID == current_break & #only utterances in current break
      loudness > 0 & #it must be an utterance
      !is.na(to) # & #there must be an addressee
      # (from == pupil_act | from == pupil_target | to == pupil_act | to == pupil_target) & #only arcs incident with current ego and alter are relevant
      # !(from == pupil_act & to == pupil_target) &
      # !(to == pupil_act & from == pupil_target) #but we can't use direct arcs between the two
    ) %>%
    #recode negativity score (assuming no missing values)
    mutate(negative = ifelse(negative == 0, 1, -1)) %>%
    #keep relevant variables
    select(from, to, onset, negative)
  # create table with all (semi)paths of length 2 starting at pupil_act
  # symmetrize
  semipaths <- help_utterances %>%
    #exchange sender and receiver
    rename(to = from, from = to) %>%
    #add to original table (result is symmetric)
    bind_rows(help_utterances) %>%
    #keep last for each from-to pair (is also last for from, to pair)
    group_by(from, to) %>%
    slice_max(order_by = onset, n = 1) %>%
    ungroup()
  #construct semipaths of length 2 emanating from pupil_act; can be empty!
  semipaths <- semipaths %>%
    #join with itself: receiver of first arc is sender of second arc
    full_join(semipaths, by = c("to" = "from")) %>%
    #calculate valence of remaining semipaths
    mutate(sign = negative.x * negative.y) %>%
    #keep relevant variables
    select(from, to = to.y, sign) %>%
    #keep cases where pupil_act equals the (first) sender and is different rom (second) receiver
    #can be empty
    filter(from == pupil_act & to != pupil_act) %>%
    #aggregate: proportion of semipaths with negative sign (requiring neg action to get balanced)
    group_by(from, to) %>%
    summarise(balance = mean(sign == -1, na.rm = T), .groups="drop")
  #cleanup
  rm(help_utterances)

  #determine if this pupil is going to act
  #    IVs:
  #    - pupil-specific speech activity level = adhd level
  #    - playmates group(?) activity: saying something because someone in your group said something
  #    - reciprocity: higher probability of answering if you are addressed
  #    - transitivity
  if (
    #adhd activity yields probabilities between 0.2 and 0.8 = 6 to 24 realized actions per 30 min break
    (2 + pupils$adhd[pupils$ID == pupil_act])/(max(pupils$adhd) + 4) > runif(1, 0, 1) |
    #utterance in group: 60% probability
    (nrow(play_group_utterances) > 0 & runif(1, 0, 1) < 0.6) |
    #reciprocity: 80% probability
    (nrow(received_utterances) > 0 & runif(1, 0, 1) < 0.8) |
    #transitivity: 90% probability
    (nrow(semipaths) > 0 & runif(1, 0, 1) < 0.9)
    ) {
    # this pupil is going to act, so:
    # b. the action is directed towards whom? ; including to no one (general utterance)
    if (
      # - higher no addressee probability if pupil has higher speech activity level (adhd level)
      pupils$adhd[pupils$ID == pupil_act]/max(pupils$adhd) > runif(1, 0.4, 1) |
      # - higher no addressee probability for higher loudness level in the playgroup
      ( nrow(play_group_utterances) > 0 & mean(play_group_utterances$loudness, na.rm = T) > runif(1, 0.5, 1) )
      ) {
      # the pupil is not speaking to someone (in particular)
      pupil_target <- NA
    } else {
      # the pupil is speaking to someone in particular, so select an alter
      #    - higher probability for current playmate
      #    - higher probability for member of current playgroup
      #    - reciprocity: sender of incoming utterance: high selection probability
      #    - reciprocity by utterance negativity interaction: even higher probability
      #    - higher probability for adhd similarity
      #    - add transitivity as reason to select a particular alter to speak to, otherwise balance will not work
      # create data frame of all alters (potential new playmates and playmates to be dropped)
      alters <- data_frame(from = pupil_act, to = 1:n_pupils) %>%
        # eliminate this pupil from the array
        filter(to != pupil_act) %>%
        # add simil_adhd
        left_join(pairs_const, by = c("from", "to")) %>%
        # add current playmate indicator
        left_join(pupils_playmates, by = c("from", "to")) %>%
        # determine if an alter addressed the selected pupil (and how)
        # note: switch from & to, so utterances received by the selected student are linked
        left_join(received_utterances, by = c("from" = "to", "to" = "from")) %>%
        # determine semipaths between selected pupil and alter (and their balance)
        left_join(semipaths, by = c("from", "to")) %>%
        #set NA to 0 or 0.5 (scale midpoint)
        mutate(
          playmate = ifelse(is.na(playmate), 0, playmate),
          # alter is in the current playgroup (component) of the selected pupil but not the actual playmate
          playgroup = ifelse(play_component[to] == play_component[pupil_act] & playmate != 1, 1, 0),
          loudness_recip = ifelse(is.na(loudness), 0, loudness),
          negative_recip = ifelse(is.na(negative), 0.5, negative),
          neg_balance = ifelse(is.na(balance), 0.5, balance),
          smaht_recip = ifelse(is.na(smaht), 0, smaht)
          ) %>%
        select(from:playmate, playgroup:smaht_recip) %>%
        #calculate attractiveness score of all eligible utterance addressees
        #play around with the weights
        mutate( attractiveness =
                  #    - higher probability for current playmate
                  2.5 * playmate +
                  #    - higher probability for member of current playgroup (but not current playmate)
                  2.0 * playgroup + #was 1.0
                  #    - reciprocity: sender of incoming utterance: high selection probability
                  4 * (loudness_recip > 0) +
                  #    - reciprocity by utterance negativity interaction: even higher probability
                  1.5 * (loudness_recip > 0) * (negative_recip != 0.5) +
                  #    - higher probability for adhd similarity
                  0.1 * simil_adhd +
                  #    - higher probability for
                  3 * (neg_balance != 0.5)
                #no random error! highest/lowest value is selected
        )
      #store the ID of the top scoring alter
      pupil_target <- alters %>%
        #select the top scoring alter
        slice_max(order_by = attractiveness, n = 1, with_ties = FALSE) %>%
        #pull out the ID number of alter
        pull(var = to)
    } #end of selecting addressee (if any) loop

    # set loudness (for regression intro and contagion of numerical variable)
    # normalized to 0 (no utterance, silent), 0.1 (whisper), 1.0 (cry out loud)
    #    - pupil-specific loudness level (weakly correlated with adhd level and sex)
    #    - louder for higher overall loudness level
    #    - loudness reciprocity (direct addressee)
    #    - loudness reciprocity by sex interaction (girls adjust more to alter's loudness)
    # move loudness level in the direction of loudness of incoming utterance and overall loudness level
    # note: loudness_recip is 0 if there is no incoming utterance
    if (is.na(pupil_target) | #no addressee
         alters$loudness_recip[alters$to == pupil_target] == 0) { #no utterance received from addressee
      #no incoming utterance from pupil_target, so loudness is combination of personal overall level
      #random weight for personal and overall (maximum) loudness level
      weight <- runif(1, 0.5, 0.75) #change min and max for more/less variability
      pupil_loudness <- weight*vertices_fixed_simulation$loudnesslevel[vertices_fixed_simulation$ID == pupil_act] +
        (1 - weight) * ifelse(max(play_group_utterances$loudness, na.rm = T) > 0, max(play_group_utterances$loudness, na.rm = T), 0) +
        rnorm(1, mean = 0, sd = 0.05)
      #negativity depends only on pupil-specific negativity level; 0/1 outcome!
      pupil_negativity <- ifelse(vertices_fixed_simulation$negativity[vertices_fixed_simulation$ID == pupil_act] > runif(1, 0, 1), 1, 0)
    } else{
      # incoming utterance, loudness is combination of personal, reciprocated, and overall level
      weight <- runif(1, 0.25, 0.5) #random weight for personal loudness level
      #random weight for alter's loudness level
      #if pupil is male, it's sex is 0, so max p is 1 - 1.5*weight; for females, max p is 1 - weight: larger
      weight1 <- runif(1, 0, 1 - (1 + 0.5*(1 - pupils$sex[pupils$ID == pupil_act]))*weight)
      pupil_loudness <- weight*vertices_fixed_simulation$loudnesslevel[vertices_fixed_simulation$ID == pupil_act] +
        weight1*alters$loudness_recip[alters$to == pupil_target] +
        (1 - weight - weight1) * ifelse(max(play_group_utterances$loudness, na.rm = T) > 0, max(play_group_utterances$loudness, na.rm = T), 0) +
        rnorm(1, mean = 0, sd = 0.05)
    }

    # set negativity score (for predicting tie sign from balance theory)
      # include balance
      # = number of negative semipaths of length 2 between pupil_act and pupil_target
      #   in preceding 5 minutes (or shorter)
      # calculate the pupil's negativity score
    if (is.na(pupil_target)) {
      #no addressee, so simulate negativity from pupil-specific negativity level
      pupil_negativity <- ifelse(
        ( # pupil-specific negativity level (weakly neg correlated with number of friends; activity sender)
          vertices_fixed_simulation$negativity[vertices_fixed_simulation$ID == pupil_act] +
          # random component
          rnorm(1, mean = 0, sd = 1.0)
         ) > 0.5, 1, 0)
    } else {
      pupil_negativity <- ifelse(
        #calculate mean of normalized [0,1] predictors
        ( # pupil-specific negativity level (weakly neg correlated with number of friends; activity sender)
          2 * vertices_fixed_simulation$negativity[vertices_fixed_simulation$ID == pupil_act] +
          # (recipient popularity) more negativity for addressee with lower adhd score
          0.4 * (1  - pupils$adhd[pupils$ID == pupil_target] / max(pupils$adhd)) +
          # friends: less negativity
          1.2 * (1 - alters$friend[alters$to == pupil_target]) +
          # in/outgroup (playmate groups): more negativity between groups
          0.8 * (1 - alters$playgroup[alters$to == pupil_target]) +
          # reciprocity for negativity (0 = last pos incoming message, 0.5 = incoming, 1 = last incoming message was neg)
          1.2 * alters$negative_recip[alters$to == pupil_target] +
          # transitivity/balance for negativity (proportion of balanced triples; 0.5 if no triples)
          4 * alters$neg_balance[alters$to == pupil_target] +
          # random component
          rnorm(1, mean = 0, sd = 1.5)
         ) / 10  > 0.5, 1, 0) #if mean proportion larger than 0.5 (experiment!), utterance is negative
    }

    ## finally, add a row & check (describe and model) the resulting network of utterances
    pairs_dyn <- pairs_dyn %>%
      bind_rows(
        data_frame(
          from = pupil_act,
          to = pupil_target, #addressee of the utterance (NA if none)
          breakID = current_break, #start with first break
          onset = i/n_pupils, #event start time (in minutes per break)
          terminus = i/n_pupils + runif(1, 0.1, 1), #event end time (onset plus uniform length between 0.1 and 1 minute)
          onset.censored = FALSE, #`ndtv` event start unobserved
          terminus.censored = FALSE, #`ndtv` event end unobserved (externally forced)
          playmate = NA_integer_, #event is start/end of playing together? (0 = no, 1 = yes)
          loudness = pupil_loudness, #loudness of utterance: NA or 0 = no utterance
          negative = pupil_negativity, #utterance valence towards recipient: 1 = negative, 0 = neutral/positive, NA = no utterance
          smaht = NA_integer_, #utterance includes 'wicked smaht': (0 = no, 1 = yes, NA = no utterance); target = nil if utterance without addressee
          #utterance rate predictors
          # adhd activity of selected pupil
          fromadhd = pupils$adhd[pupils$ID == pupil_act],
          # number of utterances in group: 60% probability
          groupspeech = nrow(play_group_utterances),
          # number of utterances received, being addressed (reciprocity): 90% probability
          addressed = nrow(received_utterances),
          #utterance no addressee predictors
          # adhd activity of selected pupil (see above)
          # loudness level in the playgroup (see below: grouploudness)
          #utterance addressee predictors
          currentplaymates = ifelse(is.na(pupil_target), NA, alters$playmate[alters$to == pupil_target]),
          #loudness predictors
          fromloudnesslevel = vertices_fixed_simulation$loudnesslevel[vertices_fixed_simulation$ID == pupil_act],
          reciploudness = ifelse(!is.na(pupil_target) && alters$loudness_recip[alters$to == pupil_target] > 0, alters$loudness_recip[alters$to == pupil_target], 0),
          fromsex = pupils$sex[pupils$ID == pupil_act],
          grouploudness = ifelse(max(play_group_utterances$loudness, na.rm = T) > 0, max(play_group_utterances$loudness, na.rm = T), 0),
          #negativity predictors
          # pupil-specific negativity level (weakly neg correlated with number of friends; activity sender)
          fromnegativitylevel = vertices_fixed_simulation$negativity[vertices_fixed_simulation$ID == pupil_act],
          # (recipient popularity) more negativity for addressee with lower adhd score
          toadhd = ifelse(is.na(pupil_target), NA, (1  - pupils$adhd[pupils$ID == pupil_target] / max(pupils$adhd))),
          # friends: less negativity
          friends = ifelse(is.na(pupil_target), NA, alters$friend[alters$to == pupil_target]),
          # in/outgroup (playmate groups): more negativity between groups
          sameplaygroup = ifelse(is.na(pupil_target), NA, alters$playgroup[alters$to == pupil_target]),
          # reciprocity for negativity (0 = last pos incoming message or no incoming, 1 = last incoming message was neg)
          recipnegative = ifelse(is.na(pupil_target), NA, alters$negative_recip[alters$to == pupil_target]),
          # transitivity/balance for negativity (proportion of balanced triples; personal neg level if no triples)
          balance = ifelse(is.na(pupil_target), NA, alters$neg_balance[alters$to == pupil_target])
        )
      )
  } #end of yes/no act loop

} #end of time loop

# set utterance end times to 30 and terminus.censored to TRUE if end time is later than 30
pairs_dyn <- pairs_dyn %>%
  mutate(
    terminus = ifelse(terminus < 30, terminus, 30),
    terminus.censored = ifelse(terminus == 30, TRUE, FALSE)
  )

#cleanup
rm(weight, weight1, i, current_break, play_component,
   alters, play_group_utterances, received_utterances, semipaths,
   pupil_act, pupil_loudness, pupil_target, pupils_playmates)

# Analyze addressee selection ####
#    - higher probability for current playmate [from playmates network]
#    - higher probability for member of current playgroup (but not current playmate) [3 cases, ignore]
#    - reciprocity: sender of incoming utterance: high selection probability [from dependent network]
#    - reciprocity by utterance negativity interaction: even higher probability
#    - higher probability for adhd similarity [from fixed node attribute]
#describe frequencies of utterances
speech_dyn <- pairs_dyn %>% filter(loudness > 0)
hist(count(speech_dyn,from)$n)
range(count(speech_dyn,from)$n) #about 15 to 35 utterances per pupil per break
hist(speech_dyn$onset) #quite uniform distribution over time (except first few minutes)
speech_dyn %>% #many very short gaps (< 2 minutes), few very long gaps (about 9 minutes) = OK (one pupil remains silent for about 9 minutes)
  #calculate gap between successive utterances by a pupil
  arrange(from, onset) %>%
  group_by(from) %>%
  mutate(gap = onset - lag(onset)) %>%
  filter(!is.na(gap)) %>%
  ggplot(aes(x = gap)) +
  geom_histogram(bins = 18)
hist(speech_dyn$terminus - speech_dyn$onset) #as intended, uniform between 0.1 and 1.0 minute
range(speech_dyn$terminus - speech_dyn$onset)
#utterance rate predictors
hist(speech_dyn$fromadhd) # adhd activity of selected pupil: distributed as among pupils
hist(speech_dyn$groupspeech)
range(speech_dyn$groupspeech) # 0-20 preceding utterances in playgroup
hist(speech_dyn$addressed) # 0 - 4 utterances received in preceding 3 minutes (reciprocity)
mean(speech_dyn$addressed > 0) # 70% of utterances have preceding 'incoming' utterances, so reciprocity is possible
#addressee selection
mean(is.na(speech_dyn$to)) #26 percent of utterances have no addressee
mean(speech_dyn$currentplaymates[!is.na(speech_dyn$to)], na.rm = T) #52% of 'addressed' utterances between playmates
mean(speech_dyn$sameplaygroup[!is.na(speech_dyn$to)], na.rm = T) #2% of 'addressed' utterances to play group (except playmate)
mean(speech_dyn$reciploudness[!is.na(speech_dyn$to)] > 0, na.rm = T) #72% of 'addressed' utterances reciprocate an utterance received in the preceding 3 minutes
#goldfish model: who speaks to whom
# Step 1. Define the nodeset (defineNodes)
# Data frame with fields label (chr) and present (logical) and optionally
# starting values for node characteristics (character, boolean, or numeric?).
# use previously created node set playmates
# Select relevant variables from pupils.
# Step 1A. (optional). Define changes to node attributes (linkEvents).
# no time-varying node attributes used to simulate this data set

# Step 2. Define the network(s) (defineNetwork)
# Creates empty matrix of actors.
# All outcome/predictor networks must be defined on the node set (matrix) created in Step 1.
speechNetwork <- defineNetwork(nodes = playmates, directed = TRUE)
# Create additional networks used as (dyadic) predictors
# Use the static network of friendships created earlier: friendsNetwork
# Use the dynamic network of current playmates created earlier: playNetwork
# Never mind the current playgroup members that are not current playmates: only 3 cases

# Step 3. Define the eventlist of the network (linkEvents)
# The data frame changeEvents must contain (only?) variables: time, sender,
# receiver, and increment. Character fields sender and receiver must contain the
# labels that occur in data frame actors.
# Time can be POSIXct, integer, and numeric(with decimals?)
# Increment: create (1) or dissolve (-1) action (numeric).
# Instead of increment: replace, which updates the value of an attribute or tie (pair)?
# select and adjust the playmate events
# How do we specify right-censoring? Just don't add the terminating event?
# Note: onset and terminus must become different rows.
# speech events (utterances)
speech <- pairs_dyn %>%
  #select utterances with an addressee (and all rows, directed data)
  filter(loudness > 0 & !is.na(to)) %>%
  #add labels to IDs (so goldfish can match the matrix to the nodes)
  left_join(pupils[,c("ID", "label")], by = c("from" = "ID")) %>%
  left_join(pupils[,c("ID", "label")], by = c("to" = "ID")) %>%
  #rename onset and terminus
  rename(
    onset.time = onset,
    terminus.time = terminus
  ) %>%
  #stack onset and terminus
  pivot_longer(
    cols = onset.time:terminus.censored,
    names_to = c("type", ".value"),
    names_sep = "\\.",
    values_drop_na = TRUE
  ) %>%
  # #filter out censored times (why?)
  # filter(!censored) %>%
  #recode increment into 1 for onset and -1 for terminus
  mutate(increment = ifelse(
    type == "onset", 1, -1
  )) %>%
  #select and rename relevant variables
  select(time, sender = label.x, receiver = label.y, increment) %>%
  #order by time
  arrange(time) %>%
  #a tibble throws a warning: Unknown or uninitialised column: `replace`.
  as.data.frame()
# create the network
speechNetwork <- linkEvents(x = speechNetwork, changeEvents = speech, nodes = playmates)

# Step 4. Define the dependent events
speechDependent <- defineDependentEvents(
  events = speech, #data frame containing the event list that should be considered as a dependent variable in models
  nodes = playmates, #data frame or a nodes.goldfish object containing the nodes used in the event list
  defaultNetwork = speechNetwork #name of a goldfish network object
)

# Step 5. Estimate the model.
# A multinomial receiver choice model
model_speech <- estimate(speechDependent ~
                         tie(friendsNetwork) + #friendship as dyadic predictor
                         tie(playNetwork) + #currently playing together as dyadic predictor
                         recip(speechNetwork) + #reciprocity
                         trans(speechNetwork) + #transitivity in choice of speech target
                         same(playmates$sex) + #sex homophily
                         same(playmates$ethnicity) + #ethnicity homophily
                         sim(playmates$adhd), #adhd similarity
                       model = "DyNAM", subModel = "choice")
summary(model_speech)
# A rate model
model_speech_rate <- estimate(speechDependent ~
                           ego(playmates$sex) + #sex homophily
                           ego(playmates$adhd), #adhd similarity
                         model = "DyNAM", subModel = "rate")
summary(model_speech_rate)


# Analyze loudness of utterances ####
#    - pupil-specific loudness level (weakly correlated with adhd level and sex)
#    - louder for higher overall loudness level
#    - loudness reciprocity (direct addressee)
#    - loudness reciprocity by sex interaction (girls adjust more to alter's loudness)
#description
hist(speech_dyn$loudness) #skewed, few whispers
hist(speech_dyn$reciploudness)
mean(speech_dyn$reciploudness == 0) #47% of utterances have 0 for incoming utterance loudness (including the 28% without addressee)
speech_dyn %>% #average loudness increases slightly in first 10 minutes (and last few minutes)
  ggplot(aes(x = onset, y = loudness)) +
  geom_point(alpha = 0.5) +
  geom_smooth()
# average loudness by sex and adhd (grouped, time-constant predictors): girls are less loud (more adhd is more loud)
speech_dyn %>%
  group_by(from, fromadhd, fromsex) %>%
  summarise(avg_loudness = mean(loudness, na.rm = T)) %>%
  lm(avg_loudness ~ fromadhd + fromsex, data = .) %>%
  summary()
# loudness by sex and adhd (time-constant) and loudness levels (time-varying) (ungrouped)
summary(lm(loudness ~ fromadhd + fromsex + grouploudness, data = speech_dyn))
# with individual level loudness level
summary(lm(loudness ~ fromadhd + fromsex + fromloudnesslevel + grouploudness, data = speech_dyn))
# without individual loudness level but with random intercepts for pupils
library(lme4)
model_loudness_ml <- lmer(
  loudness ~ fromadhd + reciploudness*fromsex + grouploudness + (1 | from),
  data = speech_dyn
)
summary(model_loudness_ml)
confint(model_loudness_ml)
#plot(model_loudness_ml) #fitted versus residuals
#compare estimated random effects to loudnesslevel used in simulation (quite different!; no matching order?)
as.data.frame(ranef(model_loudness_ml)) %>%
  mutate(ID = as.numeric(grp)) %>%
  left_join(vertices_fixed_simulation, by = "ID") %>%
  ggplot(aes(x = loudnesslevel, y = condval)) +
    geom_point()

# Analyze negativity of utterances ####
#    - pupil-specific negativity level (weakly neg correlated with number of friends; activity sender)
#    - (recipient popularity) more negativity for addressee with higher adhd score (?)
#    - friends: less negativity
#    - in/outgroup (playmate groups): more negativity between groups
#    - reciprocity for negativity
#    - transitivity/balance for negativity
#description
mean(speech_dyn$negative == 1, na.rm = T) #53% of utterances are negative
hist(speech_dyn$recipnegative) #0 (only positive received), 0.5 (including no incoming) or 1 (only negative received)
hist(speech_dyn$balance) #scores are 0 (balance if positive), 0.5 (no semipaths) and 1 (balance if negative)
mean((speech_dyn$balance) == 0.5, na.rm = T) #39% of cases have 0.5 as balance score
#(cross-nested) multilevel logistic regression model
# prepare data
negative_data <- pairs_dyn %>%
  #select the utterances with an addressee
  filter(loudness > 0 & !is.na(to))
#glmer(): random intercepts, no pupil-level predictors
model_negative1 <- glmer(
  negative ~ toadhd + friends + sameplaygroup + recipnegative + balance + (1 | from),
  data = negative_data,
  family = binomial(link = "logit")
)
summary(model_negative1)
#glmer(): random intercepts, with pupil-level predictors
model_negative2 <- glmer(
  negative ~ toadhd + friends + sameplaygroup + recipnegative + balance + fromadhd + fromsex + (1 | from),
  data = negative_data,
  family = binomial(link = "logit")
)
summary(model_negative2)

# Milestone pairs_dyn
# copy pairs_dyn for backup
pairs_dyn_speech <- pairs_dyn

#save workspace
save.image("playmates_speech.RData")

#reset pairs_dyn to milestone
pairs_dyn <- pairs_dyn_speech

# Generate more breaks for gameapp diffusion ####
# Assuming that pupils can only install the app between breaks, e.g., at home with consent from parents.

# Step 1: add gameapp playing to one pupil in break 1
# pupil 18 has several play mates in break 1, who are exposed to the app
# this pupil is playing with the app nearly the entire break
# create table for time-varying characteristics of pupils
# with one record, containing playing with the gameapp in the first break
pupils_dyn <- data_frame(
  ID = 18, #selected pupil
  breakID = 1, #first break
  onset = 1.4, #start of activity
  onset.censored = FALSE, #voluntary start
  terminus = 30, #end time
  terminus.censored = TRUE, #forced end due to end of break
  gameapp = 1 #playing with new gameapp
)
# In addition, start new table to contain only playmate ties over all breaks
playmates_dyn <- pairs_dyn %>%
  #select only playmate ties
  filter(playmate == 1) %>%
  #select relevant variables
  # we don't need the playmate indicator variable
  select(from:terminus.censored)
# And create adopters start set: all pupils who have adopted before the current break
#   with the breakID before which they adopted
adopters <- pupils_dyn %>%
  #select the gameplay rows for all preceding breaks
  filter(gameapp == 1 & breakID < i) %>%
  #keep one row for each pupil who played the app with the first breakID
  group_by(ID) %>%
  summarise(breakAdopt = min(breakID))

# Step 2: generate breaks 2 and more (until most pupils have/use the app?)
# only generate playmates and gameapp use
set.seed(5482)
for (j in 2:n_breaks) {
  #for the second break to the Xth break, generate playmates and gameapp use

  # Step A. simulate gameapp purchase/installation after previous break
  # - positive effect of exposure in preceding break
  # - pupil-specific openness for new games (adoptionlevel, = desire to use gameapps?)
  # - sex effect: boys are more into the gameapp
  # create the risk set: all pupils who haven't played the game (as proxy for haven't installed)
  riskset <- pupils %>% #note: riskset will be expanded below
    #now select all pupils who did NOT play the app
    anti_join(adopters, by = "ID")
  # for non-adopters (risk set), calculate exposure as time (minutes) played with someone using the gameapp in the preceding break
  adoption <- playmates_dyn %>% #playmate ties are symmetrical, so we may use 'to' as 'from's playmate
    #select the previous break
    filter(breakID == j - 1) %>%
    #add game use of 'from' from pupils_dyn
    left_join(pupils_dyn, by = c("to" = "ID", "breakID" = "breakID")) %>%
    #keep playmate ties that overlap with from's gameapp use
    filter(
      onset.x < terminus.y & #playing must start before end of from's gaming
      terminus.x > onset.y   #and playing must end after start of from's gaming
    ) %>%
    #calculate shared time (playing while from is gaming)
    mutate(duration =
             ifelse(terminus.x < terminus.y, terminus.x, terminus.y) - #smallest end time minus
             ifelse(onset.x > onset.y, onset.x, onset.y)) %>% #largest starting time
    #sum exposure time for each 'from'
    group_by(from) %>%
    summarise(exposure = sum(duration, na.rm = T), .groups="drop") %>%
    #add to risk set
    right_join(riskset, by = c("from" = "ID")) %>%
    #set all exposure missings to zero (no exposure)
    mutate(exposure = ifelse(is.na(exposure), 0, exposure)) %>%
    #add pupil-specific characteristics
    left_join(vertices_fixed_simulation, by=c("from" = "ID")) %>%
    #simulate gameapp purchase/installation after previous break
    # - positive effect of exposure in preceding break
    # - pupil-specific openness for new games (adoptionlevel, = desire to use gameapps?)
    # - sex effect: boys are more into the gameapp (interaciton with adoptionlevel)
    mutate(adoption = ifelse(
      runif(1, min = 0, max = 1) < 0.6*exposure/30 | #replace 30 by max(exposure)? exposure can be over 30
      runif(1, min = 0, max = 1) < 0.5*(adoptionlevel - sex*0.2*adoptionlevel), #change weight for slower/faster general adoption
      1, #adopt
      0 #do not adopt
      )
    )
  #add new adopters to adopters set
  adopters <- adoption %>%
    #select adopters
    filter(adoption == 1) %>%
    #set adoption break to current break
    mutate(breakAdopt = j) %>%
    #select and rename variables
    select(ID = from, breakAdopt) %>%
    #add to adopters table
    bind_rows(adopters)
  #remove (new) adopters from riskset
  riskset <- riskset %>%
    anti_join(adopters, by = "ID")

  # Step B. create playmate ties and gameapp use in the current break
  # with tendency for players to establish ties and for ties to continue playing
  # new gameapp adopters start playing nearly at the break start
  # declining interest in playing when having the gameapp for a longer time
  #create playmate ties and gameplaying start and end
  for (i in 1:(30*n_pupils)) {
    #for each time point (minute * number of pupils, so each pupil may try to act once each minute) do:

    #determine who is playing the game
    gameplaying <- pupils_dyn %>%
      #select if current break, onset before current time and terminus NA (not reached)
      filter(breakID == j & onset <= i/n_pupils & is.na(terminus)) %>%
      #only keep ID and gameapp
      select(ID, gameapp)

    #select one random pupil
    pupil_act <- round(runif(1, 0.51, 0.49 + n_pupils))
    #determine the current playmates of this pupil
    # (unordered relation, so it suffices to use only target pupils)
    pupil_playmates <- pairs_dyn %>%
      #select cases in current time with this pupil as playmate
      filter(
        from == pupil_act & #this pupils is involved in the event
          playmate == 1 & #event is playing together
          onset < i/n_pupils & #event started before current time point
          is.na(terminus) #event has not ended yet
      ) %>%
      #add info if playmate is gaming at this moment
      left_join(gameplaying, by = c("to" = "ID")) %>%
      #keep necessary variables
      select(from, to, gameapp)

    #determine if this pupil is going to act
    # if a random proportion is smaller than its activity probability or, if not
    # playing yet, its activity probability plus a time bonus (higher activity at
    # break start)
    if (runif(1, 0, 1) < vertices_fixed_simulation[[pupil_act, "activity_play"]] |
        (nrow(pupil_playmates) == 0 & runif(1, 0, 1) < (vertices_fixed_simulation[[pupil_act, "activity_play"]] +
                                                        round(dlnorm(c((i - 1) * 0.05), meanlog = 0), digits = 3))) |
        nrow(pupil_playmates) > 4 #necessary?
    ) {
      # create data frame of all alters (potential new playmates and playmates to be dropped)
      alters <- data_frame(ID = 1:n_pupils) %>%
        # eliminate this pupil from the array
        filter(ID != pupil_act) # %>%
      # assign score to all alters (either for selecting new playmate or dropping current paymate):
      # - pupil's sociability score (interaction with pupil's popularity): from vertices_...
      # - pupil's friendship dependence score (interaction with friendship tie): from vertices_...
      # - friendship tie with pupil (positive): from pairs_const
      # - ethnicity homophily: from pairs_const
      # - adhd similarity: from pairs_const
      # - (time) number of days since last time played together (negative - positive - negative): from pairs_dyn
      # - number of shared playmates (closure): from pairs_dyn
      # - popularity (number of current playmates) of candidate pupil (positive): from pairs_dyn
      # helper data frame: playmate ties among all pupils
      pupils_playmates <- pairs_dyn %>%
        #select cases of playmates in current time
        filter(
          playmate == 1 & #event is playing together
            onset < i/n_pupils & #event started before current time point
            is.na(terminus) #event has not ended yet
        ) %>%
        #keep unique alters for each pupil; rename alter to tertius
        group_by(from, tertius = to) %>%
        summarise(.groups="drop")
      # add other tie characteristics and calculate attractiveness score of all alters
      alters <- pupils_playmates %>%
        # count number of shared playmates (closure) and add to alters
        # join head of first to tail of second to get playmates at two steps
        full_join(pupils_playmates, by = c("tertius" = "from")) %>%
        # select if head equals the selected pupil
        filter(from == pupil_act) %>%
        # count number of times Step-2 playmate (tertius.y) appears = number of shared playmates (tail)
        count(tertius.y) %>%
        # add to eligible playmates
        right_join(alters, by = c("tertius.y" = "ID")) %>%
        # if no shared playmates, n == NA, so set new variable to 0
        mutate(n_shared_playmates = ifelse(is.na(n), 0, n)) %>%
        # remove original count variable
        select(ID = tertius.y, n_shared_playmates) %>%
        #add this pupil to create (ordered) pairs
        mutate(
          from = pupil_act,
          to = ID #rename potential playmate variable
        ) %>%
        #add candidate's current number of playmates
        left_join(pupils_playmates, by = c("to" = "from")) %>%
        #count candidate's number of current playmates (tertius)
        group_by(from, to, n_shared_playmates) %>%
        summarise(popularity = sum(!is.na(tertius)), .groups="keep") %>%
        #add number of days since last played together from pairs_dyn (use only necessary variables)
        left_join(pairs_dyn[,c("from", "to", "onset", "lastplayed")], by = c("from", "to")) %>%
        #retain last (is current) value of lastplayed
        filter(!is.na(lastplayed)) %>%
        arrange(from, to, desc(onset)) %>%
        group_by(from, to, n_shared_playmates, popularity) %>%
        summarise(lastplayed = first(lastplayed), .groups="drop") %>%
        #add this pupil's friendship dependency and sociability (constants!)
        left_join(vertices_fixed_simulation, by = c("from" = "ID")) %>%
        #add friendship tie variable (and other fixed pair variables: similarities)
        left_join(pairs_const, by = c("from", "to")) %>%
        #add both gaming indicator variable
        left_join(gameplaying, by = c("from" = "ID")) %>%
        left_join(gameplaying, by = c("to" = "ID")) %>%
        mutate(bothgaming = ifelse(!is.na(gameapp.x) & !is.na(gameapp.y), 1, 0)) %>%
        #keep relevant variables
        select(-label, -degree, -gameapp.x, -gameapp.y) %>%
        #calculate attractiveness score of all eligible playmates
        #play around with the weights
        mutate( attractiveness =
                  # - pupil's sociability score (interaction with pupil's popularity)
                  # - popularity (number of current playmates) of candidate pupil (positive)
                  scale(sociability, center = TRUE, scale = FALSE) * popularity +
                  # - pupil's friendship dependence score (interaction with friendship tie)
                  # - friendship tie with pupil (positive)
                  5 * friend * frienddep +
                  # - ethnicity homophily
                  1.5 * simil_ethn +
                  # - adhd similarity
                  2 * simil_adhd +
                  # - (time) number of days since last time played together (negative - positive - negative)
                  -5 * I(lastplayed == 0) + -0.2 * lastplayed +
                  # - number of shared playmates (closure)
                  5 * n_shared_playmates +
                  # - additional attraction between gamers
                  3 * bothgaming
                #no random error! highest/lowest value is selected
        )
      # use sociability to determine if new playmate selected or playmate dumped
      if ( nrow(pupil_playmates) < 4 & #don't create new tie if ego already has 4+ playmates
           (
             nrow(pupil_playmates) == 0 | #no playmates or ...
             (max(alters$n_shared_playmates) > 0 & max(alters$n_shared_playmates) < 2) | #closure in small group
             runif(1, 0, 1) < vertices_fixed_simulation[[pupil_act, "sociability"]] #...sufficiently sociable
           )
      ){
        # select new playmate: alter with highest attractiveness, who is not already a playmate
        candidate_playmate <- alters %>%
          #remove current playmates
          anti_join(pupil_playmates, by=c("to" = "to")) %>%
          #select case with highest attractiveness
          slice_max(attractiveness, n = 1) %>%
          #add variables that appear in pairs_dyn (to which this case may be added)
          mutate(
            breakID = j,
            onset = i/n_pupils, #time as minute with decimal places
            terminus = NA, #event has just been started, not finished
            onset.censored = F, #event start observed
            terminus.censored = F, #event ending not (yet) forced
            playmate = 1, #start of (event =) playing together
            loudness = NA, #not an utterance
            negative = NA, #not an utterance
            smaht = NA, #not an utterance
            lastplayed = 0 #last played together: now (0 days before)
          )
        # add arc in opposite direction (playmate event is symmetrical)
        candidate_playmate <- candidate_playmate %>%
          #exchange head and tail
          rename(from = to, to = from) %>%
          #add to original row
          bind_rows(candidate_playmate)

        # get accepted as playmate
        # probability of candidate acceptance by intended playmate depends on (on average high prob):
        # - number of shared playmates: full acceptance for small group
        # - number of candidate's playmates (popularity curvilinear: high - low - high)
        if ( candidate_playmate$n_shared_playmates[1] < 4 & #do not add new tie in not so small play group
             (candidate_playmate$n_shared_playmates[1] > 0  | #always close small play group
              ((candidate_playmate$popularity[1] < 2) & (runif(1, 0, 1) < 0.8 )) | #high acceptance if no or 1 playmate
              ((candidate_playmate$popularity[1] >= 2) & (candidate_playmate$popularity[1] < 5) & (runif(1, 0, 1) < 0.2 )) #low acceptance
             )
        ) {
          # candidate accepts playing together
          pairs_dyn <- candidate_playmate %>%
            # remove variables that do not belong in pairs_dyn
            select(from, to, breakID:smaht, lastplayed) %>%
            #add rows
            bind_rows(pairs_dyn)
        }
      } else {
        # dump one playmate (there is at least one)
        # select playmate with lowest attractiveness
        dump_playmate <- alters %>%
          #keep current playmates (if any, result can be empty)
          semi_join(pupil_playmates, by=c("to" = "to")) %>%
          #select case with lowest attractiveness
          slice_min(attractiveness, n = 1)
        # dump this playmate: set terminus to current time
        pairs_dyn <- pairs_dyn %>%
          #set terminus time (terminus.censored is FALSE by default)
          mutate( terminus = ifelse(
            (from == pupil_act | to == pupil_act) & #this pupil is involved in the event as initial sender or target
              (to == dump_playmate$to[1] | from == dump_playmate$to[1]) & #
              playmate == 1 & #event is playing together
              onset < i/n_pupils & #event started before current time point (superfluous?)
              is.na(terminus), #event has not ended yet
            i/n_pupils, #tie to be closed (time = break minute with decimals)
            terminus #other ties, not to be changed
          )
          )
      } #end of add versus dump choice
    } else if (pupil_act %in% adopters$ID) {
      #pupil does not change playmates but pupil has gameapp, so may start/stop playing
      if (pupil_act %in% gameplaying$ID) {
        #if gaming, consider stop gaming if you are already gaming for a long time
        if (pupils_dyn$onset[
              pupils_dyn$ID == pupil_act & #for the currently selected pupil
              pupils_dyn$gameapp == 1 & #select the gameapp rows
              is.na(pupils_dyn$terminus) #with terminus still unset
              ] < i/n_pupils - 5 &  #if the start time is at least 5 minutes ago
            runif(1, 0, 1) < 0.1 #stop with as small probability
          ) {
          #stop gaming: set end time censoring and end time (NA if gaming is ongoing)
          pupils_dyn$terminus.censored[
            pupils_dyn$ID == pupil_act & #for the currently selected pupil
              pupils_dyn$gameapp == 1 & #select the gameapp rows
              is.na(pupils_dyn$terminus) #with terminus still unset
            ] <- FALSE
          pupils_dyn$terminus[
            pupils_dyn$ID == pupil_act & #for the currently selected pupil
            pupils_dyn$gameapp == 1 & #select the gameapp rows
            is.na(pupils_dyn$terminus) #with terminus still unset
            ] <- i/n_pupils
        }
      } else {
        #if not playing, start playing, especially if this is the first break the pupil has the app
        if (
          adopters$breakAdopt[adopters$ID == pupil_act] == j & #this is the pupil's first break with the app
          nrow(pupils_dyn[pupils_dyn$ID == pupil_act & pupils_dyn$gameapp == 1, ]) == 0 #and s/he hasn't gamed yet
          ) {
          #first break with game, so start playing: add row to pupils_dyn
          pupils_dyn <- pupils_dyn %>%
            bind_rows(
              data_frame(
                ID = pupil_act,
                breakID = j,
                onset = i/n_pupils,
                onset.censored = F,
                terminus = NA,
                terminus.censored = NA,
                gameapp = 1
              )
            )
        } else if (nrow(pupil_playmates) > 0) {
          #pupil has playmates, start gaming if any is gaming (this is not the first break))
          if (sum(!is.na(pupil_playmates$gameapp)) > 0) {
            #start playing: add row to pupils_dyn
            pupils_dyn <- pupils_dyn %>%
              bind_rows(
                data_frame(
                  ID = pupil_act,
                  breakID = j,
                  onset = i/n_pupils,
                  onset.censored = F,
                  terminus = NA,
                  terminus.censored = NA,
                  gameapp = 1
                )
              )
          }
        } else if (runif(1, 0, 1) < 0.5) {
          #small probability of starting gaming: add row to pupils_dyn
          pupils_dyn <- pupils_dyn %>%
            bind_rows(
              data_frame(
                ID = pupil_act,
                breakID = j,
                onset = i/n_pupils,
                onset.censored = F,
                terminus = NA,
                terminus.censored = NA,
                gameapp = 1
              )
            )
        }
      } #end of if not playing loop
    } #end of if-else loop: act?
  } #end of loop: for each time point

  #terminate all 'open' playmate events with terminus.censored set to TRUE (play
  # ended by school bell / break end)
  pairs_dyn <- pairs_dyn %>%
    mutate(
      terminus.censored = ifelse(is.na(terminus), TRUE, FALSE),
      terminus = ifelse(is.na(terminus), i/n_pupils, terminus)
    )
  #terminate all 'open' gaming rows
  pupils_dyn <- pupils_dyn %>%
    mutate(
      terminus.censored = ifelse(is.na(terminus), TRUE, FALSE),
      terminus = ifelse(is.na(terminus), i/n_pupils, terminus)
    )

}

#cleanup
rm(adoption, gameplaying)
rm(adopters, riskset)

#visualize the network for break 2
#remove cases for onset == 0 (initial situation) from pairs_dyn
#library(ndtv)
edges2 <- pairs_dyn %>%
  filter(onset > 0 & playmate == 1 & from < to & breakID == 2) %>%
  #make vertex IDs integers
  mutate(head = as.integer(from), tail = as.integer(to)) %>%
  #and reorder for display with network & ndtv
  select(head, tail, breakID:lastplayed)

#visually inspect playmates network (in one break?)
#create base network (undirected)
break2 <- network(edges2, vertex.attr = pupils, matrix.type = "edgelist",
                  directed = F, loops = F, multiple = F,
                  ignore.eval = F #otherwise, edge attributes are not accessible
)
break2 %v% "sexcolor" <- colors[break2 %v% "sex" + 1]
# plot(break1, vertex.col="sexcolor") #check
# NOTE: networkDynamic() cannot handle tibbles.
break2_dyn <- networkDynamic(
  base.net = break2,
  vertex.spells = as.data.frame(pupils[,c(4,5,1)]),
  edge.spells = as.data.frame(edges2[,c(4,5,1,2)])
)
#static plot
plot(break2_dyn, vertex.col = break1 %v% "sexcolor", displaylabels = T)
# film strip
# note: enlarge Plots panel to avoid error 'margins too large'
# filmstrip(break1_dyn, displaylabels=F, mfrow=c(1, 5),
#           slice.par=list(start=1, end=30, interval=6,
#                          aggregate.dur=6, rule='any'))
# movie
# to speed up, calculate animated layout first
# note: with end=29, last frame shows change from 29 to 30
compute.animation(break2_dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=29, interval=1,
                                 aggregate.dur=1, rule='any'))

render.d3movie(break2_dyn, usearrows = F,
               displaylabels = F,
               vertex.col = break2 %v% "sexcolor",
               launchBrowser=T, filename="Break2-Network-Dynamic.html",
               render.par=list(tween.frames = 30, show.time = T),
               plot.par=list(mar=c(0,0,0,0)) )

#Analyze gameapp adoption ####
#create table of adoption times
adopters <- pupils_dyn %>%
  #select gameapp use cases
  filter(gameapp == 1) %>%
  #select earliest break with game use as adoption break for each pupil
  group_by(ID) %>%
  summarise(breakAdopt = min(breakID))
  #note: non-adopters are not included
ggplot(adopters, aes(x = breakAdopt)) + #rising number of adopters in breaks 1-3,fewer in 4, 2 laggards in 8
  geom_bar()
nrow(pupils) - nrow(adopters) #only 1 pupil has not adopted in the end (break 10; right-censored)

#calculate exposure to gaming as time (in minutes) spent with a gaming playmate
# in the preceding break (can be over 30 if pupil is exposed to more than one gamer at a time)
exposure <- pairs_dyn %>%
  #select the playmate ties in all breaks
  filter(playmate == 1) %>%
  #add the gaming times within the break to the receiver (to) of the playmate tie
  left_join(
    pupils_dyn,
    by = c("to" = "ID", "breakID" = "breakID")
  ) %>%
  #select cases where 'from' is gaming during the playmate tie
  filter(
    onset.x < terminus.y & #playing must start before end of gaming
      terminus.x > onset.y   #and playing must end after start of gaming
  ) %>%
  #calculate shared time (playing while from is gaming)
  mutate(duration =
           ifelse(terminus.x < terminus.y, terminus.x, terminus.y) - #smallest end time minus
           ifelse(onset.x > onset.y, onset.x, onset.y)) %>% #largest starting time
  #sum exposure time for each 'from'
  group_by(from, breakID) %>%
  summarise(exposure = sum(duration, na.rm = T), .groups="drop") %>%
  #add 1 to breakID: exposure in break 1 must predict adoption in break 2
  mutate(breakID = breakID + 1)
#create cases for discrete event history model
diffusion_data <- pupils %>%
  #create a case for every alter-break combination
  full_join(
    data_frame(breakID = 1:n_breaks), #new data frame: 1 variable, 10 cases, values 1 to 10
    by = character()
  ) %>%
  #add adoption times
  left_join(adopters, by = "ID") %>%
  #remove cases for breaks after the adoption break (pupil no longer at risk to adopt)
  filter(breakID <= breakAdopt | is.na(breakAdopt)) %>%
  #create dependent variable: 0 for not yet adopted, 1 for adopted
  mutate(adoption = case_when(
    breakID == breakAdopt ~ 1,
    TRUE ~0
    )
  ) %>%
  #add exposure score (in preceding break) to pupils (if any)
  left_join(exposure, by = c("ID" = "from", "breakID" = "breakID")) %>%
  #set missing exposure values to zero: no exposure
  mutate(exposure = ifelse(is.na(exposure), 0, exposure))


#time functions
model_diffusion.linear <- glm(
  adoption ~ breakID,
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion.linear)
model_diffusion.quadratic <- glm(
  adoption ~ breakID + I(breakID^2),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion.quadratic)
model_diffusion.cubic <- glm(
  adoption ~ breakID + I(breakID^2) + I(breakID^3),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion.cubic)
model_diffusion.quart <- glm(
  adoption ~ breakID + I(breakID^2) + I(breakID^3) + I(breakID^4),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion.quart)
#collect (average) predicted baseline functions and graph them
diffusion_data %>%
  #calculate observed probability as proportion of adoptions per break
  group_by(breakID) %>%
  summarise(obs_prop = mean(adoption)) %>%
  #add predictions with quadratic function
  bind_cols(
    lin_prop = predict.glm(model_diffusion.linear, newdata = data.frame(breakID = 1:10), type = "response"),
    quad_prop = predict.glm(model_diffusion.quadratic, newdata = data.frame(breakID = 1:10), type = "response"),
    cubic_prop = predict.glm(model_diffusion.cubic, newdata = data.frame(breakID = 1:10), type = "response"),
    quart_prop = predict.glm(model_diffusion.quart, newdata = data.frame(breakID = 1:10), type = "response")
  ) %>%
  #plot
  ggplot(aes(x = breakID)) +
    geom_step(aes(y = obs_prop), color = "grey") +
    geom_line(aes(y = lin_prop), color = "darkgrey") +
    geom_line(aes(y = quad_prop), color = "blue") +
    geom_line(aes(y = cubic_prop), color = "black") +
    geom_line(aes(y = quart_prop), color = "lightblue") +
    scale_x_continuous(name = "Break", breaks = 1:n_breaks, minor_breaks = NULL) +
    theme_bw()
#no model fits nicely; quadratic model seems to be fine (AIC hardly higher than cubic)

#effect of exposure; sqrt() transformation of exposure score has better fit than raw exposure scores
# linear
model_diffusion_const_time <- glm(
  adoption ~ sqrt(exposure),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion_const_time)
# linear
model_diffusion_lin_time <- glm(
  adoption ~ breakID + sqrt(exposure),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion_lin_time)
# quadratic
model_diffusion_quad_time <- glm(
  adoption ~ breakID + I(breakID^2) + sqrt(exposure),
  data = diffusion_data,
  family = binomial(link = "logit")
)
summary(model_diffusion_quad_time)

# milestone
# copy pairs_dyn for backup
pairs_dyn_diffusion <- pairs_dyn

#save workspace
save.image("playmates_diffusion.RData")




# PROBLEM: `ndtv::` (and `networkDynamic::`) ####
# seems not to be able to handle
# handle a vertex.spells table with repeated rows for vertices (to define
# dynamic node attributes).
# In addition, it cannot move from (instantaneous) event to event.
# Finally, it seems unable to display dynamic vertex colours.
#visually inspect playmates network (in Break 1)
#first add loudness property to pupils
vertices <- pupils %>%
  mutate(loudness = 0)
#create base network (undirected)
break1 <- network(edges, vertex.attr = vertices, matrix.type = "edgelist",
                  directed = F, loops = F, multiple = F,
                  ignore.eval = F #otherwise, edge attributes are not accessible
)
# add loudness (on/off) colors to base network
colors_stress <- c("gray", "black") # select two colors (numbered 1 and 2)
break1 %v% "loudnesscolor" <- colors_stress[break1 %v% "loudness" + 1]
break1 %v% "sexcolor" <- colors[break1 %v% "sex" + 1]
#plot(break1, vertex.col="loudnesscolor") #check
# create
break1_dyn2 <- networkDynamic(
  base.net = break1,
  vertex.spells = as.data.frame(pupils[,c(4,5,1)]),
  edge.spells = as.data.frame(edges[,c(4,5,1,2)])
)
# add loudnesscolor values to the vertices, such that a color remains until next
# vertex changes
# activate.vertex.attribute() cannot assign a dyn attribute to different instances of the same case
# set all loudnesscolor until first vertex event
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "grey",
                          onset=0, terminus=speech_dyn$onset[1])
for (i in 1:(nrow(speech_dyn) - 1)) {
# set loudnesscolor for this utterance until next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "tomato",
                          onset=speech_dyn$onset[i],
                          terminus=speech_dyn$onset[i + 1],
                          v=speech_dyn$from[i])
# set all loudnesscolors between this and next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "grey",
                          onset=speech_dyn$onset[i],
                          terminus=speech_dyn$onset[i + 1],
                          v=pupils$ID[pupils$ID != speech_dyn$from[i]])
}
# set loudnesscolor for last utterance until end
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "tomato",
                          onset=speech_dyn$onset[nrow(speech_dyn)],
                          terminus=30 * n_pupils,
                          v=speech_dyn$from[nrow(speech_dyn)])
# set all loudnesscolors between this and next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "grey",
                          onset=speech_dyn$onset[nrow(speech_dyn)],
                          terminus=30 * n_pupils,
                          v=pupils$ID[pupils$ID != speech_dyn$from[nrow(speech_dyn)]])

#checks
get.vertex.attribute.active(break1_dyn2, "loudnesscolor",
                            onset = speech_dyn$onset[3]+0.01,
                            terminus = speech_dyn$onset[3]+0.14,
                            rule = "earliest")
#deactivate.vertex.attribute(break1_dyn2, "loudnesscolor")
#static plot (works)
plot(break1_dyn2, vertex.col = break1_dyn2 %v% "loudnesscolor.active")

# movie (does not work)
# to speed up, calculate animated layout first
# note: with end=29, last frame shows change from 29 to 30
compute.animation(break1_dyn2, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=29, interval=1,
                                 aggregate.dur=1, rule='earliest'))
# still does not display dynamic vertex colors...
render.d3movie(break1_dyn2, usearrows = F,
               displaylabels = F,
               vertex.col = break1_dyn2 %v% "loudnesscolor.active",
               launchBrowser=T, filename="Break1-Network-Dynamic2.html",
               render.par=list(tween.frames = 30, show.time = T),
               plot.par=list(mar=c(0,0,0,0)) )
