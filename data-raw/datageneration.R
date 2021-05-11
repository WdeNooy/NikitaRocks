# Generating the data.

library(tidyverse)
library(randomNames)
library(igraph)
library(knitr)

# Constants ####
n_pupils = 26

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
  select(ID, label, present, onset, terminus, sex, ethnicity, adhd)

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
             1 / (0.05 + abs(adhd.x - adhd.y)) + #adhd similarity
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
    simil_adhd = 0.5 / (0.05 + abs(adhd.x - adhd.y)),
    friend = ifelse(is.na(friend) & is.na(friend), 0, 1)
    ) %>%
  #keep relevant variables
  select(from = ID.x, to = ID.y, friend:simil_adhd)
# cleanup
rm(friends, l)

# Generate breaks ####
# 5 days (numbered) with two breaks each (morning and afternoon).
breaks <- data_frame(
  breakID = 1:10,
  day = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  period = rep(c(0, 1), 5) # 0 = morning break, 1 = afternoon break
)

# Generate dynamic dyadic data: ####
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
    smaht = NA_integer_, #utterance includes 'wicked smaht': (0 = no, 1 = yes, NA = no utterance); to = NA if utterance without recipient
    gameapp = 0 #tail/sender uses new gameapp (0 = no, 1 = yes); target = nil if app used alone
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
  select(from:gameapp, lastplayed)

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
    frienddep = runif(n_pupils, 0, 1)
  ) %>%
  #keep relevant variables
  select(ID, label, degree:frienddep)
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
       round(dlnorm(c((i - 1) * 0.05), meanlog = 0), digits = 3)))) {
    # create data frame of al alters (potential new playmates and playmates to be dropped)
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
          0.5 * simil_ethn +
          # - adhd similarity
          simil_adhd +
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
          gameapp = 0, #not playing together with the new game app
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
          select(from, to, breakID:gameapp, lastplayed) %>%
          #add rows
          bind_rows(pairs_dyn)
      }
    } else {
      # dump one playmate (there is at least one)
        # select playmate with lowest attractiveness
        dump_playmate <- alters %>%
          #keep current playmates (if any, result can be empty)
          semi_join(pupil_playmates, by=c("to" = "to")) %>%
          #select case with highest attractiveness
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
    terminus = ifelse(is.na(terminus), i/n_pupils, terminus),
    terminus.censored = TRUE
    )

#go to next break, adjust current_break and add 2 days (weekend) to lastplayed for all pupils?

#cleanup
rm(alters, candidate_playmate, dump_playmate, pupil_playmates, pupils_playmates, i, pupil_act)

#remove cases for onset == 0 (initial situation) from pairs_dyn
library(ndtv)
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
plot(break1_dyn, vertex.col = break1 %v% "sexcolor")
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
#    - reciprocity: sender of incoming utterance: high selection probability
#    - reciprocity by utterance negativity interaction: even higher probability
#    - higher probability for adhd similarity
#    -
# c. what is the nature of the action? loudness, negativity (negative vs. neu/pos), using 'smaht' expression for predicting event (tie) value
#    IVs:
#    loudness:
#    - pupil-specific loudness level (weakly correlated with adhd level)
#    - louder for higher overall loudness level
#    - loudness reciprocity (direct addressee)
#    negativity (negative vs. neu/pos):
#    - pupil-specific negativity level (weakly neg correlated with number of friends)
#    - friends: less negativity, more positivity
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
        is.na(terminus) #event has not ended yet
    ) %>%
    #keep necessary variables
    select(from, to, playmate)
  # add loop for last vertex, so all vertices are always present in the network
  edges_play <- pupils_playmates %>%
    bind_rows(data_frame(from = n_pupils, to = n_pupils))
  # create network
  net_play <- network(edges_play, vertex.attr = pupils, matrix.type = "edgelist",
                        directed = T, loops = F, multiple = F, ignore.eval = T)
  # extract vector of component membership for al pupils
  # note: each isolate has its own component
  play_component <- component.dist(net_play)$membership

  #determine if there was an utterance in the preceding n_pupils time points (1 minute)
  play_group_utterances <- pairs_dyn  %>%
    #filter relevant utterances
    filter(
      from != pupil_act & #disregard selected pupil's own utterances
      !is.na(loudness) & #event must be utterance
      i < (onset + 1)*n_pupils & #utterance must have started less than a minute before the current time
      play_component[from] == play_component[pupil_act] #utterance must be by playgroup mate of selected pupil
    )

  #collect utterances addressing the selected pupil in the preceding n_pupils time points (1 minute)
  received_utterances <- test %>% #26 pairs_dyn  %>%
    #filter relevant utterances
    filter(
      to == pupil_act & #selected pupil was addressee
      !is.na(loudness) & #event must be utterance
      i < (onset + 1)*n_pupils #utterance must have started less than a minute before the current time
    )

  #determine if this pupil is going to act
  #    IVs:
  #    - pupil-specific speech activity level = adhd level
  #    - playmates group(?) activity: saying something because someone in your group said something
  #    - reciprocity: higher probability of answering if you are addressed
  if (
    #adhd activity yields probabilities between 0.2 and 0.8 = 6 to 24 realized actions per 30 min break
    (2 + pupils[[pupil_act, "adhd"]])/(max(pupils$adhd) + 4) > runif(1, 0, 1) |
    #utterance in group: 60% probability
    (nrow(play_group_utterances) > 0 & runif(1, 0, 1) < 0.6) |
    #reciprocity: 80% probability
    (nrow(received_utterances) > 0 & runif(1, 0, 1) < 0.8)
    ) {
    #debug: add a row (asymmetric) with loudness set at 1 to check activity level
    pairs_dyn <- pairs_dyn %>%
      bind_rows(
        data_frame(
          from = pupil_act,
          to = NA, #no addressee of the utterance
          breakID = current_break, #start with first break
          onset = i/n_pupils, #event start time (in minutes per break)
          terminus = i/n_pupils, #event end time (assumed to be the same as onset)
          onset.censored = FALSE, #`ndtv` event start unobserved
          terminus.censored = FALSE, #`ndtv` event end unobserved (externally forced)
          playmate = 0, #pair are not playmates (0 = no, 1 = yes)
          loudness = 1, #loudness of utterance: NA = no utterance
          negative = NA_integer_, #utterance valence towards recipient: 1 = negative, 0 = neutral/positive, NA = no utterance
          smaht = NA_integer_, #utterance includes 'wicked smaht': (0 = no, 1 = yes, NA = no utterance); target = nil if utterance without addressee
          gameapp = 0 #sender (from) uses new gameapp (0 = no, 1 = yes); to = NA if app used alone
        )
      )


  } #end of yes/no act loop

} #end of time loop

#debug: describe frequencies of utterances
pupils_dyn <- pairs_dyn %>% filter(is.na(to))
hist(pupils_dyn$from) #1-26 utterances per pupil, quite uniformly distributed
hist(pupils_dyn$onset) #quite uniform distribution over time
pupils_dyn %>% #many very short gaps (< 2 minutes), few very long gaps (about 17 minutes)
  #calculate gap between successive utterances by a pupil
  arrange(from, onset) %>%
  group_by(from) %>%
  mutate(gap = onset - lag(onset)) %>%
  filter(!is.na(gap)) %>%
  ggplot(aes(x = gap)) +
    geom_histogram(bins = 18)

# PROBLEM: `ndtv::` (and `networkDynamic::`) seem not to be able to handle
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
                          onset=0, terminus=pupils_dyn$onset[1])
for (i in 1:(nrow(pupils_dyn) - 1)) {
# set loudnesscolor for this utterance until next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "tomato",
                          onset=pupils_dyn$onset[i],
                          terminus=pupils_dyn$onset[i + 1],
                          v=pupils_dyn$from[i])
# set all loudnesscolors between this and next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "grey",
                          onset=pupils_dyn$onset[i],
                          terminus=pupils_dyn$onset[i + 1],
                          v=pupils$ID[pupils$ID != pupils_dyn$from[i]])
}
# set loudnesscolor for last utterance until end
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "tomato",
                          onset=pupils_dyn$onset[nrow(pupils_dyn)],
                          terminus=30 * n_pupils,
                          v=pupils_dyn$from[nrow(pupils_dyn)])
# set all loudnesscolors between this and next utterance
activate.vertex.attribute(break1_dyn2, "loudnesscolor", "grey",
                          onset=pupils_dyn$onset[nrow(pupils_dyn)],
                          terminus=30 * n_pupils,
                          v=pupils$ID[pupils$ID != pupils_dyn$from[nrow(pupils_dyn)]])

#checks
get.vertex.attribute.active(break1_dyn2, "loudnesscolor",
                            onset = pupils_dyn$onset[3]+0.01,
                            terminus = pupils_dyn$onset[3]+0.14,
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
