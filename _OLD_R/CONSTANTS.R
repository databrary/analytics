# constants.R

NON_INSTITUTIONS = c(2, 9, 10, 15, 8545, 8717, 8720, 9238, 10551, 12569)

weekly_interval <- 1
weekly <- "weeks"
quarterly_interval <- 13 #(weeks)

max_volume_id <- 1520 # TODO: Need to find a better way to handle this

select_tags <-
  c(
    'numerical cognition',
    'mathematical ability',
    'number',
    'teaching',
    'teaching clips',
    'abacus',
    'classroom',
    'mathematical equivalence',
    'mental arithmetic',
    'number comprehension',
    'number discrimination',
    'science',
    'stem',
    'statistical learning',
    'school readiness'
  )

FIGSHARE_TB <- 3500 #https://knowledge.figshare.com/plus
DRYAD_TB <- 5000 #https://datadryad.org/stash/faq
