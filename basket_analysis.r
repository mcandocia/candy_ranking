## basket_analysis.r
# Max Candocia - maxcandocia@gmail.com
# 2020-11-12
#
# analyze correlations between candies

library(tidyverse)
library(wCorr)
source('voting_functions.r')
source('raking.r')
source('constants.r')

dir.create('figure_basket')
bpng <- function(x, ...){
  png(file.path(
    'figure_basket',x
  ),...)
}

wcor <- function(x, ...){
  xvars = names(x)
  nvars = length(xvars)
  cormat = matrix(nrow=nvars, ncol=nvars)
  for (i in 1:(nvars-1)){
    for (j in (i+1):nvars){
      if (i == j){
        cormat[i,j] = 1
      } else{
        cormat[i,j] = cormat[j,i] = weightedCorr(
          x=x[,i,T],
          y=x[,j,T],
          ...
        )
      }
    }
  }
  colnames(cormat) = rownames(cormat) = xvars
  diag(cormat)=1
  return(cormat)
}


candy_df = read_csv(
  'data/candy_cleaned.csv'
) %>%
  rename_at(
    vars(contains('Three Musketeers')),
    function(x) gsub('Three','3', x)
  )

candy_df$weight = rake_data(
  as.data.frame(candy_df),
  c('Gender','AgeGroup','State'),
  demographic_stats,
  max_iter = 5,
  verbose=F
)

# fix missing levels of rankings
candy_df = shift_rankings(candy_df)

# fill in (like = 6; none = 7, dislike = 8)
filled_candy_df = candy_df

rank_candies = gsub('Rank ', '', names(candy_df)[grepl('^Rank', names(candy_df))])

for (candy in rank_candies){
  v_rank = paste('Rank', candy)
  v_like = paste('Like', candy)

  filled_candy_df[,v_rank] = case_when(
    !is.na(filled_candy_df[,v_rank,T]) ~ filled_candy_df[,v_rank,T],
    is.na(filled_candy_df[,v_like,T]) ~ 7,
    TRUE ~ 7 - filled_candy_df[,v_like,T]
  )
}

p_cormat = wcor(
  filled_candy_df %>%
    select_at(vars(matches('^Rank'))) %>%
    rename_all(function(x) gsub('Rank ','', x)),
  weights=filled_candy_df$weight,
  method='Pearson'
)

s_cormat = wcor(
  filled_candy_df %>%
    select_at(vars(matches('^Rank'))) %>%
    rename_all(function(x) gsub('Rank ','', x)),
  method='Spearman',
  weights=filled_candy_df$weight
)

pc_cormat = wcor(
  filled_candy_df %>%
    select_at(vars(matches('^Rank'))) %>%
    rename_all(function(x) gsub('Rank ','', x)),
  method='Polychoric',
  weights=filled_candy_df$weight
)

p_clusts = hclust(
  as.dist(1-p_cormat),
  method='ward.D2'
)
p_cuts = cutree(p_clusts, k=13)

s_clusts = hclust(
  as.dist(1-s_cormat),
  method='ward.D2'
)
s_cuts = cutree(s_clusts, k=12)

pc_clusts = hclust(
  as.dist(1-pc_cormat),
  method='ward.D2'
)
pc_cuts = cutree(pc_clusts, k=12)

pc_cuts_wide = cutree(pc_clusts, k=5)

plot(ape::as.phylo(p_clusts), type='fan',
     tip.color=RColorBrewer::brewer.pal(8, 'Dark2')[(p_cuts %% 8 + 1)])

plot(ape::as.phylo(s_clusts), type='fan',
     tip.color=RColorBrewer::brewer.pal(8, 'Dark2')[(s_cuts %% 8 + 1)])

# use polychoric (best for statistical case)
plot(ape::as.phylo(pc_clusts),
     tip.color=RColorBrewer::brewer.pal(8, 'Dark2')[(pc_cuts %% 8 + 1)])


cor_df = bind_rows(
  melt(p_cormat) %>% mutate(method='Pearson'),
  melt(s_cormat) %>% mutate(method='Spearman'),
  melt(pc_cormat) %>% mutate(method='Polychoric')
) %>%
  rename(
    Candy1 = Var1, Candy2=Var2
  ) %>%
  mutate_at(1:2, function(x) gsub('Rank ', '', x)) %>%
  mutate_at(
    1:2,
    function(x) factor(x, levels=rank_candies[pc_clusts$order])
  )

# for each candy, get order and group in DF
# group by group, then calculate min and max of each group

pc_groups = data.frame(
  candies = rank_candies[pc_clusts$order]
) %>%
  mutate(
    grp = pc_cuts[candies]
  ) %>%
  group_by(
    grp
  ) %>%
  summarize(
    min_candy=candies[1],
    max_candy=candies %>% tail(1),
    .groups='drop'
  ) %>%
  mutate_if(is.character, function(x) factor(x, levels=levels(cor_df$Candy1)))

pc_groups_wide = data.frame(
  candies = rank_candies[pc_clusts$order]
) %>%
  mutate(
    grp = pc_cuts_wide[candies]
  ) %>%
  group_by(
    grp
  ) %>%
  summarize(
    min_candy=candies[1],
    max_candy=candies %>% tail(1),
    .groups='drop'
  ) %>%
  mutate_if(is.character, function(x) factor(x, levels=levels(cor_df$Candy1)))

# figure out which candies are not desirable
bad_freqs = filled_candy_df %>%
  summarize_at(
    vars(matches('Rank ')),
    list(~weighted.mean((.)==8, weight) - weighted.mean((.)<=6, weight))
  ) %>%
  melt() %>%
  transmute(
    Candy=gsub('Rank ','', variable) %>% factor(levels=levels(cor_df$Candy1)),
    value
  ) %>%
  filter(
    value > 0
  )

good_freqs = filled_candy_df %>%
  summarize_at(
    vars(matches('Rank ')),
    list(~weighted.mean((.)==8, weight) - weighted.mean((.)<=6, weight))
  ) %>%
  melt() %>%
  transmute(
    Candy=gsub('Rank ','', variable),
    value=-value
  )  %>%
  arrange(
    desc(value)
  ) %>%
  mutate(
    Candy=forcats::as_factor(Candy),
    Likeability = case_when(
      value < -0.3 ~ 'Widely Disliked',
      value < -0.05 ~ 'Slightly Disliked',
      value < 0.05 ~ 'Neutral',
      value < 0.3 ~ 'Slightly Liked',
      TRUE ~ 'Well-Liked'
    ) %>%
      factor(levels=c('Well-Liked','Slightly Liked','Neutral','Slightly Disliked','Widely Disliked'))
  )



ggplot(cor_df %>% filter(method=='Polychoric')) +
  geom_tile(
    aes(x=Candy1, y=Candy2, fill=value)
  ) +
  scale_fill_gradientn(
    'Correlation',
    colors=cetcolor::cet_pal(7, 'cbd1'),
    limits=c(-1,1)
  ) +
  geom_rect(
    data=pc_groups,
    aes(xmin=min_candy %>% as.numeric() - 0.5,
        ymin=min_candy %>% as.numeric() - 0.5,
        xmax=max_candy %>% as.numeric() + 0.5,
        ymax=max_candy %>% as.numeric() + 0.5),
    color='red',
    fill=NA
  ) +
  geom_rect(
    data=pc_groups_wide,
    aes(xmin=min_candy %>% as.numeric() - 0.5,
        ymin=min_candy %>% as.numeric() - 0.5,
        xmax=max_candy %>% as.numeric() + 0.5,
        ymax=max_candy %>% as.numeric() + 0.5),
    color='purple',
    fill=NA
  ) +
  geom_vline(data=pc_groups, aes(xintercept=min_candy %>% as.numeric() - 0.5),
             color='black', lty='dashed', size=0.2) +
  geom_hline(data=pc_groups, aes(yintercept=min_candy %>% as.numeric() - 0.5),
             color='black', lty='dashed', size=0.2) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  xlab('') + ylab('') + ggtitle('Correlations of Candy Preferences') +
  labs(caption='source: https://maxcandocia.com/article/2020/Nov/13/candy-bundles')

ggplot(bad_freqs %>% arrange(desc(value))) +
  geom_bar(aes(x=Candy %>% as.character() %>% forcats::as_factor(), y=value),
           stat='identity',
           fill='orange') +
  theme_bw() +
  scale_y_continuous(label=scales::percent) +
  xlab('Candy') +
  ylab('Dislike % - Like %') +
  ggtitle('Least Desirable Candies in US')

ggplot(good_freqs %>% arrange(desc(value))) +
  geom_bar(aes(x=Candy, y=value, fill=Likeability),
           stat='identity') +
  theme_bw() +
  xlab('Candy') +
  ylab('Like % - Disike %') +
  ggtitle('Candies in the US Arranged by Likeability',
          subtitle = 'based on percentage of individuals that like a candy minus \nthe percentage that dislike it') +
  coord_flip() +
  scale_fill_manual(
    values=(hcl.colors(5, palette='Plasma') %>% rev())
  ) +
  scale_x_discrete(limits=rev(levels(good_freqs$Candy))) +
  scale_y_reverse(label=scales::percent, breaks=seq(-0.6,1,0.2)) +
  theme(plot.subtitle=element_text(size=rel(0.65)), plot.caption=element_text(hjust=0.5)) +
  labs(caption='source: https://maxcandocia.com/article/2020/Nov/13/candy-bundles')
